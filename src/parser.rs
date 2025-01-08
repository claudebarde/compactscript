extern crate swc_common;
extern crate swc_ecma_parser;
use std::collections::HashMap;
use std::fs::File;
use std::io::Write;
use std::path::Path;
use std::vec;
use swc_common::sync::Lrc;
use swc_common::SourceMap;
use swc_ecma_ast::{
    Accessibility, ClassDecl, ClassProp, ClassMethod, Decl, Expr, ExprStmt, Lit, ModuleDecl, ModuleItem, PropName, Stmt, TsLit, TsType, TsTypeRef, TsKeywordTypeKind
};
use swc_ecma_parser::{Parser, StringInput, Syntax};

const DEFAULT_UINT_RANGE: usize = 32;

enum ExecContext {
    ContractInit,
    LedgerInit,
}

#[derive(Debug, Clone)]
enum CompactType {
    Counter,
    Cell(Box<CompactType>),
    Bytes(usize),
    Uint(usize),
    Void
}
impl CompactType {
    fn from_str(type_name: &str, arg: Option<String>) -> Result<Self, String> {
        match arg {
            None => match type_name {
                "Counter" => Ok(CompactType::Counter),
                _ => Err(format!("Unknown ledger type with 0 argument: {}", type_name)),
            },
            Some(arg) => match type_name {
                "Bytes" => {
                    if let Ok(size) = arg.parse::<usize>() {
                        Ok(CompactType::Bytes(size))
                    } else {
                        Err(format!("Invalid argument for Bytes: {}", arg))
                    }
                }
                "Uint" => {
                    if let Ok(size) = arg.parse::<usize>() {
                        Ok(CompactType::Uint(size))
                    } else {
                        Err(format!("Invalid argument for Uint: {}", arg))
                    }
                }
                "Cell" => match CompactType::from_str(&arg, None) {
                    Ok(subtype) => Ok(CompactType::Cell(Box::new(subtype))),
                    Err(e) => Err(e),
                },
                _ => Err(format!("Unknown ledger type with 1 argument: {}", type_name)),
            },
        } 
    }

    fn print(&self) -> Result<String, String> {
        match self {
            CompactType::Counter => Ok("Counter".to_string()),
            CompactType::Cell(subtype) => {
                let printed_subtype = subtype.print()?;
                Ok(format!("Cell<{}>", printed_subtype))
            }
            CompactType::Bytes(range) => Ok(format!("Bytes<{}>", range)),
            CompactType::Uint(size) => Ok(format!("Uint<{}>", size)),
            CompactType::Void => Ok("Void".to_string())
        }
    }
}

#[derive(Debug, Clone)]
enum CompactValue {
    Counter(usize),                             // value
    Cell(Box<CompactValue>),      // value type, value
    Bytes(String),                               // range, value
    Uint(usize),                               // range, value
}

#[derive(Debug, Clone)]
struct CompactVar {
    kind: CompactType,
    name: String,
    value: Option<CompactValue>,
    is_ledger_value: bool,
}

#[derive(Debug, Clone)]
struct CompactLedger {
    props: HashMap<String, CompactVar>, // name, value
}
impl CompactLedger {
    fn add_prop(&mut self, prop_name: &str, prop_val: CompactVar) -> Result<(), String> {
        match self.props.get(prop_name) {
            None => {
                self.props.insert(prop_name.to_string(), prop_val);
                return Ok(());
            }
            Some(_) => return Err(format!("Ledger property already exists: {}", prop_name)),
        }
    }
    
    fn update_prop(&mut self, prop_name: &str, new_val: CompactVar) -> Result<(), String> {
        match self.props.get(prop_name) {
            None => return Err(format!("Ledger property not found: {}", prop_name)),
            Some(_) => {
                self.props.insert(prop_name.to_string(), new_val);
                return Ok(());
            },
        }
    }
}

#[derive(Debug, Clone)]
enum ContractItemKind {
    CompilerVersion(String),
    LanguageVersion(String),
    StdImport,
    ExportLedgerValue((String, CompactType)),
    LedgerConstructor,
    LedgerPropInit(CompactVar),
    CircuitExport(String), // name of the circuit
    CircuitInternal(String), // name of the circuit
    CircuitParam(String, CompactType), // name, type
    CircuitReturnType(CompactType), // type
    Empty, // some TS code doesn't yield any Compact code
}

#[derive(Debug, Clone)]
struct ContractItem {
    kind: ContractItemKind,
    children: Vec<ContractItem>,
    indent: usize,
}
impl ContractItem {
    fn new(kind: ContractItemKind, indent: usize, children: Vec<ContractItem>) -> Self {
        ContractItem {
            kind,
            children: children,
            indent,
        }
    }

    fn print(&self) -> Result<String, String> {
        let mut output = String::from("");
        let indent_str = "  ".repeat(self.indent);
        match &self.kind {
            ContractItemKind::CompilerVersion(version) => {
                let print = format!(
                    "{}{}pragma compiler_version >= {};\n",
                    output, indent_str, version
                );
                output.push_str(&print);
            }
            ContractItemKind::LanguageVersion(version) => {
                let print = format!(
                    "{}{}pragma language_version >= {};\n",
                    output, indent_str, version
                );
                output.push_str(&print);
            }
            ContractItemKind::StdImport => {
                let print = format!("{}{}import CompactStandardLibrary;\n", output, indent_str);
                output.push_str(&print);
            }
            ContractItemKind::ExportLedgerValue((name, ledger_type)) => {
                let print = match ledger_type {
                    CompactType::Counter => {
                        format!("{}{}export ledger {}: Counter;", output, indent_str, name)
                    }
                    CompactType::Cell(subtype) => {
                        let printed_subtype = subtype.print()?;
                        format!("{}{}export ledger {}: Cell<{}>;", output, indent_str, name, printed_subtype)
                    }
                    CompactType::Bytes(range) => {
                        format!("{}{}export ledger {}: Bytes<{}>;", output, indent_str, name, range)
                    }
                    CompactType::Uint(range) => {
                        format!("{}{}export ledger {}: Uint<{}>;", output, indent_str, name, range)
                    }
                    CompactType::Void => {
                        // FIXME: Void may not be a valid ledger type in Compact
                        format!("{}{}export ledger {}: VOID;", output, indent_str, name)
                    }
                };
                output.push_str(&print);
            },
            ContractItemKind::LedgerConstructor => {
                let children = self.children.clone().into_iter().map(|child| child.print()).collect::<Result<Vec<String>, String>>()?;
                let children_output = children.join("");
                let print = 
                    if children_output.len() > 0 {
                        format!("\nconstructor() {{\n{}}}\n", children_output)
                    } else {
                        format!("\nconstructor() {{}}\n")
                    };
                output.push_str(&print);
            }
            ContractItemKind::LedgerPropInit(ledger_var) => {
                match &ledger_var.value {
                    None => {
                        println!("--Ledger Var {:#?}", ledger_var);
                        panic!("Non initialized property value in ledger")
                    },
                    Some(ledger_value) => {
                        let print = match ledger_value {
                            CompactValue::Counter(_) => "".to_string(),
                            CompactValue::Cell(cell_val) => {
                                match *cell_val.clone() {
                                    CompactValue::Bytes(val) => {
                                        format!("{}{}{} = \"{}\" // {}.write(\"{}\")\n", output, indent_str, ledger_var.name, val, ledger_var.name, val)
                                    }
                                    CompactValue::Uint(val) => {
                                        format!("{}{}{} = {} // {}.write({})\n", output, indent_str, ledger_var.name, val, ledger_var.name, val)
                                    }
                                    _ => panic!("Unknown ledger value type to print: {:#?}", *cell_val),
                                }
                            }
                            CompactValue::Bytes(val) => {
                                format!("{}{}{} = {}\n", output, indent_str, ledger_var.name, val)
                            },
                            CompactValue::Uint(val) => {
                                format!("{}{}{} = {}\n", output, indent_str, ledger_var.name, val)
                            },
                        };
                        output.push_str(&print);                        
                    }
                }
            }
            ContractItemKind::CircuitExport(name) => {
                // prints the parameters of the circuit
                let params = 
                    self.children.clone()
                    .into_iter()
                    .filter(|child| match child.kind {
                        ContractItemKind::CircuitParam(_, _) => true,
                        _ => false
                    })
                    .map(|child| child.print())
                    .collect::<Result<Vec<String>, String>>()?;
                let params_output = params.join(", ");
                // prints the return type of the circuit
                let return_type = 
                    self.children.clone()
                    .into_iter()
                    .filter(|child| match child.kind {
                        ContractItemKind::CircuitReturnType(_) => true,
                        _ => false
                    })
                    .map(|child| child.print())
                    .collect::<Result<Vec<String>, String>>()?;
                let return_type_output = return_type.get(0).ok_or(format!("Circuit `{}` return type not found", name))?;
                
                let print = format!("{}{}export circuit {}({}): {} {{}}\n", output, indent_str, name, params_output, return_type_output);
                output.push_str(&print);
            }
            ContractItemKind::CircuitInternal(name) => {
                let print = format!("{}{}circuit {}() {{}}\n", output, indent_str, name);
                output.push_str(&print);
            }
            ContractItemKind::CircuitParam(name, circuit_type) => {
                let print = match circuit_type {
                    CompactType::Counter => {
                        format!("{}: Counter", name)
                    }
                    CompactType::Cell(subtype) => {
                        let printed_subtype = subtype.print()?;
                        format!("{}: Cell<{}>;\n", name, printed_subtype)
                    }
                    CompactType::Bytes(range) => {
                        format!("{}: Bytes<{}>", name, range)
                    }
                    CompactType::Uint(range) => {
                        format!("{}: Uint<{}>", name, range)
                    }
                    CompactType::Void => {
                        // FIXME: Void may not be a valid ledger type in Compact
                        format!("{}: VOID", name)
                    }
                };
                output.push_str(&print);
            }
            ContractItemKind::CircuitReturnType(circuit_type) => {
                output.push_str(&format!("{}", circuit_type.print()?));
            }
            ContractItemKind::Empty => (),
        }

        return Ok(output.to_string());
    }
}

fn build_compact_type_from_ts_type_ref(param: &TsTypeRef) -> Result<CompactType, String> {
    // println!("--Type Ref {:#?}", param);
    let type_name = param.clone().type_name.expect_ident().sym.as_str().to_string();
    match &param.type_params {
        None => CompactType::from_str(type_name.as_str(), None),
        Some(type_params) => {
            if type_params.params.len() == 0 {                
                return CompactType::from_str(&type_name, None)
            } else if type_params.params.len() == 1 {
                let ts_type = type_params.params.get(0).unwrap();
                match *ts_type.clone() {
                    TsType::TsLitType(ts_lit_type) => {
                        match ts_lit_type.lit {
                            TsLit::Str(str_lit) => {
                                let arg = str_lit.value.as_str().to_string();
                                return CompactType::from_str(&type_name, Some(arg));
                            },
                            TsLit::Number(num_lit) => {
                                let arg = num_lit.value.to_string();
                                return CompactType::from_str(&type_name, Some(arg));
                            },
                            _ => todo!("Handle other TsLitType in type params")
                        }
                    }
                    TsType::TsTypeRef(ts_type_ref) => {   
                        // the type must be a type that accepts a subtype
                        match type_name.as_str() {
                            "Cell" => {
                                let subtype = build_compact_type_from_ts_type_ref(&ts_type_ref)?;
                                return Ok(CompactType::Cell(Box::new(subtype)));
                            },
                            _ => return Err(format!("Type {} does not accept a subtype", type_name))
                        }
                    }
                    TsType::TsKeywordType(keyword) => {
                        match keyword.kind {
                            TsKeywordTypeKind::TsNumberKeyword => {
                                // number in TypeScript is automatically cast to Uint<32>
                                return CompactType::from_str("Uint", Some(DEFAULT_UINT_RANGE.to_string()));
                            },
                            _ => todo!("Handle other TsKeywordType in type params")
                        }
                    },
                    _ => {
                        println!("--TsType param {:#?}\nts_type {:#?}", param, ts_type);
                        todo!("Handle other TsType in type params")
                    }
                }
            } else {
                todo!("Handle type params with more than 1 argument")
            }
        }
    }
}

fn parse_ledger(
    ledger: &mut CompactLedger,
    class_decl: ClassDecl,
) -> Result<Vec<ContractItem>, String> {
    // get the properties of the ledger class
    let class_props = class_decl
        .clone()
        .class
        .body
        .into_iter()
        .filter(|class_member| class_member.is_class_prop())
        .map(|class_member| class_member.class_prop().unwrap())
        .collect::<Vec<ClassProp>>();
    if class_props.len() > 0 {
        // println!("--Ledger Props {:#?}", class_props);
        let props = class_props
            .into_iter()
            .map(|class_prop| {
                // finds the name of the ledger property
                let prop_name = match class_prop.key {
                    PropName::Ident(ident) => Ok(ident.to_string()),
                    _ => Err("Ledger prop name is missing".to_string()),
                }?;
                // finds the type of the ledger property
                let ledger_type = match class_prop.type_ann {
                    Some(type_ann) => match type_ann.type_ann.as_ts_type_ref() {
                        None => Err("Ledger prop type annotation is missing".to_string()),
                        Some(ts_type_ref) => {
                            // println!("--Type Ref {:#?}", ts_type_ref);
                            // let type_name = ts_type_ref.clone().type_name.expect_ident().sym;
                            // Ok(type_name.as_str().to_string())
                            build_compact_type_from_ts_type_ref(ts_type_ref)
                        }
                    },
                    None => Err("Ledger prop type is missing".to_string()),
                }?;
                // checks if the type is a valid ledger type
                // let ledger_type = CompactType::from_str(&prop_type)?;

                return Ok((prop_name, ledger_type));
            })
            .collect::<Result<Vec<(String, CompactType)>, String>>()?;
        // println!("--Prop Items: {:#?}", props); 
        let mut ledger_items = props
            .into_iter()
            .map(|(name, ledger_type)| {
                // let value = match ledger_type {
                //     CompactType::Counter => CompactValue::Counter(None),
                //     CompactType::Cell(subtype) => CompactValue::Cell(*subtype, None),
                //     CompactType::Bytes(range) => CompactValue::Bytes(range, None),    
                //     CompactType::Uint(range) => CompactValue::Uint(range, None),    
                //     _ => CompactValue::Unknown,
                // };
                let compact_var = CompactVar {
                    kind: ledger_type.clone(),
                    value: None,
                    is_ledger_value: true,
                    name: name.clone()
                };
                match ledger.add_prop(&name, compact_var) {
                    Ok(_) => Ok(ContractItem::new(ContractItemKind::ExportLedgerValue((name, ledger_type)), 0, vec![])),
                    Err(e) => Err(e),
                }
            })
            .collect::<Result<Vec<ContractItem>, String>>()?;
        // get the constructor of the ledger class
        let constructor = class_decl
            .clone()
            .class
            .body
            .into_iter()
            .find(|class_member| class_member.is_constructor())
            .map(|class_member| class_member.constructor().unwrap());
        if let Some(constructor) = constructor {
            if let Some(constructor_body) = constructor.body {
                if constructor_body.stmts.len() > 0 {
                    // println!("--Ledger Constructor Body: {:#?}", constructor_body.stmts);
                    let constructor_items = constructor_body
                        .stmts
                        .into_iter()
                        .map(|stmt| match stmt {
                            Stmt::Expr(expr) => parse_expr(expr, ExecContext::LedgerInit, ledger),
                            _ => Err("Constructor body statement not implemented".to_string()),
                        })
                        .collect::<Result<Vec<ContractItem>, String>>()?;
                    // ledger_items = [ledger_items.clone(), constructor_items].concat();
                    let constructor_item = ContractItem::new(ContractItemKind::LedgerConstructor, 0, constructor_items);
                    ledger_items.push(constructor_item);
                }
            }
        }
        // TODO: check that there are no methods in the ledger class
        // println!("--Ledger Items: {:#?}", ledger_items);
        return Ok(ledger_items);
    } else {
        return Ok(vec![]);
    }
}

fn parse_expr(
    expr: ExprStmt,
    exec_context: ExecContext,
    ledger: &mut CompactLedger,
) -> Result<ContractItem, String> {
    // println!("--Expr {:#?}", expr);
    match *expr.expr {
        Expr::Assign(assign_expr) => {
            let (left, right) = (assign_expr.left, assign_expr.right);
            match left {
                swc_ecma_ast::AssignTarget::Simple(simple_assign_target) => {
                    if simple_assign_target.is_member() {
                        let member_expr = simple_assign_target.member().unwrap();
                        if member_expr.obj.is_this() && member_expr.prop.is_ident() {
                            // initializing ledger property
                            let member_ident = member_expr.prop.expect_ident();
                            let prop_name = member_ident.sym.as_str();
                            let prop = ledger.props.get(prop_name).ok_or(format!("Ledger property not found: {}", prop_name))?;
                            // println!("--Find Prop {:#?}", prop);
                            match &prop.value {
                                Some(_) => Err(format!("Property value in ledger is already initialized: {:#?}", prop)),
                                None => {
                                    match &prop.kind {
                                        CompactType::Counter => {
                                            match *right {
                                                Expr::New(new_expr) => {
                                                    let new_expr_callee = new_expr.callee;
                                                    if new_expr_callee.is_ident() {
                                                        let new_expr_ident = new_expr_callee.expect_ident();
                                                        if new_expr_ident.sym == "Counter" {
                                                            // Counter values don't need to be initialized in Compact
                                                            match new_expr.args {
                                                                None=> Ok(ContractItem::new(
                                                                    ContractItemKind::Empty,
                                                                    0, 
                                                                    vec![]
                                                                )),
                                                                Some(args) => {
                                                                    if args.len() == 0 {
                                                                        Ok(ContractItem::new(
                                                                            ContractItemKind::Empty,
                                                                            0, 
                                                                            vec![],
                                                                        ))
                                                                    } else if args.len() == 1 {
                                                                        todo!("Handle Counter constructor with 1 argument")
                                                                    } else {
                                                                        Err("Counter constructor takes 0 or 1 argument".to_string())
                                                                    }
                                                                }
                                                            }
                                                            
                                                        } else {
                                                            Err(format!("Property {} is of an unknown type `{}`", prop_name, new_expr_ident.sym))
                                                        }
                                                    } else {
                                                        todo!("Handle other new expressions when assigning value in expression parsing")
                                                    }
                                                }
                                                _ => todo!("Handle other right expressions when assigning value in expression parsing for Counter"),
                                            }
                                        }
                                        CompactType::Cell(subtype) => {
                                            match *right {
                                                Expr::New(new_expr) => {
                                                    let new_expr_callee = new_expr.callee;
                                                    if new_expr_callee.is_ident() {
                                                        let new_expr_ident = new_expr_callee.expect_ident();
                                                        if new_expr_ident.sym == "Cell" {
                                                            match new_expr.args {
                                                                None => Err("Cell constructor takes 1 argument".to_string()),
                                                                Some(args) => {
                                                                    if args.len() == 0 {
                                                                        Err("Cell constructor takes 1 argument, 0 provided".to_string())
                                                                    } else if args.len() == 1 {
                                                                        let arg = args.get(0).unwrap();                                                                
                                                                        // println!("--Arg {:#?} \n--Ledger {:#?}", arg, ledger);
                                                                        // verify that the argument is of the right type
                                                                        let expr = arg.expr.clone();
                                                                        match *expr {
                                                                            Expr::Lit(lit) => {
                                                                                let initial_value = match lit {
                                                                                    Lit::Num(num_lit) => Ok(num_lit.value.to_string()),
                                                                                    Lit::Str(str_lit) => Ok(str_lit.value.as_str().to_string()),
                                                                                    _ => Err("Cell constructor argument must be a number".to_string())
                                                                                }?;
                                                                                if initial_value.len() != 0 {
                                                                                    // Cell is not initialized
                                                                                    let cell_value = match **subtype {
                                                                                        CompactType::Bytes(_) => CompactValue::Bytes(initial_value),
                                                                                        CompactType::Uint(_) => CompactValue::Uint(initial_value.parse::<usize>().map_err(|e| e.to_string())?),
                                                                                        _ => todo!("Handle other cell types"),
                                                                                    };
                                                                                    let compact_var = CompactVar {
                                                                                        kind: prop.kind.clone(),
                                                                                        value: Some(cell_value),
                                                                                        is_ledger_value: true,
                                                                                        name: prop.name.clone()
                                                                                    };
                                                                                    ledger.update_prop(prop_name, compact_var.clone())?;
                                                                                    Ok(ContractItem::new(
                                                                                        ContractItemKind::LedgerPropInit(compact_var),
                                                                                        1,
                                                                                        vec![],
                                                                                    ))
                                                                                } else {
                                                                                    todo!("Handle other initial values for Cell")
                                                                                }
                                                                            },
                                                                            _ => Err("Cell constructor argument must be a number".to_string())
                                                                            }
                                                                    } else {
                                                                        Err(format!("Cell constructor takes 1 argument, {} provided", args.len()))
                                                                    }
                                                                }
                                                            }                                                    
                                                        } else {
                                                            Err(format!("Property {} is of an unknown type `{}`", prop_name, new_expr_ident.sym))
                                                        }
                                                    } else {
                                                        todo!("Handle other new expressions when assigning value in expression parsing")
                                                    }
                                                }
                                                _ => todo!("Handle other right expressions when assigning value in expression parsing for Cell"),
                                            }
                                        }
                                        CompactType::Bytes(_ ) => {
                                            // println!("--Bytes: range: {} / value: {:#?} / right: {:#?}", range, value, right);
                                            match *right {
                                                Expr::New(expr) => {
                                                    match expr.args {
                                                        None => Err("No argument provided of type Bytes".to_string()),
                                                        Some(expr_args) => {
                                                            if expr_args.len() == 0 {
                                                                Err("No argument provided of type Bytes".to_string())
                                                            } else if expr_args.len() == 1 {
                                                                let expr_arg = *expr_args.get(0).unwrap().expr.clone();
                                                                match expr_arg {
                                                                    Expr::Lit(lit) => {
                                                                        match lit {
                                                                            Lit::Str(str_lit) => {
                                                                                let str_val = str_lit.value.as_str().to_string();
                                                                                // TODO: verify that the string is valid bytes
                                                                                let new_value = CompactValue::Bytes(str_val);
                                                                                let compact_var = CompactVar {
                                                                                    kind: prop.kind.clone(),
                                                                                    value: Some(new_value),
                                                                                    is_ledger_value: true,
                                                                                    name: prop_name.to_string()
                                                                                };
                                                                                ledger.update_prop(prop_name, compact_var.clone())?;
                                                                                Ok(ContractItem::new(
                                                                                    ContractItemKind::LedgerPropInit(compact_var),
                                                                                    0, 
                                                                                    vec![],
                                                                                ))
                                                                            },
                                                                            _ => Err("Bytes argument must be a string".to_string())
                                                                        }
                                                                    },
                                                                    _ => Err("Bytes argument must be a string".to_string())
                                                                }
                                                            } else {
                                                                Err(format!("Expected 1 argument of type Bytes, got {}", expr_args.len()))
                                                            }
                                                        }
                                                    }
                                                }
                                                _ => todo!("Handle Bytes RHS that are not NewExpr")
                                            }
                                        }
                                        CompactType::Uint(_) => {
                                            // println!("--Bytes: range: {} / value: {:#?} / right: {:#?}", range, value, right);
                                            match *right {
                                                Expr::New(expr) => {
                                                    match expr.args {
                                                        None => Err("No argument provided of type Uint".to_string()),
                                                        Some(expr_args) => {
                                                            if expr_args.len() == 0 {
                                                                Err("No argument provided of type Uint".to_string())
                                                            } else if expr_args.len() == 1 {
                                                                let expr_arg = *expr_args.get(0).unwrap().expr.clone();
                                                                match expr_arg {
                                                                    Expr::Lit(lit) => {
                                                                        match lit {
                                                                            Lit::Num(num_lit) => {
                                                                                let num_val = num_lit.value as usize;
                                                                                let new_value = CompactValue::Uint(num_val);
                                                                                let compact_var = CompactVar {
                                                                                    kind: prop.kind.clone(),
                                                                                    value: Some(new_value),
                                                                                    is_ledger_value: true,
                                                                                    name: prop_name.to_string()
                                                                                };
                                                                                ledger.update_prop(prop_name, compact_var.clone())?;
                                                                                Ok(ContractItem::new(
                                                                                    ContractItemKind::LedgerPropInit(compact_var),
                                                                                    1, vec![],
                                                                                ))
                                                                            },
                                                                            _ => Err("Uint argument must be a number".to_string())
                                                                        }
                                                                    },
                                                                    _ => Err("Uint argument must be a number".to_string())
                                                                }
                                                            } else {
                                                                Err(format!("Expected 1 argument of type Uint, got {}", expr_args.len()))
                                                            }
                                                        }
                                                    }
                                                }
                                                _ => todo!("Handle Uint RHS that are not NewExpr")
                                            }
                                        }
                                        CompactType::Void => Err("Void type is not a valid ledger type".to_string())
                                    }
                                }
                            }
                        } else {
                            todo!("Handle other member expressions when parsing expression")
                        }
                    } else {
                        todo!("Handle other assign targets when parsing expression")
                    }
                }
                _ => todo!("Handle other assign targets when parsing expression"),
            }
        }
        _ => todo!("Handle other expressions when parsing expression"),
    }
}

fn parse_contract(ledger: &mut CompactLedger, class_decl: ClassDecl) -> Result<Vec<ContractItem>, String> {
    // finds the methods of the contract class
    let methods = class_decl
        .clone()
        .class
        .body
        .into_iter()
        .filter(|class_member| class_member.is_method())
        .map(|class_member| class_member.method().unwrap())
        .collect::<Vec<ClassMethod>>();

    println!("--Methods {:#?}", methods);
    let circuits = methods
        .into_iter()
        .map(|method| {
            let mut circuit_children = vec![];
            let method_name = match method.key.ident() {
                Some(ident) => Ok(ident.sym.to_string()),
                None => Err("Method name is missing".to_string()),
            }?;
            // checks if the circuit has arguments
            let params = method.function.params;
            if params.len() > 0 {
                let param_items = params
                    .into_iter()
                    .map(|param| {
                        let (param_name, param_type) = match param.pat {
                            swc_ecma_ast::Pat::Ident(ident) => {
                                let param_name = ident.sym.to_string();
                                let param_type = match ident.type_ann {
                                    None => Err(format!("Parameter type is missing on method `{}`", method_name)),
                                    Some(ts_type_ann) => {
                                        match ts_type_ann.type_ann.as_ts_keyword_type() {
                                            None => Err(format!("Parameter type is missing on method `{}`", method_name)),
                                            Some(keyword) => {
                                                match keyword.kind {
                                                    TsKeywordTypeKind::TsNumberKeyword => Ok(CompactType::Uint(DEFAULT_UINT_RANGE)),
                                                    _ => todo!("Handle other TsKeywordType in parameter type on circuit methods"),
                                                }
                                            }
                                        }
                                    }
                                }?;
                                Ok((param_name, param_type))
                            },
                            _ => Err(format!("Parameter name is missing on method `{}`", method_name)),
                        }?;
                        return Ok(ContractItem::new(ContractItemKind::CircuitParam(param_name, param_type), 0, vec![]));
                    })
                    .collect::<Result<Vec<ContractItem>, String>>()?;
                circuit_children = param_items;
            }
            // checks that the circuit has a return type
            let return_type = match method.function.return_type {
                None => return Err(format!("Method `{}` must have a return type", method_name)),
                Some(return_type) => {
                    match return_type.type_ann.as_ts_keyword_type() {
                        None => Err(format!("Method `{}` must have a return type", method_name)),
                        Some(keyword) => {
                            match keyword.kind {
                                TsKeywordTypeKind::TsNumberKeyword => Ok(CompactType::Uint(DEFAULT_UINT_RANGE)),
                                TsKeywordTypeKind::TsVoidKeyword => Ok(CompactType::Void),
                                _ => todo!("Handle other TsKeywordType in parameter type on circuit methods"),
                            }
                        }
                    }
                },
            }?;
            circuit_children.push(ContractItem::new(ContractItemKind::CircuitReturnType(return_type), 0, vec![]));
            // checks the body of the circuit

            let circuit_item = match method.accessibility {
                Some(accessibility) => match accessibility {
                    Accessibility::Public => Ok(ContractItem::new(ContractItemKind::CircuitExport(method_name.clone()), 0, circuit_children)),
                    Accessibility::Private => Ok(ContractItem::new(ContractItemKind::CircuitInternal(method_name.clone()), 0, circuit_children)),
                    _ => Err(format!("Method {} has unknown accessibility", method_name)),
                },
                None => Err(format!("Method {} must be marked as `public` or `private`", method_name)),
            }?;
            return Ok(circuit_item);
        })
        .collect::<Result<Vec<ContractItem>, String>>()?;

    Ok(circuits)
}

pub fn parse(file_path: &str) -> Result<String, String> {
    let mut contract_items: Vec<ContractItem> = vec![];
    let mut has_ledger_class = false;
    let mut has_contract_class = false;
    let mut contract_class_extends_compact_contract = false;
    let mut ledger = CompactLedger { props: HashMap::new() };

    let cm: Lrc<SourceMap> = Default::default();
    let comments: swc_common::comments::SingleThreadedComments = Default::default();

    let fm = cm
        .load_file(Path::new(file_path))
        .expect("failed to load contract.ts");
    let ts_config = swc_ecma_parser::TsSyntax {
        decorators: true,
        ..Default::default()
    };

    let mut parser = Parser::new(
        Syntax::Typescript(ts_config),
        StringInput::from(&*fm),
        Some(&comments),
    );

    let module = parser
        .parse_typescript_module()
        .expect("failed to parser module");

    // finds comment on first line about compiler or language version
    let (leading, _) = comments.borrow_all();
    let version_comment = leading.clone().into_values().flatten().find(|comment| {
        comment.kind == swc_common::comments::CommentKind::Line && comment.text.contains("version")
    });

    match version_comment {
        None => (),
        Some(v_c) => {
            match cm.span_to_lines(v_c.span) {
                Ok(lines) => {
                    if lines.lines.len() > 0 && lines.lines[0].line_index == 0 {
                        // checks if compiler version or language version
                        let re = regex::Regex::new(r"<(\w+)-version\s+([\d\.]+)>").unwrap();
                        if let Some(caps) = re.captures(&v_c.text) {
                            let version_type = caps.get(1).map_or("", |m| m.as_str());
                            let version_number = caps.get(2).map_or("", |m| m.as_str());
                            if version_type == "compiler" {
                                contract_items.push(ContractItem::new(
                                    ContractItemKind::CompilerVersion(version_number.to_string()),
                                    0, vec![],
                                ));
                            } else if version_type == "language" {
                                contract_items.push(ContractItem::new(
                                    ContractItemKind::LanguageVersion(version_number.to_string()),
                                    0, vec![],
                                ));
                            }
                        }
                    } else {
                        return Err(format!(
                            "Comment line length for compiler/language-version is 0"
                        ));
                    }
                }
                Err(e) => {
                    return Err(format!(
                        "Unable to find line of compiler/language-version: {:?}",
                        e
                    ));
                }
            }
        }
    };

    module.body.into_iter().try_for_each(|module_item| {
        // println!("-- {:#?}", module_item);
        match module_item {
            ModuleItem::Stmt(stmt) => {
                // println!("-- {:#?}", stmt);
                match stmt {
                    // CLASS DECLARATION
                    Stmt::Decl(Decl::Class(class_decl)) => {
                        // LEDGER PARSING
                        if class_decl.ident.sym == "Ledger" {
                            has_ledger_class = true;
                            let ledger_items = parse_ledger(&mut ledger, class_decl.clone())?;
                            contract_items = [contract_items.clone(), ledger_items].concat();
                        // METHODS PARSING
                        } else if class_decl.ident.sym == "Contract" {
                            has_contract_class = true;
                            match class_decl.clone().class.super_class {
                                Some(super_class) => match *super_class {
                                    Expr::Ident(ident) => {
                                        if ident.sym == "CompactContract" {
                                            contract_class_extends_compact_contract = true;
                                        }
                                    }
                                    _ => {
                                        return Err(format!(
                                            "Contract class does not extend CompactContract"
                                        ));
                                    }
                                },
                                None => {
                                    return Err(format!(
                                        "Contract class does not extend CompactContract"
                                    ));
                                }
                            }
                            // TODO: check that the TS contract has a property called "ledger" of type "Ledger"
                            // TODO: check that the TS contract has the proper constructor
                            let circuits = parse_contract(&mut ledger, class_decl)?;
                            contract_items = [contract_items.clone(), circuits].concat();
                        } else {
                            todo!("Handle other class declarations");
                        }
                        Ok(())
                    }
                    _ => {
                        // TODO: Handle module statements
                        Ok(())
                    }
                }
            }
            ModuleItem::ModuleDecl(decl) => match decl {
                ModuleDecl::Import(import_decl) => {
                    if import_decl.specifiers.len() > 0 {
                        // TODO: check that the named specifiers exist
                        // finds import from std
                        if import_decl.src.value.contains("std") {
                            contract_items.push(ContractItem::new(ContractItemKind::StdImport, 0, vec![]));
                        }
                        Ok(())
                    } else {
                        return Err(format!(
                            "There is no import specifier ({:?})",
                            import_decl.span
                        ));
                    }
                }
                _ => {
                    // TODO: handle other module declarations
                    Ok(())
                }
            },
        }
        // finds ledger type
    })?;

    if has_ledger_class == false {
        return Err(format!("Ledger class not found"));
    } else if has_contract_class == false {
        return Err(format!("Contract class not found"));
    } 

    let root_path = std::env::current_dir().map_err(|e| e.to_string())?;
    let output_path = format!("{}/compact/counter/contract.compact", root_path.display());
    let mut output_file = File::create(&output_path).map_err(|e| e.to_string())?;

    // println!("-- Contract Items: {:#?}", contract_items);
    for item in &contract_items {
        let printed_output = item.print()?;
        writeln!(output_file, "{}", printed_output).map_err(|e| e.to_string())?;
    }

    let file_output = std::fs::read_to_string(&output_path).map_err(|e| e.to_string())?;

    Ok(file_output)
}
