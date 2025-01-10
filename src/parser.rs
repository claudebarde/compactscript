extern crate swc_common;
extern crate swc_ecma_parser;
use std::{f64::consts::E, fs::File};
use std::io::Write;
use std::path::Path;
use std::collections::HashMap;
use std::{mem, vec};
use swc_common::sync::Lrc;
use swc_common::SourceMap;
use swc_ecma_ast::{
    Accessibility, Callee, ClassDecl, ClassMethod, ClassProp, Decl, Expr, Lit, MemberExpr, ModuleDecl, ModuleItem, PropName, Stmt, TsKeywordTypeKind, TsLit, TsType, TsTypeRef
};
use swc_ecma_parser::{Parser, StringInput, Syntax};
use crate::types::{CompactType, CompactLedger, CompactValue, CompactVar, ContractItem, ContractItemKind, ExecContext, ErrorMessage};

const DEFAULT_UINT_RANGE: usize = 32;

fn build_compact_type_from_ts_type_ref(param: &TsTypeRef) -> Result<CompactType, ErrorMessage> {
    // println!("--Type Ref {:#?}", param);
    let type_name = param.clone().type_name.expect_ident().sym.as_str().to_string();
    match &param.type_params {
        None => CompactType::from_str(type_name.as_str(), None),
        Some(type_params) => {
            if type_params.params.len() == 0 {                
                CompactType::from_str(&type_name, None)
            } else if type_params.params.len() == 1 {
                let ts_type = type_params.params.get(0).unwrap();
                match *ts_type.clone() {
                    TsType::TsLitType(ts_lit_type) => {
                        match ts_lit_type.lit {
                            TsLit::Str(str_lit) => {
                                let arg = str_lit.value.as_str().to_string();
                                CompactType::from_str(&type_name, Some(arg))
                            },
                            TsLit::Number(num_lit) => {
                                let arg = num_lit.value.to_string();
                                CompactType::from_str(&type_name, Some(arg))
                            },
                            _ => todo!("Handle other TsLitType in type params")
                        }
                    }
                    TsType::TsTypeRef(ts_type_ref) => {   
                        // the type must be a type that accepts a subtype
                        match type_name.as_str() {
                            "Cell" => {
                                let subtype = build_compact_type_from_ts_type_ref(&ts_type_ref)?;
                                Ok(CompactType::Cell(Box::new(subtype)))
                            },
                            _ => Err(ErrorMessage::NotAComplexType(type_name))
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
) -> Result<Vec<ContractItem>, ErrorMessage> {
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
                    _ => Err(ErrorMessage::MissingPropName),
                }?;
                // finds the type of the ledger property
                let ledger_type = match class_prop.type_ann {
                    Some(type_ann) => match type_ann.type_ann.as_ts_type_ref() {
                        None => Err(ErrorMessage::MissingPropTypeAnnotation),
                        Some(ts_type_ref) => {
                            // println!("--Type Ref {:#?}", ts_type_ref);
                            // let type_name = ts_type_ref.clone().type_name.expect_ident().sym;
                            // Ok(type_name.as_str().to_string())
                            build_compact_type_from_ts_type_ref(ts_type_ref)
                        }
                    },
                    None => Err(ErrorMessage::MissingPropTypeAnnotation),
                }?;
                // checks if the type is a valid ledger type
                // let ledger_type = CompactType::from_str(&prop_type)?;

                return Ok((prop_name, ledger_type));
            })
            .collect::<Result<Vec<(String, CompactType)>, ErrorMessage>>()?;
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
            .collect::<Result<Vec<ContractItem>, ErrorMessage>>()?;
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
                            Stmt::Expr(expr) => parse_expr(*expr.expr, ExecContext::LedgerInit, ledger),
                            _ => Err(ErrorMessage::NoConstructorBody),
                        })
                        .collect::<Result<Vec<ContractItem>, ErrorMessage>>()?;
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
    expr: Expr,
    exec_context: ExecContext,
    ledger: &mut CompactLedger,
) -> Result<ContractItem, ErrorMessage> {
    // println!("--Expr {:#?}", expr);
    match expr {
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
                            let prop = ledger.props.get(prop_name).ok_or(ErrorMessage::LedgerPropertyNotFound(prop_name.to_string()))?;
                            // println!("--Find Prop {:#?}", prop);
                            match &prop.value {
                                Some(_) => Err(ErrorMessage::LedgerPropertyExists(prop_name.to_string())),
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
                                                                        Err(ErrorMessage::CounterTooManyArgs(args.len()))
                                                                    }
                                                                }
                                                            }
                                                            
                                                        } else {
                                                            Err(ErrorMessage::UnknownTypeProperty(new_expr_ident.sym.to_string(), prop_name.to_string()))
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
                                                                None => Err(ErrorMessage::CellConstructorArgs(0)),
                                                                Some(args) => {
                                                                    if args.len() == 0 {
                                                                        Err(ErrorMessage::CellConstructorArgs(0))
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
                                                                                    // FIXME: handle other types in error message
                                                                                    _ => Err(ErrorMessage::CellConstructorArgType("number".to_string(), "other".to_string()))
                                                                                }?;
                                                                                if initial_value.len() != 0 {
                                                                                    // Cell is not initialized
                                                                                    let cell_value = match **subtype {
                                                                                        CompactType::Bytes(_) => CompactValue::Bytes(initial_value),
                                                                                        CompactType::Uint(_) => CompactValue::Uint(initial_value.parse::<usize>().map_err(|e| ErrorMessage::Custom(e.to_string()))?),
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
                                                                            // FIXME: handle other types in error message
                                                                            _ => Err(ErrorMessage::CellConstructorArgType("number".to_string(), "other".to_string()))
                                                                            }
                                                                    } else {
                                                                        Err(ErrorMessage::CellConstructorArgs(args.len()))
                                                                    }
                                                                }
                                                            }                                                    
                                                        } else {
                                                            Err(ErrorMessage::UnknownTypeProperty(new_expr_ident.sym.to_string(), prop_name.to_string()))
                                                        }
                                                    } else {
                                                        todo!("Handle other new expressions when assigning value in expression parsing")
                                                    }
                                                }
                                                _ => todo!("Handle other right expressions when assigning value in expression parsing for Cell"),
                                            }
                                        }
                                        CompactType::Bytes(_) => {
                                            // println!("--Bytes: range: {} / value: {:#?} / right: {:#?}", range, value, right);
                                            match *right {
                                                Expr::New(expr) => {
                                                    match expr.args {
                                                        // FIXME: handle other types in error message
                                                        None => Err(ErrorMessage::UnexpectedArgType("bytes".to_string(), "other".to_string())),
                                                        Some(expr_args) => {
                                                            if expr_args.len() == 0 {
                                                                // FIXME: handle other types in error message
                                                                Err(ErrorMessage::UnexpectedArgType("bytes".to_string(), "other".to_string()))
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
                                                                            // FIXME: handle other types in error message
                                                                            _ => Err(ErrorMessage::InvalidBytesArgument("other".to_string()))
                                                                        }
                                                                    },
                                                                    // FIXME: handle other types in error message
                                                                    _ => Err(ErrorMessage::InvalidBytesArgument("string".to_string()))
                                                                }
                                                            } else {
                                                                Err(ErrorMessage::UnexpectedArgNumber(1, expr_args.len()))
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
                                                        None => Err(ErrorMessage::UnexpectedArgNumber(1, 0)),
                                                        Some(expr_args) => {
                                                            if expr_args.len() == 0 {
                                                                Err(ErrorMessage::UnexpectedArgNumber(1, 0))
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
                                                                            // FIXME: handle other types in error message
                                                                            _ => Err(ErrorMessage::InvalidUintArgument("other".to_string()))
                                                                        }
                                                                    },
                                                                    // FIXME: handle other types in error message
                                                                    _ => Err(ErrorMessage::InvalidUintArgument("other".to_string()))
                                                                }
                                                            } else {
                                                                // FIXME: it could be nice to pinpoint the type that triggered the error
                                                                Err(ErrorMessage::UnexpectedArgNumber(1, expr_args.len()))
                                                            }
                                                        }
                                                    }
                                                }
                                                _ => todo!("Handle Uint RHS that are not NewExpr")
                                            }
                                        }
                                        CompactType::Void => Err(ErrorMessage::InvalidLedgerType("Void".to_string())),
                                        CompactType::Boolean => todo!("Handle Boolean type in ledger properties"),
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
        Expr::Call(call_expr) => {
            // function call
            match call_expr.callee {
                Callee::Expr(expr) => {
                    // member expression: https://stackoverflow.com/questions/47972903/difference-between-callexpression-and-memberexpression
                    let res = parse_expr(*expr, exec_context, ledger);
                    println!("--Call Expr {:#?}", res);

                    todo!("Handle call expressions when parsing expression")
                }
                Callee::Super(_) => {
                    todo!("Handle super call expressions when parsing expression")
                }
                Callee::Import(_) => {
                    todo!("Handle import call expressions when parsing expression")
                }
            }
        }
        Expr::Member(member_expr) => {
            let res = parse_expr(*member_expr.obj.clone(), exec_context, ledger)?;
            match res.kind {
                ContractItemKind::This => {
                    // the following property must be "ledger"
                    // TODO: allow other properties after "this" in a method call
                    match member_expr.prop.ident() {
                        Some(ident) => {
                            let prop_name = ident.sym.to_string();
                            if prop_name == "ledger" {
                                Ok(ContractItem::new(ContractItemKind::Ledger, 0, vec![]))
                            } else {
                                Err(ErrorMessage::InvalidThisProperty(prop_name.to_string()))
                            }
                        }
                        None => Err(ErrorMessage::InvalidThisProperty("".to_string())),
                    }
                }
                ContractItemKind::Ledger => {
                    match member_expr.prop.ident() {
                        Some(ident) => {
                            let prop_name = ident.sym.to_string();
                            match ledger.props.get(&prop_name) {
                                Some(_) => Ok(ContractItem::new(ContractItemKind::LedgerProp(prop_name), 1, vec![])),
                                None => Err(ErrorMessage::LedgerPropertyNotFound(prop_name.to_string())),
                            }
                        }
                        None => Err(ErrorMessage::MissingLedgerProperty),
                    }
                },
                ContractItemKind::LedgerProp(prop_name) => {
                    println!("--Member Expr {:#?}", member_expr);
                    todo!("Handle ledger prop member expressions when parsing expression")
                },
                _ => todo!("Handle member expressions when parsing expression")
            }
        }
        Expr::This(_) => match exec_context {
            ExecContext::Circuit => Ok(ContractItem::new(ContractItemKind::This, 0, vec![])),
            _ => Err(ErrorMessage::InvalidThisContext),
        },
        _ => todo!("Handle other expressions when parsing expression"),
    }
}

fn parse_contract(ledger: &mut CompactLedger, class_decl: ClassDecl) -> Result<Vec<ContractItem>, ErrorMessage> {
    // verifies that the contract has a property called "ledger" of type "Ledger"
    let class_props = class_decl
        .clone()
        .class
        .body
        .into_iter()
        .filter(|class_member| class_member.is_class_prop())
        .map(|class_member| class_member.class_prop().unwrap())
        .collect::<Vec<ClassProp>>();
    if class_props.len() == 0 {
        return Err(ErrorMessage::MissingLedgerProperty);
    } else if class_props.len() > 1 {
        return Err(ErrorMessage::TooManyContractProperties);
    } else {
        // checks that the property is called "ledger" and implements the Ledger class
        let ledger_prop = class_props.get(0).unwrap();
        match ledger_prop.clone().key.ident() {
            Some(ident) => {
                let prop_name = ident.sym.to_string();
                if prop_name != "ledger" {
                    return Err(ErrorMessage::MissingLedgerProperty);
                } else {
                    match &ledger_prop.type_ann {
                        Some(type_ann) => match type_ann.type_ann.as_ts_type_ref() {
                            None => return Err(ErrorMessage::MissingLedgerPropertyAnnotation),
                            Some(ts_type_ref) => {
                                match ts_type_ref.type_name.clone().ident() {
                                    None => return Err(ErrorMessage::MissingLedgerPropertyAnnotation),
                                    Some(type_name) => {
                                        if type_name.sym.to_string() != "Ledger" {
                                            return Err(ErrorMessage::MissingLedgerPropertyAnnotation);
                                        }
                                    }
                                }
                            }
                        },
                        None => return Err(ErrorMessage::MissingPropTypeAnnotation),
                    }
                }
            },
            None => return Err(ErrorMessage::MissingLedgerProperty),
        }
    }
    // finds the methods of the contract class
    let methods = class_decl
        .clone()
        .class
        .body
        .into_iter()
        .filter(|class_member| class_member.is_method())
        .map(|class_member| class_member.method().unwrap())
        .collect::<Vec<ClassMethod>>();

    // println!("--Methods {:#?}", methods);
    let circuits = methods
        .into_iter()
        .map(|method| {
            let mut circuit_children = vec![];
            let method_name = match method.key.ident() {
                Some(ident) => Ok(ident.sym.to_string()),
                None => Err(ErrorMessage::MissingMethodName),
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
                                    None => Err(ErrorMessage::MissingParamTypeOnMethod(method_name.to_string())),
                                    Some(ts_type_ann) => {
                                        match ts_type_ann.type_ann.as_ts_keyword_type() {
                                            None => Err(ErrorMessage::MissingParamNameOnMethod(method_name.to_string())),
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
                            _ => Err(ErrorMessage::MissingParamNameOnMethod(method_name.to_string())),
                        }?;
                        return Ok(ContractItem::new(ContractItemKind::CircuitParam(param_name, param_type), 0, vec![]));
                    })
                    .collect::<Result<Vec<ContractItem>, ErrorMessage>>()?;
                circuit_children = param_items;
            }
            // checks that the circuit has a return type
            let return_type = match method.function.return_type {
                None => return Err(ErrorMessage::ReturnTypeOnMethod(method_name)),
                Some(return_type) => {
                    match return_type.type_ann.as_ts_keyword_type() {
                        None => Err(ErrorMessage::ReturnTypeOnMethod(method_name.to_string())),
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
            match method.function.body {
                None => Err(ErrorMessage::MissingMethodBody(method_name.to_string())),
                Some(body) => {
                    if body.stmts.len() == 0 {
                        Err(ErrorMessage::MissingMethodBody(method_name.to_string()))
                    } else {
                        let body_items = body
                            .stmts
                            .into_iter()
                            .map(|stmt| match stmt {
                                Stmt::Expr(expr) => parse_expr(*expr.expr, ExecContext::Circuit, ledger),
                                _ => Err(ErrorMessage::InvalidMethodBody(method_name.to_string())),
                            })
                            .collect::<Result<Vec<ContractItem>, ErrorMessage>>()?;
                        circuit_children = [circuit_children.clone(), body_items].concat();
                        Ok(())
                    }
                }
            }?;
            
            let circuit_item = match method.accessibility {
                Some(accessibility) => match accessibility {
                    Accessibility::Public if method.is_static == false => 
                    Ok(ContractItem::new(ContractItemKind::CircuitExport(method_name.to_string()), 0, circuit_children)),
                    Accessibility::Private if method.is_static == false => 
                    Ok(ContractItem::new(ContractItemKind::CircuitInternal(method_name.clone()), 0, circuit_children)),
                    _ => Err(ErrorMessage::Custom(format!("Method {} has unknown accessibility", method_name))),
                },
                None => Err(ErrorMessage::InaccessibleMethod(method_name)),
            }?;

            return Ok(circuit_item);
        })
        .collect::<Result<Vec<ContractItem>, ErrorMessage>>()?;

    Ok(circuits)
}

pub fn parse(file_path: &str) -> Result<String, ErrorMessage> {
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
                        return Err(ErrorMessage::Custom(format!(
                            "Comment line length for compiler/language-version is 0"
                        )));
                    }
                }
                Err(e) => {
                    return Err(ErrorMessage::Custom(format!(
                        "Unable to find line of compiler/language-version: {:?}",
                        e
                    )));
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
                                        return Err(ErrorMessage::Custom(format!(
                                            "Contract class does not extend CompactContract"
                                        )));
                                    }
                                },
                                None => {
                                    return Err(ErrorMessage::Custom(format!(
                                        "Contract class does not extend CompactContract"
                                    )));
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
                        return Err(ErrorMessage::Custom(format!(
                            "There is no import specifier ({:?})",
                            import_decl.span
                        )));
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
        return Err(ErrorMessage::LedgerClassNotFound);
    } else if has_contract_class == false {
        return Err(ErrorMessage::ContractClassNotFound);
    } 

    let root_path = std::env::current_dir().map_err(|e| ErrorMessage::Custom(e.to_string()))?;
    let output_path = format!("{}/compact/counter/contract.compact", root_path.display());
    let mut output_file = File::create(&output_path).map_err(|e| ErrorMessage::Custom(e.to_string()))?;

    // println!("-- Contract Items: {:#?}", contract_items);
    for item in &contract_items {
        let printed_output = item.print()?;
        writeln!(output_file, "{}", printed_output).map_err(|e| ErrorMessage::Custom(e.to_string()))?;
    }

    let file_output = std::fs::read_to_string(&output_path).map_err(|e| ErrorMessage::Custom(e.to_string()))?;

    Ok(file_output)
}
