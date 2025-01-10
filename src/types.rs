use std::{collections::HashMap, f32::consts::E, fmt::Error};

pub enum ExecContext {
    ContractInit,
    LedgerInit,
    Circuit,
}

#[derive(Debug, Clone)]
pub enum CompactType {
    Counter,
    Cell(Box<CompactType>),
    Bytes(usize),
    Uint(usize),
    Boolean,
    Void,
}
impl CompactType {
    pub fn from_str(type_name: &str, arg: Option<String>) -> Result<Self, ErrorMessage> {
        match arg {
            None => match type_name {
                "Counter" => Ok(CompactType::Counter),
                _ => Err(ErrorMessage::UnknownLedgerType(type_name.to_string())),
            },
            Some(arg) => match type_name {
                "Bytes" => {
                    if let Ok(size) = arg.parse::<usize>() {
                        Ok(CompactType::Bytes(size))
                    } else {
                        Err(ErrorMessage::InvalidBytesArgument(arg))
                    }
                }
                "Uint" => {
                    if let Ok(size) = arg.parse::<usize>() {
                        Ok(CompactType::Uint(size))
                    } else {
                        Err(ErrorMessage::InvalidUintArgument(arg))
                    }
                }
                "Cell" => match CompactType::from_str(&arg, None) {
                    Ok(subtype) => Ok(CompactType::Cell(Box::new(subtype))),
                    Err(e) => Err(e),
                },
                _ => Err(ErrorMessage::UnknownLedgerType(type_name.to_string())),
            },
        }
    }

    pub fn print(&self) -> String {
        match self {
            CompactType::Counter => "Counter".to_string(),
            CompactType::Cell(subtype) => {
                let printed_subtype = subtype.print();
                format!("Cell<{}>", printed_subtype)
            }
            CompactType::Bytes(range) => format!("Bytes<{}>", range),
            CompactType::Uint(size) => format!("Uint<{}>", size),
            CompactType::Void => "Void".to_string(),
            CompactType::Boolean => "Boolean".to_string(),
        }
    }

    pub fn is_method_valid(&self, method_name: &str) -> bool {
        match self {
            CompactType::Counter => {
                let valid_methods = HashMap::from([
                    (
                        "increment",
                        (Some(CompactType::Uint(usize::MAX)), CompactType::Void),
                    ),
                    (
                        "decrement",
                        (Some(CompactType::Uint(usize::MAX)), CompactType::Void),
                    ),
                    ("less_than", (None, CompactType::Boolean)),
                    ("read", (None, CompactType::Uint(usize::MAX))),
                    ("reset_to_default", (None, CompactType::Void)),
                ]);
                if valid_methods.contains_key(method_name) {
                    return true;
                } else {
                    return false;
                }
            }
            CompactType::Cell(_) => match method_name {
                "write" => true,
                _ => false,
            },
            CompactType::Bytes(_) => match method_name {
                "write" => true,
                _ => false,
            },
            CompactType::Uint(_) => match method_name {
                "write" => true,
                _ => false,
            },
            CompactType::Void => false,
            CompactType::Boolean => false,
        }
    }
}

#[derive(Debug, Clone)]
pub enum CompactValue {
    Counter(usize),          // value
    Cell(Box<CompactValue>), // value type, value
    Bytes(String),           // range, value
    Uint(usize),             // range, value
}

#[derive(Debug, Clone)]
pub struct CompactVar {
    pub kind: CompactType,
    pub name: String,
    pub value: Option<CompactValue>,
    pub is_ledger_value: bool,
}

#[derive(Debug, Clone)]
pub struct CompactLedger {
    pub props: HashMap<String, CompactVar>, // name, value
}
impl CompactLedger {
    pub fn add_prop(&mut self, prop_name: &str, prop_val: CompactVar) -> Result<(), ErrorMessage> {
        match self.props.get(prop_name) {
            None => {
                self.props.insert(prop_name.to_string(), prop_val);
                return Ok(());
            }
            Some(_) => return Err(ErrorMessage::LedgerPropertyExists(prop_name.to_string())),
        }
    }

    pub fn update_prop(
        &mut self,
        prop_name: &str,
        new_val: CompactVar,
    ) -> Result<(), ErrorMessage> {
        match self.props.get(prop_name) {
            None => return Err(ErrorMessage::LedgerPropertyNotFound(prop_name.to_string())),
            Some(_) => {
                self.props.insert(prop_name.to_string(), new_val);
                return Ok(());
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum ContractItemKind {
    CompilerVersion(String),
    LanguageVersion(String),
    StdImport,
    ExportLedgerValue((String, CompactType)),
    LedgerConstructor,
    LedgerPropInit(CompactVar),
    CircuitExport(String),             // name of the circuit
    CircuitInternal(String),           // name of the circuit
    CircuitParam(String, CompactType), // name, type
    CircuitReturnType(CompactType),    // type
    This,                              // not meant to be printed, used for verification
    Empty,                             // some TS code doesn't yield any Compact code
}

#[derive(Debug, Clone)]
pub struct ContractItem {
    pub kind: ContractItemKind,
    pub children: Vec<ContractItem>,
    pub indent: usize,
}
impl ContractItem {
    pub fn new(kind: ContractItemKind, indent: usize, children: Vec<ContractItem>) -> Self {
        ContractItem {
            kind,
            children: children,
            indent,
        }
    }

    pub fn print(&self) -> Result<String, ErrorMessage> {
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
                        let printed_subtype = subtype.print();
                        format!(
                            "{}{}export ledger {}: Cell<{}>;",
                            output, indent_str, name, printed_subtype
                        )
                    }
                    CompactType::Bytes(range) => {
                        format!(
                            "{}{}export ledger {}: Bytes<{}>;",
                            output, indent_str, name, range
                        )
                    }
                    CompactType::Uint(range) => {
                        format!(
                            "{}{}export ledger {}: Uint<{}>;",
                            output, indent_str, name, range
                        )
                    }
                    CompactType::Void => {
                        // FIXME: Void may not be a valid ledger type in Compact
                        format!("{}{}export ledger {}: Void;", output, indent_str, name)
                    }
                    CompactType::Boolean => {
                        format!("{}{}export ledger {}: Boolean;", output, indent_str, name)
                    }
                };
                output.push_str(&print);
            }
            ContractItemKind::LedgerConstructor => {
                let children = self
                    .children
                    .clone()
                    .into_iter()
                    .map(|child| child.print())
                    .collect::<Result<Vec<String>, ErrorMessage>>()?;
                let children_output = children.join("");
                let print = if children_output.len() > 0 {
                    format!("\nconstructor() {{\n{}}}\n", children_output)
                } else {
                    format!("\nconstructor() {{}}\n")
                };
                output.push_str(&print);
            }
            ContractItemKind::LedgerPropInit(ledger_var) => match &ledger_var.value {
                None => {
                    println!("--Ledger Var {:#?}", ledger_var);
                    panic!("Non initialized property value in ledger")
                }
                Some(ledger_value) => {
                    let print = match ledger_value {
                        CompactValue::Counter(_) => "".to_string(),
                        CompactValue::Cell(cell_val) => match *cell_val.clone() {
                            CompactValue::Bytes(val) => {
                                format!(
                                    "{}{}{} = \"{}\" // {}.write(\"{}\")\n",
                                    output, indent_str, ledger_var.name, val, ledger_var.name, val
                                )
                            }
                            CompactValue::Uint(val) => {
                                format!(
                                    "{}{}{} = {} // {}.write({})\n",
                                    output, indent_str, ledger_var.name, val, ledger_var.name, val
                                )
                            }
                            _ => panic!("Unknown ledger value type to print: {:#?}", *cell_val),
                        },
                        CompactValue::Bytes(val) => {
                            format!("{}{}{} = {}\n", output, indent_str, ledger_var.name, val)
                        }
                        CompactValue::Uint(val) => {
                            format!("{}{}{} = {}\n", output, indent_str, ledger_var.name, val)
                        }
                    };
                    output.push_str(&print);
                }
            },
            ContractItemKind::CircuitExport(name) => {
                // prints the parameters of the circuit
                let params = self
                    .children
                    .clone()
                    .into_iter()
                    .filter(|child| match child.kind {
                        ContractItemKind::CircuitParam(_, _) => true,
                        _ => false,
                    })
                    .map(|child| child.print())
                    .collect::<Result<Vec<String>, ErrorMessage>>()?;
                let params_output = params.join(", ");
                // prints the return type of the circuit
                let return_type = self
                    .children
                    .clone()
                    .into_iter()
                    .filter(|child| match child.kind {
                        ContractItemKind::CircuitReturnType(_) => true,
                        _ => false,
                    })
                    .map(|child| child.print())
                    .collect::<Result<Vec<String>, ErrorMessage>>()?;
                let return_type_output = return_type
                    .get(0)
                    .ok_or(ErrorMessage::CircuitReturnTypeNotFound(name.to_string()))?;

                let print = format!(
                    "{}{}export circuit {}({}): {} {{}}\n",
                    output, indent_str, name, params_output, return_type_output
                );
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
                        let printed_subtype = subtype.print();
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
                        format!("{}: Void", name)
                    }
                    CompactType::Boolean => {
                        format!("{}: Boolean", name)
                    }
                };
                output.push_str(&print);
            }
            ContractItemKind::CircuitReturnType(circuit_type) => {
                output.push_str(&format!("{}", circuit_type.print()));
            }
            ContractItemKind::Empty | ContractItemKind::This => (),
        }

        return Ok(output.to_string());
    }
}

#[derive(Debug, Clone)]
pub enum ErrorMessage {
    NotAComplexType(String),
    UnknownLedgerType(String),
    InvalidBytesArgument(String),
    InvalidUintArgument(String),
    LedgerPropertyExists(String),
    LedgerPropertyNotFound(String),
    CircuitReturnTypeNotFound(String),
    NoConstructorBody,
    CounterTooManyArgs(usize),
    UnknownTypeProperty(String, String),    // type, property
    CellConstructorArgs(usize),             // got
    CellConstructorArgType(String, String), // expected type, got
    UnexpectedArgType(String, String),      // expected type, got
    UnexpectedArgNumber(usize, usize),      // expected number, got
    InvalidLedgerType(String),
    InaccessibleMethod(String),
    LedgerClassNotFound,
    ContractClassNotFound,
    ReturnTypeOnMethod(String),
    MissingMethodName,
    MissingParamNameOnMethod(String),
    MissingParamTypeOnMethod(String),
    MissingPropTypeAnnotation,
    MissingPropName,
    MissingMethodBody(String),
    InvalidMethodBody(String),
    InvalidThisContext,
    MissingLedgerProperty,
    TooManyContractProperties,
    MissingLedgerPropertyAnnotation,
    Custom(String),
}
impl ErrorMessage {
    pub fn print(&self) -> String {
        match self {
            ErrorMessage::NotAComplexType(name) => format!("{} is not a complex type", name),
            ErrorMessage::UnknownLedgerType(name) => format!("Unknown Compact type {}", name),
            ErrorMessage::InvalidBytesArgument(got) => format!(
                "Invalid argument for Bytes, expected string but got {}",
                got
            ),
            ErrorMessage::InvalidUintArgument(got) => {
                format!("Invalid argument for Uint, expected number but got {}", got)
            }
            ErrorMessage::LedgerPropertyExists(name) => {
                format!("Ledger property already exists: {}", name)
            }
            ErrorMessage::LedgerPropertyNotFound(name) => {
                format!("Ledger property not found: {}", name)
            }
            ErrorMessage::CircuitReturnTypeNotFound(name) => {
                format!("Circuit `{}` return type not found", name)
            }
            ErrorMessage::NoConstructorBody => {
                format!("Ledger constructor has no body")
            }
            ErrorMessage::CounterTooManyArgs(got) => {
                format!("Counter takes zero or one argument, got {}", got)
            }
            ErrorMessage::UnknownTypeProperty(type_name, prop_name) => {
                format!("Unknown type {} for property {}", type_name, prop_name)
            }
            ErrorMessage::CellConstructorArgs(got) => {
                format!("Cell constructor takes one argument, got {}", got)
            }
            ErrorMessage::CellConstructorArgType(expected_type, got) => {
                format!(
                    "Cell constructor argument must be a {}, got {}",
                    expected_type, got
                )
            }
            ErrorMessage::UnexpectedArgType(expected_type, got) => {
                format!("Expected argument of type {}, got {}", expected_type, got)
            }
            ErrorMessage::UnexpectedArgNumber(expected, got) => {
                format!("Expected {} arguments, got {}", expected, got)
            }
            ErrorMessage::InvalidLedgerType(name) => {
                format!("Invalid ledger type: {}", name)
            }
            ErrorMessage::InaccessibleMethod(name) => {
                format!("Method {} must be marked as `public` or `private`", name)
            }
            ErrorMessage::LedgerClassNotFound => {
                format!("Ledger class not found")
            }
            ErrorMessage::ContractClassNotFound => {
                format!("Contract class not found")
            }
            ErrorMessage::ReturnTypeOnMethod(name) => {
                format!("Method {} must have a return type", name)
            }
            ErrorMessage::MissingParamNameOnMethod(name) => {
                format!("Method {} must have a name for each parameter", name)
            }
            ErrorMessage::MissingParamTypeOnMethod(name) => {
                format!("Method {} must have a type for each parameter", name)
            }
            ErrorMessage::MissingMethodName => {
                format!("Method must have a name")
            }
            ErrorMessage::MissingPropTypeAnnotation => {
                format!("Property must have a type annotation")
            }
            ErrorMessage::MissingPropName => {
                format!("Property must have a name")
            }
            ErrorMessage::MissingMethodBody(name) => {
                format!("Method {} must have a body", name)
            }
            ErrorMessage::InvalidMethodBody(name) => {
                format!("Method {} has an invalid body", name)
            }
            ErrorMessage::InvalidThisContext => {
                format!("`this` can only be used in a method body")
            }
            ErrorMessage::MissingLedgerProperty => {
                format!("Ledger property not found on Contract class")
            }
            ErrorMessage::TooManyContractProperties => {
                format!("Too many properties on Contract class, expected only one")
            }
            ErrorMessage::MissingLedgerPropertyAnnotation => {
                format!("Ledger property must implement the user-defined `Ledger` class")
            }
            ErrorMessage::Custom(msg) => {
                format!("{}", msg)
            }
        }
    }
}
