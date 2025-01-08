mod parser;

fn main() {
    match parser::parse(
        "/Users/claudebarde/Desktop/current_projects/compactscript/compact/counter/contract.ts",
    ) {
        Ok(res) => println!("Result: \n\n{}", res),
        Err(e) => println!("Error: {}", e),
    }
}
