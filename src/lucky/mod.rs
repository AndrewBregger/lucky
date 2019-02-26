// mod report;
// mod report;

// mod super::{Reporter, ExecutionResult};
pub mod ast;
pub mod report;
pub mod scanner;
pub mod parser;

pub use self::report::Reporter;
pub use self::scanner::{TokenStream, Token};
pub use self::parser::{Parser};

use std::io;
use std::io::Write;
use std::io::{BufRead};

enum ExecutionResult {
    Success,
    Fail
}

enum ReplCommands {
    Quit,
}

pub struct Lucky {
    reporter: Reporter,
    file: Option<String>
}

impl Lucky {
    pub fn with_file(file: &str) -> Self {
        Self {
            reporter: Reporter::new(),
            file: Some(file.to_owned())
        }
    }

    pub fn no_file() -> Self {
        Lucky {
            reporter: Reporter::new(),
            file: None
        }
    }

    pub fn run(& mut self) {
        if self.file.is_none() {
            self.run_repl()
        }
        else {
            self.compile_file()
        }
    }

    pub fn run_repl(&mut self) {
        let stdin = io::stdin();

        loop {
            let mut input = String::new();
            print!("> ");
            io::stdout().flush().ok();
            let res = stdin.lock().read_line(&mut input);
            if res.is_ok() {
                input = input.trim().to_string();
                if input.len() == 0 {
                    continue
                }

                if let Some(cmd) = Lucky::parse_command(input.as_str()) {
                    match cmd {
                        Quit => break,
                    }
                }

                // let tokens = TokenStream::from_string(input.as_str()).collect::<Vec<_>>();
                let mut parser = Parser::from_string(&mut self.reporter, input.as_str());

                println!("{:?}", parser.parse_expr(None));
            }
            else {
                continue;
            }
        }
    }

    pub fn compile_file(&mut self) {

    }

    fn parse_command(input: &str) -> Option<ReplCommands> {
        if let Some(ch) = input.chars().nth(0) {
            if ch == ':' {
                if let Some(cmd) = input.chars().nth(1) {
                    match cmd {
                        'q' | 'Q' => Some(ReplCommands::Quit),
                        _ => None,
                    }
                }
                else {
                    None
                }
            }
            else {
                None
            }
        }
        else {
            None
        }
    }
}