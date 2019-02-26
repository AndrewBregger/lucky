pub struct Reporter {
    numErrors: u32,
    numWarnings: u32
}

pub struct File {
}

pub struct ReportPosition {
    pub line: u32,
    pub column: u32,
    pub span: u32,
    pub file: Option<File>
}

impl Reporter {
    pub fn new() -> Reporter {
        Reporter {
            numErrors: 0,
            numWarnings: 0
        }
    }

    pub fn report_error(&mut self, pos: ReportPosition, msg: String) {
        self.report_error_(pos, msg.as_str());
    }

    pub fn report_error_(&mut self, pos: ReportPosition, msg: &str) {
        self.numErrors += 1;
        println!("{}:{} Error: {}", pos.line, pos.column, msg);
    }
}