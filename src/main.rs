mod lucky;
use std::env;

fn main() {
    // skips the first argument (the executable name)
    let mut args = env::args().skip(1).collect::<Vec<String>>();
    let mut lucky: lucky::Lucky;

    if args.is_empty() {
        lucky = lucky::Lucky::no_file();
    }
    else {
        lucky = lucky::Lucky::with_file(args[0].as_str());
    }

    lucky.run();
}
