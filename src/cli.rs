use crate::numerals::{RomanNumeral, RomanNumeralError, RomanNumeralResult};
use clap::{Parser, Subcommand};
use inquire::ui::{RenderConfig, Styled};
use inquire::Text;

#[derive(Parser)]
#[command(version, about, long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Option<Commands>,
}

#[derive(Subcommand)]
enum Commands {
    /// Convert integers to Roman numerals and vice versa
    Convert {
        /// The value to convert, e.g. 4 or IV
        value: String,
    },
    /// Run the read-eval-print-loop for Roman numerals
    Repl,
}

/// Run the CLI and handle any commands
pub fn run_cli() {
    let cli = Cli::parse();

    match &cli.command {
        // Default to running the repl
        None => repl(),
        Some(Commands::Convert { value }) => convert_or_print_error(value),
        Some(Commands::Repl) => repl(),
    }
}

fn repl() {
    let prompt_prefix = Styled::new(">");
    let prompt_style: RenderConfig = RenderConfig::default().with_prompt_prefix(prompt_prefix);
    let prompt = Text::new("").with_render_config(prompt_style);

    println!(
        r#"Welcome to the Roman numerals REPL (read-eval-print-loop)!

Enter a single Roman numeral or integer at the prompt and press <enter> to
convert the input. Some examples to try: IV, 4, MMDC, IX, 729.

To exit, enter `quit`, `exit`, or `end` and press <enter>;
or, press CTRL+D or CTRL+C."#
    );
    loop {
        let result = prompt.clone().prompt();
        match result {
            Ok(input) => match input.as_str() {
                "" => continue,
                "quit" | "exit" | "end" => break,
                _ => convert_or_print_error(&input),
            },
            Err(_err) => {
                break;
            }
        }
    }
    eprintln!("Have a nice day!");
}

fn convert_or_print_error(value: &str) {
    // First see if the input is an integer
    match value.parse::<i32>() {
        Ok(integer) => {
            // The input is an integer so try converting it to a Roman numeral.
            let result: RomanNumeralResult = integer.try_into();
            match result {
                Ok(numeral) => println!("{}", numeral),
                Err(err) => print_error(err),
            }
        }
        Err(_) => {
            // The input wasn't an integer so try it as the String representation of a
            // Roman numeral.
            match value.parse::<RomanNumeral>() {
                Ok(numeral) => {
                    let i: i32 = numeral.into();
                    println!("{}", i);
                }
                Err(err) => print_error(err),
            }
        }
    }
}

fn print_error(err: RomanNumeralError) {
    match err {
        RomanNumeralError::NumeralHasErrors(errors) => {
            for error in errors {
                print_error(error);
            }
        }
        error => eprintln!("{}", error),
    }
}
