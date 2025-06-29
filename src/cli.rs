use crate::numerals::{RomanNumeral, RomanNumeralError, RomanNumeralResult};
use clap::{Parser, Subcommand};

#[derive(Parser)]
#[command(version, about, long_about = None)]
#[command(arg_required_else_help(true))]
struct Cli {
    #[command(subcommand)]
    command: Option<Commands>,
}

#[derive(Subcommand)]
enum Commands {
    /// Converts integers to Roman numerals and vice versa.
    Convert {
        /// The value to convert, e.g. 4 or IV
        value: String,
    },
}

/// Run the CLI and handle any commands
pub fn run_cli() {
    let cli = Cli::parse();

    match &cli.command {
        None => {}
        Some(Commands::Convert { value }) => {
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
    }
}

fn print_error(err: RomanNumeralError) {
    match err {
        RomanNumeralError::NumeralHasErrors(errors) => {
            for error in errors {
                print_error(error);
            }
        }
        error => println!("{}", error),
    }
}
