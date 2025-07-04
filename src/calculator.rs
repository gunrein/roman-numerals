use crate::calculator::CalculatorError::{ExprErrorWithPosition, TokenErrors};
use crate::calculator::TokenError::UnexpectedCharacters;
use crate::numerals::{RomanNumeral, RomanNumeralError, RomanNumeralResult};
use std::fmt;
use std::fmt::{Display, Formatter};
use std::iter::Peekable;
use thiserror::Error;
use unicode_segmentation::{Graphemes, UnicodeSegmentation};

#[derive(Clone, Error, Debug)]
pub enum CalculatorError {
    #[error(transparent)]
    RomanNumeralError(#[from] RomanNumeralError),
    #[error("The expression is longer than what is supported.")]
    ExpressionTooLong,
    #[error("{error:?} at {position}")]
    ExprErrorWithPosition {
        error: RomanNumeralError,
        position: ParserPosition,
    },
    #[error("Unexpected characters were in the expression. Only decimal digits, Roman numerals, +, -, *, /, (, ), and whitespace is accepted.")]
    TokenErrors(Vec<TokenError>),
}

#[derive(Clone, Error, Debug)]
pub enum TokenError {
    #[error("Unexpected characters {characters:?} at {position}")]
    UnexpectedCharacters {
        characters: String,
        position: ParserPosition,
    },
}

pub type CalculatorResult = Result<RomanNumeral, CalculatorError>;

pub fn evaluate(expression: &str) -> CalculatorResult {
    let expr = parse(expression)?;
    eval(expr)
}

#[derive(Clone, Debug, PartialEq)]
struct TokenInput(String);

#[derive(Clone, Debug, PartialEq)]
enum Token {
    Plus {
        position: ParserPosition,
    },
    Minus {
        position: ParserPosition,
    },
    Multiply {
        position: ParserPosition,
    },
    Divide {
        position: ParserPosition,
    },
    LeftParen {
        position: ParserPosition,
    },
    RightParen {
        position: ParserPosition,
    },
    Integer {
        input: TokenInput,
        position: ParserPosition,
    },
    Numeral {
        input: TokenInput,
        position: ParserPosition,
    },
    WhitespaceSection {
        input: TokenInput,
        position: ParserPosition,
    },
    UnexpectedInput {
        input: TokenInput,
        position: ParserPosition,
    },
}

impl Token {
    fn length(&self) -> usize {
        match self {
            Token::Plus { position }
            | Token::Minus { position }
            | Token::Multiply { position }
            | Token::Divide { position }
            | Token::LeftParen { position }
            | Token::RightParen { position }
            | Token::Integer { position, .. }
            | Token::Numeral { position, .. }
            | Token::WhitespaceSection { position, .. }
            | Token::UnexpectedInput { position, .. } => position.length,
        }
    }
}

fn is_whitespace(grapheme: &str) -> bool {
    // TODO handle all UTF whitespace graphemes
    matches!(grapheme, " " | "\t" | "\n")
}

fn is_decimal(grapheme: &str) -> bool {
    matches!(grapheme, "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9")
}

fn is_roman_numeral_grapheme(grapheme: &str) -> bool {
    matches!(grapheme, "M" | "D" | "C" | "L" | "X" | "V" | "I")
}

fn is_left_paren(grapheme: &str) -> bool {
    "(" == grapheme
}

fn is_right_paren(grapheme: &str) -> bool {
    ")" == grapheme
}

fn is_plus(grapheme: &str) -> bool {
    "+" == grapheme
}

fn is_minus(grapheme: &str) -> bool {
    "-" == grapheme
}

fn is_star(grapheme: &str) -> bool {
    "*" == grapheme
}

fn is_slash(grapheme: &str) -> bool {
    "/" == grapheme
}

fn is_valid(grapheme: &str) -> bool {
    is_whitespace(grapheme)
        || is_decimal(grapheme)
        || is_roman_numeral_grapheme(grapheme)
        || is_left_paren(grapheme)
        || is_right_paren(grapheme)
        || is_plus(grapheme)
        || is_minus(grapheme)
        || is_star(grapheme)
        || is_slash(grapheme)
}

fn consume_token<F>(
    first_grapheme: &str,
    graphemes: &mut Peekable<Graphemes>,
    predicate: &mut F,
) -> TokenInput
where
    F: FnMut(&str) -> bool,
{
    let mut input: String = first_grapheme.to_string();
    while let Some(next_grapheme) = graphemes.next_if(|g| {
        let x = *g;
        predicate(x)
    }) {
        input.push_str(next_grapheme);
    }
    TokenInput(input)
}

fn next_token(graphemes: &mut Peekable<Graphemes>, start: usize) -> Option<Token> {
    while let Some(grapheme) = graphemes.next() {
        if is_whitespace(grapheme) {
            let input = consume_token(grapheme, graphemes, &mut is_whitespace);
            let length = input.0.len();
            return Some(Token::WhitespaceSection {
                input,
                position: ParserPosition { start, length },
            });
        }

        if is_decimal(grapheme) {
            let input = consume_token(grapheme, graphemes, &mut is_decimal);
            let length = input.0.len();
            return Some(Token::Integer {
                input,
                position: ParserPosition { start, length },
            });
        }

        if is_roman_numeral_grapheme(grapheme) {
            let input = consume_token(grapheme, graphemes, &mut is_roman_numeral_grapheme);
            let length = input.0.len();
            return Some(Token::Numeral {
                input,
                position: ParserPosition { start, length },
            });
        }

        return match grapheme {
            "+" => Some(Token::Plus {
                position: ParserPosition { start, length: 1 },
            }),
            "-" => Some(Token::Minus {
                position: ParserPosition { start, length: 1 },
            }),
            "*" => Some(Token::Multiply {
                position: ParserPosition { start, length: 1 },
            }),
            "/" => Some(Token::Divide {
                position: ParserPosition { start, length: 1 },
            }),
            "(" => Some(Token::LeftParen {
                position: ParserPosition { start, length: 1 },
            }),
            ")" => Some(Token::RightParen {
                position: ParserPosition { start, length: 1 },
            }),
            _ => {
                let input = consume_token(grapheme, graphemes, &mut |g| !is_valid(g));
                let length = input.0.len();
                Some(Token::UnexpectedInput {
                    input,
                    position: ParserPosition { start, length },
                })
            }
        };
    }

    None
}

fn tokenize(expression: &str) -> Result<Vec<Token>, CalculatorError> {
    let mut graphemes = UnicodeSegmentation::graphemes(expression, true).peekable();
    let mut tokens: Vec<Token> = Vec::new();
    let mut token_errors = Vec::new();
    let mut start = 0;
    while let Some(token) = next_token(&mut graphemes, start) {
        // Check for overflow
        if let Some(new_start) = start.checked_add(token.length()) {
            start = new_start
        } else {
            return Err(CalculatorError::ExpressionTooLong);
        }

        match token {
            Token::Plus { .. }
            | Token::Minus { .. }
            | Token::Multiply { .. }
            | Token::Divide { .. }
            | Token::LeftParen { .. }
            | Token::RightParen { .. }
            | Token::Integer { .. }
            | Token::Numeral { .. } => tokens.push(token),
            // WhitespaceSections are filtered from the final output, so just ignore them here
            Token::WhitespaceSection { .. } => {}
            Token::UnexpectedInput { input, position } => {
                token_errors.push(UnexpectedCharacters {
                    characters: input.0,
                    position,
                });
            }
        }
    }
    if !token_errors.is_empty() {
        Err(TokenErrors(token_errors))
    } else {
        Ok(tokens)
    }
}

fn parse(expression: &str) -> Result<Expr, CalculatorError> {
    // Tokenize
    let tokens = tokenize(expression)?;

    // Build expressions from tokens
    // TODO
    Ok(Expr::BinaryOperator {
        operator: BinaryOperator::Plus,
        left: Box::new(Expr::I32 {
            value: 1,
            position: ParserPosition {
                start: 0,
                length: 1,
            },
        }),
        right: Box::new(Expr::I32 {
            value: 2,
            position: ParserPosition {
                start: 4,
                length: 1,
            },
        }),
        position: ParserPosition {
            start: 0,
            length: 6,
        },
    })
}

fn eval(expr: Expr) -> CalculatorResult {
    match expr {
        Expr::RomanNumeral { value, .. } => Ok(value),
        Expr::I32 { value, position } => {
            let result: RomanNumeralResult = value.try_into();
            match result {
                Ok(numeral) => Ok(numeral),
                Err(error) => Err(ExprErrorWithPosition { error, position }),
            }
        }
        Expr::BinaryOperator {
            operator,
            left,
            right,
            position,
        } => {
            let lhs = eval(*left)?;
            let rhs = eval(*right)?;
            let result = match operator {
                BinaryOperator::Plus => lhs.plus(&rhs),
                BinaryOperator::Minus => lhs.minus(&rhs),
                BinaryOperator::Multiply => lhs.multiply(&rhs),
                BinaryOperator::Divide => lhs.divide(&rhs),
            };
            match result {
                Ok(numeral) => Ok(numeral),
                Err(error) => Err(ExprErrorWithPosition { error, position }),
            }
        }
        Expr::Parens { expr, .. } => eval(*expr),
    }
}

#[derive(Clone, Debug)]
enum Expr {
    RomanNumeral {
        value: RomanNumeral,
        position: ParserPosition,
    },
    I32 {
        value: i32,
        position: ParserPosition,
    },
    BinaryOperator {
        operator: BinaryOperator,
        left: Box<Expr>,
        right: Box<Expr>,
        position: ParserPosition,
    },
    Parens {
        expr: Box<Expr>,
        position: ParserPosition,
    }, // Expr {
       //     left: Expr,
       //     right: Expr,
       //     position: ParserPosition,
       // },
}

#[derive(Clone, Debug)]
enum BinaryOperator {
    Plus,
    Minus,
    Multiply,
    Divide,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ParserPosition {
    start: usize,
    length: usize,
}

impl Display for ParserPosition {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "between {} and {}", self.start, self.start + self.length)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::calculator::BinaryOperator::Plus;
    use crate::calculator::Expr::I32;

    #[test]
    fn test_tokenize() {
        let input = "  (VII + 123)   ";
        let tokens = tokenize(input).expect("Error tokenizing input");
        assert_eq!(
            tokens,
            vec![
                Token::LeftParen {
                    position: ParserPosition {
                        start: 2,
                        length: 1
                    }
                },
                Token::Numeral {
                    input: TokenInput("VII".to_string()),
                    position: ParserPosition {
                        start: 3,
                        length: 3
                    }
                },
                Token::Plus {
                    position: ParserPosition {
                        start: 7,
                        length: 1
                    }
                },
                Token::Integer {
                    input: TokenInput("123".to_string()),
                    position: ParserPosition {
                        start: 9,
                        length: 3
                    }
                },
                Token::RightParen {
                    position: ParserPosition {
                        start: 12,
                        length: 1
                    }
                },
            ]
        );
    }

    #[test]
    fn parse_integer_token() {
        let input = "123";
        let mut graphemes = UnicodeSegmentation::graphemes(input, true).peekable();
        let token = next_token(&mut graphemes, 0);
        assert_eq!(
            token,
            Some(Token::Integer {
                input: TokenInput(input.to_string()),
                position: ParserPosition {
                    start: 0,
                    length: 3
                }
            })
        );

        let input = "123   ";
        let mut graphemes = UnicodeSegmentation::graphemes(input, true).peekable();
        let token = next_token(&mut graphemes, 0);
        assert_eq!(
            token,
            Some(Token::Integer {
                input: TokenInput("123".to_string()),
                position: ParserPosition {
                    start: 0,
                    length: 3
                }
            })
        );

        let input = "   123";
        let mut graphemes = UnicodeSegmentation::graphemes(input, true).peekable();
        let token = next_token(&mut graphemes, 0);
        assert_eq!(
            token,
            Some(Token::Integer {
                input: TokenInput("123".to_string()),
                position: ParserPosition {
                    start: 3,
                    length: 3
                }
            })
        );

        let input = "   123   ";
        let mut graphemes = UnicodeSegmentation::graphemes(input, true).peekable();
        let token = next_token(&mut graphemes, 0);
        assert_eq!(
            token,
            Some(Token::Integer {
                input: TokenInput("123".to_string()),
                position: ParserPosition {
                    start: 3,
                    length: 3
                }
            })
        );
    }

    #[test]
    fn parse_numeral_token() {
        let input = "VII";
        let mut graphemes = UnicodeSegmentation::graphemes(input, true).peekable();
        let token = next_token(&mut graphemes, 0);
        assert_eq!(
            token,
            Some(Token::Numeral {
                input: TokenInput(input.to_string()),
                position: ParserPosition {
                    start: 0,
                    length: 3
                }
            })
        );

        let input = "VII   ";
        let mut graphemes = UnicodeSegmentation::graphemes(input, true).peekable();
        let token = next_token(&mut graphemes, 0);
        assert_eq!(
            token,
            Some(Token::Numeral {
                input: TokenInput("VII".to_string()),
                position: ParserPosition {
                    start: 0,
                    length: 3
                }
            })
        );

        let input = "   VII";
        let mut graphemes = UnicodeSegmentation::graphemes(input, true).peekable();
        let token = next_token(&mut graphemes, 0);
        assert_eq!(
            token,
            Some(Token::Numeral {
                input: TokenInput("VII".to_string()),
                position: ParserPosition {
                    start: 3,
                    length: 3
                }
            })
        );

        let input = "   VII   ";
        let mut graphemes = UnicodeSegmentation::graphemes(input, true).peekable();
        let token = next_token(&mut graphemes, 0);
        assert_eq!(
            token,
            Some(Token::Numeral {
                input: TokenInput("VII".to_string()),
                position: ParserPosition {
                    start: 3,
                    length: 3
                }
            })
        );
    }

    #[test]
    fn plus() {
        let one_plus_two = binary_op(Plus, ONE, TWO);
        let numeral = eval(one_plus_two.clone()).unwrap();
        let number: i32 = numeral.into();
        assert_eq!(number, 3);

        let three_plus_three =
            eval(binary_op(Plus, one_plus_two.clone(), one_plus_two.clone())).unwrap();
        let number: i32 = three_plus_three.into();
        assert_eq!(number, 6);

        // TODO errors
    }

    // TODO minus, multiply, divide, parens

    fn binary_op(operator: BinaryOperator, left: Expr, right: Expr) -> Expr {
        Expr::BinaryOperator {
            operator,
            left: Box::new(left),
            right: Box::new(right),
            position: ParserPosition {
                start: 0,
                length: 6,
            },
        }
    }

    const ONE: Expr = I32 {
        value: 1,
        position: ParserPosition {
            start: 0,
            length: 1,
        },
    };
    const TWO: Expr = I32 {
        value: 2,
        position: ParserPosition {
            start: 4,
            length: 1,
        },
    };
}
