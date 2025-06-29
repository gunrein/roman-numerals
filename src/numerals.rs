/// Roman numerals based on the definitions at
/// https://en.wikipedia.org/wiki/Roman_numerals
use std::str::FromStr;
use thiserror::Error;
use unicode_segmentation::UnicodeSegmentation;

#[derive(Error, Debug, PartialEq)]
pub enum RomanNumeralError {
    #[error("Not a Roman numeral: {0}")]
    NotRomanNumeral(String),
    #[error("Compound numeral has errors")]
    NumeralsHasError(Vec<RomanNumeralError>),
}

pub type RomanNumeralResult = Result<RomanNumeral, RomanNumeralError>;

#[derive(PartialEq, Debug)]
pub enum RomanNumeral {
    // Individual numerals
    I,
    V,
    X,
    L,
    C,
    D,
    M,
    // Subtractive forms
    IV,
    IX,
    XL,
    XC,
    CD,
    CM,
    // Compound numeral
    Compound(Vec<RomanNumeral>),
}

impl FromStr for RomanNumeral {
    type Err = RomanNumeralError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let trimmed = s.trim();
        let (ok_numerals, errors): (Vec<RomanNumeralResult>, Vec<RomanNumeralResult>) =
            UnicodeSegmentation::graphemes(trimmed, true)
                .map(|grapheme| parse_numeral_from_grapheme(grapheme))
                .partition(Result::is_ok);

        if errors.is_empty() {
            let mut numerals_iter = ok_numerals
                .into_iter()
                // unwrap should be safe here since there are no error results in the vec based on the partition above
                .map(|r| r.unwrap())
                .into_iter()
                .peekable();

            // Reduce any subtractive forms to their singular representations
            let mut simplified_numerals: Vec<RomanNumeral> = Vec::new();
            while let Some(numeral) = numerals_iter.next() {
                if let Some(next) = numerals_iter.peek() {
                    match numeral {
                        RomanNumeral::I => {
                            if next == &RomanNumeral::V {
                                // Merge into a single numeral
                                simplified_numerals.push(RomanNumeral::IV);
                                // and consume the second numeral
                                numerals_iter.next();
                            } else if next == &RomanNumeral::X {
                                // Merge into a single numeral
                                simplified_numerals.push(RomanNumeral::IX);
                                // and consume the second numeral
                                numerals_iter.next();
                            } else {
                                simplified_numerals.push(numeral);
                            }
                        }
                        RomanNumeral::X => {
                            if next == &RomanNumeral::L {
                                // Merge into a single numeral
                                simplified_numerals.push(RomanNumeral::XL);
                                // and consume the second numeral
                                numerals_iter.next();
                            } else if next == &RomanNumeral::C {
                                // Merge into a single numeral
                                simplified_numerals.push(RomanNumeral::XC);
                                // and consume the second numeral
                                numerals_iter.next();
                            } else {
                                simplified_numerals.push(numeral);
                            }
                        }
                        RomanNumeral::C => {
                            if next == &RomanNumeral::D {
                                // Merge into a single numeral
                                simplified_numerals.push(RomanNumeral::CD);
                                // and consume the second numeral
                                numerals_iter.next();
                            } else if next == &RomanNumeral::M {
                                // Merge into a single numeral
                                simplified_numerals.push(RomanNumeral::CM);
                                // and consume the second numeral
                                numerals_iter.next();
                            } else {
                                simplified_numerals.push(numeral);
                            }
                        }
                        // All of these cases simply add themselves
                        RomanNumeral::V
                        | RomanNumeral::L
                        | RomanNumeral::D
                        | RomanNumeral::M
                        | RomanNumeral::IV
                        | RomanNumeral::IX
                        | RomanNumeral::XL
                        | RomanNumeral::XC
                        | RomanNumeral::CD
                        | RomanNumeral::CM
                        | RomanNumeral::Compound(_) => simplified_numerals.push(numeral),
                    }
                } else {
                    // No next numeral so just add the current one
                    simplified_numerals.push(numeral)
                }
            }

            if simplified_numerals.is_empty() {
                Err(RomanNumeralError::NotRomanNumeral(trimmed.to_string()))
            } else if simplified_numerals.len() == 1 {
                // If there is a single numeral already, just return it
                Ok(simplified_numerals.pop().unwrap())
            } else {
                // Must be a compound numeral
                Ok(RomanNumeral::Compound(simplified_numerals))
            }
        } else {
            let unwrapped_errors = errors.into_iter().map(|e| e.unwrap_err()).collect();
            Err(RomanNumeralError::NumeralsHasError(unwrapped_errors))
        }
    }
}

fn parse_numeral_from_grapheme(grapheme: &str) -> RomanNumeralResult {
    match grapheme {
        "I" => Ok(RomanNumeral::I),
        "V" => Ok(RomanNumeral::V),
        "X" => Ok(RomanNumeral::X),
        "L" => Ok(RomanNumeral::L),
        "C" => Ok(RomanNumeral::C),
        "D" => Ok(RomanNumeral::D),
        "M" => Ok(RomanNumeral::M),
        other => Err(RomanNumeralError::NotRomanNumeral(other.to_string())),
    }
}

impl From<&RomanNumeral> for i32 {
    fn from(value: &RomanNumeral) -> Self {
        into_i32(value)
    }
}

impl From<RomanNumeral> for i32 {
    fn from(value: RomanNumeral) -> Self {
        into_i32(&value)
    }
}

fn into_i32(numeral: &RomanNumeral) -> i32 {
    match numeral {
        RomanNumeral::I => 1,
        RomanNumeral::V => 5,
        RomanNumeral::X => 10,
        RomanNumeral::L => 50,
        RomanNumeral::C => 100,
        RomanNumeral::D => 500,
        RomanNumeral::M => 1000,
        RomanNumeral::IV => 4,
        RomanNumeral::IX => 9,
        RomanNumeral::XL => 40,
        RomanNumeral::XC => 90,
        RomanNumeral::CD => 400,
        RomanNumeral::CM => 900,
        RomanNumeral::Compound(numerals) => numerals
            .iter()
            .map(into_i32)
            .reduce(|left, right| left + right)
            .map_or(0, |f| f),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::numerals::RomanNumeral::*;

    #[test]
    fn one() {
        let n = "I".parse::<RomanNumeral>().unwrap();
        assert_eq!(n, I);
        assert_eq!(i32::from(n), 1);
    }

    #[test]
    fn four() {
        let n = "IV".parse::<RomanNumeral>().unwrap();
        assert_eq!(n, IV);
        assert_eq!(i32::from(n), 4);
    }

    #[test]
    fn five() {
        let n = "V".parse::<RomanNumeral>().unwrap();
        assert_eq!(n, RomanNumeral::V);
        assert_eq!(i32::from(n), 5);
    }

    #[test]
    fn nine() {
        let n = "IX".parse::<RomanNumeral>().unwrap();
        assert_eq!(n, IX);
        assert_eq!(i32::from(n), 9);
    }

    #[test]
    fn ten() {
        let n = "X".parse::<RomanNumeral>().unwrap();
        assert_eq!(n, RomanNumeral::X);
        assert_eq!(i32::from(n), 10);
    }

    #[test]
    fn forty() {
        let n = "XL".parse::<RomanNumeral>().unwrap();
        assert_eq!(n, XL);
        assert_eq!(i32::from(n), 40);
    }

    #[test]
    fn fifty() {
        let n = "L".parse::<RomanNumeral>().unwrap();
        assert_eq!(n, RomanNumeral::L);
        assert_eq!(i32::from(n), 50);
    }

    #[test]
    fn ninety() {
        let n = "XC".parse::<RomanNumeral>().unwrap();
        assert_eq!(n, XC);
        assert_eq!(i32::from(n), 90);
    }

    #[test]
    fn one_hundred() {
        let n = "C".parse::<RomanNumeral>().unwrap();
        assert_eq!(n, RomanNumeral::C);
        assert_eq!(i32::from(n), 100);
    }

    #[test]
    fn four_hundred() {
        let n = "CD".parse::<RomanNumeral>().unwrap();
        assert_eq!(n, CD);
        assert_eq!(i32::from(n), 400);
    }

    #[test]
    fn five_hundred() {
        let n = "D".parse::<RomanNumeral>().unwrap();
        assert_eq!(n, RomanNumeral::D);
        assert_eq!(i32::from(n), 500);
    }

    #[test]
    fn nine_hundred() {
        let n = "CM".parse::<RomanNumeral>().unwrap();
        assert_eq!(n, CM);
        assert_eq!(i32::from(n), 900);
    }

    #[test]
    fn one_thousand() {
        let n = "M".parse::<RomanNumeral>().unwrap();
        assert_eq!(n, RomanNumeral::M);
        assert_eq!(i32::from(n), 1000);
    }

    #[test]
    fn one_thousand_six_hundred_sixty_six() {
        // Tests all singular numerals in one pass
        let n = "MDCLXVI".parse::<RomanNumeral>().unwrap();
        assert_eq!(n, Compound(vec![M, D, C, L, X, V, I]));
        assert_eq!(i32::from(n), 1666);
    }

    #[test]
    fn largest_roman_numeral_in_standard_form() {
        let n = "MMMCMXCIX".parse::<RomanNumeral>().unwrap();
        assert_eq!(n, Compound(vec![M, M, M, CM, XC, IX]));
        assert_eq!(i32::from(n), 3999);
    }

    #[test]
    fn nested_compound_to_i32() {
        assert_eq!(i32::from(Compound(vec![Compound(vec![M, C, IX])])), 1109);
    }

    #[test]
    fn parse_errors() {
        let e = "".parse::<RomanNumeral>().unwrap_err();
        match e {
            RomanNumeralError::NotRomanNumeral(value) if value == "".to_string() => {} // correct
            RomanNumeralError::NotRomanNumeral(value) => panic!("Incorrect value {value}"),
            RomanNumeralError::NumeralsHasError(_) => {
                panic!("Incorrect error type")
            }
        }

        let e = "     ".parse::<RomanNumeral>().unwrap_err();
        match e {
            RomanNumeralError::NotRomanNumeral(value) if value == "".to_string() => {} // correct
            RomanNumeralError::NotRomanNumeral(value) => panic!("Incorrect value {value}"),
            RomanNumeralError::NumeralsHasError(_) => {
                panic!("Incorrect error type")
            }
        }

        let e = " I Z V X L P C 🔺D M ".parse::<RomanNumeral>().unwrap_err();
        match e {
            RomanNumeralError::NumeralsHasError(errors) => {
                assert_eq!(
                    errors,
                    vec![
                        RomanNumeralError::NotRomanNumeral(" ".to_string()),
                        RomanNumeralError::NotRomanNumeral("Z".to_string()),
                        RomanNumeralError::NotRomanNumeral(" ".to_string()),
                        RomanNumeralError::NotRomanNumeral(" ".to_string()),
                        RomanNumeralError::NotRomanNumeral(" ".to_string()),
                        RomanNumeralError::NotRomanNumeral(" ".to_string()),
                        RomanNumeralError::NotRomanNumeral("P".to_string()),
                        RomanNumeralError::NotRomanNumeral(" ".to_string()),
                        RomanNumeralError::NotRomanNumeral(" ".to_string()),
                        RomanNumeralError::NotRomanNumeral("🔺".to_string()),
                        RomanNumeralError::NotRomanNumeral(" ".to_string()),
                    ]
                );
            }
            RomanNumeralError::NotRomanNumeral(_) => {
                panic!("Incorrect error type")
            }
        }
    }
}
