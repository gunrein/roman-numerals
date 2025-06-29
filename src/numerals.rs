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
    #[error("Unknown Roman numeral error")]
    Unknown,
}

pub type RomanNumeralResult = Result<RomanNumeral, RomanNumeralError>;

#[derive(PartialEq, Debug)]
pub enum RomanNumeral {
    I,
    V,
    X,
    L,
    C,
    D,
    M,
}

impl FromStr for RomanNumeral {
    type Err = RomanNumeralError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.trim() {
            "I" => Ok(Self::I),
            "V" => Ok(Self::V),
            "X" => Ok(Self::X),
            "L" => Ok(Self::L),
            "C" => Ok(Self::C),
            "D" => Ok(Self::D),
            "M" => Ok(Self::M),
            other => Err(RomanNumeralError::NotRomanNumeral(other.to_string())),
        }
    }
}

impl From<RomanNumeral> for i32 {
    fn from(value: RomanNumeral) -> Self {
        match value {
            RomanNumeral::I => 1,
            RomanNumeral::V => 5,
            RomanNumeral::X => 10,
            RomanNumeral::L => 50,
            RomanNumeral::C => 100,
            RomanNumeral::D => 500,
            RomanNumeral::M => 1000,
        }
    }
}

#[derive(PartialEq, Debug)]
pub struct RomanNumerals {
    pub numerals: Vec<RomanNumeral>,
}

impl FromStr for RomanNumerals {
    type Err = RomanNumeralError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let trimmed = s.trim();

        let (ok_numerals, errors): (Vec<RomanNumeralResult>, Vec<RomanNumeralResult>) =
            UnicodeSegmentation::graphemes(trimmed, true)
                .map(|grapheme| grapheme.parse::<RomanNumeral>())
                .partition(Result::is_ok);

        if errors.is_empty() {
            let numerals = ok_numerals
                .into_iter()
                // unwrap should be safe here since there are no error results in the set
                .map(|r| r.unwrap())
                .collect();
            Ok(RomanNumerals { numerals })
        } else {
            let unwrapped_errors = errors.into_iter().map(|e| e.unwrap_err()).collect();
            Err(RomanNumeralError::NumeralsHasError(unwrapped_errors))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn numerals_parse() {
        assert_eq!(
            "MDCLXVI".parse::<RomanNumerals>().unwrap().numerals,
            vec![
                RomanNumeral::M,
                RomanNumeral::D,
                RomanNumeral::C,
                RomanNumeral::L,
                RomanNumeral::X,
                RomanNumeral::V,
                RomanNumeral::I,
            ]
        );
        assert_eq!(
            "IVXLCDM".parse::<RomanNumerals>().unwrap().numerals,
            vec![
                RomanNumeral::I,
                RomanNumeral::V,
                RomanNumeral::X,
                RomanNumeral::L,
                RomanNumeral::C,
                RomanNumeral::D,
                RomanNumeral::M,
            ]
        );

        let e = " I Z V X L P C 🔺D M "
            .parse::<RomanNumerals>()
            .unwrap_err();
        match e {
            RomanNumeralError::NumeralsHasError(errors) => {
                assert_eq!(
                    errors,
                    vec![
                        RomanNumeralError::NotRomanNumeral("".to_string()),
                        RomanNumeralError::NotRomanNumeral("Z".to_string()),
                        RomanNumeralError::NotRomanNumeral("".to_string()),
                        RomanNumeralError::NotRomanNumeral("".to_string()),
                        RomanNumeralError::NotRomanNumeral("".to_string()),
                        RomanNumeralError::NotRomanNumeral("".to_string()),
                        RomanNumeralError::NotRomanNumeral("P".to_string()),
                        RomanNumeralError::NotRomanNumeral("".to_string()),
                        RomanNumeralError::NotRomanNumeral("".to_string()),
                        RomanNumeralError::NotRomanNumeral("🔺".to_string()),
                        RomanNumeralError::NotRomanNumeral("".to_string()),
                    ]
                );
            }
            RomanNumeralError::Unknown | RomanNumeralError::NotRomanNumeral(_) => {
                panic!("Incorrect error type")
            }
        }
    }

    #[test]
    fn numeral_parse() {
        assert_eq!("I".parse::<RomanNumeral>().unwrap(), RomanNumeral::I);
        assert_eq!("V".parse::<RomanNumeral>().unwrap(), RomanNumeral::V);
        assert_eq!("X".parse::<RomanNumeral>().unwrap(), RomanNumeral::X);
        assert_eq!("L".parse::<RomanNumeral>().unwrap(), RomanNumeral::L);
        assert_eq!("C".parse::<RomanNumeral>().unwrap(), RomanNumeral::C);
        assert_eq!("D".parse::<RomanNumeral>().unwrap(), RomanNumeral::D);
        assert_eq!("M".parse::<RomanNumeral>().unwrap(), RomanNumeral::M);

        let e = "".parse::<RomanNumeral>().unwrap_err();
        match e {
            RomanNumeralError::NotRomanNumeral(value) if value == "".to_string() => {} // correct
            RomanNumeralError::NotRomanNumeral(value) => panic!("Incorrect value {value}"),
            RomanNumeralError::Unknown | RomanNumeralError::NumeralsHasError(_) => {
                panic!("Incorrect error type")
            }
        }

        let e = "blah".parse::<RomanNumeral>().unwrap_err();
        match e {
            RomanNumeralError::NotRomanNumeral(value) if value == "blah".to_string() => {} // correct
            RomanNumeralError::NotRomanNumeral(value) => panic!("Incorrect value {value}"),
            RomanNumeralError::Unknown | RomanNumeralError::NumeralsHasError(_) => {
                panic!("Incorrect error type")
            }
        }

        let e = "Z".parse::<RomanNumeral>().unwrap_err();
        match e {
            RomanNumeralError::NotRomanNumeral(value) if value == "Z".to_string() => {} // correct
            RomanNumeralError::NotRomanNumeral(value) => panic!("Incorrect value {value}"),
            RomanNumeralError::Unknown | RomanNumeralError::NumeralsHasError(_) => {
                panic!("Incorrect error type")
            }
        }
    }

    #[test]
    fn numeral_values() {
        assert_eq!(i32::from(RomanNumeral::I), 1_i32);
        assert_eq!(i32::from(RomanNumeral::V), 5_i32);
        assert_eq!(i32::from(RomanNumeral::X), 10_i32);
        assert_eq!(i32::from(RomanNumeral::L), 50_i32);
        assert_eq!(i32::from(RomanNumeral::C), 100_i32);
        assert_eq!(i32::from(RomanNumeral::D), 500_i32);
        assert_eq!(i32::from(RomanNumeral::M), 1000_i32);
    }
}
