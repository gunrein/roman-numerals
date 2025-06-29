use crate::numerals::RomanNumeral::*;
use std::fmt;
use std::fmt::{Display, Formatter};
/// Roman numerals based on the definitions at
/// https://en.wikipedia.org/wiki/Roman_numerals
use std::str::FromStr;
use thiserror::Error;
use unicode_segmentation::UnicodeSegmentation;

#[derive(Clone, Error, Debug, PartialEq)]
pub enum RomanNumeralError {
    #[error("Not a Roman numeral: {0}")]
    NotRomanNumeral(String),
    #[error("Compound numeral has errors")]
    NumeralsHasError(Vec<RomanNumeralError>),
}

pub type RomanNumeralResult = Result<RomanNumeral, RomanNumeralError>;

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
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
                .map(parse_numeral_from_grapheme)
                .partition(Result::is_ok);

        if errors.is_empty() {
            let numerals = ok_numerals
                .iter()
                // unwrap should be safe here since there are no error results in the vec based on the partition above
                .map(|r| r.clone().unwrap())
                .collect();

            flatten_numeral(&Compound(numerals), trimmed)
        } else {
            let unwrapped_errors = errors.into_iter().map(|e| e.unwrap_err()).collect();
            Err(RomanNumeralError::NumeralsHasError(unwrapped_errors))
        }
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

impl Display for RomanNumeral {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", into_string(self))
    }
}

impl TryFrom<i32> for RomanNumeral {
    type Error = RomanNumeralError;

    fn try_from(value: i32) -> Result<Self, Self::Error> {
        // Check bounds for standard form of Roman numerals
        if !(1..=3999).contains(&value) {
            Err(RomanNumeralError::NotRomanNumeral(value.to_string()))
        } else {
            let mut numeral = Vec::new();

            let thousands = value / 1000;
            for _ in 0..thousands {
                numeral.push(M);
            }

            let hundreds = (value - (thousands * 1000)) / 100;
            match hundreds {
                0 => {} // no op
                1 => numeral.push(C),
                2 => numeral.push(Compound(vec![C, C])),
                3 => numeral.push(Compound(vec![C, C, C])),
                4 => numeral.push(CD),
                5 => numeral.push(D),
                6 => numeral.push(Compound(vec![D, C])),
                7 => numeral.push(Compound(vec![D, C, C])),
                8 => numeral.push(Compound(vec![D, C, C, C])),
                9 => numeral.push(CM),
                out_of_bounds => {
                    return Err(RomanNumeralError::NotRomanNumeral(format!(
                        "{value} has an invalid hundreds amount {out_of_bounds}"
                    )))
                }
            }

            let tens = (value - (thousands * 1000 + hundreds * 100)) / 10;
            match tens {
                0 => {} // no op
                1 => numeral.push(X),
                2 => numeral.push(Compound(vec![X, X])),
                3 => numeral.push(Compound(vec![X, X, X])),
                4 => numeral.push(XL),
                5 => numeral.push(L),
                6 => numeral.push(Compound(vec![L, X])),
                7 => numeral.push(Compound(vec![L, X, X])),
                8 => numeral.push(Compound(vec![L, X, X, X])),
                9 => numeral.push(XC),
                out_of_bounds => {
                    return Err(RomanNumeralError::NotRomanNumeral(format!(
                        "{value} has an invalid tens amount {out_of_bounds}"
                    )))
                }
            }

            let ones = value - (thousands * 1000 + hundreds * 100 + tens * 10);
            match ones {
                0 => {} // no op
                1 => numeral.push(I),
                2 => numeral.push(Compound(vec![I, I])),
                3 => numeral.push(Compound(vec![I, I, I])),
                4 => numeral.push(IV),
                5 => numeral.push(V),
                6 => numeral.push(Compound(vec![V, I])),
                7 => numeral.push(Compound(vec![V, I, I])),
                8 => numeral.push(Compound(vec![V, I, I, I])),
                9 => numeral.push(IX),
                out_of_bounds => {
                    return Err(RomanNumeralError::NotRomanNumeral(format!(
                        "{value} has an invalid tens amount {out_of_bounds}"
                    )))
                }
            }

            // Flatten any nested Compound numerals
            flatten_numeral(&Compound(numeral), &value.to_string())
        }
    }
}

fn parse_numeral_from_grapheme(grapheme: &str) -> RomanNumeralResult {
    match grapheme {
        "I" => Ok(I),
        "V" => Ok(V),
        "X" => Ok(X),
        "L" => Ok(L),
        "C" => Ok(C),
        "D" => Ok(D),
        "M" => Ok(M),
        other => Err(RomanNumeralError::NotRomanNumeral(other.to_string())),
    }
}

// Reduce any subtractive forms to their singular representations
fn merge_subtractive_forms(ok_numerals: Vec<RomanNumeral>) -> Vec<RomanNumeral> {
    let mut numerals_iter = ok_numerals.into_iter().peekable();

    let mut merged_numerals: Vec<RomanNumeral> = Vec::new();
    while let Some(numeral) = numerals_iter.next() {
        if let Some(next) = numerals_iter.peek() {
            if let Some(merged) = subtractive_forms(&numeral, next) {
                // Merge the individual numerals of the subtractive form
                merged_numerals.push(merged);
                // and consume the second numeral
                numerals_iter.next();
            } else {
                merged_numerals.push(numeral);
            }
        } else {
            // No next numeral so just add the current one
            merged_numerals.push(numeral)
        }
    }

    merged_numerals
}

fn subtractive_forms(numeral: &RomanNumeral, next: &RomanNumeral) -> Option<RomanNumeral> {
    match (numeral, next) {
        (I, V) => Some(IV),
        (I, X) => Some(IX),
        (X, L) => Some(XL),
        (X, C) => Some(XC),
        (C, D) => Some(CD),
        (C, M) => Some(CM),
        // Avoiding default matches is strongly preferable, but in this case it seems ok.
        _ => None,
    }
}

fn into_i32(numeral: &RomanNumeral) -> i32 {
    match numeral {
        I => 1,
        V => 5,
        X => 10,
        L => 50,
        C => 100,
        D => 500,
        M => 1000,
        IV => 4,
        IX => 9,
        XL => 40,
        XC => 90,
        CD => 400,
        CM => 900,
        Compound(numerals) => numerals
            .iter()
            .map(into_i32)
            .reduce(|left, right| left + right)
            .map_or(0, |i| i),
    }
}

fn into_string(numeral: &RomanNumeral) -> String {
    match numeral {
        I => "I".into(),
        V => "V".into(),
        X => "X".into(),
        L => "L".into(),
        C => "C".into(),
        D => "D".into(),
        M => "M".into(),
        IV => "IV".into(),
        IX => "IX".into(),
        XL => "XL".into(),
        XC => "XC".into(),
        CD => "CD".into(),
        CM => "CM".into(),
        Compound(numerals) => numerals
            .iter()
            .map(into_string)
            .reduce(|left, right| format!("{left}{right}"))
            .map_or("".into(), |s| s),
    }
}

fn flatten_numeral(numeral: &RomanNumeral, input: &str) -> RomanNumeralResult {
    match numeral {
        I | V | X | L | C | D | M | IV | IX | XL | XC | CD | CM => Ok(numeral.clone()),
        Compound(numerals) => {
            // let flattened: Vec<&RomanNumeral> = numerals.iter().map(|n| flatten_helper(n).clone()).collect();
            let mut flattened: Vec<RomanNumeral> = Vec::new();
            for n in numerals {
                for x in flatten_helper(n) {
                    flattened.push(x.clone())
                }
            }

            let merged = merge_subtractive_forms(flattened);

            if merged.is_empty() {
                Err(RomanNumeralError::NotRomanNumeral(input.to_string()))
            } else if merged.len() == 1 {
                // If there is a single numeral already, just return it
                Ok(merged.first().unwrap().clone())
            } else {
                // Must be a compound numeral
                Ok(Compound(merged))
            }
        }
    }
}

fn flatten_helper(numeral: &RomanNumeral) -> Vec<&RomanNumeral> {
    match numeral {
        I | V | X | L | C | D | M | IV | IX | XL | XC | CD | CM => vec![numeral],
        Compound(numerals) => {
            let mut flattened: Vec<&RomanNumeral> = Vec::new();
            for n in numerals {
                for x in flatten_helper(n) {
                    flattened.push(x)
                }
            }
            flattened
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn check_numeral(input: &str, expected_numeral: RomanNumeral, expected_int: i32) {
        let n = input.parse::<RomanNumeral>().unwrap();
        assert_eq!(n, expected_numeral);
        let i: i32 = n.into();
        assert_eq!(i, expected_int);
        let r: RomanNumeral = i.try_into().unwrap();
        assert_eq!(r, expected_numeral);
        assert_eq!(r.to_string(), input);
    }

    #[test]
    fn one() {
        check_numeral("I", I, 1);
    }

    #[test]
    fn four() {
        check_numeral("IV", IV, 4);
    }

    #[test]
    fn five() {
        check_numeral("V", V, 5);
    }

    #[test]
    fn nine() {
        check_numeral("IX", IX, 9);
    }

    #[test]
    fn ten() {
        check_numeral("X", X, 10);
    }

    #[test]
    fn forty() {
        check_numeral("XL", XL, 40);
    }

    #[test]
    fn fifty() {
        check_numeral("L", L, 50);
    }

    #[test]
    fn ninety() {
        check_numeral("XC", XC, 90);
    }

    #[test]
    fn one_hundred() {
        check_numeral("C", C, 100);
    }

    #[test]
    fn four_hundred() {
        check_numeral("CD", CD, 400);
    }

    #[test]
    fn five_hundred() {
        check_numeral("D", D, 500);
    }

    #[test]
    fn nine_hundred() {
        check_numeral("CM", CM, 900);
    }

    #[test]
    fn one_thousand() {
        check_numeral("M", M, 1000);
    }

    #[test]
    fn one_thousand_six_hundred_sixty_six() {
        // Tests all singular numerals in one pass
        check_numeral("MDCLXVI", Compound(vec![M, D, C, L, X, V, I]), 1666);
    }

    #[test]
    fn largest_roman_numeral_in_standard_form() {
        check_numeral("MMMCMXCIX", Compound(vec![M, M, M, CM, XC, IX]), 3999);
    }

    #[test]
    fn out_of_bounds() {
        let result: RomanNumeralResult = 0.try_into();
        let e = result.unwrap_err();
        match e {
            RomanNumeralError::NotRomanNumeral(value) if value == "0".to_string() => {} // correct
            RomanNumeralError::NotRomanNumeral(value) => panic!("Incorrect value {value}"),
            RomanNumeralError::NumeralsHasError(_) => {
                panic!("Incorrect error type")
            }
        }

        let result: RomanNumeralResult = 4000.try_into();
        let e = result.unwrap_err();
        match e {
            RomanNumeralError::NotRomanNumeral(value) if value == "4000".to_string() => {} // correct
            RomanNumeralError::NotRomanNumeral(value) => panic!("Incorrect value {value}"),
            RomanNumeralError::NumeralsHasError(_) => {
                panic!("Incorrect error type")
            }
        }
    }

    #[test]
    fn flatten() {
        // Not an actually-valid Roman numeral, just a complicated expression to check that flatten works
        let nested = Compound(vec![
            M,
            Compound(vec![M, M, M, Compound(vec![M, M]), Compound(vec![M, M])]),
            M,
        ]);
        let expected = Compound(vec![M, M, M, M, M, M, M, M, M]);
        assert_eq!(flatten_numeral(&nested, "nested").unwrap(), expected);
    }

    #[test]
    fn nested_compound_to_i32() {
        assert_eq!(i32::from(Compound(vec![Compound(vec![M, C, IX])])), 1109);
    }

    fn check_empty_error(e: RomanNumeralError) {
        match e {
            RomanNumeralError::NotRomanNumeral(value) if value == "".to_string() => {} // correct
            RomanNumeralError::NotRomanNumeral(value) => panic!("Incorrect value {value}"),
            RomanNumeralError::NumeralsHasError(_) => {
                panic!("Incorrect error type")
            }
        }
    }

    #[test]
    fn parse_errors() {
        check_empty_error("".parse::<RomanNumeral>().unwrap_err());

        check_empty_error("     ".parse::<RomanNumeral>().unwrap_err());

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
