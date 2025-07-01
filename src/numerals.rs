/// An implementation of standard form Roman numerals based on the definitions at
/// https://en.wikipedia.org/wiki/Roman_numerals
/// There are several ways to use the module:
/// - Parse from a `String` or `&str`, e.g. `let result = "MIV".parse::<RomanNumeral>();`
///   or `let result: RomanNumeralResult = "MIV".parse();`
/// - Convert from an `i32`, e.g. `let result: RomanNumeralResult = 1_044.try_into();`
/// - Convert from a `RomanNumeral` to an `i32`, e.g. `let i: i32 = numeral.into();`
/// - Display from a `RomanNumeral`
/// - Add, subtract, multiply, and divide `RomanNumeral`, but keep in mind the narrow range of
///   values representable as RomanNumeral (overflow is more common than with larger sets of
///   integers)
use crate::numerals::RomanNumeral::*;
use std::fmt;
use std::fmt::{Display, Formatter};
use std::str::FromStr;
use thiserror::Error;
use unicode_segmentation::UnicodeSegmentation;

#[derive(Clone, Error, Debug, PartialEq)]
pub enum RomanNumeralError {
    #[error("Not a Roman numeral: {0}")]
    NotRomanNumeral(String),
    #[error("{0} is greater than 3,999, the largest value a Roman numeral can have.")]
    ValueTooGreat(String),
    #[error("{0} is less than 1, the smallest Roman numeral value can have.")]
    ValueTooSmall(String),
    #[error("Numerals are out of order: {0}")]
    NumeralsOutOfOrder(String),
    #[error("Errors with Roman numeral")]
    NumeralHasErrors(Vec<RomanNumeralError>),
}

pub type RomanNumeralResult = Result<RomanNumeral, RomanNumeralError>;

/// The abstract representation of Roman numerals. Individual numerals and subtractive forms
/// are both explicitly represented so that simplification can be done once.
/// Compound numerals can be arbitrarily nested structurally but are flattened when parsing or
/// converting into a single level Compound numeral.
#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub enum RomanNumeral {
    // Individual numerals and subtractive forms in order from the smallest value to the greatest
    // value. This order is important to the implementation.
    I,
    IV,
    V,
    IX,
    X,
    XL,
    L,
    XC,
    C,
    CD,
    D,
    CM,
    M,
    // Compound numeral
    Compound(Vec<RomanNumeral>),
}

impl RomanNumeral {
    /// Add two RomanNumeral
    pub fn plus(&self, rhs: &RomanNumeral) -> RomanNumeralResult {
        let lhs_i32: i32 = self.into();
        let rhs_i32: i32 = rhs.into();
        let sum = lhs_i32 + rhs_i32;

        sum.try_into()
    }

    /// Subtract rhs from self
    pub fn minus(&self, rhs: &RomanNumeral) -> RomanNumeralResult {
        let lhs_i32: i32 = self.into();
        let rhs_i32: i32 = rhs.into();
        let sum = lhs_i32 - rhs_i32;

        sum.try_into()
    }

    /// Multiply two RomanNumeral
    pub fn multiply(&self, rhs: &RomanNumeral) -> RomanNumeralResult {
        let lhs_i32: i32 = self.into();
        let rhs_i32: i32 = rhs.into();
        let sum = lhs_i32 * rhs_i32;

        sum.try_into()
    }

    /// Divide two RomanNumeral, truncating any fractional part on division just like i32
    pub fn divide(&self, rhs: &RomanNumeral) -> RomanNumeralResult {
        let lhs_i32: i32 = self.into();
        let rhs_i32: i32 = rhs.into();
        let sum = lhs_i32 / rhs_i32;

        sum.try_into()
    }
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
            Err(RomanNumeralError::NumeralHasErrors(unwrapped_errors))
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
        if value < 1 {
            Err(RomanNumeralError::ValueTooSmall(value.to_string()))
        } else if value > 3_999 {
            Err(RomanNumeralError::ValueTooGreat(value.to_string()))
        } else {
            let mut numeral = Vec::new();

            let thousands = value / 1_000;
            for _ in 0..thousands {
                numeral.push(M);
            }

            let hundreds = (value - (thousands * 1_000)) / 100;
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
    // A while loop is used to avoid ownership complications with for loops
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

/// Constant function for converting pairs of numerals into subtractive forms when there is a match.
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

/// Flattens the given numeral into either a single unitary numeral or a single Compound
/// numeral with no nested Compound numerals.
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

            // Check that no numerals are out of order. This must be done after the subtractive
            // forms are merged.
            let mut merged_iter = merged.clone().into_iter().peekable();
            while let Some(numeral) = merged_iter.next() {
                if let Some(next) = merged_iter.peek() {
                    // Check that the numerals are in order
                    if &numeral < next {
                        return Err(RomanNumeralError::NumeralsOutOfOrder(format!(
                            "{} must come before {}",
                            next, numeral
                        )));
                    }
                }
            }

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

/// Recursive helper for flattening Compound numerals
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

    #[test]
    fn plus() {
        let two: i32 = I.plus(&I).unwrap().into();
        assert_eq!(two, 2);

        let thousand: i32 = D.plus(&D).unwrap().into();
        assert_eq!(thousand, 1_000);

        check_4000_error(M.plus(&M).unwrap().plus(&M).unwrap().plus(&M).unwrap_err());
    }

    #[test]
    fn minus() {
        let three: i32 = IV.minus(&I).unwrap().into();
        assert_eq!(three, 3);

        let eight_hundred: i32 = CM.minus(&C).unwrap().into();
        assert_eq!(eight_hundred, 800);

        check_0_error(I.minus(&I).unwrap_err());
    }

    #[test]
    fn multiply() {
        let sixteen: i32 = IV.multiply(&IV).unwrap().into();
        assert_eq!(sixteen, 16);

        let four_hundred: i32 = C.multiply(&IV).unwrap().into();
        assert_eq!(four_hundred, 400);

        check_4000_error(M.multiply(&IV).unwrap_err());
    }

    #[test]
    fn divide() {
        let ten: i32 = C.divide(&X).unwrap().into();
        assert_eq!(ten, 10);

        let twenty_five: i32 = C.divide(&IV).unwrap().into();
        assert_eq!(twenty_five, 25);

        check_0_error(I.divide(&IV).unwrap_err());
    }

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
        check_numeral("M", M, 1_000);
    }

    #[test]
    fn one_thousand_six_hundred_sixty_six() {
        // Tests all singular numerals in one pass
        check_numeral("MDCLXVI", Compound(vec![M, D, C, L, X, V, I]), 1_666);
    }

    #[test]
    fn largest_roman_numeral_in_standard_form() {
        check_numeral("MMMCMXCIX", Compound(vec![M, M, M, CM, XC, IX]), 3_999);
    }

    #[test]
    fn out_of_order() {
        let e = "MIVM".parse::<RomanNumeral>().unwrap_err();
        match e {
            RomanNumeralError::NumeralsOutOfOrder(value)
                if value == "M must come before IV".to_string() => {} // correct
            RomanNumeralError::NumeralHasErrors(_) => panic!("Incorrect error type"),
            RomanNumeralError::NotRomanNumeral(value)
            | RomanNumeralError::ValueTooGreat(value)
            | RomanNumeralError::ValueTooSmall(value)
            | RomanNumeralError::NumeralsOutOfOrder(value) => {
                panic!("Incorrect error type {value}")
            }
        }
    }

    #[test]
    fn out_of_bounds() {
        let result: RomanNumeralResult = 0.try_into();
        check_0_error(result.unwrap_err());

        let result: RomanNumeralResult = 4000.try_into();
        check_4000_error(result.unwrap_err());
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

    fn check_0_error(e: RomanNumeralError) {
        match e {
            RomanNumeralError::ValueTooSmall(value) if value == "0".to_string() => {} // correct
            RomanNumeralError::ValueTooSmall(value) => panic!("Incorrect value {value}"),
            RomanNumeralError::NumeralHasErrors(_)
            | RomanNumeralError::ValueTooGreat(_)
            | RomanNumeralError::NotRomanNumeral(_)
            | RomanNumeralError::NumeralsOutOfOrder(_) => panic!("Incorrect error type"),
        }
    }

    fn check_4000_error(e: RomanNumeralError) {
        match e {
            RomanNumeralError::ValueTooGreat(value) if value == "4000".to_string() => {} // correct
            RomanNumeralError::ValueTooGreat(value) => panic!("Incorrect value {value}"),
            RomanNumeralError::NumeralHasErrors(_)
            | RomanNumeralError::ValueTooSmall(_)
            | RomanNumeralError::NotRomanNumeral(_)
            | RomanNumeralError::NumeralsOutOfOrder(_) => {
                panic!("Incorrect error type")
            }
        }
    }

    fn check_empty_error(e: RomanNumeralError) {
        match e {
            RomanNumeralError::NotRomanNumeral(value) if value == "".to_string() => {} // correct
            RomanNumeralError::NumeralHasErrors(_)
            | RomanNumeralError::ValueTooGreat(_)
            | RomanNumeralError::ValueTooSmall(_)
            | RomanNumeralError::NotRomanNumeral(_)
            | RomanNumeralError::NumeralsOutOfOrder(_) => panic!("Incorrect error type"),
        }
    }

    #[test]
    fn parse_errors() {
        check_empty_error("".parse::<RomanNumeral>().unwrap_err());

        check_empty_error("     ".parse::<RomanNumeral>().unwrap_err());

        let e = " I Z V X L P C 🔺D M ".parse::<RomanNumeral>().unwrap_err();
        match e {
            RomanNumeralError::NumeralHasErrors(errors) => {
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
            RomanNumeralError::NotRomanNumeral(_)
            | RomanNumeralError::ValueTooGreat(_)
            | RomanNumeralError::ValueTooSmall(_)
            | RomanNumeralError::NumeralsOutOfOrder(_) => panic!("Incorrect error type"),
        }
    }
}
