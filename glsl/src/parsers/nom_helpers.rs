//! Various nom parser helpers.

use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{anychar, multispace1, newline};
use nom::combinator::{map, recognize, value};
use nom::error::{ErrorKind, VerboseError, VerboseErrorKind};
use nom::multi::fold_many0;
use nom::{Err as NomErr, IResult};

use nom::{AsBytes, InputLength, Slice};

use super::ParseInput;

pub type ParserResult<'c, 'd, O> = IResult<ParseInput<'c, 'd>, O, VerboseError<ParseInput<'c, 'd>>>;

// A constant parser that just forwards the value it’s parametered with without reading anything
// from the input. Especially useful as “fallback” in an alternative parser.
pub fn cnst<'a, 'b, T, E>(t: T) -> impl Fn(ParseInput<'a, 'b>) -> Result<(ParseInput<'a, 'b>, T), E>
where
  T: 'a + Clone,
  'a: 'b,
{
  move |i| Ok((i, t.clone()))
}

// End-of-input parser.
//
// Yields `()` if the parser is at the end of the input; an error otherwise.
pub fn eoi<'c, 'd>(i: ParseInput<'c, 'd>) -> ParserResult<'c, 'd, ()> {
  if i.input_len() == 0 {
    Ok((i, ()))
  } else {
    Err(NomErr::Error(VerboseError {
      errors: vec![(i, VerboseErrorKind::Nom(ErrorKind::Eof))],
    }))
  }
}

// A newline parser that accepts:
//
// - A newline.
// - The end of input.
pub fn eol<'c, 'd>(i: ParseInput<'c, 'd>) -> ParserResult<'c, 'd, ()> {
  alt((
    eoi, // this one goes first because it’s very cheap
    value((), newline),
  ))(i)
}

// Apply the `f` parser until `g` succeeds. Both parsers consume the input.
pub fn till<'c, 'd, A, B, F, G>(
  f: F,
  g: G,
) -> impl Fn(ParseInput<'c, 'd>) -> ParserResult<'c, 'd, ()>
where
  'c: 'd,
  F: Fn(ParseInput<'c, 'd>) -> ParserResult<'c, 'd, A>,
  G: Fn(ParseInput<'c, 'd>) -> ParserResult<'c, 'd, B>,
{
  move |mut i| loop {
    if let Ok((i2, _)) = g(i) {
      break Ok((i2, ()));
    }

    let (i2, _) = f(i)?;
    i = i2;
  }
}

// A version of many0 that discards the result of the parser, preventing allocating.
pub fn many0_<'c, 'd, A, F>(f: F) -> impl Fn(ParseInput<'c, 'd>) -> ParserResult<'c, 'd, ()>
where
  'c: 'd,
  F: Fn(ParseInput<'c, 'd>) -> ParserResult<'c, 'd, A>,
{
  move |i| fold_many0(&f, (), |_, _| ())(i)
}

/// Parse a string until the end of line.
///
/// This parser accepts the multiline annotation (\) to break the string on several lines.
///
/// Discard any leading newline.
pub fn str_till_eol<'c, 'd>(i: ParseInput<'c, 'd>) -> ParserResult<'c, 'd, ParseInput<'c, 'd>> {
  map(
    recognize(till(alt((value((), tag("\\\n")), value((), anychar))), eol)),
    |i| {
      if i.as_bytes().last() == Some(&b'\n') {
        i.slice(0..i.input_len() - 1)
      } else {
        i
      }
    },
  )(i)
}

// Blank base parser.
//
// This parser succeeds with multispaces and multiline annotation.
//
// Taylor Swift loves it.
pub fn blank_space<'c, 'd>(i: ParseInput<'c, 'd>) -> ParserResult<'c, 'd, ParseInput<'c, 'd>> {
  recognize(many0_(alt((multispace1, tag("\\\n")))))(i)
}
