//! Various nom parser helpers.

use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{anychar, multispace1, newline};
use nom::combinator::{map, recognize, value};
use nom::error::{ErrorKind, VerboseError, VerboseErrorKind};
use nom::multi::fold_many0;
use nom::{Err as NomErr, IResult};

use nom::{AsBytes, InputLength, Slice};
use nom_locate::{position, LocatedSpan};

pub type ParserResult<'a, O> = IResult<LocatedSpan<&'a str>, O, VerboseError<LocatedSpan<&'a str>>>;

// A constant parser that just forwards the value it’s parametered with without reading anything
// from the input. Especially useful as “fallback” in an alternative parser.
pub fn cnst<'a, T, E>(t: T) -> impl Fn(LocatedSpan<&'a str>) -> Result<(LocatedSpan<&'a str>, T), E>
where
  T: 'a + Clone,
{
  move |i| Ok((i, t.clone()))
}

// End-of-input parser.
//
// Yields `()` if the parser is at the end of the input; an error otherwise.
pub fn eoi(i: LocatedSpan<&str>) -> ParserResult<()> {
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
pub fn eol(i: LocatedSpan<&str>) -> ParserResult<()> {
  alt((
    eoi, // this one goes first because it’s very cheap
    value((), newline),
  ))(i)
}

// Apply the `f` parser until `g` succeeds. Both parsers consume the input.
pub fn till<'a, A, B, F, G>(f: F, g: G) -> impl Fn(LocatedSpan<&'a str>) -> ParserResult<'a, ()>
where
  F: Fn(LocatedSpan<&'a str>) -> ParserResult<'a, A>,
  G: Fn(LocatedSpan<&'a str>) -> ParserResult<'a, B>,
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
pub fn many0_<'a, A, F>(f: F) -> impl Fn(LocatedSpan<&'a str>) -> ParserResult<'a, ()>
where
  F: Fn(LocatedSpan<&'a str>) -> ParserResult<'a, A>,
{
  move |i| fold_many0(&f, (), |_, _| ())(i)
}

/// Parse a string until the end of line.
///
/// This parser accepts the multiline annotation (\) to break the string on several lines.
///
/// Discard any leading newline.
pub fn str_till_eol(i: LocatedSpan<&str>) -> ParserResult<LocatedSpan<&str>> {
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
pub fn blank_space(i: LocatedSpan<&str>) -> ParserResult<LocatedSpan<&str>> {
  recognize(many0_(alt((multispace1, tag("\\\n")))))(i)
}
