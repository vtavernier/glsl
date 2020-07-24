use std::cell::RefCell;

use nom_locate::LocatedSpan;

use crate::syntax;

#[derive(Debug, Clone)]
pub struct ParseContext<'s> {
  comments: RefCell<Option<Vec<syntax::Node<syntax::Comment<'s>>>>>,
}

pub struct ContextComments<'s, 'b> {
  guard: std::cell::Ref<'b, Option<Vec<syntax::Node<syntax::Comment<'s>>>>>,
}

impl<'s> std::ops::Deref for ContextComments<'s, '_> {
  type Target = Option<Vec<syntax::Node<syntax::Comment<'s>>>>;

  fn deref(&self) -> &Self::Target {
    &*self.guard
  }
}

impl<'s> ParseContext<'s> {
  pub fn new() -> Self {
    Self {
      comments: RefCell::new(None),
    }
  }

  pub fn with_comments() -> Self {
    Self {
      comments: RefCell::new(Some(Vec::new())),
    }
  }

  pub fn has_comments(&self) -> bool {
    self.comments.borrow().is_some()
  }

  pub fn add_comment(&self, f: impl FnOnce() -> syntax::Node<syntax::Comment<'s>>) {
    if let Some(c) = self.comments.borrow_mut().as_mut() {
      c.push(f());
    }
  }

  pub fn comments(&self) -> ContextComments<'_, 's> {
    ContextComments {
      guard: self.comments.borrow(),
    }
  }
}

#[derive(Debug, Clone, Copy)]
pub struct ParseInput<'c: 'd, 'd> {
  pub input: LocatedSpan<&'c str>,
  pub context: &'d ParseContext<'c>,
}

impl<'c, 'd> ParseInput<'c, 'd> {
  pub fn new(input: &'c str, context: &'d ParseContext<'c>) -> Self {
    Self {
      input: LocatedSpan::new(input),
      context,
    }
  }

  pub fn fragment(&self) -> &'c str {
    self.input.fragment()
  }
}

impl<'c> std::ops::Deref for ParseInput<'c, '_> {
  type Target = LocatedSpan<&'c str>;

  fn deref(&self) -> &Self::Target {
    &self.input
  }
}

impl std::cmp::PartialEq for ParseInput<'_, '_> {
  fn eq(&self, other: &Self) -> bool {
    self.input.eq(&other.input)
  }
}

impl std::fmt::Display for ParseInput<'_, '_> {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    self.input.fmt(f)
  }
}

impl nom::AsBytes for ParseInput<'_, '_> {
  fn as_bytes(&self) -> &[u8] {
    self.input.as_bytes()
  }
}

impl<'a> nom::Compare<&'a str> for ParseInput<'_, '_> {
  fn compare(&self, t: &'a str) -> nom::CompareResult {
    self.input.compare(t)
  }

  fn compare_no_case(&self, t: &'a str) -> nom::CompareResult {
    self.input.compare(t)
  }
}

impl<'a, 'b, 'c, 'd> nom::Compare<ParseInput<'a, 'b>> for ParseInput<'c, 'd> {
  fn compare(&self, t: ParseInput<'a, 'b>) -> nom::CompareResult {
    self.input.compare(t.input)
  }

  fn compare_no_case(&self, t: ParseInput<'a, 'b>) -> nom::CompareResult {
    self.input.compare(t.input)
  }
}

impl<'c, 'd> nom::ExtendInto for ParseInput<'c, 'd> {
  type Item = <LocatedSpan<&'c str> as nom::ExtendInto>::Item;
  type Extender = <LocatedSpan<&'c str> as nom::ExtendInto>::Extender;

  fn new_builder(&self) -> Self::Extender {
    self.input.new_builder()
  }

  fn extend_into(&self, acc: &mut Self::Extender) {
    self.input.extend_into(acc)
  }
}

impl<'c> nom::FindSubstring<&'c str> for ParseInput<'c, '_> {
  fn find_substring(&self, substr: &'c str) -> Option<usize> {
    self.input.find_substring(substr)
  }
}

impl nom::HexDisplay for ParseInput<'_, '_> {
  fn to_hex(&self, chunk_size: usize) -> String {
    self.input.to_hex(chunk_size)
  }

  fn to_hex_from(&self, chunk_size: usize, from: usize) -> String {
    self.input.to_hex_from(chunk_size, from)
  }
}

impl<'c> nom::InputIter for ParseInput<'c, '_> {
  type Item = <LocatedSpan<&'c str> as nom::InputIter>::Item;
  type Iter = <LocatedSpan<&'c str> as nom::InputIter>::Iter;
  type IterElem = <LocatedSpan<&'c str> as nom::InputIter>::IterElem;

  fn iter_indices(&self) -> Self::Iter {
    self.input.iter_indices()
  }

  fn iter_elements(&self) -> Self::IterElem {
    self.input.iter_elements()
  }

  fn position<P>(&self, predicate: P) -> Option<usize>
  where
    P: Fn(Self::Item) -> bool,
  {
    self.input.position(predicate)
  }

  fn slice_index(&self, count: usize) -> Option<usize> {
    self.input.slice_index(count)
  }
}

impl nom::InputLength for ParseInput<'_, '_> {
  fn input_len(&self) -> usize {
    self.input.input_len()
  }
}

impl nom::InputTake for ParseInput<'_, '_> {
  fn take(&self, count: usize) -> Self {
    Self {
      input: self.input.take(count),
      context: self.context,
    }
  }

  fn take_split(&self, count: usize) -> (Self, Self) {
    let (a, b) = self.input.take_split(count);
    (
      Self {
        input: a,
        context: self.context,
      },
      Self {
        input: b,
        context: self.context,
      },
    )
  }
}

// Taken from nom_locate because of the generic E type
impl<'c> nom::InputTakeAtPosition for ParseInput<'c, '_> {
  type Item = <LocatedSpan<&'c str> as nom::InputTakeAtPosition>::Item;

  fn split_at_position_complete<P, E: nom::error::ParseError<Self>>(
    &self,
    predicate: P,
  ) -> nom::IResult<Self, Self, E>
  where
    P: Fn(Self::Item) -> bool,
  {
    use nom::{InputLength, InputTake};

    match self.split_at_position(predicate) {
      Err(nom::Err::Incomplete(_)) => Ok(self.take_split(self.input_len())),
      res => res,
    }
  }

  fn split_at_position<P, E: nom::error::ParseError<Self>>(
    &self,
    predicate: P,
  ) -> nom::IResult<Self, Self, E>
  where
    P: Fn(Self::Item) -> bool,
  {
    use nom::{InputIter, InputTake};

    match self.input.position(predicate) {
      Some(n) => Ok(self.take_split(n)),
      None => Err(nom::Err::Incomplete(nom::Needed::Size(1))),
    }
  }

  fn split_at_position1<P, E: nom::error::ParseError<Self>>(
    &self,
    predicate: P,
    e: nom::error::ErrorKind,
  ) -> nom::IResult<Self, Self, E>
  where
    P: Fn(Self::Item) -> bool,
  {
    use nom::{InputIter, InputTake};

    match self.input.position(predicate) {
      Some(0) => Err(nom::Err::Error(E::from_error_kind(self.clone(), e))),
      Some(n) => Ok(self.take_split(n)),
      None => Err(nom::Err::Incomplete(nom::Needed::Size(1))),
    }
  }

  fn split_at_position1_complete<P, E: nom::error::ParseError<Self>>(
    &self,
    predicate: P,
    e: nom::error::ErrorKind,
  ) -> nom::IResult<Self, Self, E>
  where
    P: Fn(Self::Item) -> bool,
  {
    use nom::{InputIter, InputLength, InputTake};

    match self.input.position(predicate) {
      Some(0) => Err(nom::Err::Error(E::from_error_kind(self.clone(), e))),
      Some(n) => Ok(self.take_split(n)),
      None => {
        if self.input.input_len() == 0 {
          Err(nom::Err::Error(E::from_error_kind(self.clone(), e)))
        } else {
          Ok(self.take_split(self.input_len()))
        }
      }
    }
  }
}

impl nom::Offset for ParseInput<'_, '_> {
  fn offset(&self, second: &Self) -> usize {
    self.input.offset(&second.input)
  }
}

impl<R: std::str::FromStr> nom::ParseTo<R> for ParseInput<'_, '_> {
  fn parse_to(&self) -> Option<R> {
    self.input.parse_to()
  }
}

impl nom::Slice<std::ops::Range<usize>> for ParseInput<'_, '_> {
  fn slice(&self, range: std::ops::Range<usize>) -> Self {
    Self {
      input: self.input.slice(range),
      context: self.context,
    }
  }
}

impl nom::Slice<std::ops::RangeFrom<usize>> for ParseInput<'_, '_> {
  fn slice(&self, range: std::ops::RangeFrom<usize>) -> Self {
    Self {
      input: self.input.slice(range),
      context: self.context,
    }
  }
}

impl nom::Slice<std::ops::RangeFull> for ParseInput<'_, '_> {
  fn slice(&self, range: std::ops::RangeFull) -> Self {
    Self {
      input: self.input.slice(range),
      context: self.context,
    }
  }
}

impl nom::Slice<std::ops::RangeTo<usize>> for ParseInput<'_, '_> {
  fn slice(&self, range: std::ops::RangeTo<usize>) -> Self {
    Self {
      input: self.input.slice(range),
      context: self.context,
    }
  }
}
