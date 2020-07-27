use std::cell::RefCell;
use std::num::NonZeroUsize;

use nom_locate::LocatedSpan;

use crate::syntax;

pub type ContextComments<'s> = std::collections::HashMap<usize, syntax::Node<syntax::Comment<'s>>>;
pub type ContextSpans = bimap::BiBTreeMap<syntax::NodeSpan, NonZeroUsize>;

#[derive(Debug, Clone)]
pub struct ParseContextData<'s> {
  comments: Option<ContextComments<'s>>,
  spans: Option<ContextSpans>,
  current_id: NonZeroUsize,
  current_source: usize,
  source_ends: Vec<usize>,
}

impl<'s> ParseContextData<'s> {
  pub fn new() -> Self {
    Self {
      comments: None,
      spans: None,
      current_id: unsafe { NonZeroUsize::new_unchecked(1) },
      current_source: 0,
      source_ends: Vec::new(),
    }
  }

  pub fn with_spans() -> Self {
    Self {
      comments: None,
      spans: Some(Default::default()),
      current_id: unsafe { NonZeroUsize::new_unchecked(1) },
      current_source: 0,
      source_ends: Vec::new(),
    }
  }

  pub fn with_comments() -> Self {
    Self {
      comments: Some(Default::default()),
      spans: Some(Default::default()),
      current_id: unsafe { NonZeroUsize::new_unchecked(1) },
      current_source: 0,
      source_ends: Vec::new(),
    }
  }

  pub fn comments(&self) -> Option<&ContextComments<'s>> {
    self.comments.as_ref()
  }

  pub fn spans(&self) -> Option<&ContextSpans> {
    self.spans.as_ref()
  }

  pub fn get_span(&self, span_id: Option<NonZeroUsize>) -> Option<&syntax::NodeSpan> {
    if let (Some(id), Some(spans)) = (span_id, self.spans.as_ref()) {
      return spans.get_by_right(&id);
    }

    None
  }

  pub fn get_source(&self) -> Option<usize> {
    if self.current_source == 0 {
      None
    } else {
      Some(self.current_source - 1)
    }
  }

  pub fn source_end(&self, source_id: usize) -> Option<syntax::NodeSpan> {
    if source_id < self.current_source {
      return Some(syntax::NodeSpan::new_end(
        source_id,
        self.source_ends[source_id],
      ));
    }

    None
  }

  fn next_source(&mut self) -> usize {
    let res = self.current_source;
    self.current_source += 1;
    self.source_ends.push(0);
    res
  }

  fn commit_span<T: syntax::NodeContents>(
    &mut self,
    contents: T,
    span: syntax::NodeSpan,
  ) -> syntax::Node<T> {
    assert!(
      span.source_id < self.current_source,
      "span.source_id is out of range"
    );

    let span_id = if let Some(s) = &mut self.spans {
      if let Some(existing) = s.get_by_left(&span) {
        Some(*existing)
      } else {
        let span_id = self.current_id;
        self.current_id = NonZeroUsize::new(span_id.get() + 1).unwrap();

        s.insert_no_overwrite(span, span_id)
          .expect("another span covering the same range already exists");

        if let Some(se) = self.source_ends.get_mut(span.source_id) {
          *se = (*se).max(span.offset + span.length);
        }

        Some(span_id)
      }
    } else {
      None
    };

    syntax::Node::new(contents, span_id)
  }
}

#[derive(Debug)]
pub(crate) struct ParseContext<'s, 'd> {
  data: RefCell<&'d mut ParseContextData<'s>>,
  source_id: usize,
}

pub(crate) struct ContextData<'b, 's, 'd> {
  guard: std::cell::Ref<'b, &'d mut ParseContextData<'s>>,
}

impl<'s> std::ops::Deref for ContextData<'_, 's, '_> {
  type Target = ParseContextData<'s>;

  fn deref(&self) -> &Self::Target {
    &*self.guard
  }
}

impl<'s, 'd> ParseContext<'s, 'd> {
  pub fn new(data: &'d mut ParseContextData<'s>) -> Self {
    let source_id = data.next_source();

    Self {
      data: RefCell::new(data),
      source_id,
    }
  }

  pub fn add_comment(&self, cmt: syntax::Node<syntax::Comment<'s>>) {
    if let Some(c) = self.data.borrow_mut().comments.as_mut() {
      // If we're tracking comments we are also tracking spans
      c.insert(cmt.span_id.unwrap().get(), cmt);
    }
  }

  pub fn commit_span<T: syntax::NodeContents>(
    &self,
    contents: T,
    span: ParseInput,
  ) -> syntax::Node<T> {
    self
      .data
      .borrow_mut()
      .commit_span(contents, (self.source_id, span).into())
  }
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct ParseInput<'c, 'd, 'e> {
  pub input: LocatedSpan<&'c str>,
  pub context: &'e ParseContext<'c, 'd>,
}

impl<'c, 'd, 'e> ParseInput<'c, 'd, 'e> {
  pub fn new(input: &'c str, context: &'e ParseContext<'c, 'd>) -> Self {
    Self {
      input: LocatedSpan::new(input),
      context,
    }
  }

  pub fn fragment(&self) -> &'c str {
    self.input.fragment()
  }
}

impl<'c> AsRef<str> for ParseInput<'c, '_, '_> {
  fn as_ref(&self) -> &str {
    self.fragment()
  }
}

impl<'c> std::ops::Deref for ParseInput<'c, '_, '_> {
  type Target = LocatedSpan<&'c str>;

  fn deref(&self) -> &Self::Target {
    &self.input
  }
}

impl std::cmp::PartialEq for ParseInput<'_, '_, '_> {
  fn eq(&self, other: &Self) -> bool {
    self.input.eq(&other.input)
  }
}

impl std::fmt::Display for ParseInput<'_, '_, '_> {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    self.input.fmt(f)
  }
}

impl nom::AsBytes for ParseInput<'_, '_, '_> {
  fn as_bytes(&self) -> &[u8] {
    self.input.as_bytes()
  }
}

impl<'a> nom::Compare<&'a str> for ParseInput<'_, '_, '_> {
  fn compare(&self, t: &'a str) -> nom::CompareResult {
    self.input.compare(t)
  }

  fn compare_no_case(&self, t: &'a str) -> nom::CompareResult {
    self.input.compare(t)
  }
}

impl<'a, 'b, 'c, 'd> nom::Compare<ParseInput<'a, 'b, '_>> for ParseInput<'c, 'd, '_> {
  fn compare(&self, t: ParseInput<'a, 'b, '_>) -> nom::CompareResult {
    self.input.compare(t.input)
  }

  fn compare_no_case(&self, t: ParseInput<'a, 'b, '_>) -> nom::CompareResult {
    self.input.compare(t.input)
  }
}

impl<'c, 'd> nom::ExtendInto for ParseInput<'c, 'd, '_> {
  type Item = <LocatedSpan<&'c str> as nom::ExtendInto>::Item;
  type Extender = <LocatedSpan<&'c str> as nom::ExtendInto>::Extender;

  fn new_builder(&self) -> Self::Extender {
    self.input.new_builder()
  }

  fn extend_into(&self, acc: &mut Self::Extender) {
    self.input.extend_into(acc)
  }
}

impl<'c> nom::FindSubstring<&'c str> for ParseInput<'c, '_, '_> {
  fn find_substring(&self, substr: &'c str) -> Option<usize> {
    self.input.find_substring(substr)
  }
}

impl nom::HexDisplay for ParseInput<'_, '_, '_> {
  fn to_hex(&self, chunk_size: usize) -> String {
    self.input.to_hex(chunk_size)
  }

  fn to_hex_from(&self, chunk_size: usize, from: usize) -> String {
    self.input.to_hex_from(chunk_size, from)
  }
}

impl<'c> nom::InputIter for ParseInput<'c, '_, '_> {
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

impl nom::InputLength for ParseInput<'_, '_, '_> {
  fn input_len(&self) -> usize {
    self.input.input_len()
  }
}

impl nom::InputTake for ParseInput<'_, '_, '_> {
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
impl<'c> nom::InputTakeAtPosition for ParseInput<'c, '_, '_> {
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

impl nom::Offset for ParseInput<'_, '_, '_> {
  fn offset(&self, second: &Self) -> usize {
    self.input.offset(&second.input)
  }
}

impl<R: std::str::FromStr> nom::ParseTo<R> for ParseInput<'_, '_, '_> {
  fn parse_to(&self) -> Option<R> {
    self.input.parse_to()
  }
}

impl nom::Slice<std::ops::Range<usize>> for ParseInput<'_, '_, '_> {
  fn slice(&self, range: std::ops::Range<usize>) -> Self {
    Self {
      input: self.input.slice(range),
      context: self.context,
    }
  }
}

impl nom::Slice<std::ops::RangeFrom<usize>> for ParseInput<'_, '_, '_> {
  fn slice(&self, range: std::ops::RangeFrom<usize>) -> Self {
    Self {
      input: self.input.slice(range),
      context: self.context,
    }
  }
}

impl nom::Slice<std::ops::RangeFull> for ParseInput<'_, '_, '_> {
  fn slice(&self, range: std::ops::RangeFull) -> Self {
    Self {
      input: self.input.slice(range),
      context: self.context,
    }
  }
}

impl nom::Slice<std::ops::RangeTo<usize>> for ParseInput<'_, '_, '_> {
  fn slice(&self, range: std::ops::RangeTo<usize>) -> Self {
    Self {
      input: self.input.slice(range),
      context: self.context,
    }
  }
}
