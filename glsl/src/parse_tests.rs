use std::borrow::Cow;

use crate::{assert_ceq, parsers::*, syntax};

trait Unspan: Sized {
  type Unspanned: Sized;

  fn unspan(self) -> Self::Unspanned;
}

macro_rules! impl_unspan {
  ($t:ty) => {
    impl Unspan for $t {
      type Unspanned = $t;

      fn unspan(self) -> Self::Unspanned {
        self
      }
    }
  };
}

impl<T, E> Unspan for Result<T, E>
where
  T: Unspan,
  E: Unspan,
{
  type Unspanned = Result<<T as Unspan>::Unspanned, <E as Unspan>::Unspanned>;

  fn unspan(self) -> Self::Unspanned {
    self.map(Unspan::unspan).map_err(Unspan::unspan)
  }
}

impl<'c> Unspan for ParseInput<'c, '_, '_> {
  type Unspanned = &'c str;

  fn unspan(self) -> Self::Unspanned {
    self.fragment()
  }
}

impl<T, U> Unspan for (T, U)
where
  T: Unspan,
  U: Unspan,
{
  type Unspanned = (<T as Unspan>::Unspanned, <U as Unspan>::Unspanned);

  fn unspan(self) -> Self::Unspanned {
    (self.0.unspan(), self.1.unspan())
  }
}

impl<T: syntax::NodeContents> Unspan for T {
  type Unspanned = T;

  fn unspan(self) -> Self::Unspanned {
    self
  }
}

impl<T: syntax::NodeContents> Unspan for syntax::Node<T> {
  type Unspanned = syntax::Node<T>;

  fn unspan(self) -> Self::Unspanned {
    self
  }
}

impl<T: Unspan> Unspan for Option<T> {
  type Unspanned = Option<<T as Unspan>::Unspanned>;

  fn unspan(self) -> Self::Unspanned {
    self.map(Unspan::unspan)
  }
}

impl<E: Unspan> Unspan for nom::Err<nom::error::VerboseError<E>> {
  type Unspanned = nom::Err<nom::error::VerboseError<<E as Unspan>::Unspanned>>;

  fn unspan(self) -> Self::Unspanned {
    self.map(|e| nom::error::VerboseError {
      errors: e.errors.into_iter().map(|(i, k)| (i.unspan(), k)).collect(),
    })
  }
}

impl_unspan!(());
impl_unspan!(bool);
impl_unspan!(char);
impl_unspan!(u16);
impl_unspan!(i32);
impl_unspan!(u32);
impl_unspan!(f32);
impl_unspan!(f64);
impl_unspan!(std::num::ParseIntError);

#[test]
fn parse_uniline_comment() {
  let mut data = ParseContextData::with_comments();
  let mut context = ParseContext::new(&mut data);

  assert_ceq!(
    context.parse("// lol", comment).unspan(),
    Ok(("", syntax::Comment::Single(Cow::Borrowed(" lol")))),
  );
  let cmt = data.comments().unwrap();
  assert_ceq!(cmt.len(), 1);
  let first_cmt = cmt.iter().next().unwrap();
  assert_ceq!(first_cmt.1.text(), " lol");
  let span = first_cmt.0;
  assert_ceq!(span.offset, 0);
  assert_ceq!(span.length, 6);

  let mut data = ParseContextData::with_comments();
  let mut context = ParseContext::new(&mut data);
  assert_ceq!(
    context.parse("// lol\nfoo", comment).unspan(),
    Ok(("foo", syntax::Comment::Single(Cow::Borrowed(" lol")))),
  );
  let cmt = data.comments().unwrap();
  assert_ceq!(cmt.len(), 1);
  let first_cmt = cmt.iter().next().unwrap();
  assert_ceq!(first_cmt.1.text(), " lol");

  let mut data = ParseContextData::with_comments();
  let mut context = ParseContext::new(&mut data);
  assert_ceq!(
    context.parse("// lol\\\nfoo", comment).unspan(),
    Ok(("", syntax::Comment::Single(Cow::Borrowed(" lol\\\nfoo")))),
  );
  let cmt = data.comments().unwrap();
  assert_ceq!(cmt.len(), 1);
  let first_cmt = cmt.iter().next().unwrap();
  assert_ceq!(first_cmt.1.text(), " lol\\\nfoo");

  let mut data = ParseContextData::with_comments();
  let mut context = ParseContext::new(&mut data);
  assert_ceq!(
    context.parse("// lol   \\\n   foo\n", comment).unspan(),
    Ok((
      "",
      syntax::Comment::Single(Cow::Borrowed(" lol   \\\n   foo")),
    )),
  );
  let cmt = data.comments().unwrap();
  assert_ceq!(cmt.len(), 1);
  let first_cmt = cmt.iter().next().unwrap();
  assert_ceq!(first_cmt.1.text(), " lol   \\\n   foo");
}

#[test]
fn parse_multiline_comment() {
  assert_ceq!(
    comment("/* lol\nfoo\n*/bar".into()).unspan(),
    Ok(("bar", syntax::Comment::Multi(Cow::Borrowed(" lol\nfoo\n"))))
  )
}

#[test]
fn parse_unsigned_suffix() {
  assert_ceq!(unsigned_suffix("u".into()).unspan(), Ok(("", 'u')));
  assert_ceq!(unsigned_suffix("U".into()).unspan(), Ok(("", 'U')));
}

#[test]
fn parse_nonzero_digits() {
  assert_ceq!(nonzero_digits("3".into()).unspan(), Ok(("", "3")));
  assert_ceq!(
    nonzero_digits("12345953".into()).unspan(),
    Ok(("", "12345953"))
  );
}

#[test]
fn parse_decimal_lit() {
  assert_ceq!(decimal_lit("3".into()).unspan(), Ok(("", Ok(3))));
  assert_ceq!(decimal_lit("3".into()).unspan(), Ok(("", Ok(3))));
  assert_ceq!(decimal_lit("13".into()).unspan(), Ok(("", Ok(13))));
  assert_ceq!(decimal_lit("42".into()).unspan(), Ok(("", Ok(42))));
  assert_ceq!(decimal_lit("123456".into()).unspan(), Ok(("", Ok(123456))));
}

#[test]
fn parse_octal_lit() {
  assert_ceq!(octal_lit("0".into()).unspan(), Ok(("", Ok(0o0))));
  assert_ceq!(octal_lit("03 ".into()).unspan(), Ok((" ", Ok(0o3))));
  assert_ceq!(octal_lit("012 ".into()).unspan(), Ok((" ", Ok(0o12))));
  assert_ceq!(
    octal_lit("07654321 ".into()).unspan(),
    Ok((" ", Ok(0o7654321)))
  );
}

#[test]
fn parse_hexadecimal_lit() {
  assert_ceq!(hexadecimal_lit("0x3 ".into()).unspan(), Ok((" ", Ok(0x3))));
  assert_ceq!(
    hexadecimal_lit("0x0123789".into()).unspan(),
    Ok(("", Ok(0x0123789)))
  );
  assert_ceq!(
    hexadecimal_lit("0xABCDEF".into()).unspan(),
    Ok(("", Ok(0xabcdef)))
  );
  assert_ceq!(
    hexadecimal_lit("0xabcdef".into()).unspan(),
    Ok(("", Ok(0xabcdef)))
  );
}

#[test]
fn parse_integral_lit() {
  assert_ceq!(integral_lit("0".into()).unspan(), Ok(("", 0)));
  assert_ceq!(integral_lit("3".into()).unspan(), Ok(("", 3)));
  assert_ceq!(integral_lit("3 ".into()).unspan(), Ok((" ", 3)));
  assert_ceq!(integral_lit("03 ".into()).unspan(), Ok((" ", 3)));
  assert_ceq!(integral_lit("076556 ".into()).unspan(), Ok((" ", 0o76556)));
  assert_ceq!(integral_lit("012 ".into()).unspan(), Ok((" ", 0o12)));
  assert_ceq!(integral_lit("0x3 ".into()).unspan(), Ok((" ", 0x3)));
  assert_ceq!(
    integral_lit("0x9ABCDEF".into()).unspan(),
    Ok(("", 0x9ABCDEF))
  );
  assert_ceq!(
    integral_lit("0x9ABCDEF".into()).unspan(),
    Ok(("", 0x9ABCDEF))
  );
  assert_ceq!(
    integral_lit("0x9abcdef".into()).unspan(),
    Ok(("", 0x9abcdef))
  );
  assert_ceq!(
    integral_lit("0x9abcdef".into()).unspan(),
    Ok(("", 0x9abcdef))
  );
  assert_ceq!(
    integral_lit("0xffffffff".into()).unspan(),
    Ok(("", 0xffffffffu32 as i32))
  );
}

#[test]
fn parse_integral_neg_lit() {
  assert_ceq!(integral_lit("-3".into()).unspan(), Ok(("", -3)));
  assert_ceq!(integral_lit("-3 ".into()).unspan(), Ok((" ", -3)));
  assert_ceq!(integral_lit("-03 ".into()).unspan(), Ok((" ", -3)));
  assert_ceq!(
    integral_lit("-076556 ".into()).unspan(),
    Ok((" ", -0o76556))
  );
  assert_ceq!(integral_lit("-012 ".into()).unspan(), Ok((" ", -0o12)));
  assert_ceq!(integral_lit("-0x3 ".into()).unspan(), Ok((" ", -0x3)));
  assert_ceq!(
    integral_lit("-0x9ABCDEF".into()).unspan(),
    Ok(("", -0x9ABCDEF))
  );
  assert_ceq!(
    integral_lit("-0x9ABCDEF".into()).unspan(),
    Ok(("", -0x9ABCDEF))
  );
  assert_ceq!(
    integral_lit("-0x9abcdef".into()).unspan(),
    Ok(("", -0x9abcdef))
  );
  assert_ceq!(
    integral_lit("-0x9abcdef".into()).unspan(),
    Ok(("", -0x9abcdef))
  );
}

#[test]
fn parse_unsigned_lit() {
  assert_ceq!(
    unsigned_lit("0xffffffffU".into()).unspan(),
    Ok(("", 0xffffffff as u32))
  );
  assert_ceq!(
    unsigned_lit("-1u".into()).unspan(),
    Ok(("", 0xffffffff as u32))
  );
  assert!(unsigned_lit("0xfffffffffU".into()).is_err());
}

#[test]
fn parse_float_lit() {
  assert_ceq!(float_lit("0.;".into()).unspan(), Ok((";", 0.)));
  assert_ceq!(float_lit(".0;".into()).unspan(), Ok((";", 0.)));
  assert_ceq!(float_lit(".035 ".into()).unspan(), Ok((" ", 0.035)));
  assert_ceq!(float_lit("0. ".into()).unspan(), Ok((" ", 0.)));
  assert_ceq!(float_lit("0.035 ".into()).unspan(), Ok((" ", 0.035)));
  assert_ceq!(float_lit(".035f".into()).unspan(), Ok(("", 0.035)));
  assert_ceq!(float_lit("0.f".into()).unspan(), Ok(("", 0.)));
  assert_ceq!(float_lit("314.f".into()).unspan(), Ok(("", 314.)));
  assert_ceq!(float_lit("0.035f".into()).unspan(), Ok(("", 0.035)));
  assert_ceq!(float_lit(".035F".into()).unspan(), Ok(("", 0.035)));
  assert_ceq!(float_lit("0.F".into()).unspan(), Ok(("", 0.)));
  assert_ceq!(float_lit("0.035F".into()).unspan(), Ok(("", 0.035)));
  assert_ceq!(float_lit("1.03e+34 ".into()).unspan(), Ok((" ", 1.03e+34)));
  assert_ceq!(float_lit("1.03E+34 ".into()).unspan(), Ok((" ", 1.03E+34)));
  assert_ceq!(float_lit("1.03e-34 ".into()).unspan(), Ok((" ", 1.03e-34)));
  assert_ceq!(float_lit("1.03E-34 ".into()).unspan(), Ok((" ", 1.03E-34)));
  assert_ceq!(float_lit("1.03e+34f".into()).unspan(), Ok(("", 1.03e+34)));
  assert_ceq!(float_lit("1.03E+34f".into()).unspan(), Ok(("", 1.03E+34)));
  assert_ceq!(float_lit("1.03e-34f".into()).unspan(), Ok(("", 1.03e-34)));
  assert_ceq!(float_lit("1.03E-34f".into()).unspan(), Ok(("", 1.03E-34)));
  assert_ceq!(float_lit("1.03e+34F".into()).unspan(), Ok(("", 1.03e+34)));
  assert_ceq!(float_lit("1.03E+34F".into()).unspan(), Ok(("", 1.03E+34)));
  assert_ceq!(float_lit("1.03e-34F".into()).unspan(), Ok(("", 1.03e-34)));
  assert_ceq!(float_lit("1.03E-34F".into()).unspan(), Ok(("", 1.03E-34)));
}

#[test]
fn parse_float_neg_lit() {
  assert_ceq!(float_lit("-.035 ".into()).unspan(), Ok((" ", -0.035)));
  assert_ceq!(float_lit("-0. ".into()).unspan(), Ok((" ", -0.)));
  assert_ceq!(float_lit("-0.035 ".into()).unspan(), Ok((" ", -0.035)));
  assert_ceq!(float_lit("-.035f".into()).unspan(), Ok(("", -0.035)));
  assert_ceq!(float_lit("-0.f".into()).unspan(), Ok(("", -0.)));
  assert_ceq!(float_lit("-0.035f".into()).unspan(), Ok(("", -0.035)));
  assert_ceq!(float_lit("-.035F".into()).unspan(), Ok(("", -0.035)));
  assert_ceq!(float_lit("-0.F".into()).unspan(), Ok(("", -0.)));
  assert_ceq!(float_lit("-0.035F".into()).unspan(), Ok(("", -0.035)));
  assert_ceq!(
    float_lit("-1.03e+34 ".into()).unspan(),
    Ok((" ", -1.03e+34))
  );
  assert_ceq!(
    float_lit("-1.03E+34 ".into()).unspan(),
    Ok((" ", -1.03E+34))
  );
  assert_ceq!(
    float_lit("-1.03e-34 ".into()).unspan(),
    Ok((" ", -1.03e-34))
  );
  assert_ceq!(
    float_lit("-1.03E-34 ".into()).unspan(),
    Ok((" ", -1.03E-34))
  );
  assert_ceq!(float_lit("-1.03e+34f".into()).unspan(), Ok(("", -1.03e+34)));
  assert_ceq!(float_lit("-1.03E+34f".into()).unspan(), Ok(("", -1.03E+34)));
  assert_ceq!(float_lit("-1.03e-34f".into()).unspan(), Ok(("", -1.03e-34)));
  assert_ceq!(float_lit("-1.03E-34f".into()).unspan(), Ok(("", -1.03E-34)));
  assert_ceq!(float_lit("-1.03e+34F".into()).unspan(), Ok(("", -1.03e+34)));
  assert_ceq!(float_lit("-1.03E+34F".into()).unspan(), Ok(("", -1.03E+34)));
  assert_ceq!(float_lit("-1.03e-34F".into()).unspan(), Ok(("", -1.03e-34)));
  assert_ceq!(float_lit("-1.03E-34F".into()).unspan(), Ok(("", -1.03E-34)));
}

#[test]
fn parse_double_lit() {
  assert_ceq!(double_lit("0.;".into()).unspan(), Ok((";", 0.)));
  assert_ceq!(double_lit(".0;".into()).unspan(), Ok((";", 0.)));
  assert_ceq!(double_lit(".035 ".into()).unspan(), Ok((" ", 0.035)));
  assert_ceq!(double_lit("0. ".into()).unspan(), Ok((" ", 0.)));
  assert_ceq!(double_lit("0.035 ".into()).unspan(), Ok((" ", 0.035)));
  assert_ceq!(double_lit("0.lf".into()).unspan(), Ok(("", 0.)));
  assert_ceq!(double_lit("0.035lf".into()).unspan(), Ok(("", 0.035)));
  assert_ceq!(double_lit(".035lf".into()).unspan(), Ok(("", 0.035)));
  assert_ceq!(double_lit(".035LF".into()).unspan(), Ok(("", 0.035)));
  assert_ceq!(double_lit("0.LF".into()).unspan(), Ok(("", 0.)));
  assert_ceq!(double_lit("0.035LF".into()).unspan(), Ok(("", 0.035)));
  assert_ceq!(double_lit("1.03e+34lf".into()).unspan(), Ok(("", 1.03e+34)));
  assert_ceq!(double_lit("1.03E+34lf".into()).unspan(), Ok(("", 1.03E+34)));
  assert_ceq!(double_lit("1.03e-34lf".into()).unspan(), Ok(("", 1.03e-34)));
  assert_ceq!(double_lit("1.03E-34lf".into()).unspan(), Ok(("", 1.03E-34)));
  assert_ceq!(double_lit("1.03e+34LF".into()).unspan(), Ok(("", 1.03e+34)));
  assert_ceq!(double_lit("1.03E+34LF".into()).unspan(), Ok(("", 1.03E+34)));
  assert_ceq!(double_lit("1.03e-34LF".into()).unspan(), Ok(("", 1.03e-34)));
  assert_ceq!(double_lit("1.03E-34LF".into()).unspan(), Ok(("", 1.03E-34)));
}

#[test]
fn parse_double_neg_lit() {
  assert_ceq!(double_lit("-0.;".into()).unspan(), Ok((";", -0.)));
  assert_ceq!(double_lit("-.0;".into()).unspan(), Ok((";", -0.)));
  assert_ceq!(double_lit("-.035 ".into()).unspan(), Ok((" ", -0.035)));
  assert_ceq!(double_lit("-0. ".into()).unspan(), Ok((" ", -0.)));
  assert_ceq!(double_lit("-0.035 ".into()).unspan(), Ok((" ", -0.035)));
  assert_ceq!(double_lit("-0.lf".into()).unspan(), Ok(("", -0.)));
  assert_ceq!(double_lit("-0.035lf".into()).unspan(), Ok(("", -0.035)));
  assert_ceq!(double_lit("-.035lf".into()).unspan(), Ok(("", -0.035)));
  assert_ceq!(double_lit("-.035LF".into()).unspan(), Ok(("", -0.035)));
  assert_ceq!(double_lit("-0.LF".into()).unspan(), Ok(("", -0.)));
  assert_ceq!(double_lit("-0.035LF".into()).unspan(), Ok(("", -0.035)));
  assert_ceq!(
    double_lit("-1.03e+34lf".into()).unspan(),
    Ok(("", -1.03e+34))
  );
  assert_ceq!(
    double_lit("-1.03E+34lf".into()).unspan(),
    Ok(("", -1.03E+34))
  );
  assert_ceq!(
    double_lit("-1.03e-34lf".into()).unspan(),
    Ok(("", -1.03e-34))
  );
  assert_ceq!(
    double_lit("-1.03E-34lf".into()).unspan(),
    Ok(("", -1.03E-34))
  );
  assert_ceq!(
    double_lit("-1.03e+34LF".into()).unspan(),
    Ok(("", -1.03e+34))
  );
  assert_ceq!(
    double_lit("-1.03E+34LF".into()).unspan(),
    Ok(("", -1.03E+34))
  );
  assert_ceq!(
    double_lit("-1.03e-34LF".into()).unspan(),
    Ok(("", -1.03e-34))
  );
  assert_ceq!(
    double_lit("-1.03E-34LF".into()).unspan(),
    Ok(("", -1.03E-34))
  );
}

#[test]
fn parse_bool_lit() {
  assert_ceq!(bool_lit("false".into()).unspan(), Ok(("", false)));
  assert_ceq!(bool_lit("true".into()).unspan(), Ok(("", true)));
}

#[test]
fn parse_identifier() {
  assert_ceq!(identifier("a".into()).unspan(), Ok(("", "a".into())));
  assert_ceq!(
    identifier("ab_cd".into()).unspan(),
    Ok(("", "ab_cd".into()))
  );
  assert_ceq!(
    identifier("Ab_cd".into()).unspan(),
    Ok(("", "Ab_cd".into()))
  );
  assert_ceq!(
    identifier("Ab_c8d".into()).unspan(),
    Ok(("", "Ab_c8d".into()))
  );
  assert_ceq!(
    identifier("Ab_c8d9".into()).unspan(),
    Ok(("", "Ab_c8d9".into()))
  );
}

#[test]
fn parse_unary_op_add() {
  assert_ceq!(
    unary_op("+ ".into()).unspan(),
    Ok((" ", syntax::UnaryOp::Add))
  );
}

#[test]
fn parse_unary_op_minus() {
  assert_ceq!(
    unary_op("- ".into()).unspan(),
    Ok((" ", syntax::UnaryOp::Minus))
  );
}

#[test]
fn parse_unary_op_not() {
  assert_ceq!(
    unary_op("!".into()).unspan(),
    Ok(("", syntax::UnaryOp::Not))
  );
}

#[test]
fn parse_unary_op_complement() {
  assert_ceq!(
    unary_op("~".into()).unspan(),
    Ok(("", syntax::UnaryOp::Complement))
  );
}

#[test]
fn parse_unary_op_inc() {
  assert_ceq!(
    unary_op("++".into()).unspan(),
    Ok(("", syntax::UnaryOp::Inc))
  );
}

#[test]
fn parse_unary_op_dec() {
  assert_ceq!(
    unary_op("--".into()).unspan(),
    Ok(("", syntax::UnaryOp::Dec))
  );
}

#[test]
fn parse_array_specifier_dimension_unsized() {
  assert_ceq!(
    array_specifier_dimension("[]".into()).unspan(),
    Ok(("", syntax::ArraySpecifierDimension::Unsized))
  );
  assert_ceq!(
    array_specifier_dimension("[ ]".into()).unspan(),
    Ok(("", syntax::ArraySpecifierDimension::Unsized))
  );
  assert_ceq!(
    array_specifier_dimension("[\n]".into()).unspan(),
    Ok(("", syntax::ArraySpecifierDimension::Unsized))
  );
}

#[test]
fn parse_array_specifier_dimension_sized() {
  let ix = syntax::Expr::IntConst(0);

  assert_ceq!(
    array_specifier_dimension("[0]".into()).unspan(),
    Ok((
      "",
      syntax::ArraySpecifierDimension::ExplicitlySized(Box::new(ix.clone()))
    ))
  );
  assert_ceq!(
    array_specifier_dimension("[\n0   \t]".into()).unspan(),
    Ok((
      "",
      syntax::ArraySpecifierDimension::ExplicitlySized(Box::new(ix))
    ))
  );
}

#[test]
fn parse_array_specifier_unsized() {
  assert_ceq!(
    array_specifier("[]".into()).unspan(),
    Ok((
      "",
      syntax::ArraySpecifier {
        dimensions: syntax::NonEmpty(vec![syntax::ArraySpecifierDimension::Unsized])
      }
    ))
  )
}

#[test]
fn parse_array_specifier_sized() {
  let ix = syntax::Expr::IntConst(123);

  assert_ceq!(
    array_specifier("[123]".into()).unspan(),
    Ok((
      "",
      syntax::ArraySpecifier {
        dimensions: syntax::NonEmpty(vec![syntax::ArraySpecifierDimension::ExplicitlySized(
          Box::new(ix)
        )])
      }
    ))
  )
}

#[test]
fn parse_array_specifier_sized_multiple() {
  let a = syntax::Expr::IntConst(2);
  let b = syntax::Expr::IntConst(100);
  let d = syntax::Expr::IntConst(5);

  assert_ceq!(
    array_specifier("[2][100][][5]".into()).unspan(),
    Ok((
      "",
      syntax::ArraySpecifier {
        dimensions: syntax::NonEmpty(vec![
          syntax::ArraySpecifierDimension::ExplicitlySized(Box::new(a)),
          syntax::ArraySpecifierDimension::ExplicitlySized(Box::new(b)),
          syntax::ArraySpecifierDimension::Unsized,
          syntax::ArraySpecifierDimension::ExplicitlySized(Box::new(d)),
        ])
      }
    ))
  )
}

#[test]
fn parse_precise_qualifier() {
  assert_ceq!(precise_qualifier("precise ".into()).unspan(), Ok((" ", ())));
}

#[test]
fn parse_invariant_qualifier() {
  assert_ceq!(
    invariant_qualifier("invariant ".into()).unspan(),
    Ok((" ", ()))
  );
}

#[test]
fn parse_interpolation_qualifier() {
  assert_ceq!(
    interpolation_qualifier("smooth ".into()).unspan(),
    Ok((" ", syntax::InterpolationQualifier::Smooth))
  );
  assert_ceq!(
    interpolation_qualifier("flat ".into()).unspan(),
    Ok((" ", syntax::InterpolationQualifier::Flat))
  );
  assert_ceq!(
    interpolation_qualifier("noperspective ".into()).unspan(),
    Ok((" ", syntax::InterpolationQualifier::NoPerspective))
  );
}

#[test]
fn parse_precision_qualifier() {
  assert_ceq!(
    precision_qualifier("highp ".into()).unspan(),
    Ok((" ", syntax::PrecisionQualifier::High))
  );
  assert_ceq!(
    precision_qualifier("mediump ".into()).unspan(),
    Ok((" ", syntax::PrecisionQualifier::Medium))
  );
  assert_ceq!(
    precision_qualifier("lowp ".into()).unspan(),
    Ok((" ", syntax::PrecisionQualifier::Low))
  );
}

#[test]
fn parse_storage_qualifier() {
  assert_ceq!(
    storage_qualifier("const ".into()).unspan(),
    Ok((" ", syntax::StorageQualifier::Const))
  );
  assert_ceq!(
    storage_qualifier("inout ".into()).unspan(),
    Ok((" ", syntax::StorageQualifier::InOut))
  );
  assert_ceq!(
    storage_qualifier("in ".into()).unspan(),
    Ok((" ", syntax::StorageQualifier::In))
  );
  assert_ceq!(
    storage_qualifier("out ".into()).unspan(),
    Ok((" ", syntax::StorageQualifier::Out))
  );
  assert_ceq!(
    storage_qualifier("centroid ".into()).unspan(),
    Ok((" ", syntax::StorageQualifier::Centroid))
  );
  assert_ceq!(
    storage_qualifier("patch ".into()).unspan(),
    Ok((" ", syntax::StorageQualifier::Patch))
  );
  assert_ceq!(
    storage_qualifier("sample ".into()).unspan(),
    Ok((" ", syntax::StorageQualifier::Sample))
  );
  assert_ceq!(
    storage_qualifier("uniform ".into()).unspan(),
    Ok((" ", syntax::StorageQualifier::Uniform))
  );
  assert_ceq!(
    storage_qualifier("attribute ".into()).unspan(),
    Ok((" ", syntax::StorageQualifier::Attribute))
  );
  assert_ceq!(
    storage_qualifier("varying ".into()).unspan(),
    Ok((" ", syntax::StorageQualifier::Varying))
  );
  assert_ceq!(
    storage_qualifier("buffer ".into()).unspan(),
    Ok((" ", syntax::StorageQualifier::Buffer))
  );
  assert_ceq!(
    storage_qualifier("shared ".into()).unspan(),
    Ok((" ", syntax::StorageQualifier::Shared))
  );
  assert_ceq!(
    storage_qualifier("coherent ".into()).unspan(),
    Ok((" ", syntax::StorageQualifier::Coherent))
  );
  assert_ceq!(
    storage_qualifier("volatile ".into()).unspan(),
    Ok((" ", syntax::StorageQualifier::Volatile))
  );
  assert_ceq!(
    storage_qualifier("restrict ".into()).unspan(),
    Ok((" ", syntax::StorageQualifier::Restrict))
  );
  assert_ceq!(
    storage_qualifier("readonly ".into()).unspan(),
    Ok((" ", syntax::StorageQualifier::ReadOnly))
  );
  assert_ceq!(
    storage_qualifier("writeonly ".into()).unspan(),
    Ok((" ", syntax::StorageQualifier::WriteOnly))
  );
  assert_ceq!(
    storage_qualifier("subroutine a".into()).unspan(),
    Ok((" a", syntax::StorageQualifier::Subroutine(vec![])))
  );

  let a = syntax::TypeName("vec3".to_owned());
  let b = syntax::TypeName("float".to_owned());
  let c = syntax::TypeName("dmat43".to_owned());
  let types = vec![a, b, c];
  assert_ceq!(
    storage_qualifier("subroutine (  vec3 , float \\\n, dmat43)".into()).unspan(),
    Ok(("", syntax::StorageQualifier::Subroutine(types)))
  );
}

#[test]
fn parse_layout_qualifier_std430() {
  let expected = syntax::LayoutQualifier {
    ids: syntax::NonEmpty(vec![syntax::LayoutQualifierSpec::Identifier(
      "std430".into(),
      None,
    )]),
  };

  assert_ceq!(
    layout_qualifier("layout (std430)".into()).unspan(),
    Ok(("", expected.clone()))
  );
  assert_ceq!(
    layout_qualifier("layout  (std430   )".into()).unspan(),
    Ok(("", expected.clone()))
  );
  assert_ceq!(
    layout_qualifier("layout \n\t (  std430  )".into()).unspan(),
    Ok(("", expected.clone()))
  );
  assert_ceq!(
    layout_qualifier("layout(std430)".into()).unspan(),
    Ok(("", expected))
  );
}

#[test]
fn parse_layout_qualifier_shared() {
  let expected = syntax::LayoutQualifier {
    ids: syntax::NonEmpty(vec![syntax::LayoutQualifierSpec::Shared]),
  };

  assert_ceq!(
    layout_qualifier("layout (shared)".into()).unspan(),
    Ok(("", expected.clone()))
  );
  assert_ceq!(
    layout_qualifier("layout ( shared )".into()).unspan(),
    Ok(("", expected.clone()))
  );
  assert_ceq!(
    layout_qualifier("layout(shared)".into()).unspan(),
    Ok(("", expected))
  );
}

#[test]
fn parse_layout_qualifier_list() {
  let id_0 = syntax::LayoutQualifierSpec::Shared;
  let id_1 = syntax::LayoutQualifierSpec::Identifier("std140".into(), None);
  let id_2 = syntax::LayoutQualifierSpec::Identifier(
    "max_vertices".into(),
    Some(Box::new(syntax::Expr::IntConst(3))),
  );
  let expected = syntax::LayoutQualifier {
    ids: syntax::NonEmpty(vec![id_0, id_1, id_2]),
  };

  assert_ceq!(
    layout_qualifier("layout (shared, std140, max_vertices = 3)".into()).unspan(),
    Ok(("", expected.clone()))
  );
  assert_ceq!(
    layout_qualifier("layout(shared,std140,max_vertices=3)".into()).unspan(),
    Ok(("", expected.clone()))
  );
  assert_ceq!(
    layout_qualifier("layout\n\n\t (    shared , std140, max_vertices= 3)".into()).unspan(),
    Ok(("", expected.clone()))
  );
}

#[test]
fn parse_type_qualifier() {
  let storage_qual = syntax::TypeQualifierSpec::Storage(syntax::StorageQualifier::Const);
  let id_0 = syntax::LayoutQualifierSpec::Shared;
  let id_1 = syntax::LayoutQualifierSpec::Identifier("std140".into(), None);
  let id_2 = syntax::LayoutQualifierSpec::Identifier(
    "max_vertices".into(),
    Some(Box::new(syntax::Expr::IntConst(3))),
  );
  let layout_qual = syntax::TypeQualifierSpec::Layout(syntax::LayoutQualifier {
    ids: syntax::NonEmpty(vec![id_0, id_1, id_2]),
  });
  let expected = syntax::TypeQualifier {
    qualifiers: syntax::NonEmpty(vec![storage_qual, layout_qual]),
  };

  assert_ceq!(
    type_qualifier("const layout (shared, std140, max_vertices = 3)".into()).unspan(),
    Ok(("", expected.clone()))
  );
  assert_ceq!(
    type_qualifier("const layout(shared,std140,max_vertices=3)".into()).unspan(),
    Ok(("", expected))
  );
}

#[test]
fn parse_struct_field_specifier() {
  let expected = syntax::StructFieldSpecifier {
    qualifier: None,
    ty: syntax::TypeSpecifier {
      ty: syntax::TypeSpecifierNonArray::Vec4,
      array_specifier: None,
    },
    identifiers: syntax::NonEmpty(vec!["foo".into()]),
  };

  assert_ceq!(
    struct_field_specifier("vec4 foo;".into()).unspan(),
    Ok(("", expected.clone()))
  );
  assert_ceq!(
    struct_field_specifier("vec4     foo ; ".into()).unspan(),
    Ok((" ", expected.clone()))
  );
}

#[test]
fn parse_struct_field_specifier_type_name() {
  let expected = syntax::StructFieldSpecifier {
    qualifier: None,
    ty: syntax::TypeSpecifier {
      ty: syntax::TypeSpecifierNonArray::TypeName("S0238_3".into()),
      array_specifier: None,
    },
    identifiers: syntax::NonEmpty(vec!["x".into()]),
  };

  assert_ceq!(
    struct_field_specifier("S0238_3 x;".into()).unspan(),
    Ok(("", expected.clone()))
  );
  assert_ceq!(
    struct_field_specifier("S0238_3     x ;".into()).unspan(),
    Ok(("", expected.clone()))
  );
}

#[test]
fn parse_struct_field_specifier_several() {
  let expected = syntax::StructFieldSpecifier {
    qualifier: None,
    ty: syntax::TypeSpecifier {
      ty: syntax::TypeSpecifierNonArray::Vec4,
      array_specifier: None,
    },
    identifiers: syntax::NonEmpty(vec!["foo".into(), "bar".into(), "zoo".into()]),
  };

  assert_ceq!(
    struct_field_specifier("vec4 foo, bar, zoo;".into()).unspan(),
    Ok(("", expected.clone()))
  );
  assert_ceq!(
    struct_field_specifier("vec4     foo , bar  , zoo ;".into()).unspan(),
    Ok(("", expected.clone()))
  );
}

#[test]
fn parse_struct_specifier_one_field() {
  let field = syntax::StructFieldSpecifier {
    qualifier: None,
    ty: syntax::TypeSpecifier {
      ty: syntax::TypeSpecifierNonArray::Vec4,
      array_specifier: None,
    },
    identifiers: syntax::NonEmpty(vec!["foo".into()]),
  };
  let expected = syntax::StructSpecifier {
    name: Some("TestStruct".into()),
    fields: syntax::NonEmpty(vec![field]),
  };

  assert_ceq!(
    struct_specifier("struct TestStruct { vec4 foo; }".into()).unspan(),
    Ok(("", expected.clone()))
  );
  assert_ceq!(
    struct_specifier("struct      TestStruct \n \n\n {\n    vec4   foo  ;}".into()).unspan(),
    Ok(("", expected))
  );
}

#[test]
fn parse_struct_specifier_multi_fields() {
  let a = syntax::StructFieldSpecifier {
    qualifier: None,
    ty: syntax::TypeSpecifier {
      ty: syntax::TypeSpecifierNonArray::Vec4,
      array_specifier: None,
    },
    identifiers: syntax::NonEmpty(vec!["foo".into()]),
  };
  let b = syntax::StructFieldSpecifier {
    qualifier: None,
    ty: syntax::TypeSpecifier {
      ty: syntax::TypeSpecifierNonArray::Float,
      array_specifier: None,
    },
    identifiers: syntax::NonEmpty(vec!["bar".into()]),
  };
  let c = syntax::StructFieldSpecifier {
    qualifier: None,
    ty: syntax::TypeSpecifier {
      ty: syntax::TypeSpecifierNonArray::UInt,
      array_specifier: None,
    },
    identifiers: syntax::NonEmpty(vec!["zoo".into()]),
  };
  let d = syntax::StructFieldSpecifier {
    qualifier: None,
    ty: syntax::TypeSpecifier {
      ty: syntax::TypeSpecifierNonArray::BVec3,
      array_specifier: None,
    },
    identifiers: syntax::NonEmpty(vec!["foo_BAR_zoo3497_34".into()]),
  };
  let e = syntax::StructFieldSpecifier {
    qualifier: None,
    ty: syntax::TypeSpecifier {
      ty: syntax::TypeSpecifierNonArray::TypeName("S0238_3".into()),
      array_specifier: None,
    },
    identifiers: syntax::NonEmpty(vec!["x".into()]),
  };
  let expected = syntax::StructSpecifier {
    name: Some("_TestStruct_934i".into()),
    fields: syntax::NonEmpty(vec![a, b, c, d, e]),
  };

  assert_ceq!(
    struct_specifier(
      "struct _TestStruct_934i { vec4 foo; float bar; uint zoo; bvec3 foo_BAR_zoo3497_34; S0238_3 x; }".into()
    ).unspan(),
    Ok(("", expected.clone()))
  );
  assert_ceq!(
    struct_specifier(
      "struct _TestStruct_934i{vec4 foo;float bar;uint zoo;bvec3 foo_BAR_zoo3497_34;S0238_3 x;}"
        .into()
    )
    .unspan(),
    Ok(("", expected.clone()))
  );
  assert_ceq!(struct_specifier("struct _TestStruct_934i\n   {  vec4\nfoo ;   \n\t float\n\t\t  bar  ;   \nuint   zoo;    \n bvec3   foo_BAR_zoo3497_34\n\n\t\n\t\n  ; S0238_3 x;}".into()).unspan(), Ok(("", expected)));
}

#[test]
fn parse_type_specifier_non_array() {
  assert_ceq!(
    type_specifier_non_array("bool".into()).unspan(),
    Ok(("", syntax::TypeSpecifierNonArray::Bool))
  );
  assert_ceq!(
    type_specifier_non_array("int".into()).unspan(),
    Ok(("", syntax::TypeSpecifierNonArray::Int))
  );
  assert_ceq!(
    type_specifier_non_array("uint".into()).unspan(),
    Ok(("", syntax::TypeSpecifierNonArray::UInt))
  );
  assert_ceq!(
    type_specifier_non_array("float".into()).unspan(),
    Ok(("", syntax::TypeSpecifierNonArray::Float))
  );
  assert_ceq!(
    type_specifier_non_array("double".into()).unspan(),
    Ok(("", syntax::TypeSpecifierNonArray::Double))
  );
  assert_ceq!(
    type_specifier_non_array("vec2".into()).unspan(),
    Ok(("", syntax::TypeSpecifierNonArray::Vec2))
  );
  assert_ceq!(
    type_specifier_non_array("vec3".into()).unspan(),
    Ok(("", syntax::TypeSpecifierNonArray::Vec3))
  );
  assert_ceq!(
    type_specifier_non_array("vec4".into()).unspan(),
    Ok(("", syntax::TypeSpecifierNonArray::Vec4))
  );
  assert_ceq!(
    type_specifier_non_array("dvec2".into()).unspan(),
    Ok(("", syntax::TypeSpecifierNonArray::DVec2))
  );
  assert_ceq!(
    type_specifier_non_array("dvec3".into()).unspan(),
    Ok(("", syntax::TypeSpecifierNonArray::DVec3))
  );
  assert_ceq!(
    type_specifier_non_array("dvec4".into()).unspan(),
    Ok(("", syntax::TypeSpecifierNonArray::DVec4))
  );
  assert_ceq!(
    type_specifier_non_array("bvec2".into()).unspan(),
    Ok(("", syntax::TypeSpecifierNonArray::BVec2))
  );
  assert_ceq!(
    type_specifier_non_array("bvec3".into()).unspan(),
    Ok(("", syntax::TypeSpecifierNonArray::BVec3))
  );
  assert_ceq!(
    type_specifier_non_array("bvec4".into()).unspan(),
    Ok(("", syntax::TypeSpecifierNonArray::BVec4))
  );
  assert_ceq!(
    type_specifier_non_array("ivec2".into()).unspan(),
    Ok(("", syntax::TypeSpecifierNonArray::IVec2))
  );
  assert_ceq!(
    type_specifier_non_array("ivec3".into()).unspan(),
    Ok(("", syntax::TypeSpecifierNonArray::IVec3))
  );
  assert_ceq!(
    type_specifier_non_array("ivec4".into()).unspan(),
    Ok(("", syntax::TypeSpecifierNonArray::IVec4))
  );
  assert_ceq!(
    type_specifier_non_array("uvec2".into()).unspan(),
    Ok(("", syntax::TypeSpecifierNonArray::UVec2))
  );
  assert_ceq!(
    type_specifier_non_array("uvec3".into()).unspan(),
    Ok(("", syntax::TypeSpecifierNonArray::UVec3))
  );
  assert_ceq!(
    type_specifier_non_array("uvec4".into()).unspan(),
    Ok(("", syntax::TypeSpecifierNonArray::UVec4))
  );
  assert_ceq!(
    type_specifier_non_array("mat2".into()).unspan(),
    Ok(("", syntax::TypeSpecifierNonArray::Mat2))
  );
  assert_ceq!(
    type_specifier_non_array("mat3".into()).unspan(),
    Ok(("", syntax::TypeSpecifierNonArray::Mat3))
  );
  assert_ceq!(
    type_specifier_non_array("mat4".into()).unspan(),
    Ok(("", syntax::TypeSpecifierNonArray::Mat4))
  );
  assert_ceq!(
    type_specifier_non_array("mat2x2".into()).unspan(),
    Ok(("", syntax::TypeSpecifierNonArray::Mat2))
  );
  assert_ceq!(
    type_specifier_non_array("mat2x3".into()).unspan(),
    Ok(("", syntax::TypeSpecifierNonArray::Mat23))
  );
  assert_ceq!(
    type_specifier_non_array("mat2x4".into()).unspan(),
    Ok(("", syntax::TypeSpecifierNonArray::Mat24))
  );
  assert_ceq!(
    type_specifier_non_array("mat3x2".into()).unspan(),
    Ok(("", syntax::TypeSpecifierNonArray::Mat32))
  );
  assert_ceq!(
    type_specifier_non_array("mat3x3".into()).unspan(),
    Ok(("", syntax::TypeSpecifierNonArray::Mat3))
  );
  assert_ceq!(
    type_specifier_non_array("mat3x4".into()).unspan(),
    Ok(("", syntax::TypeSpecifierNonArray::Mat34))
  );
  assert_ceq!(
    type_specifier_non_array("mat4x2".into()).unspan(),
    Ok(("", syntax::TypeSpecifierNonArray::Mat42))
  );
  assert_ceq!(
    type_specifier_non_array("mat4x3".into()).unspan(),
    Ok(("", syntax::TypeSpecifierNonArray::Mat43))
  );
  assert_ceq!(
    type_specifier_non_array("mat4x4".into()).unspan(),
    Ok(("", syntax::TypeSpecifierNonArray::Mat4))
  );
  assert_ceq!(
    type_specifier_non_array("dmat2".into()).unspan(),
    Ok(("", syntax::TypeSpecifierNonArray::DMat2))
  );
  assert_ceq!(
    type_specifier_non_array("dmat3".into()).unspan(),
    Ok(("", syntax::TypeSpecifierNonArray::DMat3))
  );
  assert_ceq!(
    type_specifier_non_array("dmat4".into()).unspan(),
    Ok(("", syntax::TypeSpecifierNonArray::DMat4))
  );
  assert_ceq!(
    type_specifier_non_array("dmat2x2".into()).unspan(),
    Ok(("", syntax::TypeSpecifierNonArray::DMat2))
  );
  assert_ceq!(
    type_specifier_non_array("dmat2x3".into()).unspan(),
    Ok(("", syntax::TypeSpecifierNonArray::DMat23))
  );
  assert_ceq!(
    type_specifier_non_array("dmat2x4".into()).unspan(),
    Ok(("", syntax::TypeSpecifierNonArray::DMat24))
  );
  assert_ceq!(
    type_specifier_non_array("dmat3x2".into()).unspan(),
    Ok(("", syntax::TypeSpecifierNonArray::DMat32))
  );
  assert_ceq!(
    type_specifier_non_array("dmat3x3".into()).unspan(),
    Ok(("", syntax::TypeSpecifierNonArray::DMat3))
  );
  assert_ceq!(
    type_specifier_non_array("dmat3x4".into()).unspan(),
    Ok(("", syntax::TypeSpecifierNonArray::DMat34))
  );
  assert_ceq!(
    type_specifier_non_array("dmat4x2".into()).unspan(),
    Ok(("", syntax::TypeSpecifierNonArray::DMat42))
  );
  assert_ceq!(
    type_specifier_non_array("dmat4x3".into()).unspan(),
    Ok(("", syntax::TypeSpecifierNonArray::DMat43))
  );
  assert_ceq!(
    type_specifier_non_array("dmat4x4".into()).unspan(),
    Ok(("", syntax::TypeSpecifierNonArray::DMat4))
  );
  assert_ceq!(
    type_specifier_non_array("sampler1D".into()).unspan(),
    Ok(("", syntax::TypeSpecifierNonArray::Sampler1D))
  );
  assert_ceq!(
    type_specifier_non_array("image1D".into()).unspan(),
    Ok(("", syntax::TypeSpecifierNonArray::Image1D))
  );
  assert_ceq!(
    type_specifier_non_array("sampler2D".into()).unspan(),
    Ok(("", syntax::TypeSpecifierNonArray::Sampler2D))
  );
  assert_ceq!(
    type_specifier_non_array("image2D".into()).unspan(),
    Ok(("", syntax::TypeSpecifierNonArray::Image2D))
  );
  assert_ceq!(
    type_specifier_non_array("sampler3D".into()).unspan(),
    Ok(("", syntax::TypeSpecifierNonArray::Sampler3D))
  );
  assert_ceq!(
    type_specifier_non_array("image3D".into()).unspan(),
    Ok(("", syntax::TypeSpecifierNonArray::Image3D))
  );
  assert_ceq!(
    type_specifier_non_array("samplerCube".into()).unspan(),
    Ok(("", syntax::TypeSpecifierNonArray::SamplerCube))
  );
  assert_ceq!(
    type_specifier_non_array("imageCube".into()).unspan(),
    Ok(("", syntax::TypeSpecifierNonArray::ImageCube))
  );
  assert_ceq!(
    type_specifier_non_array("sampler2DRect".into()).unspan(),
    Ok(("", syntax::TypeSpecifierNonArray::Sampler2DRect))
  );
  assert_ceq!(
    type_specifier_non_array("image2DRect".into()).unspan(),
    Ok(("", syntax::TypeSpecifierNonArray::Image2DRect))
  );
  assert_ceq!(
    type_specifier_non_array("sampler1DArray".into()).unspan(),
    Ok(("", syntax::TypeSpecifierNonArray::Sampler1DArray))
  );
  assert_ceq!(
    type_specifier_non_array("image1DArray".into()).unspan(),
    Ok(("", syntax::TypeSpecifierNonArray::Image1DArray))
  );
  assert_ceq!(
    type_specifier_non_array("sampler2DArray".into()).unspan(),
    Ok(("", syntax::TypeSpecifierNonArray::Sampler2DArray))
  );
  assert_ceq!(
    type_specifier_non_array("image2DArray".into()).unspan(),
    Ok(("", syntax::TypeSpecifierNonArray::Image2DArray))
  );
  assert_ceq!(
    type_specifier_non_array("samplerBuffer".into()).unspan(),
    Ok(("", syntax::TypeSpecifierNonArray::SamplerBuffer))
  );
  assert_ceq!(
    type_specifier_non_array("imageBuffer".into()).unspan(),
    Ok(("", syntax::TypeSpecifierNonArray::ImageBuffer))
  );
  assert_ceq!(
    type_specifier_non_array("sampler2DMS".into()).unspan(),
    Ok(("", syntax::TypeSpecifierNonArray::Sampler2DMS))
  );
  assert_ceq!(
    type_specifier_non_array("image2DMS".into()).unspan(),
    Ok(("", syntax::TypeSpecifierNonArray::Image2DMS))
  );
  assert_ceq!(
    type_specifier_non_array("sampler2DMSArray".into()).unspan(),
    Ok(("", syntax::TypeSpecifierNonArray::Sampler2DMSArray))
  );
  assert_ceq!(
    type_specifier_non_array("image2DMSArray".into()).unspan(),
    Ok(("", syntax::TypeSpecifierNonArray::Image2DMSArray))
  );
  assert_ceq!(
    type_specifier_non_array("samplerCubeArray".into()).unspan(),
    Ok(("", syntax::TypeSpecifierNonArray::SamplerCubeArray))
  );
  assert_ceq!(
    type_specifier_non_array("imageCubeArray".into()).unspan(),
    Ok(("", syntax::TypeSpecifierNonArray::ImageCubeArray))
  );
  assert_ceq!(
    type_specifier_non_array("sampler1DShadow".into()).unspan(),
    Ok(("", syntax::TypeSpecifierNonArray::Sampler1DShadow))
  );
  assert_ceq!(
    type_specifier_non_array("sampler2DShadow".into()).unspan(),
    Ok(("", syntax::TypeSpecifierNonArray::Sampler2DShadow))
  );
  assert_ceq!(
    type_specifier_non_array("sampler2DRectShadow".into()).unspan(),
    Ok(("", syntax::TypeSpecifierNonArray::Sampler2DRectShadow))
  );
  assert_ceq!(
    type_specifier_non_array("sampler1DArrayShadow".into()).unspan(),
    Ok(("", syntax::TypeSpecifierNonArray::Sampler1DArrayShadow))
  );
  assert_ceq!(
    type_specifier_non_array("sampler2DArrayShadow".into()).unspan(),
    Ok(("", syntax::TypeSpecifierNonArray::Sampler2DArrayShadow))
  );
  assert_ceq!(
    type_specifier_non_array("samplerCubeShadow".into()).unspan(),
    Ok(("", syntax::TypeSpecifierNonArray::SamplerCubeShadow))
  );
  assert_ceq!(
    type_specifier_non_array("samplerCubeArrayShadow".into()).unspan(),
    Ok(("", syntax::TypeSpecifierNonArray::SamplerCubeArrayShadow))
  );
  assert_ceq!(
    type_specifier_non_array("isampler1D".into()).unspan(),
    Ok(("", syntax::TypeSpecifierNonArray::ISampler1D))
  );
  assert_ceq!(
    type_specifier_non_array("iimage1D".into()).unspan(),
    Ok(("", syntax::TypeSpecifierNonArray::IImage1D))
  );
  assert_ceq!(
    type_specifier_non_array("isampler2D".into()).unspan(),
    Ok(("", syntax::TypeSpecifierNonArray::ISampler2D))
  );
  assert_ceq!(
    type_specifier_non_array("iimage2D".into()).unspan(),
    Ok(("", syntax::TypeSpecifierNonArray::IImage2D))
  );
  assert_ceq!(
    type_specifier_non_array("isampler3D".into()).unspan(),
    Ok(("", syntax::TypeSpecifierNonArray::ISampler3D))
  );
  assert_ceq!(
    type_specifier_non_array("iimage3D".into()).unspan(),
    Ok(("", syntax::TypeSpecifierNonArray::IImage3D))
  );
  assert_ceq!(
    type_specifier_non_array("isamplerCube".into()).unspan(),
    Ok(("", syntax::TypeSpecifierNonArray::ISamplerCube))
  );
  assert_ceq!(
    type_specifier_non_array("iimageCube".into()).unspan(),
    Ok(("", syntax::TypeSpecifierNonArray::IImageCube))
  );
  assert_ceq!(
    type_specifier_non_array("isampler2DRect".into()).unspan(),
    Ok(("", syntax::TypeSpecifierNonArray::ISampler2DRect))
  );
  assert_ceq!(
    type_specifier_non_array("iimage2DRect".into()).unspan(),
    Ok(("", syntax::TypeSpecifierNonArray::IImage2DRect))
  );
  assert_ceq!(
    type_specifier_non_array("isampler1DArray".into()).unspan(),
    Ok(("", syntax::TypeSpecifierNonArray::ISampler1DArray))
  );
  assert_ceq!(
    type_specifier_non_array("iimage1DArray".into()).unspan(),
    Ok(("", syntax::TypeSpecifierNonArray::IImage1DArray))
  );
  assert_ceq!(
    type_specifier_non_array("isampler2DArray".into()).unspan(),
    Ok(("", syntax::TypeSpecifierNonArray::ISampler2DArray))
  );
  assert_ceq!(
    type_specifier_non_array("iimage2DArray".into()).unspan(),
    Ok(("", syntax::TypeSpecifierNonArray::IImage2DArray))
  );
  assert_ceq!(
    type_specifier_non_array("isamplerBuffer".into()).unspan(),
    Ok(("", syntax::TypeSpecifierNonArray::ISamplerBuffer))
  );
  assert_ceq!(
    type_specifier_non_array("iimageBuffer".into()).unspan(),
    Ok(("", syntax::TypeSpecifierNonArray::IImageBuffer))
  );
  assert_ceq!(
    type_specifier_non_array("isampler2DMS".into()).unspan(),
    Ok(("", syntax::TypeSpecifierNonArray::ISampler2DMS))
  );
  assert_ceq!(
    type_specifier_non_array("iimage2DMS".into()).unspan(),
    Ok(("", syntax::TypeSpecifierNonArray::IImage2DMS))
  );
  assert_ceq!(
    type_specifier_non_array("isampler2DMSArray".into()).unspan(),
    Ok(("", syntax::TypeSpecifierNonArray::ISampler2DMSArray))
  );
  assert_ceq!(
    type_specifier_non_array("iimage2DMSArray".into()).unspan(),
    Ok(("", syntax::TypeSpecifierNonArray::IImage2DMSArray))
  );
  assert_ceq!(
    type_specifier_non_array("isamplerCubeArray".into()).unspan(),
    Ok(("", syntax::TypeSpecifierNonArray::ISamplerCubeArray))
  );
  assert_ceq!(
    type_specifier_non_array("iimageCubeArray".into()).unspan(),
    Ok(("", syntax::TypeSpecifierNonArray::IImageCubeArray))
  );
  assert_ceq!(
    type_specifier_non_array("atomic_uint".into()).unspan(),
    Ok(("", syntax::TypeSpecifierNonArray::AtomicUInt))
  );
  assert_ceq!(
    type_specifier_non_array("usampler1D".into()).unspan(),
    Ok(("", syntax::TypeSpecifierNonArray::USampler1D))
  );
  assert_ceq!(
    type_specifier_non_array("uimage1D".into()).unspan(),
    Ok(("", syntax::TypeSpecifierNonArray::UImage1D))
  );
  assert_ceq!(
    type_specifier_non_array("usampler2D".into()).unspan(),
    Ok(("", syntax::TypeSpecifierNonArray::USampler2D))
  );
  assert_ceq!(
    type_specifier_non_array("uimage2D".into()).unspan(),
    Ok(("", syntax::TypeSpecifierNonArray::UImage2D))
  );
  assert_ceq!(
    type_specifier_non_array("usampler3D".into()).unspan(),
    Ok(("", syntax::TypeSpecifierNonArray::USampler3D))
  );
  assert_ceq!(
    type_specifier_non_array("uimage3D".into()).unspan(),
    Ok(("", syntax::TypeSpecifierNonArray::UImage3D))
  );
  assert_ceq!(
    type_specifier_non_array("usamplerCube".into()).unspan(),
    Ok(("", syntax::TypeSpecifierNonArray::USamplerCube))
  );
  assert_ceq!(
    type_specifier_non_array("uimageCube".into()).unspan(),
    Ok(("", syntax::TypeSpecifierNonArray::UImageCube))
  );
  assert_ceq!(
    type_specifier_non_array("usampler2DRect".into()).unspan(),
    Ok(("", syntax::TypeSpecifierNonArray::USampler2DRect))
  );
  assert_ceq!(
    type_specifier_non_array("uimage2DRect".into()).unspan(),
    Ok(("", syntax::TypeSpecifierNonArray::UImage2DRect))
  );
  assert_ceq!(
    type_specifier_non_array("usampler1DArray".into()).unspan(),
    Ok(("", syntax::TypeSpecifierNonArray::USampler1DArray))
  );
  assert_ceq!(
    type_specifier_non_array("uimage1DArray".into()).unspan(),
    Ok(("", syntax::TypeSpecifierNonArray::UImage1DArray))
  );
  assert_ceq!(
    type_specifier_non_array("usampler2DArray".into()).unspan(),
    Ok(("", syntax::TypeSpecifierNonArray::USampler2DArray))
  );
  assert_ceq!(
    type_specifier_non_array("uimage2DArray".into()).unspan(),
    Ok(("", syntax::TypeSpecifierNonArray::UImage2DArray))
  );
  assert_ceq!(
    type_specifier_non_array("usamplerBuffer".into()).unspan(),
    Ok(("", syntax::TypeSpecifierNonArray::USamplerBuffer))
  );
  assert_ceq!(
    type_specifier_non_array("uimageBuffer".into()).unspan(),
    Ok(("", syntax::TypeSpecifierNonArray::UImageBuffer))
  );
  assert_ceq!(
    type_specifier_non_array("usampler2DMS".into()).unspan(),
    Ok(("", syntax::TypeSpecifierNonArray::USampler2DMS))
  );
  assert_ceq!(
    type_specifier_non_array("uimage2DMS".into()).unspan(),
    Ok(("", syntax::TypeSpecifierNonArray::UImage2DMS))
  );
  assert_ceq!(
    type_specifier_non_array("usampler2DMSArray".into()).unspan(),
    Ok(("", syntax::TypeSpecifierNonArray::USampler2DMSArray))
  );
  assert_ceq!(
    type_specifier_non_array("uimage2DMSArray".into()).unspan(),
    Ok(("", syntax::TypeSpecifierNonArray::UImage2DMSArray))
  );
  assert_ceq!(
    type_specifier_non_array("usamplerCubeArray".into()).unspan(),
    Ok(("", syntax::TypeSpecifierNonArray::USamplerCubeArray))
  );
  assert_ceq!(
    type_specifier_non_array("uimageCubeArray".into()).unspan(),
    Ok(("", syntax::TypeSpecifierNonArray::UImageCubeArray))
  );
  assert_ceq!(
    type_specifier_non_array("ReturnType".into()).unspan(),
    Ok((
      "",
      syntax::TypeSpecifierNonArray::TypeName(syntax::TypeName::new("ReturnType").unwrap())
    ))
  );
}

#[test]
fn parse_type_specifier() {
  assert_ceq!(
    type_specifier("uint;".into()).unspan(),
    Ok((
      ";",
      syntax::TypeSpecifier {
        ty: syntax::TypeSpecifierNonArray::UInt,
        array_specifier: None
      }
    ))
  );
  assert_ceq!(
    type_specifier("iimage2DMSArray[35];".into()).unspan(),
    Ok((
      ";",
      syntax::TypeSpecifier {
        ty: syntax::TypeSpecifierNonArray::IImage2DMSArray,
        array_specifier: Some(syntax::ArraySpecifier {
          dimensions: syntax::NonEmpty(vec![syntax::ArraySpecifierDimension::ExplicitlySized(
            Box::new(syntax::Expr::IntConst(35))
          )])
        })
      }
    ))
  );
}

#[test]
fn parse_fully_specified_type() {
  let ty = syntax::TypeSpecifier {
    ty: syntax::TypeSpecifierNonArray::IImage2DMSArray,
    array_specifier: None,
  };
  let expected = syntax::FullySpecifiedType {
    qualifier: None,
    ty,
  };

  assert_ceq!(
    fully_specified_type("iimage2DMSArray;".into()).unspan(),
    Ok((";", expected.clone()))
  );
}

#[test]
fn parse_fully_specified_type_with_qualifier() {
  let qual_spec = syntax::TypeQualifierSpec::Storage(syntax::StorageQualifier::Subroutine(vec![
    "vec2".into(),
    "S032_29k".into(),
  ]));
  let qual = syntax::TypeQualifier {
    qualifiers: syntax::NonEmpty(vec![qual_spec]),
  };
  let ty = syntax::TypeSpecifier {
    ty: syntax::TypeSpecifierNonArray::IImage2DMSArray,
    array_specifier: None,
  };
  let expected = syntax::FullySpecifiedType {
    qualifier: Some(qual),
    ty,
  };

  assert_ceq!(
    fully_specified_type("subroutine (vec2, S032_29k) iimage2DMSArray;".into()).unspan(),
    Ok((";", expected.clone()))
  );
  assert_ceq!(
    fully_specified_type("subroutine (  vec2\t\n \t , \n S032_29k   )\n iimage2DMSArray ;".into())
      .unspan(),
    Ok((" ;", expected.clone()))
  );
  assert_ceq!(
    fully_specified_type("subroutine(vec2,S032_29k)iimage2DMSArray;".into()).unspan(),
    Ok((";", expected))
  );
}

#[test]
fn parse_primary_expr_intconst() {
  assert_ceq!(
    primary_expr("0 ".into()).unspan(),
    Ok((" ", syntax::Expr::IntConst(0)))
  );
  assert_ceq!(
    primary_expr("1 ".into()).unspan(),
    Ok((" ", syntax::Expr::IntConst(1)))
  );
}

#[test]
fn parse_primary_expr_uintconst() {
  assert_ceq!(
    primary_expr("0u ".into()).unspan(),
    Ok((" ", syntax::Expr::UIntConst(0)))
  );
  assert_ceq!(
    primary_expr("1u ".into()).unspan(),
    Ok((" ", syntax::Expr::UIntConst(1)))
  );
}

#[test]
fn parse_primary_expr_floatconst() {
  assert_ceq!(
    primary_expr("0.f ".into()).unspan(),
    Ok((" ", syntax::Expr::FloatConst(0.)))
  );
  assert_ceq!(
    primary_expr("1.f ".into()).unspan(),
    Ok((" ", syntax::Expr::FloatConst(1.)))
  );
  assert_ceq!(
    primary_expr("0.F ".into()).unspan(),
    Ok((" ", syntax::Expr::FloatConst(0.)))
  );
  assert_ceq!(
    primary_expr("1.F ".into()).unspan(),
    Ok((" ", syntax::Expr::FloatConst(1.)))
  );
}

#[test]
fn parse_primary_expr_doubleconst() {
  assert_ceq!(
    primary_expr("0. ".into()).unspan(),
    Ok((" ", syntax::Expr::FloatConst(0.)))
  );
  assert_ceq!(
    primary_expr("1. ".into()).unspan(),
    Ok((" ", syntax::Expr::FloatConst(1.)))
  );
  assert_ceq!(
    primary_expr("0.lf ".into()).unspan(),
    Ok((" ", syntax::Expr::DoubleConst(0.)))
  );
  assert_ceq!(
    primary_expr("1.lf ".into()).unspan(),
    Ok((" ", syntax::Expr::DoubleConst(1.)))
  );
  assert_ceq!(
    primary_expr("0.LF ".into()).unspan(),
    Ok((" ", syntax::Expr::DoubleConst(0.)))
  );
  assert_ceq!(
    primary_expr("1.LF ".into()).unspan(),
    Ok((" ", syntax::Expr::DoubleConst(1.)))
  );
}

#[test]
fn parse_primary_expr_boolconst() {
  assert_ceq!(
    primary_expr("false".into()).unspan(),
    Ok(("", syntax::Expr::BoolConst(false.to_owned())))
  );
  assert_ceq!(
    primary_expr("true".into()).unspan(),
    Ok(("", syntax::Expr::BoolConst(true.to_owned())))
  );
}

#[test]
fn parse_primary_expr_parens() {
  assert_ceq!(
    primary_expr("(0)".into()).unspan(),
    Ok(("", syntax::Expr::IntConst(0)))
  );
  assert_ceq!(
    primary_expr("(  0 )".into()).unspan(),
    Ok(("", syntax::Expr::IntConst(0)))
  );
  assert_ceq!(
    primary_expr("(  .0 )".into()).unspan(),
    Ok(("", syntax::Expr::FloatConst(0.)))
  );
  assert_ceq!(
    primary_expr("(  (.0) )".into()).unspan(),
    Ok(("", syntax::Expr::FloatConst(0.)))
  );
  assert_ceq!(
    primary_expr("(true) ".into()).unspan(),
    Ok((" ", syntax::Expr::BoolConst(true)))
  );
}

#[test]
fn parse_postfix_function_call_no_args() {
  let fun = syntax::FunIdentifier::Identifier("vec3".into());
  let args = Vec::new();
  let expected = syntax::Expr::FunCall(fun, args);

  assert_ceq!(
    postfix_expr("vec3();".into()).unspan(),
    Ok((";", expected.clone()))
  );
  assert_ceq!(
    postfix_expr("vec3   (  ) ;".into()).unspan(),
    Ok((" ;", expected.clone()))
  );
  assert_ceq!(
    postfix_expr("vec3   (\nvoid\n) ;".into()).unspan(),
    Ok((" ;", expected))
  );
}

#[test]
fn parse_postfix_function_call_one_arg() {
  let fun = syntax::FunIdentifier::Identifier("foo".into());
  let args = vec![syntax::Expr::IntConst(0)];
  let expected = syntax::Expr::FunCall(fun, args);

  assert_ceq!(
    postfix_expr("foo(0);".into()).unspan(),
    Ok((";", expected.clone()))
  );
  assert_ceq!(
    postfix_expr("foo   ( 0 ) ;".into()).unspan(),
    Ok((" ;", expected.clone()))
  );
  assert_ceq!(
    postfix_expr("foo   (\n0\t\n) ;".into()).unspan(),
    Ok((" ;", expected))
  );
}

#[test]
fn parse_postfix_function_call_multi_arg() {
  let fun = syntax::FunIdentifier::Identifier("foo".into());
  let args = vec![
    syntax::Expr::IntConst(0),
    syntax::Expr::BoolConst(false),
    syntax::Expr::Variable("bar".into()),
  ];
  let expected = syntax::Expr::FunCall(fun, args);

  assert_ceq!(
    postfix_expr("foo(0, false, bar);".into()).unspan(),
    Ok((";", expected.clone()))
  );
  assert_ceq!(
    postfix_expr("foo   ( 0\t, false    ,\t\tbar) ;".into()).unspan(),
    Ok((" ;", expected))
  );
}

#[test]
fn parse_postfix_expr_bracket() {
  let id = syntax::Expr::Variable("foo".into());
  let array_spec = syntax::ArraySpecifier {
    dimensions: syntax::NonEmpty(vec![syntax::ArraySpecifierDimension::ExplicitlySized(
      Box::new(syntax::Expr::IntConst(7354)),
    )]),
  };
  let expected = syntax::Expr::Bracket(Box::new(id), array_spec);

  assert_ceq!(
    postfix_expr("foo[7354];".into()).unspan(),
    Ok((";", expected.clone()))
  );
  assert_ceq!(
    postfix_expr("foo[\n  7354    ] ;".into()).unspan(),
    Ok((";", expected))
  );
}

#[test]
fn parse_postfix_expr_dot() {
  let foo = Box::new(syntax::Expr::Variable("foo".into()));
  let expected = syntax::Expr::Dot(foo, "bar".into());

  assert_ceq!(
    postfix_expr("foo.bar;".into()).unspan(),
    Ok((";", expected.clone()))
  );
  assert_ceq!(
    postfix_expr("(foo).bar;".into()).unspan(),
    Ok((";", expected))
  );
}

#[test]
fn parse_postfix_expr_dot_several() {
  let foo = Box::new(syntax::Expr::Variable("foo".into()));
  let expected = syntax::Expr::Dot(Box::new(syntax::Expr::Dot(foo, "bar".into())), "zoo".into());

  assert_ceq!(
    postfix_expr("foo.bar.zoo;".into()).unspan(),
    Ok((";", expected.clone()))
  );
  assert_ceq!(
    postfix_expr("(foo).bar.zoo;".into()).unspan(),
    Ok((";", expected.clone()))
  );
  assert_ceq!(
    postfix_expr("(foo.bar).zoo;".into()).unspan(),
    Ok((";", expected))
  );
}

#[test]
fn parse_postfix_postinc() {
  let foo = syntax::Expr::Variable("foo".into());
  let expected = syntax::Expr::PostInc(Box::new(foo));

  assert_ceq!(
    postfix_expr("foo++;".into()).unspan(),
    Ok((";", expected.clone()))
  );
}

#[test]
fn parse_postfix_postdec() {
  let foo = syntax::Expr::Variable("foo".into());
  let expected = syntax::Expr::PostDec(Box::new(foo));

  assert_ceq!(
    postfix_expr("foo--;".into()).unspan(),
    Ok((";", expected.clone()))
  );
}

#[test]
fn parse_unary_add() {
  let foo = syntax::Expr::Variable("foo".into());
  let expected = syntax::Expr::Unary(syntax::UnaryOp::Add, Box::new(foo));

  assert_ceq!(
    unary_expr("+foo;".into()).unspan(),
    Ok((";", expected.clone()))
  );
}

#[test]
fn parse_unary_minus() {
  let foo = syntax::Expr::Variable("foo".into());
  let expected = syntax::Expr::Unary(syntax::UnaryOp::Minus, Box::new(foo));

  assert_ceq!(
    unary_expr("-foo;".into()).unspan(),
    Ok((";", expected.clone()))
  );
}

#[test]
fn parse_unary_not() {
  let foo = syntax::Expr::Variable("foo".into());
  let expected = syntax::Expr::Unary(syntax::UnaryOp::Not, Box::new(foo));

  assert_ceq!(unary_expr("!foo;".into()).unspan(), Ok((";", expected)));
}

#[test]
fn parse_unary_complement() {
  let foo = syntax::Expr::Variable("foo".into());
  let expected = syntax::Expr::Unary(syntax::UnaryOp::Complement, Box::new(foo));

  assert_ceq!(
    unary_expr("~foo;".into()).unspan(),
    Ok((";", expected.clone()))
  );
}

#[test]
fn parse_unary_inc() {
  let foo = syntax::Expr::Variable("foo".into());
  let expected = syntax::Expr::Unary(syntax::UnaryOp::Inc, Box::new(foo));

  assert_ceq!(
    unary_expr("++foo;".into()).unspan(),
    Ok((";", expected.clone()))
  );
}

#[test]
fn parse_unary_dec() {
  let foo = syntax::Expr::Variable("foo".into());
  let expected = syntax::Expr::Unary(syntax::UnaryOp::Dec, Box::new(foo));

  assert_ceq!(
    unary_expr("--foo;".into()).unspan(),
    Ok((";", expected.clone()))
  );
}

#[test]
fn parse_expr_float() {
  assert_ceq!(
    expr("314.;".into()).unspan(),
    Ok((";", syntax::Expr::FloatConst(314.)))
  );
  assert_ceq!(
    expr("314.f;".into()).unspan(),
    Ok((";", syntax::Expr::FloatConst(314.)))
  );
  assert_ceq!(
    expr("314.LF;".into()).unspan(),
    Ok((";", syntax::Expr::DoubleConst(314.)))
  );
}

#[test]
fn parse_expr_add_2() {
  let one = Box::new(syntax::Expr::IntConst(1));
  let expected = syntax::Expr::Binary(syntax::BinaryOp::Add, one.clone(), one);

  assert_ceq!(expr("1 + 1;".into()).unspan(), Ok((";", expected.clone())));
  assert_ceq!(expr("1+1;".into()).unspan(), Ok((";", expected.clone())));
  assert_ceq!(expr("(1 + 1);".into()).unspan(), Ok((";", expected)));
}

#[test]
fn parse_expr_add_3() {
  let one = Box::new(syntax::Expr::UIntConst(1));
  let two = Box::new(syntax::Expr::UIntConst(2));
  let three = Box::new(syntax::Expr::UIntConst(3));
  let expected = syntax::Expr::Binary(
    syntax::BinaryOp::Add,
    Box::new(syntax::Expr::Binary(syntax::BinaryOp::Add, one, two)),
    three,
  );

  assert_ceq!(
    expr("1u + 2u + 3u".into()).unspan(),
    Ok(("", expected.clone()))
  );
  assert_ceq!(
    expr("1u + 2u + 3u   ".into()).unspan(),
    Ok(("   ", expected.clone()))
  );
  assert_ceq!(expr("1u+2u+3u".into()).unspan(), Ok(("", expected.clone())));
  assert_ceq!(expr("((1u + 2u) + 3u)".into()).unspan(), Ok(("", expected)));
}

#[test]
fn parse_expr_add_mult_3() {
  let one = Box::new(syntax::Expr::UIntConst(1));
  let two = Box::new(syntax::Expr::UIntConst(2));
  let three = Box::new(syntax::Expr::UIntConst(3));
  let expected = syntax::Expr::Binary(
    syntax::BinaryOp::Add,
    Box::new(syntax::Expr::Binary(syntax::BinaryOp::Mult, one, two)),
    three,
  );

  assert_ceq!(
    expr("1u * 2u + 3u ;".into()).unspan(),
    Ok((" ;", expected.clone()))
  );
  assert_ceq!(
    expr("1u*2u+3u;".into()).unspan(),
    Ok((";", expected.clone()))
  );
  assert_ceq!(expr("(1u * 2u) + 3u;".into()).unspan(), Ok((";", expected)));
}

#[test]
fn parse_expr_add_sub_mult_div() {
  let one = Box::new(syntax::Expr::IntConst(1));
  let two = Box::new(syntax::Expr::IntConst(2));
  let three = Box::new(syntax::Expr::IntConst(3));
  let four = Box::new(syntax::Expr::IntConst(4));
  let five = Box::new(syntax::Expr::IntConst(5));
  let six = Box::new(syntax::Expr::IntConst(6));
  let expected = syntax::Expr::Binary(
    syntax::BinaryOp::Add,
    Box::new(syntax::Expr::Binary(
      syntax::BinaryOp::Mult,
      one,
      Box::new(syntax::Expr::Binary(syntax::BinaryOp::Add, two, three)),
    )),
    Box::new(syntax::Expr::Binary(
      syntax::BinaryOp::Div,
      four,
      Box::new(syntax::Expr::Binary(syntax::BinaryOp::Add, five, six)),
    )),
  );

  assert_ceq!(
    expr("1 * (2 + 3) + 4 / (5 + 6);".into()).unspan(),
    Ok((";", expected.clone()))
  );
}

#[test]
fn parse_complex_expr() {
  let input = "normalize((inverse(view) * vec4(ray.dir, 0.)).xyz);";
  let zero = syntax::Expr::FloatConst(0.);
  let ray = syntax::Expr::Variable("ray".into());
  let raydir = syntax::Expr::Dot(Box::new(ray), "dir".into());
  let vec4 = syntax::Expr::FunCall(
    syntax::FunIdentifier::Identifier("vec4".into()),
    vec![raydir, zero],
  );
  let view = syntax::Expr::Variable("view".into());
  let iview = syntax::Expr::FunCall(
    syntax::FunIdentifier::Identifier("inverse".into()),
    vec![view],
  );
  let mul = syntax::Expr::Binary(syntax::BinaryOp::Mult, Box::new(iview), Box::new(vec4));
  let xyz = syntax::Expr::Dot(Box::new(mul), "xyz".into());
  let normalize = syntax::Expr::FunCall(
    syntax::FunIdentifier::Identifier("normalize".into()),
    vec![xyz],
  );
  let expected = normalize;

  assert_ceq!(expr((&input[..]).into()).unspan(), Ok((";", expected)));
}

#[test]
fn parse_function_identifier_typename() {
  let expected = syntax::FunIdentifier::Identifier("foo".into());
  assert_ceq!(
    function_identifier("foo(".into()).unspan(),
    Ok(("(", expected.clone()))
  );
  assert_ceq!(
    function_identifier("foo\n\t(".into()).unspan(),
    Ok(("(", expected.clone()))
  );
  assert_ceq!(
    function_identifier("foo\n (".into()).unspan(),
    Ok(("(", expected))
  );
}

#[test]
fn parse_function_identifier_cast() {
  let expected = syntax::FunIdentifier::Identifier("vec3".into());
  assert_ceq!(
    function_identifier("vec3(".into()).unspan(),
    Ok(("(", expected.clone()))
  );
  assert_ceq!(
    function_identifier("vec3 (".into()).unspan(),
    Ok(("(", expected.clone()))
  );
  assert_ceq!(
    function_identifier("vec3\t\n\n \t (".into()).unspan(),
    Ok(("(", expected))
  );
}

#[test]
fn parse_function_identifier_cast_array_unsized() {
  let expected = syntax::FunIdentifier::Expr(Box::new(syntax::Expr::Bracket(
    Box::new(syntax::Expr::Variable("vec3".into())),
    syntax::ArraySpecifier {
      dimensions: syntax::NonEmpty(vec![syntax::ArraySpecifierDimension::Unsized]),
    },
  )));

  assert_ceq!(
    function_identifier("vec3[](".into()).unspan(),
    Ok(("(", expected.clone()))
  );
  assert_ceq!(
    function_identifier("vec3  [\t\n](".into()).unspan(),
    Ok(("(", expected))
  );
}

#[test]
fn parse_function_identifier_cast_array_sized() {
  let expected = syntax::FunIdentifier::Expr(Box::new(syntax::Expr::Bracket(
    Box::new(syntax::Expr::Variable("vec3".into())),
    syntax::ArraySpecifier {
      dimensions: syntax::NonEmpty(vec![syntax::ArraySpecifierDimension::ExplicitlySized(
        Box::new(syntax::Expr::IntConst(12)),
      )]),
    },
  )));

  assert_ceq!(
    function_identifier("vec3[12](".into()).unspan(),
    Ok(("(", expected.clone()))
  );
  assert_ceq!(
    function_identifier("vec3  [\t 12\n](".into()).unspan(),
    Ok(("(", expected))
  );
}

#[test]
fn parse_void() {
  assert_ceq!(void("void ".into()).unspan(), Ok((" ", ())));
}

#[test]
fn parse_assignment_op_equal() {
  assert_ceq!(
    assignment_op("= ".into()).unspan(),
    Ok((" ", syntax::AssignmentOp::Equal))
  );
}

#[test]
fn parse_assignment_op_mult() {
  assert_ceq!(
    assignment_op("*= ".into()).unspan(),
    Ok((" ", syntax::AssignmentOp::Mult))
  );
}

#[test]
fn parse_assignment_op_div() {
  assert_ceq!(
    assignment_op("/= ".into()).unspan(),
    Ok((" ", syntax::AssignmentOp::Div))
  );
}

#[test]
fn parse_assignment_op_mod() {
  assert_ceq!(
    assignment_op("%= ".into()).unspan(),
    Ok((" ", syntax::AssignmentOp::Mod))
  );
}

#[test]
fn parse_assignment_op_add() {
  assert_ceq!(
    assignment_op("+= ".into()).unspan(),
    Ok((" ", syntax::AssignmentOp::Add))
  );
}

#[test]
fn parse_assignment_op_sub() {
  assert_ceq!(
    assignment_op("-= ".into()).unspan(),
    Ok((" ", syntax::AssignmentOp::Sub))
  );
}

#[test]
fn parse_assignment_op_lshift() {
  assert_ceq!(
    assignment_op("<<= ".into()).unspan(),
    Ok((" ", syntax::AssignmentOp::LShift))
  );
}

#[test]
fn parse_assignment_op_rshift() {
  assert_ceq!(
    assignment_op(">>= ".into()).unspan(),
    Ok((" ", syntax::AssignmentOp::RShift))
  );
}

#[test]
fn parse_assignment_op_and() {
  assert_ceq!(
    assignment_op("&= ".into()).unspan(),
    Ok((" ", syntax::AssignmentOp::And))
  );
}

#[test]
fn parse_assignment_op_xor() {
  assert_ceq!(
    assignment_op("^= ".into()).unspan(),
    Ok((" ", syntax::AssignmentOp::Xor))
  );
}

#[test]
fn parse_assignment_op_or() {
  assert_ceq!(
    assignment_op("|= ".into()).unspan(),
    Ok((" ", syntax::AssignmentOp::Or))
  );
}

#[test]
fn parse_expr_statement() {
  let expected = Some(syntax::Expr::Assignment(
    Box::new(syntax::Expr::Variable("foo".into())),
    syntax::AssignmentOp::Equal,
    Box::new(syntax::Expr::FloatConst(314.)),
  ));

  assert_ceq!(
    expr_statement("foo = 314.f;".into()).unspan(),
    Ok(("", expected.clone()))
  );
  assert_ceq!(
    expr_statement("foo=314.f;".into()).unspan(),
    Ok(("", expected.clone()))
  );
  assert_ceq!(
    expr_statement("foo\n\t=  \n314.f;".into()).unspan(),
    Ok(("", expected))
  );
}

#[test]
fn parse_declaration_function_prototype() {
  let rt = syntax::FullySpecifiedType {
    qualifier: None,
    ty: syntax::TypeSpecifier {
      ty: syntax::TypeSpecifierNonArray::Vec3,
      array_specifier: None,
    },
  };
  let arg0_ty = syntax::TypeSpecifier {
    ty: syntax::TypeSpecifierNonArray::Vec2,
    array_specifier: None,
  };
  let arg0 = syntax::FunctionParameterDeclarationData::Unnamed(None, arg0_ty);
  let qual_spec = syntax::TypeQualifierSpec::Storage(syntax::StorageQualifier::Out);
  let qual = syntax::TypeQualifier {
    qualifiers: syntax::NonEmpty(vec![qual_spec]),
  };
  let arg1 = syntax::FunctionParameterDeclarationData::Named(
    Some(qual),
    syntax::FunctionParameterDeclarator {
      ty: syntax::TypeSpecifier {
        ty: syntax::TypeSpecifierNonArray::Float,
        array_specifier: None,
      },
      ident: "the_arg".into(),
    },
  );
  let fp = syntax::FunctionPrototypeData {
    ty: rt,
    name: "foo".into(),
    parameters: vec![arg0.into(), arg1.into()],
  };
  let expected: syntax::Declaration = syntax::DeclarationData::FunctionPrototype(fp.into()).into();

  assert_ceq!(
    declaration("vec3 foo(vec2, out float the_arg);".into()).unspan(),
    Ok(("", expected.clone()))
  );
  assert_ceq!(
    declaration("vec3 \nfoo ( vec2\n, out float \n\tthe_arg )\n;".into()).unspan(),
    Ok(("", expected.clone()))
  );
  assert_ceq!(
    declaration("vec3 foo(vec2,out float the_arg);".into()).unspan(),
    Ok(("", expected))
  );
}

#[test]
fn parse_declaration_init_declarator_list_single() {
  let ty = syntax::FullySpecifiedType {
    qualifier: None,
    ty: syntax::TypeSpecifier {
      ty: syntax::TypeSpecifierNonArray::Int,
      array_specifier: None,
    },
  };
  let sd = syntax::SingleDeclaration {
    ty,
    name: Some("foo".into()),
    array_specifier: None,
    initializer: Some(syntax::Initializer::Simple(Box::new(
      syntax::Expr::IntConst(34),
    ))),
  };
  let idl = syntax::InitDeclaratorList {
    head: sd,
    tail: Vec::new(),
  };
  let expected: syntax::Declaration = syntax::DeclarationData::InitDeclaratorList(idl).into();

  assert_ceq!(
    declaration("int foo = 34;".into()).unspan(),
    Ok(("", expected.clone()))
  );
  assert_ceq!(
    declaration("int foo=34;".into()).unspan(),
    Ok(("", expected.clone()))
  );
  assert_ceq!(
    declaration("int    \t  \nfoo =\t34  ;".into()).unspan(),
    Ok(("", expected))
  );
}

#[test]
fn parse_declaration_init_declarator_list_complex() {
  let ty = syntax::FullySpecifiedType {
    qualifier: None,
    ty: syntax::TypeSpecifier {
      ty: syntax::TypeSpecifierNonArray::Int,
      array_specifier: None,
    },
  };
  let sd = syntax::SingleDeclaration {
    ty,
    name: Some("foo".into()),
    array_specifier: None,
    initializer: Some(syntax::Initializer::Simple(Box::new(
      syntax::Expr::IntConst(34),
    ))),
  };
  let sdnt = syntax::SingleDeclarationNoType {
    ident: "bar".into(),
    initializer: Some(syntax::Initializer::Simple(Box::new(
      syntax::Expr::IntConst(12),
    ))),
  };
  let expected: syntax::Declaration =
    syntax::DeclarationData::InitDeclaratorList(syntax::InitDeclaratorList {
      head: sd,
      tail: vec![sdnt],
    })
    .into();

  assert_ceq!(
    declaration("int foo = 34, bar = 12;".into()).unspan(),
    Ok(("", expected.clone()))
  );
  assert_ceq!(
    declaration("int foo=34,bar=12;".into()).unspan(),
    Ok(("", expected.clone()))
  );
  assert_ceq!(
    declaration("int    \t  \nfoo =\t34 \n,\tbar=      12\n ;".into()).unspan(),
    Ok(("", expected))
  );
}

#[test]
fn parse_declaration_precision_low() {
  let qual = syntax::PrecisionQualifier::Low;
  let ty = syntax::TypeSpecifier {
    ty: syntax::TypeSpecifierNonArray::Float,
    array_specifier: None,
  };
  let expected: syntax::Declaration = syntax::DeclarationData::Precision(qual, ty).into();

  assert_ceq!(
    declaration("precision lowp float;".into()).unspan(),
    Ok(("", expected))
  );
}

#[test]
fn parse_declaration_precision_medium() {
  let qual = syntax::PrecisionQualifier::Medium;
  let ty = syntax::TypeSpecifier {
    ty: syntax::TypeSpecifierNonArray::Float,
    array_specifier: None,
  };
  let expected: syntax::Declaration = syntax::DeclarationData::Precision(qual, ty).into();

  assert_ceq!(
    declaration("precision mediump float;".into()).unspan(),
    Ok(("", expected))
  );
}

#[test]
fn parse_declaration_precision_high() {
  let qual = syntax::PrecisionQualifier::High;
  let ty = syntax::TypeSpecifier {
    ty: syntax::TypeSpecifierNonArray::Float,
    array_specifier: None,
  };
  let expected: syntax::Declaration = syntax::DeclarationData::Precision(qual, ty).into();

  assert_ceq!(
    declaration("precision highp float;".into()).unspan(),
    Ok(("", expected))
  );
}

#[test]
fn parse_declaration_uniform_block() {
  let qual_spec = syntax::TypeQualifierSpec::Storage(syntax::StorageQualifier::Uniform);
  let qual = syntax::TypeQualifier {
    qualifiers: syntax::NonEmpty(vec![qual_spec]),
  };
  let f0 = syntax::StructFieldSpecifier {
    qualifier: None,
    ty: syntax::TypeSpecifier {
      ty: syntax::TypeSpecifierNonArray::Float,
      array_specifier: None,
    },
    identifiers: syntax::NonEmpty(vec!["a".into()]),
  };
  let f1 = syntax::StructFieldSpecifier {
    qualifier: None,
    ty: syntax::TypeSpecifier {
      ty: syntax::TypeSpecifierNonArray::Vec3,
      array_specifier: None,
    },
    identifiers: syntax::NonEmpty(vec!["b".into()]),
  };
  let f2 = syntax::StructFieldSpecifier {
    qualifier: None,
    ty: syntax::TypeSpecifier {
      ty: syntax::TypeSpecifierNonArray::TypeName("foo".into()),
      array_specifier: None,
    },
    identifiers: syntax::NonEmpty(vec!["c".into(), "d".into()]),
  };
  let expected: syntax::Declaration = syntax::DeclarationData::Block(syntax::Block {
    qualifier: qual,
    name: "UniformBlockTest".into(),
    fields: vec![f0, f1, f2],
    identifier: None,
  })
  .into();

  assert_ceq!(
    declaration("uniform UniformBlockTest { float a; vec3 b; foo c, d; };".into()).unspan(),
    Ok(("", expected.clone()))
  );
  assert_ceq!(declaration("uniform   \nUniformBlockTest\n {\n \t float   a  \n; \nvec3 b\n; foo \nc\n, \nd\n;\n }\n\t\n\t\t \t;".into()).unspan(), Ok(("", expected)));
}

#[test]
fn parse_declaration_buffer_block() {
  let qual_spec = syntax::TypeQualifierSpec::Storage(syntax::StorageQualifier::Buffer);
  let qual = syntax::TypeQualifier {
    qualifiers: syntax::NonEmpty(vec![qual_spec]),
  };
  let f0 = syntax::StructFieldSpecifier {
    qualifier: None,
    ty: syntax::TypeSpecifier {
      ty: syntax::TypeSpecifierNonArray::Float,
      array_specifier: None,
    },
    identifiers: syntax::NonEmpty(vec!["a".into()]),
  };
  let f1 = syntax::StructFieldSpecifier {
    qualifier: None,
    ty: syntax::TypeSpecifier {
      ty: syntax::TypeSpecifierNonArray::Vec3,
      array_specifier: None,
    },
    identifiers: syntax::NonEmpty(vec![syntax::ArrayedIdentifier::new(
      "b",
      Some(syntax::ArraySpecifier {
        dimensions: syntax::NonEmpty(vec![syntax::ArraySpecifierDimension::Unsized]),
      }),
    )]),
  };
  let f2 = syntax::StructFieldSpecifier {
    qualifier: None,
    ty: syntax::TypeSpecifier {
      ty: syntax::TypeSpecifierNonArray::TypeName("foo".into()),
      array_specifier: None,
    },
    identifiers: syntax::NonEmpty(vec!["c".into(), "d".into()]),
  };
  let expected: syntax::Declaration = syntax::DeclarationData::Block(syntax::Block {
    qualifier: qual,
    name: "UniformBlockTest".into(),
    fields: vec![f0, f1, f2],
    identifier: None,
  })
  .into();

  assert_ceq!(
    declaration("buffer UniformBlockTest { float a; vec3 b[]; foo c, d; };".into()).unspan(),
    Ok(("", expected.clone()))
  );
  assert_ceq!(declaration("buffer   \nUniformBlockTest\n {\n \t float   a  \n; \nvec3 b   [   ]\n; foo \nc\n, \nd\n;\n }\n\t\n\t\t \t;".into()).unspan(), Ok(("", expected)));
}

#[test]
fn parse_selection_statement_if() {
  let cond = syntax::Expr::Binary(
    syntax::BinaryOp::LT,
    Box::new(syntax::Expr::Variable("foo".into())),
    Box::new(syntax::Expr::IntConst(10)),
  );
  let ret = Box::new(syntax::Expr::BoolConst(false));
  let st = syntax::Statement::Simple(Box::new(syntax::SimpleStatement::Jump(
    syntax::JumpStatement::Return(Some(ret)),
  )));
  let body = syntax::Statement::Compound(Box::new(
    syntax::CompoundStatementData {
      statement_list: vec![st],
    }
    .into(),
  ));
  let rest = syntax::SelectionRestStatement::Statement(Box::new(body));
  let expected = syntax::SelectionStatement {
    cond: Box::new(cond),
    rest,
  };

  assert_ceq!(
    selection_statement("if (foo < 10) { return false; }K".into()).unspan(),
    Ok(("K", expected.clone()))
  );
  assert_ceq!(
    selection_statement("if \n(foo<10\n) \t{return false;}K".into()).unspan(),
    Ok(("K", expected))
  );
}

#[test]
fn parse_selection_statement_if_else() {
  let cond = syntax::Expr::Binary(
    syntax::BinaryOp::LT,
    Box::new(syntax::Expr::Variable("foo".into())),
    Box::new(syntax::Expr::IntConst(10)),
  );
  let if_ret = Box::new(syntax::Expr::FloatConst(0.));
  let if_st = syntax::Statement::Simple(Box::new(syntax::SimpleStatement::Jump(
    syntax::JumpStatement::Return(Some(if_ret)),
  )));
  let if_body = syntax::Statement::Compound(Box::new(
    syntax::CompoundStatementData {
      statement_list: vec![if_st],
    }
    .into(),
  ));
  let else_ret = Box::new(syntax::Expr::Variable("foo".into()));
  let else_st = syntax::Statement::Simple(Box::new(syntax::SimpleStatement::Jump(
    syntax::JumpStatement::Return(Some(else_ret)),
  )));
  let else_body = syntax::Statement::Compound(Box::new(
    syntax::CompoundStatementData {
      statement_list: vec![else_st],
    }
    .into(),
  ));
  let rest = syntax::SelectionRestStatement::Else(Box::new(if_body), Box::new(else_body));
  let expected = syntax::SelectionStatement {
    cond: Box::new(cond),
    rest,
  };

  assert_ceq!(
    selection_statement("if (foo < 10) { return 0.f; } else { return foo; }".into()).unspan(),
    Ok(("", expected.clone()))
  );
  assert_ceq!(
    selection_statement("if \n(foo<10\n) \t{return 0.f\t;\n\n}\n else{\n\t return foo   ;}".into())
      .unspan(),
    Ok(("", expected))
  );
}

#[test]
fn parse_switch_statement_empty() {
  let head = Box::new(syntax::Expr::Variable("foo".into()));
  let expected = syntax::SwitchStatement {
    head,
    body: Vec::new(),
  };

  assert_ceq!(
    switch_statement("switch (foo) {}".into()).unspan(),
    Ok(("", expected.clone()))
  );
  assert_ceq!(
    switch_statement("switch(foo){}".into()).unspan(),
    Ok(("", expected.clone()))
  );
  assert_ceq!(
    switch_statement("switch\n\n (  foo  \t   \n) { \n\n   }".into()).unspan(),
    Ok(("", expected))
  );
}

#[test]
fn parse_switch_statement_cases() {
  let head = Box::new(syntax::Expr::Variable("foo".into()));
  let case0 = syntax::Statement::Simple(Box::new(syntax::SimpleStatement::CaseLabel(
    syntax::CaseLabel::Case(Box::new(syntax::Expr::IntConst(0))),
  )));
  let case1 = syntax::Statement::Simple(Box::new(syntax::SimpleStatement::CaseLabel(
    syntax::CaseLabel::Case(Box::new(syntax::Expr::IntConst(1))),
  )));
  let ret = syntax::Statement::Simple(Box::new(syntax::SimpleStatement::Jump(
    syntax::JumpStatement::Return(Some(Box::new(syntax::Expr::UIntConst(12)))),
  )));
  let expected = syntax::SwitchStatement {
    head,
    body: vec![case0, case1, ret],
  };

  assert_ceq!(
    switch_statement("switch (foo) { case 0: case 1: return 12u; }".into()).unspan(),
    Ok(("", expected.clone()))
  );
}

#[test]
fn parse_case_label_def() {
  assert_ceq!(
    case_label("default:".into()).unspan(),
    Ok(("", syntax::CaseLabel::Def))
  );
  assert_ceq!(
    case_label("default   :".into()).unspan(),
    Ok(("", syntax::CaseLabel::Def))
  );
}

#[test]
fn parse_case_label() {
  let expected = syntax::CaseLabel::Case(Box::new(syntax::Expr::IntConst(3)));

  assert_ceq!(
    case_label("case 3:".into()).unspan(),
    Ok(("", expected.clone()))
  );
  assert_ceq!(
    case_label("case\n\t 3   :".into()).unspan(),
    Ok(("", expected))
  );
}

#[test]
fn parse_iteration_statement_while_empty() {
  let cond = syntax::Condition::Expr(Box::new(syntax::Expr::Binary(
    syntax::BinaryOp::GTE,
    Box::new(syntax::Expr::Variable("a".into())),
    Box::new(syntax::Expr::Variable("b".into())),
  )));
  let st = syntax::Statement::Compound(Box::new(
    syntax::CompoundStatementData {
      statement_list: Vec::new(),
    }
    .into(),
  ));
  let expected = syntax::IterationStatement::While(cond, Box::new(st));

  assert_ceq!(
    iteration_statement("while (a >= b) {}".into()).unspan(),
    Ok(("", expected.clone()))
  );
  assert_ceq!(
    iteration_statement("while(a>=b){}".into()).unspan(),
    Ok(("", expected.clone()))
  );
  assert_ceq!(
    iteration_statement("while (  a >=\n\tb  )\t  {   \n}".into()).unspan(),
    Ok(("", expected))
  );
}

#[test]
fn parse_iteration_statement_do_while_empty() {
  let st = syntax::Statement::Compound(Box::new(
    syntax::CompoundStatementData {
      statement_list: Vec::new(),
    }
    .into(),
  ));
  let cond = Box::new(syntax::Expr::Binary(
    syntax::BinaryOp::GTE,
    Box::new(syntax::Expr::Variable("a".into())),
    Box::new(syntax::Expr::Variable("b".into())),
  ));
  let expected = syntax::IterationStatement::DoWhile(Box::new(st), cond);

  assert_ceq!(
    iteration_statement("do {} while (a >= b);".into()).unspan(),
    Ok(("", expected.clone()))
  );
  assert_ceq!(
    iteration_statement("do{}while(a>=b);".into()).unspan(),
    Ok(("", expected.clone()))
  );
  assert_ceq!(
    iteration_statement("do \n {\n} while (  a >=\n\tb  )\t  \n;".into()).unspan(),
    Ok(("", expected))
  );
}

#[test]
fn parse_iteration_statement_for_empty() {
  let init = syntax::ForInitStatement::Declaration(Box::new(
    syntax::DeclarationData::InitDeclaratorList(syntax::InitDeclaratorList {
      head: syntax::SingleDeclaration {
        ty: syntax::FullySpecifiedType {
          qualifier: None,
          ty: syntax::TypeSpecifier {
            ty: syntax::TypeSpecifierNonArray::Float,
            array_specifier: None,
          },
        },
        name: Some("i".into()),
        array_specifier: None,
        initializer: Some(syntax::Initializer::Simple(Box::new(
          syntax::Expr::FloatConst(0.),
        ))),
      },
      tail: Vec::new(),
    })
    .into(),
  ));
  let rest = syntax::ForRestStatement {
    condition: Some(syntax::Condition::Expr(Box::new(syntax::Expr::Binary(
      syntax::BinaryOp::LTE,
      Box::new(syntax::Expr::Variable("i".into())),
      Box::new(syntax::Expr::FloatConst(10.)),
    )))),
    post_expr: Some(Box::new(syntax::Expr::Unary(
      syntax::UnaryOp::Inc,
      Box::new(syntax::Expr::Variable("i".into())),
    ))),
  };
  let st = syntax::Statement::Compound(Box::new(
    syntax::CompoundStatementData {
      statement_list: Vec::new(),
    }
    .into(),
  ));
  let expected = syntax::IterationStatement::For(init, rest, Box::new(st));

  assert_ceq!(
    iteration_statement("for (float i = 0.f; i <= 10.f; ++i) {}".into()).unspan(),
    Ok(("", expected.clone()))
  );
  assert_ceq!(
    iteration_statement("for(float i=0.f;i<=10.f;++i){}".into()).unspan(),
    Ok(("", expected.clone()))
  );
  assert_ceq!(
    iteration_statement(
      "for\n\t (  \t\n\nfloat \ni \t=\n0.f\n;\ni\t<=  10.f; \n++i\n)\n{\n}".into()
    )
    .unspan(),
    Ok(("", expected))
  );
}

#[test]
fn parse_jump_continue() {
  assert_ceq!(
    jump_statement("continue;".into()).unspan(),
    Ok(("", syntax::JumpStatement::Continue))
  );
}

#[test]
fn parse_jump_break() {
  assert_ceq!(
    jump_statement("break;".into()).unspan(),
    Ok(("", syntax::JumpStatement::Break))
  );
}

#[test]
fn parse_jump_return() {
  let expected = syntax::JumpStatement::Return(Some(Box::new(syntax::Expr::IntConst(3))));
  assert_ceq!(
    jump_statement("return 3;".into()).unspan(),
    Ok(("", expected))
  );
}

#[test]
fn parse_jump_empty_return() {
  let expected = syntax::SimpleStatement::Jump(syntax::JumpStatement::Return(None));
  assert_ceq!(
    simple_statement("return;".into()).unspan(),
    Ok(("", expected))
  );
}

#[test]
fn parse_jump_discard() {
  assert_ceq!(
    jump_statement("discard;".into()).unspan(),
    Ok(("", syntax::JumpStatement::Discard))
  );
}

#[test]
fn parse_simple_statement_return() {
  let e = syntax::Expr::BoolConst(false);
  let expected = syntax::SimpleStatement::Jump(syntax::JumpStatement::Return(Some(Box::new(e))));

  assert_ceq!(
    simple_statement("return false;".into()).unspan(),
    Ok(("", expected))
  );
}

#[test]
fn parse_compound_statement_empty() {
  let expected = syntax::CompoundStatementData {
    statement_list: Vec::new(),
  }
  .into();

  assert_ceq!(compound_statement("{}".into()).unspan(), Ok(("", expected)));
}

#[test]
fn parse_compound_statement() {
  let st0 = syntax::Statement::Simple(Box::new(syntax::SimpleStatement::Selection(
    syntax::SelectionStatement {
      cond: Box::new(syntax::Expr::BoolConst(true)),
      rest: syntax::SelectionRestStatement::Statement(Box::new(syntax::Statement::Compound(
        Box::new(
          syntax::CompoundStatementData {
            statement_list: Vec::new(),
          }
          .into(),
        ),
      ))),
    },
  )));
  let st1 = syntax::Statement::Simple(Box::new(syntax::SimpleStatement::Declaration(
    syntax::DeclarationData::InitDeclaratorList(syntax::InitDeclaratorList {
      head: syntax::SingleDeclaration {
        ty: syntax::FullySpecifiedType {
          qualifier: None,
          ty: syntax::TypeSpecifier {
            ty: syntax::TypeSpecifierNonArray::ISampler3D,
            array_specifier: None,
          },
        },
        name: Some("x".into()),
        array_specifier: None,
        initializer: None,
      },
      tail: Vec::new(),
    })
    .into(),
  )));
  let st2 = syntax::Statement::Simple(Box::new(syntax::SimpleStatement::Jump(
    syntax::JumpStatement::Return(Some(Box::new(syntax::Expr::IntConst(42)))),
  )));
  let expected: syntax::CompoundStatement = syntax::CompoundStatementData {
    statement_list: vec![st0, st1, st2],
  }
  .into();

  assert_ceq!(
    compound_statement("{ if (true) {} isampler3D x; return 42 ; }".into()).unspan(),
    Ok(("", expected.clone()))
  );
  assert_ceq!(
    compound_statement("{if(true){}isampler3D x;return 42;}".into()).unspan(),
    Ok(("", expected))
  );
}

#[test]
fn parse_function_definition() {
  let rt = syntax::FullySpecifiedType {
    qualifier: None,
    ty: syntax::TypeSpecifier {
      ty: syntax::TypeSpecifierNonArray::IImage2DArray,
      array_specifier: None,
    },
  };
  let fp = syntax::FunctionPrototypeData {
    ty: rt,
    name: "foo".into(),
    parameters: Vec::new(),
  }
  .into();
  let st0 = syntax::Statement::Simple(Box::new(syntax::SimpleStatement::Jump(
    syntax::JumpStatement::Return(Some(Box::new(syntax::Expr::Variable("bar".into())))),
  )));
  let expected: syntax::FunctionDefinition = syntax::FunctionDefinitionData {
    prototype: fp,
    statement: syntax::CompoundStatementData {
      statement_list: vec![st0],
    }
    .into(),
  }
  .into();

  assert_ceq!(
    function_definition("iimage2DArray foo() { return bar; }".into()).unspan(),
    Ok(("", expected.clone())),
  );
  assert_ceq!(
    function_definition("iimage2DArray \tfoo\n()\n \n{\n return \nbar\n;}".into()).unspan(),
    Ok(("", expected.clone()))
  );
  assert_ceq!(
    function_definition("iimage2DArray foo(){return bar;}".into()).unspan(),
    Ok(("", expected))
  );
}

#[test]
fn parse_buffer_block_0() {
  let src = include_str!("../data/tests/buffer_block_0.glsl");
  let main_fn = syntax::Node {
    contents: syntax::ExternalDeclarationData::FunctionDefinition(
      syntax::FunctionDefinitionData {
        prototype: syntax::FunctionPrototypeData {
          ty: syntax::FullySpecifiedType {
            qualifier: None,
            ty: syntax::TypeSpecifier {
              ty: syntax::TypeSpecifierNonArray::Void,
              array_specifier: None,
            },
          },
          name: "main".into(),
          parameters: Vec::new(),
        }
        .into(),
        statement: syntax::CompoundStatementData {
          statement_list: Vec::new(),
        }
        .into(),
      }
      .into(),
    ),
    span: None,
  };

  let buffer_block = syntax::Node {
    contents: syntax::ExternalDeclarationData::Declaration(
      syntax::DeclarationData::Block(syntax::Block {
        qualifier: syntax::TypeQualifier {
          qualifiers: syntax::NonEmpty(vec![syntax::TypeQualifierSpec::Storage(
            syntax::StorageQualifier::Buffer,
          )]),
        },
        name: "Foo".into(),
        fields: vec![syntax::StructFieldSpecifier {
          qualifier: None,
          ty: syntax::TypeSpecifier {
            ty: syntax::TypeSpecifierNonArray::TypeName("char".into()),
            array_specifier: None,
          },
          identifiers: syntax::NonEmpty(vec![syntax::ArrayedIdentifier::new(
            "tiles",
            Some(syntax::ArraySpecifier {
              dimensions: syntax::NonEmpty(vec![syntax::ArraySpecifierDimension::Unsized]),
            }),
          )]),
        }],
        identifier: Some("main_tiles".into()),
      })
      .into(),
    ),
    span: None,
  };

  let expected = syntax::TranslationUnit(syntax::NonEmpty(vec![buffer_block, main_fn]));

  assert_ceq!(translation_unit(src.into()).unspan(), Ok(("", expected)));
}

#[test]
fn parse_layout_buffer_block_0() {
  let src = include_str!("../data/tests/layout_buffer_block_0.glsl");
  let layout = syntax::LayoutQualifier {
    ids: syntax::NonEmpty(vec![
      syntax::LayoutQualifierSpec::Identifier(
        "set".into(),
        Some(Box::new(syntax::Expr::IntConst(0))),
      ),
      syntax::LayoutQualifierSpec::Identifier(
        "binding".into(),
        Some(Box::new(syntax::Expr::IntConst(0))),
      ),
    ]),
  };
  let type_qual = syntax::TypeQualifier {
    qualifiers: syntax::NonEmpty(vec![
      syntax::TypeQualifierSpec::Layout(layout),
      syntax::TypeQualifierSpec::Storage(syntax::StorageQualifier::Buffer),
    ]),
  };
  let block = syntax::Node {
    contents: syntax::ExternalDeclarationData::Declaration(
      syntax::DeclarationData::Block(syntax::Block {
        qualifier: type_qual,
        name: "Foo".into(),
        fields: vec![syntax::StructFieldSpecifier {
          qualifier: None,
          ty: syntax::TypeSpecifier {
            ty: syntax::TypeSpecifierNonArray::TypeName("char".into()),
            array_specifier: None,
          },
          identifiers: syntax::NonEmpty(vec!["a".into()]),
        }],
        identifier: Some("foo".into()),
      })
      .into(),
    ),
    span: None,
  };

  let expected = syntax::TranslationUnit(syntax::NonEmpty(vec![block]));

  assert_ceq!(translation_unit(src.into()).unspan(), Ok(("", expected)));
}

#[test]
fn parse_pp_space0() {
  assert_ceq!(
    pp_space0("   \\\n  ".into()).unspan(),
    Ok(("", "   \\\n  "))
  );
  assert_ceq!(pp_space0("".into()).unspan(), Ok(("", "")));
}

#[test]
fn parse_pp_version_number() {
  assert_ceq!(pp_version_number("450".into()).unspan(), Ok(("", 450)));
}

#[test]
fn parse_pp_version_profile() {
  assert_ceq!(
    pp_version_profile("core".into()).unspan(),
    Ok(("", syntax::PreprocessorVersionProfile::Core))
  );
  assert_ceq!(
    pp_version_profile("compatibility".into()).unspan(),
    Ok(("", syntax::PreprocessorVersionProfile::Compatibility))
  );
  assert_ceq!(
    pp_version_profile("es".into()).unspan(),
    Ok(("", syntax::PreprocessorVersionProfile::ES))
  );
}

#[test]
fn parse_pp_version() {
  assert_ceq!(
    preprocessor("#version 450\n".into()).unspan(),
    Ok((
      "",
      syntax::PreprocessorData::Version(syntax::PreprocessorVersion {
        version: 450,
        profile: None,
      })
      .into()
    ))
  );

  assert_ceq!(
    preprocessor("#version 450 core\n".into()).unspan(),
    Ok((
      "",
      syntax::PreprocessorData::Version(syntax::PreprocessorVersion {
        version: 450,
        profile: Some(syntax::PreprocessorVersionProfile::Core)
      })
      .into()
    ))
  );
}

#[test]
fn parse_pp_version_newline() {
  assert_ceq!(
    preprocessor("#version 450\n".into()).unspan(),
    Ok((
      "",
      syntax::PreprocessorData::Version(syntax::PreprocessorVersion {
        version: 450,
        profile: None,
      })
      .into()
    ))
  );

  assert_ceq!(
    preprocessor("#version 450 core\n".into()).unspan(),
    Ok((
      "",
      syntax::PreprocessorData::Version(syntax::PreprocessorVersion {
        version: 450,
        profile: Some(syntax::PreprocessorVersionProfile::Core)
      })
      .into()
    ))
  );
}

#[test]
fn parse_pp_define() {
  let expect = |v: &str| {
    Ok((
      "",
      syntax::PreprocessorData::Define(syntax::PreprocessorDefine::ObjectLike {
        ident: "test".into(),
        value: v.to_owned(),
      })
      .into(),
    ))
  };

  assert_ceq!(
    preprocessor("#define test 1.0".into()).unspan(),
    expect("1.0")
  );
  assert_ceq!(
    preprocessor("#define test \\\n   1.0".into()).unspan(),
    expect("1.0")
  );
  assert_ceq!(
    preprocessor("#define test 1.0\n".into()).unspan(),
    expect("1.0")
  );

  assert_ceq!(
    preprocessor("#define test123 .0f\n".into()).unspan(),
    Ok((
      "",
      syntax::PreprocessorData::Define(syntax::PreprocessorDefine::ObjectLike {
        ident: "test123".into(),
        value: ".0f".to_owned()
      })
      .into()
    ))
  );

  assert_ceq!(
    preprocessor("#define test 1\n".into()).unspan(),
    Ok((
      "",
      syntax::PreprocessorData::Define(syntax::PreprocessorDefine::ObjectLike {
        ident: "test".into(),
        value: "1".to_owned()
      })
      .into()
    ))
  );
}

#[test]
fn parse_pp_define_with_args() {
  let expected: syntax::Preprocessor =
    syntax::PreprocessorData::Define(syntax::PreprocessorDefine::FunctionLike {
      ident: "add".into(),
      args: vec![
        syntax::IdentifierData::new("x").unwrap().into(),
        syntax::IdentifierData::new("y").unwrap().into(),
      ],
      value: "(x + y)".to_owned(),
    })
    .into();

  assert_ceq!(
    preprocessor("#define \\\n add(x, y) \\\n (x + y)".into()).unspan(),
    Ok(("", expected.clone()))
  );

  assert_ceq!(
    preprocessor("#define \\\n add(  x, y  ) \\\n (x + y)".into()).unspan(),
    Ok(("", expected))
  );
}

#[test]
fn parse_pp_define_multiline() {
  assert_ceq!(
    preprocessor(
      r#"#define foo \
       32"#
        .into()
    )
    .unspan(),
    Ok((
      "",
      syntax::PreprocessorData::Define(syntax::PreprocessorDefine::ObjectLike {
        ident: "foo".into(),
        value: "32".to_owned(),
      })
      .into()
    ))
  );
}

#[test]
fn parse_pp_else() {
  assert_ceq!(
    preprocessor("#    else\n".into()).unspan(),
    Ok(("", syntax::PreprocessorData::Else.into()))
  );
}

#[test]
fn parse_pp_elseif() {
  assert_ceq!(
    preprocessor("#   elseif \\\n42\n".into()).unspan(),
    Ok((
      "",
      syntax::PreprocessorData::ElseIf(syntax::PreprocessorElseIf {
        condition: "42".to_owned()
      })
      .into()
    ))
  );
}

#[test]
fn parse_pp_endif() {
  assert_ceq!(
    preprocessor("#\\\nendif".into()).unspan(),
    Ok(("", syntax::PreprocessorData::EndIf.into()))
  );
}

#[test]
fn parse_pp_error() {
  assert_ceq!(
    preprocessor("#error \\\n     some message".into()).unspan(),
    Ok((
      "",
      syntax::PreprocessorData::Error(syntax::PreprocessorError {
        message: "some message".to_owned()
      })
      .into()
    ))
  );
}

#[test]
fn parse_pp_if() {
  assert_ceq!(
    preprocessor("# \\\nif 42".into()).unspan(),
    Ok((
      "",
      syntax::PreprocessorData::If(syntax::PreprocessorIf {
        condition: "42".to_owned()
      })
      .into()
    ))
  );
}

#[test]
fn parse_pp_ifdef() {
  assert_ceq!(
    preprocessor("#ifdef       FOO\n".into()).unspan(),
    Ok((
      "",
      syntax::PreprocessorData::IfDef(syntax::PreprocessorIfDef {
        ident: syntax::IdentifierData("FOO".to_owned()).into()
      })
      .into()
    ))
  );
}

#[test]
fn parse_pp_ifndef() {
  assert_ceq!(
    preprocessor("#\\\nifndef \\\n   FOO\n".into()).unspan(),
    Ok((
      "",
      syntax::PreprocessorData::IfNDef(syntax::PreprocessorIfNDef {
        ident: syntax::IdentifierData("FOO".to_owned()).into()
      })
      .into()
    ))
  );
}

#[test]
fn parse_pp_include() {
  assert_ceq!(
    preprocessor("#include <filename>\n".into()).unspan(),
    Ok((
      "",
      syntax::PreprocessorData::Include(syntax::PreprocessorInclude {
        path: syntax::Path::Absolute("filename".to_owned())
      })
      .into()
    ))
  );

  assert_ceq!(
    preprocessor("#include \\\n\"filename\"\n".into()).unspan(),
    Ok((
      "",
      syntax::PreprocessorData::Include(syntax::PreprocessorInclude {
        path: syntax::Path::Relative("filename".to_owned())
      })
      .into()
    ))
  );
}

#[test]
fn parse_pp_line() {
  assert_ceq!(
    preprocessor("#   line \\\n2\n".into()).unspan(),
    Ok((
      "",
      syntax::PreprocessorData::Line(syntax::PreprocessorLine {
        line: 2,
        source_string_number: None,
      })
      .into()
    ))
  );

  assert_ceq!(
    preprocessor("#line 2 \\\n 4\n".into()).unspan(),
    Ok((
      "",
      syntax::PreprocessorData::Line(syntax::PreprocessorLine {
        line: 2,
        source_string_number: Some(4),
      })
      .into()
    ))
  );
}

#[test]
fn parse_pp_pragma() {
  assert_ceq!(
    preprocessor("#\\\npragma  some   flag".into()).unspan(),
    Ok((
      "",
      syntax::PreprocessorData::Pragma(syntax::PreprocessorPragma {
        command: "some   flag".to_owned()
      })
      .into()
    ))
  );
}

#[test]
fn parse_pp_undef() {
  assert_ceq!(
    preprocessor("# undef \\\n FOO".into()).unspan(),
    Ok((
      "",
      syntax::PreprocessorData::Undef(syntax::PreprocessorUndef {
        name: syntax::IdentifierData("FOO".to_owned()).into()
      })
      .into()
    ))
  );
}

#[test]
fn parse_pp_extension_name() {
  assert_ceq!(
    pp_extension_name("all".into()).unspan(),
    Ok(("", syntax::PreprocessorExtensionName::All))
  );
  assert_ceq!(
    pp_extension_name("GL_foobar_extension ".into()).unspan(),
    Ok((
      " ",
      syntax::PreprocessorExtensionName::Specific("GL_foobar_extension".to_owned())
    ))
  );
}

#[test]
fn parse_pp_extension_behavior() {
  assert_ceq!(
    pp_extension_behavior("require".into()).unspan(),
    Ok(("", syntax::PreprocessorExtensionBehavior::Require))
  );
  assert_ceq!(
    pp_extension_behavior("enable".into()).unspan(),
    Ok(("", syntax::PreprocessorExtensionBehavior::Enable))
  );
  assert_ceq!(
    pp_extension_behavior("warn".into()).unspan(),
    Ok(("", syntax::PreprocessorExtensionBehavior::Warn))
  );
  assert_ceq!(
    pp_extension_behavior("disable".into()).unspan(),
    Ok(("", syntax::PreprocessorExtensionBehavior::Disable))
  );
}

#[test]
fn parse_pp_extension() {
  assert_ceq!(
    preprocessor("#extension all: require\n".into()).unspan(),
    Ok((
      "",
      syntax::PreprocessorData::Extension(syntax::PreprocessorExtension {
        name: syntax::PreprocessorExtensionName::All,
        behavior: Some(syntax::PreprocessorExtensionBehavior::Require)
      })
      .into()
    ))
  );
}

#[test]
fn parse_dot_field_expr_array() {
  let src = "a[0].xyz;";
  let expected = syntax::Expr::Dot(
    Box::new(syntax::Expr::Bracket(
      Box::new(syntax::Expr::Variable("a".into())),
      syntax::ArraySpecifier {
        dimensions: syntax::NonEmpty(vec![syntax::ArraySpecifierDimension::ExplicitlySized(
          Box::new(syntax::Expr::IntConst(0)),
        )]),
      },
    )),
    "xyz".into(),
  );

  assert_ceq!(expr(src.into()).unspan(), Ok((";", expected)));
}

#[test]
fn parse_dot_field_expr_statement() {
  let src = "vec3 v = smoothstep(vec3(border_width), vec3(0.0), v_barycenter).zyx;";
  let fun = syntax::FunIdentifier::Identifier("smoothstep".into());
  let args = vec![
    syntax::Expr::FunCall(
      syntax::FunIdentifier::Identifier("vec3".into()),
      vec![syntax::Expr::Variable("border_width".into())],
    ),
    syntax::Expr::FunCall(
      syntax::FunIdentifier::Identifier("vec3".into()),
      vec![syntax::Expr::FloatConst(0.)],
    ),
    syntax::Expr::Variable("v_barycenter".into()),
  ];
  let ini = syntax::Initializer::Simple(Box::new(syntax::Expr::Dot(
    Box::new(syntax::Expr::FunCall(fun, args)),
    "zyx".into(),
  )));
  let sd = syntax::SingleDeclaration {
    ty: syntax::FullySpecifiedType {
      qualifier: None,
      ty: syntax::TypeSpecifier {
        ty: syntax::TypeSpecifierNonArray::Vec3,
        array_specifier: None,
      },
    },
    name: Some("v".into()),
    array_specifier: None,
    initializer: Some(ini),
  };
  let expected = syntax::Statement::Simple(Box::new(syntax::SimpleStatement::Declaration(
    syntax::DeclarationData::InitDeclaratorList(syntax::InitDeclaratorList {
      head: sd,
      tail: Vec::new(),
    })
    .into(),
  )));

  assert_ceq!(statement(src.into()).unspan(), Ok(("", expected)));
}

#[test]
fn parse_arrayed_identifier() {
  let expected = syntax::ArrayedIdentifier::new(
    "foo",
    syntax::ArraySpecifier {
      dimensions: syntax::NonEmpty(vec![syntax::ArraySpecifierDimension::Unsized]),
    },
  );

  assert_ceq!(
    arrayed_identifier("foo[]".into()).unspan(),
    Ok(("", expected.clone()))
  );
  assert_ceq!(
    arrayed_identifier("foo \t\n  [\n\t ]".into()).unspan(),
    Ok(("", expected))
  );
}

#[test]
fn parse_nested_parens() {
  let start = std::time::Instant::now();
  parens_expr("((((((((1.0f))))))))".into()).unwrap();
  let elapsed = start.elapsed();
  assert!(elapsed.as_millis() < 100, "{} ms", elapsed.as_millis());
}
