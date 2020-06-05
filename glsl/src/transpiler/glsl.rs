//! A GLSL450/GLSL460 transpiler that takes a syntax tree and writes it as a plain raw GLSL
//! [`String`].
//!
//! # Foreword
//!
//! This module exports several functions that just transform a part of a syntax tree into its raw
//! GLSL [`String`] representation.
//!
//! > Important note: this module – and actually, any [`transpiler`] module – is not responsible in
//! > optimizing the syntax tree nor semantically check its validity. This is done in other stages
//! > of the compilation process.
//!
//! In order to achieve that purpose, you could:
//!
//! - For each elements in the AST, return a [`String`] or [`Cow<str>`].
//! - Insert the string representation via a formatter.
//!
//! The second solution is better because it lets the user handle the memory the way they want:
//! they might just use a dynamic buffer that implements [`Write`] or simply pass a `&mut`
//! [`String`]. It’s up to you.
//!
//! # How to use this module
//!
//! First, head over to the [`syntax`] module. That module defines the AST items defined by GLSL. This
//! very module provides you with functions like `show_*` taking the AST item and writing it to a
//! [`Write`] object. You’re likely to be interested in [`show_translation_unit`] to start with.
//!
//! [`Cow<str>`]: std::borrow::Cow
//! [`Write`]: std::fmt::Write
//! [`show_translation_unit`]: crate::transpiler::glsl::show_translation_unit
//! [`syntax`]: crate::syntax
//! [`transpiler`]: crate::transpiler

use std::fmt::Write;

use lazy_static::lazy_static;

/// Indentation style of the output
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum IndentStyle {
  /// No indentation is generated
  None,
  /// Items are indented with tabs. In case spaces are needed for alignment,
  /// tabs are assumed to be `tab_size` characters wide.
  Tabs {
    /// Size of the tabs in characters
    tab_size: u32,
    /// Number of tab characters used per indent level
    count: u32,
  },
  /// Items are indented with spaces.
  Spaces {
    /// Number of space characters used per indent level
    count: u32,
  },
}

impl IndentStyle {
  pub fn write<F>(&self, f: &mut F, levels: u32) -> std::fmt::Result
  where
    F: Write,
  {
    match self {
      Self::None => {}
      Self::Tabs { count, .. } => {
        for _ in 0..count * levels {
          f.write_char('\t')?;
        }
      }
      Self::Spaces { count, .. } => {
        for _ in 0..count * levels {
          f.write_char(' ')?;
        }
      }
    }

    Ok(())
  }
}

impl Default for IndentStyle {
  fn default() -> Self {
    Self::Spaces { count: 4 }
  }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Whitespace {
  None,
  Space,
  Newline,
}

impl Whitespace {
  pub fn write<F>(&self, f: &mut F, s: &mut FormattingState) -> std::fmt::Result
  where
    F: Write,
  {
    match self {
      Self::None => Ok(()),
      Self::Space => f.write_char(' '),
      Self::Newline => s.new_line(true),
    }
  }
}

/// Formatting settings for the GLSL transpiler
#[derive(Debug, Clone, PartialEq)]
pub struct FormattingSettings {
  /// Indentation style of the output
  pub indent_style: IndentStyle,
  /// Insert newlines after block open braces
  pub newline_after_open_block: bool,
  /// Insert newlines before block close braces
  pub newline_before_close_block: bool,
  /// Insert newline after block close brace
  pub newline_after_close_block: bool,
  /// What to insert between fields of a struct
  pub struct_field_separator: Whitespace,
  /// What to insert after a struct declaration
  pub struct_declaration_terminator: Whitespace,
  /// What to insert after a declaration
  pub declaration_terminator: Whitespace,
  /// Insert spaces around binary ops
  pub spaces_around_binary_ops: bool,
  /// What to insert after a statement
  pub statement_terminator: Whitespace,
  /// What to insert after a function definition
  pub function_definition_terminator: Whitespace,
}

impl Default for FormattingSettings {
  fn default() -> Self {
    Self {
      indent_style: IndentStyle::default(),
      newline_after_open_block: true,
      newline_before_close_block: true,
      newline_after_close_block: true,
      struct_field_separator: Whitespace::Newline,
      struct_declaration_terminator: Whitespace::Newline,
      declaration_terminator: Whitespace::Newline,
      spaces_around_binary_ops: true,
      statement_terminator: Whitespace::Newline,
      function_definition_terminator: Whitespace::Newline,
    }
  }
}

/// Formatting state of the GLSL transpiler
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct FormattingState<'s> {
  pub settings: &'s FormattingSettings,
  indentation_level: u32,
  new_line_pending: bool,
}

impl<'s> FormattingState<'s> {
  fn write_indent<F>(&self, f: &mut F) -> std::fmt::Result
  where
    F: Write,
  {
    self.settings.indent_style.write(f, self.indentation_level)
  }

  fn write_line<F>(&mut self, f: &mut F) -> std::fmt::Result
  where
    F: Write,
  {
    f.write_char('\n')?;
    self.write_indent(f)
  }

  pub fn new_line(&mut self, required: bool) -> std::fmt::Result {
    if required {
      self.new_line_pending = true;
    }

    Ok(())
  }

  pub fn consume_newline(&mut self) {
    self.new_line_pending = false;
  }

  pub fn flush_line<F>(&mut self, f: &mut F) -> std::fmt::Result
  where
    F: Write,
  {
    if self.new_line_pending {
      self.write_line(f)?;
      self.new_line_pending = false;
    }

    Ok(())
  }

  pub fn flush_space<F>(&mut self, f: &mut F) -> std::fmt::Result
  where
    F: Write,
  {
    if self.new_line_pending {
      f.write_char(' ')?;
      self.new_line_pending = false;
    }

    Ok(())
  }

  pub fn enter_block<F>(&mut self, f: &mut F) -> std::fmt::Result
  where
    F: Write,
  {
    // Open block
    f.write_char('{')?;

    // Write line with new indentation level
    self.indentation_level += 1;
    self.new_line(self.settings.newline_after_open_block)?;

    Ok(())
  }

  pub fn exit_block<F>(&mut self, f: &mut F) -> std::fmt::Result
  where
    F: Write,
  {
    // Update indentation level
    self.indentation_level -= 1;

    // Next line
    self.new_line(self.settings.newline_before_close_block)?;

    // Flush lines
    self.flush_line(f)?;

    // Close block
    f.write_char('}')?;

    // Next line
    self.new_line(self.settings.newline_after_close_block)?;

    Ok(())
  }

  pub fn write_struct_field_separator<F>(&mut self, f: &mut F) -> std::fmt::Result
  where
    F: Write,
  {
    f.write_char(';')?;
    self.settings.struct_field_separator.write(f, self)
  }

  pub fn write_struct_declaration_terminator<F>(&mut self, f: &mut F) -> std::fmt::Result
  where
    F: Write,
  {
    f.write_char(';')?;
    self.settings.struct_declaration_terminator.write(f, self)
  }

  pub fn write_declaration_terminator<F>(&mut self, f: &mut F) -> std::fmt::Result
  where
    F: Write,
  {
    f.write_char(';')?;
    self.settings.declaration_terminator.write(f, self)
  }

  pub fn write_binary_op<F>(&self, f: &mut F, op: &str) -> std::fmt::Result
  where
    F: Write,
  {
    if self.settings.spaces_around_binary_ops {
      f.write_char(' ')?;
    }

    f.write_str(op)?;

    if self.settings.spaces_around_binary_ops {
      f.write_char(' ')?;
    }

    Ok(())
  }

  pub fn write_statement_terminator<F>(&mut self, f: &mut F) -> std::fmt::Result
  where
    F: Write,
  {
    f.write_char(';')?;
    self.settings.statement_terminator.write(f, self)
  }

  pub fn write_function_definition_terminator<F>(&mut self, f: &mut F) -> std::fmt::Result
  where
    F: Write,
  {
    self.settings.function_definition_terminator.write(f, self)
  }
}

impl<'s> From<&'s FormattingSettings> for FormattingState<'s> {
  fn from(settings: &'s FormattingSettings) -> Self {
    Self {
      settings,
      indentation_level: 0,
      new_line_pending: false,
    }
  }
}

lazy_static! {
  static ref DEFAULT_SETTINGS: FormattingSettings = FormattingSettings::default();
}

impl Default for FormattingState<'static> {
  fn default() -> Self {
    Self {
      settings: &DEFAULT_SETTINGS,
      indentation_level: 0,
      new_line_pending: false,
    }
  }
}

use crate::syntax;

// Precedence information for transpiling parentheses properly
trait HasPrecedence {
  fn precedence(&self) -> u32;
}

impl HasPrecedence for syntax::Expr {
  fn precedence(&self) -> u32 {
    match self {
      // 0 isn't a valid precedence, but we use this to represent atomic expressions
      Self::Variable(_)
      | Self::IntConst(_)
      | Self::UIntConst(_)
      | Self::BoolConst(_)
      | Self::FloatConst(_)
      | Self::DoubleConst(_) => 0,
      // Precedence operator expression is precedence of operator
      Self::Unary(op, _) => op.precedence(),
      Self::Binary(op, _, _) => op.precedence(),
      Self::Ternary(_, _, _) => 15,
      Self::Assignment(_, op, _) => op.precedence(),
      Self::Bracket(_, _)
      | Self::FunCall(_, _)
      | Self::Dot(_, _)
      | Self::PostInc(_)
      | Self::PostDec(_) => 2,
      Self::Comma(_, _) => 17,
    }
  }
}

impl HasPrecedence for syntax::UnaryOp {
  fn precedence(&self) -> u32 {
    3
  }
}

impl HasPrecedence for syntax::BinaryOp {
  fn precedence(&self) -> u32 {
    match self {
      Self::Mult | Self::Div | Self::Mod => 4,
      Self::Add | Self::Sub => 5,
      Self::LShift | Self::RShift => 6,
      Self::LT | Self::GT | Self::LTE | Self::GTE => 7,
      Self::Equal | Self::NonEqual => 8,
      Self::BitAnd => 9,
      Self::BitXor => 10,
      Self::BitOr => 11,
      Self::And => 12,
      Self::Xor => 13,
      Self::Or => 14,
    }
  }
}

impl HasPrecedence for syntax::AssignmentOp {
  fn precedence(&self) -> u32 {
    16
  }
}

pub fn show_identifier<F>(
  f: &mut F,
  i: &syntax::Identifier,
  _: &mut FormattingState<'_>,
) -> std::fmt::Result
where
  F: Write,
{
  f.write_str(&i.0)
}

pub fn show_type_name<F>(
  f: &mut F,
  t: &syntax::TypeName,
  _: &mut FormattingState<'_>,
) -> std::fmt::Result
where
  F: Write,
{
  f.write_str(&t.0)
}

pub fn show_type_specifier_non_array<F>(
  f: &mut F,
  t: &syntax::TypeSpecifierNonArray,
  s: &mut FormattingState<'_>,
) -> std::fmt::Result
where
  F: Write,
{
  match *t {
    syntax::TypeSpecifierNonArray::Void => f.write_str("void"),
    syntax::TypeSpecifierNonArray::Bool => f.write_str("bool"),
    syntax::TypeSpecifierNonArray::Int => f.write_str("int"),
    syntax::TypeSpecifierNonArray::UInt => f.write_str("uint"),
    syntax::TypeSpecifierNonArray::Float => f.write_str("float"),
    syntax::TypeSpecifierNonArray::Double => f.write_str("double"),
    syntax::TypeSpecifierNonArray::Vec2 => f.write_str("vec2"),
    syntax::TypeSpecifierNonArray::Vec3 => f.write_str("vec3"),
    syntax::TypeSpecifierNonArray::Vec4 => f.write_str("vec4"),
    syntax::TypeSpecifierNonArray::DVec2 => f.write_str("dvec2"),
    syntax::TypeSpecifierNonArray::DVec3 => f.write_str("dvec3"),
    syntax::TypeSpecifierNonArray::DVec4 => f.write_str("dvec4"),
    syntax::TypeSpecifierNonArray::BVec2 => f.write_str("bvec2"),
    syntax::TypeSpecifierNonArray::BVec3 => f.write_str("bvec3"),
    syntax::TypeSpecifierNonArray::BVec4 => f.write_str("bvec4"),
    syntax::TypeSpecifierNonArray::IVec2 => f.write_str("ivec2"),
    syntax::TypeSpecifierNonArray::IVec3 => f.write_str("ivec3"),
    syntax::TypeSpecifierNonArray::IVec4 => f.write_str("ivec4"),
    syntax::TypeSpecifierNonArray::UVec2 => f.write_str("uvec2"),
    syntax::TypeSpecifierNonArray::UVec3 => f.write_str("uvec3"),
    syntax::TypeSpecifierNonArray::UVec4 => f.write_str("uvec4"),
    syntax::TypeSpecifierNonArray::Mat2 => f.write_str("mat2"),
    syntax::TypeSpecifierNonArray::Mat3 => f.write_str("mat3"),
    syntax::TypeSpecifierNonArray::Mat4 => f.write_str("mat4"),
    syntax::TypeSpecifierNonArray::Mat23 => f.write_str("mat23"),
    syntax::TypeSpecifierNonArray::Mat24 => f.write_str("mat24"),
    syntax::TypeSpecifierNonArray::Mat32 => f.write_str("mat32"),
    syntax::TypeSpecifierNonArray::Mat34 => f.write_str("mat34"),
    syntax::TypeSpecifierNonArray::Mat42 => f.write_str("mat42"),
    syntax::TypeSpecifierNonArray::Mat43 => f.write_str("mat43"),
    syntax::TypeSpecifierNonArray::DMat2 => f.write_str("dmat2"),
    syntax::TypeSpecifierNonArray::DMat3 => f.write_str("dmat3"),
    syntax::TypeSpecifierNonArray::DMat4 => f.write_str("dmat4"),
    syntax::TypeSpecifierNonArray::DMat23 => f.write_str("dmat23"),
    syntax::TypeSpecifierNonArray::DMat24 => f.write_str("dmat24"),
    syntax::TypeSpecifierNonArray::DMat32 => f.write_str("dmat32"),
    syntax::TypeSpecifierNonArray::DMat34 => f.write_str("dmat34"),
    syntax::TypeSpecifierNonArray::DMat42 => f.write_str("dmat42"),
    syntax::TypeSpecifierNonArray::DMat43 => f.write_str("dmat43"),
    syntax::TypeSpecifierNonArray::Sampler1D => f.write_str("sampler1D"),
    syntax::TypeSpecifierNonArray::Image1D => f.write_str("image1D"),
    syntax::TypeSpecifierNonArray::Sampler2D => f.write_str("sampler2D"),
    syntax::TypeSpecifierNonArray::Image2D => f.write_str("image2D"),
    syntax::TypeSpecifierNonArray::Sampler3D => f.write_str("sampler3D"),
    syntax::TypeSpecifierNonArray::Image3D => f.write_str("image3D"),
    syntax::TypeSpecifierNonArray::SamplerCube => f.write_str("samplerCube"),
    syntax::TypeSpecifierNonArray::ImageCube => f.write_str("imageCube"),
    syntax::TypeSpecifierNonArray::Sampler2DRect => f.write_str("sampler2DRect"),
    syntax::TypeSpecifierNonArray::Image2DRect => f.write_str("image2DRect"),
    syntax::TypeSpecifierNonArray::Sampler1DArray => f.write_str("sampler1DArray"),
    syntax::TypeSpecifierNonArray::Image1DArray => f.write_str("image1DArray"),
    syntax::TypeSpecifierNonArray::Sampler2DArray => f.write_str("sampler2DArray"),
    syntax::TypeSpecifierNonArray::Image2DArray => f.write_str("image2DArray"),
    syntax::TypeSpecifierNonArray::SamplerBuffer => f.write_str("samplerBuffer"),
    syntax::TypeSpecifierNonArray::ImageBuffer => f.write_str("imageBuffer"),
    syntax::TypeSpecifierNonArray::Sampler2DMS => f.write_str("sampler2DMS"),
    syntax::TypeSpecifierNonArray::Image2DMS => f.write_str("image2DMS"),
    syntax::TypeSpecifierNonArray::Sampler2DMSArray => f.write_str("sampler2DMSArray"),
    syntax::TypeSpecifierNonArray::Image2DMSArray => f.write_str("image2DMSArray"),
    syntax::TypeSpecifierNonArray::SamplerCubeArray => f.write_str("samplerCubeArray"),
    syntax::TypeSpecifierNonArray::ImageCubeArray => f.write_str("imageCubeArray"),
    syntax::TypeSpecifierNonArray::Sampler1DShadow => f.write_str("sampler1DShadow"),
    syntax::TypeSpecifierNonArray::Sampler2DShadow => f.write_str("sampler2DShadow"),
    syntax::TypeSpecifierNonArray::Sampler2DRectShadow => f.write_str("sampler2DRectShadow"),
    syntax::TypeSpecifierNonArray::Sampler1DArrayShadow => f.write_str("sampler1DArrayShadow"),
    syntax::TypeSpecifierNonArray::Sampler2DArrayShadow => f.write_str("sampler2DArrayShadow"),
    syntax::TypeSpecifierNonArray::SamplerCubeShadow => f.write_str("samplerCubeShadow"),
    syntax::TypeSpecifierNonArray::SamplerCubeArrayShadow => f.write_str("samplerCubeArrayShadow"),
    syntax::TypeSpecifierNonArray::ISampler1D => f.write_str("isampler1D"),
    syntax::TypeSpecifierNonArray::IImage1D => f.write_str("iimage1D"),
    syntax::TypeSpecifierNonArray::ISampler2D => f.write_str("isampler2D"),
    syntax::TypeSpecifierNonArray::IImage2D => f.write_str("iimage2D"),
    syntax::TypeSpecifierNonArray::ISampler3D => f.write_str("isampler3D"),
    syntax::TypeSpecifierNonArray::IImage3D => f.write_str("iimage3D"),
    syntax::TypeSpecifierNonArray::ISamplerCube => f.write_str("isamplerCube"),
    syntax::TypeSpecifierNonArray::IImageCube => f.write_str("iimageCube"),
    syntax::TypeSpecifierNonArray::ISampler2DRect => f.write_str("isampler2DRect"),
    syntax::TypeSpecifierNonArray::IImage2DRect => f.write_str("iimage2DRect"),
    syntax::TypeSpecifierNonArray::ISampler1DArray => f.write_str("isampler1DArray"),
    syntax::TypeSpecifierNonArray::IImage1DArray => f.write_str("iimage1DArray"),
    syntax::TypeSpecifierNonArray::ISampler2DArray => f.write_str("isampler2DArray"),
    syntax::TypeSpecifierNonArray::IImage2DArray => f.write_str("iimage2DArray"),
    syntax::TypeSpecifierNonArray::ISamplerBuffer => f.write_str("isamplerBuffer"),
    syntax::TypeSpecifierNonArray::IImageBuffer => f.write_str("iimageBuffer"),
    syntax::TypeSpecifierNonArray::ISampler2DMS => f.write_str("isampler2MS"),
    syntax::TypeSpecifierNonArray::IImage2DMS => f.write_str("iimage2DMS"),
    syntax::TypeSpecifierNonArray::ISampler2DMSArray => f.write_str("isampler2DMSArray"),
    syntax::TypeSpecifierNonArray::IImage2DMSArray => f.write_str("iimage2DMSArray"),
    syntax::TypeSpecifierNonArray::ISamplerCubeArray => f.write_str("isamplerCubeArray"),
    syntax::TypeSpecifierNonArray::IImageCubeArray => f.write_str("iimageCubeArray"),
    syntax::TypeSpecifierNonArray::AtomicUInt => f.write_str("atomic_uint"),
    syntax::TypeSpecifierNonArray::USampler1D => f.write_str("usampler1D"),
    syntax::TypeSpecifierNonArray::UImage1D => f.write_str("uimage1D"),
    syntax::TypeSpecifierNonArray::USampler2D => f.write_str("usampler2D"),
    syntax::TypeSpecifierNonArray::UImage2D => f.write_str("uimage2D"),
    syntax::TypeSpecifierNonArray::USampler3D => f.write_str("usampler3D"),
    syntax::TypeSpecifierNonArray::UImage3D => f.write_str("uimage3D"),
    syntax::TypeSpecifierNonArray::USamplerCube => f.write_str("usamplerCube"),
    syntax::TypeSpecifierNonArray::UImageCube => f.write_str("uimageCube"),
    syntax::TypeSpecifierNonArray::USampler2DRect => f.write_str("usampler2DRect"),
    syntax::TypeSpecifierNonArray::UImage2DRect => f.write_str("uimage2DRect"),
    syntax::TypeSpecifierNonArray::USampler1DArray => f.write_str("usampler1DArray"),
    syntax::TypeSpecifierNonArray::UImage1DArray => f.write_str("uimage1DArray"),
    syntax::TypeSpecifierNonArray::USampler2DArray => f.write_str("usampler2DArray"),
    syntax::TypeSpecifierNonArray::UImage2DArray => f.write_str("uimage2DArray"),
    syntax::TypeSpecifierNonArray::USamplerBuffer => f.write_str("usamplerBuffer"),
    syntax::TypeSpecifierNonArray::UImageBuffer => f.write_str("uimageBuffer"),
    syntax::TypeSpecifierNonArray::USampler2DMS => f.write_str("usampler2DMS"),
    syntax::TypeSpecifierNonArray::UImage2DMS => f.write_str("uimage2DMS"),
    syntax::TypeSpecifierNonArray::USampler2DMSArray => f.write_str("usamplerDMSArray"),
    syntax::TypeSpecifierNonArray::UImage2DMSArray => f.write_str("uimage2DMSArray"),
    syntax::TypeSpecifierNonArray::USamplerCubeArray => f.write_str("usamplerCubeArray"),
    syntax::TypeSpecifierNonArray::UImageCubeArray => f.write_str("uimageCubeArray"),
    syntax::TypeSpecifierNonArray::Struct(ref st) => show_struct_non_declaration(f, st, s),
    syntax::TypeSpecifierNonArray::TypeName(ref tn) => show_type_name(f, tn, s),
  }
}

pub fn show_type_specifier<F>(
  f: &mut F,
  t: &syntax::TypeSpecifier,
  s: &mut FormattingState<'_>,
) -> std::fmt::Result
where
  F: Write,
{
  show_type_specifier_non_array(f, &t.ty, s)?;

  if let Some(ref arr_spec) = t.array_specifier {
    show_array_spec(f, arr_spec, s)?;
  }

  Ok(())
}

pub fn show_fully_specified_type<F>(
  f: &mut F,
  t: &syntax::FullySpecifiedType,
  s: &mut FormattingState<'_>,
) -> std::fmt::Result
where
  F: Write,
{
  if let Some(ref qual) = t.qualifier {
    show_type_qualifier(f, &qual, s)?;
    f.write_str(" ")?;
  }

  show_type_specifier(f, &t.ty, s)
}

pub fn show_struct_non_declaration<F>(
  f: &mut F,
  st: &syntax::StructSpecifier,
  s: &mut FormattingState<'_>,
) -> std::fmt::Result
where
  F: Write,
{
  f.write_str("struct ")?;

  if let Some(ref name) = st.name {
    write!(f, "{} ", name)?;
  }

  s.enter_block(f)?;

  for field in &st.fields.0 {
    s.flush_line(f)?;
    show_struct_field(f, field, s)?;
    s.write_struct_field_separator(f)?;
  }

  s.exit_block(f)?;

  Ok(())
}

pub fn show_struct<F>(
  f: &mut F,
  st: &syntax::StructSpecifier,
  s: &mut FormattingState<'_>,
) -> std::fmt::Result
where
  F: Write,
{
  show_struct_non_declaration(f, st, s)?;
  s.write_struct_declaration_terminator(f)
}

pub fn show_struct_field<F>(
  f: &mut F,
  field: &syntax::StructFieldSpecifier,
  s: &mut FormattingState<'_>,
) -> std::fmt::Result
where
  F: Write,
{
  if let Some(ref qual) = field.qualifier {
    show_type_qualifier(f, &qual, s)?;
    f.write_str(" ")?;
  }

  show_type_specifier(f, &field.ty, s)?;
  f.write_str(" ")?;

  // there’s at least one identifier
  let mut identifiers = field.identifiers.0.iter();
  let identifier = identifiers.next().unwrap();

  show_arrayed_identifier(f, identifier, s)?;

  // write the rest of the identifiers
  for identifier in identifiers {
    f.write_str(", ")?;
    show_arrayed_identifier(f, identifier, s)?;
  }

  Ok(())
}

pub fn show_array_spec<F>(
  f: &mut F,
  a: &syntax::ArraySpecifier,
  s: &mut FormattingState<'_>,
) -> std::fmt::Result
where
  F: Write,
{
  for dimension in &a.dimensions {
    match *dimension {
      syntax::ArraySpecifierDimension::Unsized => f.write_str("[]")?,
      syntax::ArraySpecifierDimension::ExplicitlySized(ref e) => {
        f.write_str("[")?;
        show_expr(f, &e, s)?;
        f.write_str("]")?
      }
    }
  }

  Ok(())
}

pub fn show_arrayed_identifier<F>(
  f: &mut F,
  a: &syntax::ArrayedIdentifier,
  s: &mut FormattingState<'_>,
) -> std::fmt::Result
where
  F: Write,
{
  write!(f, "{}", a.ident)?;

  if let Some(ref arr_spec) = a.array_spec {
    show_array_spec(f, arr_spec, s)?;
  }

  Ok(())
}

pub fn show_type_qualifier<F>(
  f: &mut F,
  q: &syntax::TypeQualifier,
  s: &mut FormattingState<'_>,
) -> std::fmt::Result
where
  F: Write,
{
  let mut qualifiers = q.qualifiers.0.iter();
  let first = qualifiers.next().unwrap();

  show_type_qualifier_spec(f, first, s)?;

  for qual_spec in qualifiers {
    f.write_str(" ")?;
    show_type_qualifier_spec(f, qual_spec, s)?;
  }

  Ok(())
}

pub fn show_type_qualifier_spec<F>(
  f: &mut F,
  q: &syntax::TypeQualifierSpec,
  s: &mut FormattingState<'_>,
) -> std::fmt::Result
where
  F: Write,
{
  match *q {
    syntax::TypeQualifierSpec::Storage(ref st) => show_storage_qualifier(f, &st, s),
    syntax::TypeQualifierSpec::Layout(ref l) => show_layout_qualifier(f, &l, s),
    syntax::TypeQualifierSpec::Precision(ref p) => show_precision_qualifier(f, &p, s),
    syntax::TypeQualifierSpec::Interpolation(ref i) => show_interpolation_qualifier(f, &i, s),
    syntax::TypeQualifierSpec::Invariant => f.write_str("invariant"),
    syntax::TypeQualifierSpec::Precise => f.write_str("precise"),
  }
}

pub fn show_storage_qualifier<F>(
  f: &mut F,
  q: &syntax::StorageQualifier,
  s: &mut FormattingState<'_>,
) -> std::fmt::Result
where
  F: Write,
{
  match *q {
    syntax::StorageQualifier::Const => f.write_str("const"),
    syntax::StorageQualifier::InOut => f.write_str("inout"),
    syntax::StorageQualifier::In => f.write_str("in"),
    syntax::StorageQualifier::Out => f.write_str("out"),
    syntax::StorageQualifier::Centroid => f.write_str("centroid"),
    syntax::StorageQualifier::Patch => f.write_str("patch"),
    syntax::StorageQualifier::Sample => f.write_str("sample"),
    syntax::StorageQualifier::Uniform => f.write_str("uniform"),
    syntax::StorageQualifier::Attribute => f.write_str("attribute"),
    syntax::StorageQualifier::Varying => f.write_str("varying"),
    syntax::StorageQualifier::Buffer => f.write_str("buffer"),
    syntax::StorageQualifier::Shared => f.write_str("shared"),
    syntax::StorageQualifier::Coherent => f.write_str("coherent"),
    syntax::StorageQualifier::Volatile => f.write_str("volatile"),
    syntax::StorageQualifier::Restrict => f.write_str("restrict"),
    syntax::StorageQualifier::ReadOnly => f.write_str("readonly"),
    syntax::StorageQualifier::WriteOnly => f.write_str("writeonly"),
    syntax::StorageQualifier::Subroutine(ref n) => show_subroutine(f, &n, s),
  }
}

pub fn show_subroutine<F>(
  f: &mut F,
  types: &Vec<syntax::TypeName>,
  s: &mut FormattingState<'_>,
) -> std::fmt::Result
where
  F: Write,
{
  f.write_str("subroutine")?;

  if !types.is_empty() {
    f.write_str("(")?;

    let mut types_iter = types.iter();
    let first = types_iter.next().unwrap();

    show_type_name(f, first, s)?;

    for type_name in types_iter {
      f.write_str(", ")?;
      show_type_name(f, type_name, s)?;
    }

    f.write_str(")")?;
  }

  Ok(())
}

pub fn show_layout_qualifier<F>(
  f: &mut F,
  l: &syntax::LayoutQualifier,
  s: &mut FormattingState<'_>,
) -> std::fmt::Result
where
  F: Write,
{
  let mut qualifiers = l.ids.0.iter();
  let first = qualifiers.next().unwrap();

  f.write_str("layout (")?;
  show_layout_qualifier_spec(f, first, s)?;

  for qual_spec in qualifiers {
    f.write_str(", ")?;
    show_layout_qualifier_spec(f, qual_spec, s)?;
  }

  f.write_str(")")
}

pub fn show_layout_qualifier_spec<F>(
  f: &mut F,
  l: &syntax::LayoutQualifierSpec,
  s: &mut FormattingState<'_>,
) -> std::fmt::Result
where
  F: Write,
{
  match *l {
    syntax::LayoutQualifierSpec::Identifier(ref i, Some(ref e)) => {
      write!(f, "{} = ", i)?;
      show_expr(f, &e, s)
    }
    syntax::LayoutQualifierSpec::Identifier(ref i, None) => show_identifier(f, &i, s),
    syntax::LayoutQualifierSpec::Shared => f.write_str("shared"),
  }
}

pub fn show_precision_qualifier<F>(
  f: &mut F,
  p: &syntax::PrecisionQualifier,
  _: &mut FormattingState<'_>,
) -> std::fmt::Result
where
  F: Write,
{
  match *p {
    syntax::PrecisionQualifier::High => f.write_str("highp"),
    syntax::PrecisionQualifier::Medium => f.write_str("mediump"),
    syntax::PrecisionQualifier::Low => f.write_str("low"),
  }
}

pub fn show_interpolation_qualifier<F>(
  f: &mut F,
  i: &syntax::InterpolationQualifier,
  _: &mut FormattingState<'_>,
) -> std::fmt::Result
where
  F: Write,
{
  match *i {
    syntax::InterpolationQualifier::Smooth => f.write_str("smooth"),
    syntax::InterpolationQualifier::Flat => f.write_str("flat"),
    syntax::InterpolationQualifier::NoPerspective => f.write_str("noperspective"),
  }
}

pub fn show_float<F>(f: &mut F, x: f32, _: &mut FormattingState<'_>) -> std::fmt::Result
where
  F: Write,
{
  if x.fract() == 0. {
    write!(f, "{}.", x)
  } else {
    write!(f, "{}", x)
  }
}

pub fn show_double<F>(f: &mut F, x: f64, _: &mut FormattingState<'_>) -> std::fmt::Result
where
  F: Write,
{
  if x.fract() == 0. {
    write!(f, "{}.lf", x)
  } else {
    write!(f, "{}lf", x)
  }
}

pub fn show_expr<F>(f: &mut F, expr: &syntax::Expr, s: &mut FormattingState<'_>) -> std::fmt::Result
where
  F: Write,
{
  match *expr {
    syntax::Expr::Variable(ref i) => show_identifier(f, &i, s),
    syntax::Expr::IntConst(ref x) => write!(f, "{}", x),
    syntax::Expr::UIntConst(ref x) => write!(f, "{}u", x),
    syntax::Expr::BoolConst(ref x) => write!(f, "{}", x),
    syntax::Expr::FloatConst(ref x) => show_float(f, *x, s),
    syntax::Expr::DoubleConst(ref x) => show_double(f, *x, s),
    syntax::Expr::Unary(ref op, ref e) => {
      // Note: all unary ops are right-to-left associative
      show_unary_op(f, &op, s)?;

      if e.precedence() > op.precedence() {
        f.write_str("(")?;
        show_expr(f, &e, s)?;
        f.write_str(")")
      } else if let syntax::Expr::Unary(eop, _) = &**e {
        // Prevent double-unary plus/minus turning into inc/dec
        if eop == op && (*eop == syntax::UnaryOp::Add || *eop == syntax::UnaryOp::Minus) {
          f.write_str("(")?;
          show_expr(f, &e, s)?;
          f.write_str(")")
        } else {
          show_expr(f, &e, s)
        }
      } else {
        show_expr(f, &e, s)
      }
    }
    syntax::Expr::Binary(ref op, ref l, ref r) => {
      // Note: all binary ops are left-to-right associative (<= for left part)

      if l.precedence() <= op.precedence() {
        show_expr(f, &l, s)?;
      } else {
        f.write_str("(")?;
        show_expr(f, &l, s)?;
        f.write_str(")")?;
      }

      show_binary_op(f, &op, s)?;

      if r.precedence() < op.precedence() {
        show_expr(f, &r, s)
      } else {
        f.write_str("(")?;
        show_expr(f, &r, s)?;
        f.write_str(")")
      }
    }
    syntax::Expr::Ternary(ref c, ref st, ref e) => {
      // Note: ternary is right-to-left associative (<= for right part)

      if c.precedence() < expr.precedence() {
        show_expr(f, &c, s)?;
      } else {
        f.write_str("(")?;
        show_expr(f, &c, s)?;
        f.write_str(")")?;
      }
      f.write_str(" ? ")?;
      show_expr(f, &st, s)?;
      f.write_str(" : ")?;
      if e.precedence() <= expr.precedence() {
        show_expr(f, &e, s)
      } else {
        f.write_str("(")?;
        show_expr(f, &e, s)?;
        f.write_str(")")
      }
    }
    syntax::Expr::Assignment(ref v, ref op, ref e) => {
      // Note: all assignment ops are right-to-left associative

      if v.precedence() < op.precedence() {
        show_expr(f, &v, s)?;
      } else {
        f.write_str("(")?;
        show_expr(f, &v, s)?;
        f.write_str(")")?;
      }

      show_assignment_op(f, &op, s)?;

      if e.precedence() <= op.precedence() {
        show_expr(f, &e, s)
      } else {
        f.write_str("(")?;
        show_expr(f, &e, s)?;
        f.write_str(")")
      }
    }
    syntax::Expr::Bracket(ref e, ref a) => {
      // Note: bracket is left-to-right associative

      if e.precedence() <= expr.precedence() {
        show_expr(f, &e, s)?;
      } else {
        f.write_str("(")?;
        show_expr(f, &e, s)?;
        f.write_str(")")?;
      }

      show_array_spec(f, &a, s)
    }
    syntax::Expr::FunCall(ref fun, ref args) => {
      show_function_identifier(f, &fun, s)?;
      f.write_str("(")?;

      if !args.is_empty() {
        let mut args_iter = args.iter();
        let first = args_iter.next().unwrap();
        show_expr(f, first, s)?;

        for e in args_iter {
          f.write_str(", ")?;
          show_expr(f, e, s)?;
        }
      }

      f.write_str(")")
    }
    syntax::Expr::Dot(ref e, ref i) => {
      // Note: dot is left-to-right associative

      if e.precedence() <= expr.precedence() {
        show_expr(f, &e, s)?;
      } else {
        f.write_str("(")?;
        show_expr(f, &e, s)?;
        f.write_str(")")?;
      }
      f.write_str(".")?;
      show_identifier(f, &i, s)
    }
    syntax::Expr::PostInc(ref e) => {
      // Note: post-increment is right-to-left associative

      if e.precedence() < expr.precedence() {
        show_expr(f, &e, s)?;
      } else {
        f.write_str("(")?;
        show_expr(f, &e, s)?;
        f.write_str(")")?;
      }

      f.write_str("++")
    }
    syntax::Expr::PostDec(ref e) => {
      // Note: post-decrement is right-to-left associative

      if e.precedence() < expr.precedence() {
        show_expr(f, &e, s)?;
      } else {
        f.write_str("(")?;
        show_expr(f, &e, s)?;
        f.write_str(")")?;
      }

      f.write_str("--")
    }
    syntax::Expr::Comma(ref a, ref b) => {
      // Note: comma is left-to-right associative

      if a.precedence() <= expr.precedence() {
        show_expr(f, &a, s)?;
      } else {
        f.write_str("(")?;
        show_expr(f, &a, s)?;
        f.write_str(")")?;
      }

      f.write_str(", ")?;

      if b.precedence() < expr.precedence() {
        show_expr(f, &b, s)
      } else {
        f.write_str("(")?;
        show_expr(f, &b, s)?;
        f.write_str(")")
      }
    }
  }
}

pub fn show_path<F>(f: &mut F, path: &syntax::Path, _: &mut FormattingState<'_>) -> std::fmt::Result
where
  F: Write,
{
  match path {
    syntax::Path::Absolute(s) => write!(f, "<{}>", s),
    syntax::Path::Relative(s) => write!(f, "\"{}\"", s),
  }
}

pub fn show_unary_op<F>(
  f: &mut F,
  op: &syntax::UnaryOp,
  _: &mut FormattingState<'_>,
) -> std::fmt::Result
where
  F: Write,
{
  match *op {
    syntax::UnaryOp::Inc => f.write_str("++"),
    syntax::UnaryOp::Dec => f.write_str("--"),
    syntax::UnaryOp::Add => f.write_str("+"),
    syntax::UnaryOp::Minus => f.write_str("-"),
    syntax::UnaryOp::Not => f.write_str("!"),
    syntax::UnaryOp::Complement => f.write_str("~"),
  }
}

pub fn show_binary_op<F>(
  f: &mut F,
  op: &syntax::BinaryOp,
  s: &mut FormattingState<'_>,
) -> std::fmt::Result
where
  F: Write,
{
  match *op {
    syntax::BinaryOp::Or => s.write_binary_op(f, "||"),
    syntax::BinaryOp::Xor => s.write_binary_op(f, "^^"),
    syntax::BinaryOp::And => s.write_binary_op(f, "&&"),
    syntax::BinaryOp::BitOr => s.write_binary_op(f, "|"),
    syntax::BinaryOp::BitXor => s.write_binary_op(f, "^"),
    syntax::BinaryOp::BitAnd => s.write_binary_op(f, "&"),
    syntax::BinaryOp::Equal => s.write_binary_op(f, "=="),
    syntax::BinaryOp::NonEqual => s.write_binary_op(f, "!="),
    syntax::BinaryOp::LT => s.write_binary_op(f, "<"),
    syntax::BinaryOp::GT => s.write_binary_op(f, ">"),
    syntax::BinaryOp::LTE => s.write_binary_op(f, "<="),
    syntax::BinaryOp::GTE => s.write_binary_op(f, ">="),
    syntax::BinaryOp::LShift => s.write_binary_op(f, "<<"),
    syntax::BinaryOp::RShift => s.write_binary_op(f, ">>"),
    syntax::BinaryOp::Add => s.write_binary_op(f, "+"),
    syntax::BinaryOp::Sub => s.write_binary_op(f, "-"),
    syntax::BinaryOp::Mult => s.write_binary_op(f, "*"),
    syntax::BinaryOp::Div => s.write_binary_op(f, "/"),
    syntax::BinaryOp::Mod => s.write_binary_op(f, "%"),
  }
}

pub fn show_assignment_op<F>(
  f: &mut F,
  op: &syntax::AssignmentOp,
  s: &mut FormattingState<'_>,
) -> std::fmt::Result
where
  F: Write,
{
  match *op {
    syntax::AssignmentOp::Equal => s.write_binary_op(f, "="),
    syntax::AssignmentOp::Mult => s.write_binary_op(f, "*="),
    syntax::AssignmentOp::Div => s.write_binary_op(f, "/="),
    syntax::AssignmentOp::Mod => s.write_binary_op(f, "%="),
    syntax::AssignmentOp::Add => s.write_binary_op(f, "+="),
    syntax::AssignmentOp::Sub => s.write_binary_op(f, "-="),
    syntax::AssignmentOp::LShift => s.write_binary_op(f, "<<="),
    syntax::AssignmentOp::RShift => s.write_binary_op(f, ">>="),
    syntax::AssignmentOp::And => s.write_binary_op(f, "&="),
    syntax::AssignmentOp::Xor => s.write_binary_op(f, "^="),
    syntax::AssignmentOp::Or => s.write_binary_op(f, "|="),
  }
}

pub fn show_function_identifier<F>(
  f: &mut F,
  i: &syntax::FunIdentifier,
  s: &mut FormattingState<'_>,
) -> std::fmt::Result
where
  F: Write,
{
  match *i {
    syntax::FunIdentifier::Identifier(ref n) => show_identifier(f, &n, s),
    syntax::FunIdentifier::Expr(ref e) => show_expr(f, &*e, s),
  }
}

pub fn show_declaration<F>(
  f: &mut F,
  d: &syntax::Declaration,
  s: &mut FormattingState<'_>,
) -> std::fmt::Result
where
  F: Write,
{
  match **d {
    syntax::DeclarationData::FunctionPrototype(ref proto) => {
      show_function_prototype(f, &proto, s)?;
      s.write_declaration_terminator(f)
    }
    syntax::DeclarationData::InitDeclaratorList(ref list) => {
      show_init_declarator_list(f, &list, s)?;
      s.write_declaration_terminator(f)
    }
    syntax::DeclarationData::Precision(ref qual, ref ty) => {
      show_precision_qualifier(f, &qual, s)?;
      show_type_specifier(f, &ty, s)?;
      s.write_declaration_terminator(f)
    }
    syntax::DeclarationData::Block(ref block) => {
      show_block(f, &block, s)?;
      s.write_declaration_terminator(f)
    }
    syntax::DeclarationData::Global(ref qual, ref identifiers) => {
      show_type_qualifier(f, &qual, s)?;

      if !identifiers.is_empty() {
        let mut iter = identifiers.iter();
        let first = iter.next().unwrap();
        show_identifier(f, first, s)?;

        for identifier in iter {
          write!(f, ", {}", identifier)?;
        }
      }

      s.write_declaration_terminator(f)
    }
  }
}

pub fn show_function_prototype<F>(
  f: &mut F,
  fp: &syntax::FunctionPrototype,
  s: &mut FormattingState<'_>,
) -> std::fmt::Result
where
  F: Write,
{
  show_fully_specified_type(f, &fp.ty, s)?;
  f.write_str(" ")?;
  show_identifier(f, &fp.name, s)?;

  f.write_str("(")?;

  if !fp.parameters.is_empty() {
    let mut iter = fp.parameters.iter();
    let first = iter.next().unwrap();
    show_function_parameter_declaration(f, first, s)?;

    for param in iter {
      f.write_str(", ")?;
      show_function_parameter_declaration(f, param, s)?;
    }
  }

  f.write_str(")")
}
pub fn show_function_parameter_declaration<F>(
  f: &mut F,
  p: &syntax::FunctionParameterDeclaration,
  s: &mut FormattingState<'_>,
) -> std::fmt::Result
where
  F: Write,
{
  match **p {
    syntax::FunctionParameterDeclarationData::Named(ref qual, ref fpd) => {
      if let Some(ref q) = *qual {
        show_type_qualifier(f, q, s)?;
        f.write_str(" ")?;
      }

      show_function_parameter_declarator(f, fpd, s)
    }
    syntax::FunctionParameterDeclarationData::Unnamed(ref qual, ref ty) => {
      if let Some(ref q) = *qual {
        show_type_qualifier(f, q, s)?;
        f.write_str(" ")?;
      }

      show_type_specifier(f, ty, s)
    }
  }
}

pub fn show_function_parameter_declarator<F>(
  f: &mut F,
  p: &syntax::FunctionParameterDeclarator,
  s: &mut FormattingState<'_>,
) -> std::fmt::Result
where
  F: Write,
{
  show_type_specifier(f, &p.ty, s)?;
  f.write_str(" ")?;
  show_arrayed_identifier(f, &p.ident, s)
}

pub fn show_init_declarator_list<F>(
  f: &mut F,
  i: &syntax::InitDeclaratorList,
  s: &mut FormattingState<'_>,
) -> std::fmt::Result
where
  F: Write,
{
  show_single_declaration(f, &i.head, s)?;

  for decl in &i.tail {
    f.write_str(", ")?;
    show_single_declaration_no_type(f, decl, s)?;
  }

  Ok(())
}

pub fn show_single_declaration<F>(
  f: &mut F,
  d: &syntax::SingleDeclaration,
  s: &mut FormattingState<'_>,
) -> std::fmt::Result
where
  F: Write,
{
  show_fully_specified_type(f, &d.ty, s)?;

  if let Some(ref name) = d.name {
    f.write_str(" ")?;
    show_identifier(f, name, s)?;
  }

  if let Some(ref arr_spec) = d.array_specifier {
    show_array_spec(f, arr_spec, s)?;
  }

  if let Some(ref initializer) = d.initializer {
    f.write_str(" = ")?;
    show_initializer(f, initializer, s)?;
  }

  Ok(())
}

pub fn show_single_declaration_no_type<F>(
  f: &mut F,
  d: &syntax::SingleDeclarationNoType,
  s: &mut FormattingState<'_>,
) -> std::fmt::Result
where
  F: Write,
{
  show_arrayed_identifier(f, &d.ident, s)?;

  if let Some(ref initializer) = d.initializer {
    f.write_str(" = ")?;
    show_initializer(f, initializer, s)?;
  }

  Ok(())
}

pub fn show_initializer<F>(
  f: &mut F,
  i: &syntax::Initializer,
  s: &mut FormattingState<'_>,
) -> std::fmt::Result
where
  F: Write,
{
  match *i {
    syntax::Initializer::Simple(ref e) => show_expr(f, e, s),
    syntax::Initializer::List(ref list) => {
      let mut iter = list.0.iter();
      let first = iter.next().unwrap();

      f.write_str("{ ")?;
      show_initializer(f, first, s)?;

      for ini in iter {
        f.write_str(", ")?;
        show_initializer(f, ini, s)?;
      }

      f.write_str(" }")
    }
  }
}

pub fn show_block<F>(f: &mut F, b: &syntax::Block, s: &mut FormattingState<'_>) -> std::fmt::Result
where
  F: Write,
{
  show_type_qualifier(f, &b.qualifier, s)?;
  f.write_str(" ")?;
  show_identifier(f, &b.name, s)?;
  s.enter_block(f)?;

  for field in &b.fields {
    show_struct_field(f, field, s)?;
    f.write_str("\n")?;
  }
  s.exit_block(f)?;

  if let Some(ref ident) = b.identifier {
    show_arrayed_identifier(f, ident, s)?;
  }

  Ok(())
}

pub fn show_function_definition<F>(
  f: &mut F,
  fd: &syntax::FunctionDefinition,
  s: &mut FormattingState<'_>,
) -> std::fmt::Result
where
  F: Write,
{
  show_function_prototype(f, &fd.prototype, s)?;
  f.write_str(" ")?;
  show_compound_statement(f, &fd.statement, s)?;
  s.flush_line(f)?;
  s.write_function_definition_terminator(f)
}

pub fn show_compound_statement<F>(
  f: &mut F,
  cst: &syntax::CompoundStatement,
  s: &mut FormattingState<'_>,
) -> std::fmt::Result
where
  F: Write,
{
  s.enter_block(f)?;

  for st in &cst.statement_list {
    show_statement(f, st, s)?;
  }

  s.exit_block(f)?;

  Ok(())
}

pub fn show_statement<F>(
  f: &mut F,
  st: &syntax::Statement,
  s: &mut FormattingState<'_>,
) -> std::fmt::Result
where
  F: Write,
{
  s.flush_line(f)?;

  match *st {
    syntax::Statement::Compound(ref cst) => show_compound_statement(f, cst, s),
    syntax::Statement::Simple(ref sst) => show_simple_statement(f, sst, s),
  }
}

pub fn show_simple_statement<F>(
  f: &mut F,
  sst: &syntax::SimpleStatement,
  s: &mut FormattingState<'_>,
) -> std::fmt::Result
where
  F: Write,
{
  match *sst {
    syntax::SimpleStatement::Declaration(ref d) => show_declaration(f, d, s),
    syntax::SimpleStatement::Expression(ref e) => show_expression_statement(f, e, s),
    syntax::SimpleStatement::Selection(ref st) => show_selection_statement(f, st, s),
    syntax::SimpleStatement::Switch(ref st) => show_switch_statement(f, st, s),
    syntax::SimpleStatement::CaseLabel(ref cl) => show_case_label(f, cl, s),
    syntax::SimpleStatement::Iteration(ref i) => show_iteration_statement(f, i, s),
    syntax::SimpleStatement::Jump(ref j) => show_jump_statement(f, j, s),
  }
}

pub fn show_expression_statement<F>(
  f: &mut F,
  est: &syntax::ExprStatement,
  s: &mut FormattingState<'_>,
) -> std::fmt::Result
where
  F: Write,
{
  if let Some(ref e) = *est {
    show_expr(f, e, s)?;
  }

  s.write_statement_terminator(f)
}

pub fn show_selection_statement<F>(
  f: &mut F,
  sst: &syntax::SelectionStatement,
  s: &mut FormattingState<'_>,
) -> std::fmt::Result
where
  F: Write,
{
  f.write_str("if (")?;
  show_expr(f, &sst.cond, s)?;
  f.write_str(") ")?;
  show_selection_rest_statement(f, &sst.rest, s)
}

pub fn show_selection_rest_statement<F>(
  f: &mut F,
  sst: &syntax::SelectionRestStatement,
  s: &mut FormattingState<'_>,
) -> std::fmt::Result
where
  F: Write,
{
  match *sst {
    syntax::SelectionRestStatement::Statement(ref if_st) => {
      show_statement(f, if_st, s)
    }
    syntax::SelectionRestStatement::Else(ref if_st, ref else_st) => {
      show_statement(f, if_st, s)?;
      f.write_str(" else ")?;
      // TODO: This should be configurable instead of relying on show_statement's calling
      // flush_line
      s.consume_newline();
      show_statement(f, else_st, s)
    }
  }
}

pub fn show_switch_statement<F>(
  f: &mut F,
  sst: &syntax::SwitchStatement,
  s: &mut FormattingState<'_>,
) -> std::fmt::Result
where
  F: Write,
{
  f.write_str("switch (")?;
  show_expr(f, &sst.head, s)?;
  f.write_str(") {\n")?;

  for st in &sst.body {
    show_statement(f, st, s)?;
  }

  f.write_str("}\n")
}

pub fn show_case_label<F>(
  f: &mut F,
  cl: &syntax::CaseLabel,
  s: &mut FormattingState<'_>,
) -> std::fmt::Result
where
  F: Write,
{
  match *cl {
    syntax::CaseLabel::Case(ref e) => {
      f.write_str("case ")?;
      show_expr(f, e, s)?;
      f.write_str(":\n")
    }
    syntax::CaseLabel::Def => f.write_str("default:\n"),
  }
}

pub fn show_iteration_statement<F>(
  f: &mut F,
  ist: &syntax::IterationStatement,
  s: &mut FormattingState<'_>,
) -> std::fmt::Result
where
  F: Write,
{
  match *ist {
    syntax::IterationStatement::While(ref cond, ref body) => {
      f.write_str("while (")?;
      show_condition(f, cond, s)?;
      f.write_str(") ")?;
      show_statement(f, body, s)
    }
    syntax::IterationStatement::DoWhile(ref body, ref cond) => {
      f.write_str("do ")?;
      show_statement(f, body, s)?;
      f.write_str(" while (")?;
      show_expr(f, cond, s)?;
      f.write_str(")")?;
      s.write_statement_terminator(f)
    }
    syntax::IterationStatement::For(ref init, ref rest, ref body) => {
      f.write_str("for (")?;
      show_for_init_statement(f, init, s)?;
      s.flush_space(f)?;
      show_for_rest_statement(f, rest, s)?;
      f.write_str(") ")?;
      show_statement(f, body, s)
    }
  }
}

pub fn show_condition<F>(
  f: &mut F,
  c: &syntax::Condition,
  s: &mut FormattingState<'_>,
) -> std::fmt::Result
where
  F: Write,
{
  match *c {
    syntax::Condition::Expr(ref e) => show_expr(f, e, s),
    syntax::Condition::Assignment(ref ty, ref name, ref initializer) => {
      show_fully_specified_type(f, ty, s)?;
      f.write_str(" ")?;
      show_identifier(f, name, s)?;
      f.write_str(" = ")?;
      show_initializer(f, initializer, s)
    }
  }
}

pub fn show_for_init_statement<F>(
  f: &mut F,
  i: &syntax::ForInitStatement,
  s: &mut FormattingState<'_>,
) -> std::fmt::Result
where
  F: Write,
{
  match *i {
    syntax::ForInitStatement::Expression(ref expr) => {
      if let Some(ref e) = *expr {
        show_expr(f, e, s)?;
      }

      Ok(())
    }
    syntax::ForInitStatement::Declaration(ref d) => show_declaration(f, d, s),
  }
}

pub fn show_for_rest_statement<F>(
  f: &mut F,
  r: &syntax::ForRestStatement,
  s: &mut FormattingState<'_>,
) -> std::fmt::Result
where
  F: Write,
{
  if let Some(ref cond) = r.condition {
    show_condition(f, cond, s)?;
  }

  f.write_str("; ")?;

  if let Some(ref e) = r.post_expr {
    show_expr(f, e, s)?;
  }

  Ok(())
}

pub fn show_jump_statement<F>(
  f: &mut F,
  j: &syntax::JumpStatement,
  s: &mut FormattingState<'_>,
) -> std::fmt::Result
where
  F: Write,
{
  match *j {
    syntax::JumpStatement::Continue => f.write_str("continue")?,
    syntax::JumpStatement::Break => f.write_str("break")?,
    syntax::JumpStatement::Discard => f.write_str("discard")?,
    syntax::JumpStatement::Return(ref e) => {
      f.write_str("return ")?;
      if let Some(e) = e {
        show_expr(f, e, s)?;
      }
    }
  }

  s.write_statement_terminator(f)
}

pub fn show_preprocessor<F>(
  f: &mut F,
  pp: &syntax::Preprocessor,
  s: &mut FormattingState<'_>,
) -> std::fmt::Result
where
  F: Write,
{
  match **pp {
    syntax::PreprocessorData::Define(ref pd) => show_preprocessor_define(f, pd, s),
    syntax::PreprocessorData::Else => show_preprocessor_else(f, s),
    syntax::PreprocessorData::ElseIf(ref pei) => show_preprocessor_elseif(f, pei, s),
    syntax::PreprocessorData::EndIf => show_preprocessor_endif(f, s),
    syntax::PreprocessorData::Error(ref pe) => show_preprocessor_error(f, pe, s),
    syntax::PreprocessorData::If(ref pi) => show_preprocessor_if(f, pi, s),
    syntax::PreprocessorData::IfDef(ref pid) => show_preprocessor_ifdef(f, pid, s),
    syntax::PreprocessorData::IfNDef(ref pind) => show_preprocessor_ifndef(f, pind, s),
    syntax::PreprocessorData::Include(ref pi) => show_preprocessor_include(f, pi, s),
    syntax::PreprocessorData::Line(ref pl) => show_preprocessor_line(f, pl, s),
    syntax::PreprocessorData::Pragma(ref pp) => show_preprocessor_pragma(f, pp, s),
    syntax::PreprocessorData::Undef(ref pu) => show_preprocessor_undef(f, pu, s),
    syntax::PreprocessorData::Version(ref pv) => show_preprocessor_version(f, pv, s),
    syntax::PreprocessorData::Extension(ref pe) => show_preprocessor_extension(f, pe, s),
  }
}

pub fn show_preprocessor_define<F>(
  f: &mut F,
  pd: &syntax::PreprocessorDefine,
  _: &mut FormattingState<'_>,
) -> std::fmt::Result
where
  F: Write,
{
  match *pd {
    syntax::PreprocessorDefine::ObjectLike {
      ref ident,
      ref value,
    } => write!(f, "#define {} {}\n", ident, value),

    syntax::PreprocessorDefine::FunctionLike {
      ref ident,
      ref args,
      ref value,
    } => {
      write!(f, "#define {}(", ident)?;

      if !args.is_empty() {
        write!(f, "{}", &args[0])?;

        for arg in &args[1..args.len()] {
          write!(f, ", {}", arg)?;
        }
      }

      write!(f, ") {}\n", value)
    }
  }
}

pub fn show_preprocessor_else<F>(f: &mut F, _: &mut FormattingState<'_>) -> std::fmt::Result
where
  F: Write,
{
  f.write_str("#else\n")
}

pub fn show_preprocessor_elseif<F>(
  f: &mut F,
  pei: &syntax::PreprocessorElseIf,
  _: &mut FormattingState<'_>,
) -> std::fmt::Result
where
  F: Write,
{
  write!(f, "#elseif {}\n", pei.condition)
}

pub fn show_preprocessor_error<F>(
  f: &mut F,
  pe: &syntax::PreprocessorError,
  _: &mut FormattingState<'_>,
) -> std::fmt::Result
where
  F: Write,
{
  writeln!(f, "#error {}", pe.message)
}

pub fn show_preprocessor_endif<F>(f: &mut F, _: &mut FormattingState<'_>) -> std::fmt::Result
where
  F: Write,
{
  f.write_str("#endif\n")
}

pub fn show_preprocessor_if<F>(
  f: &mut F,
  pi: &syntax::PreprocessorIf,
  _: &mut FormattingState<'_>,
) -> std::fmt::Result
where
  F: Write,
{
  write!(f, "#if {}\n", pi.condition)
}

pub fn show_preprocessor_ifdef<F>(
  f: &mut F,
  pid: &syntax::PreprocessorIfDef,
  s: &mut FormattingState<'_>,
) -> std::fmt::Result
where
  F: Write,
{
  f.write_str("#ifdef ")?;
  show_identifier(f, &pid.ident, s)?;
  f.write_str("\n")
}

pub fn show_preprocessor_ifndef<F>(
  f: &mut F,
  pind: &syntax::PreprocessorIfNDef,
  s: &mut FormattingState<'_>,
) -> std::fmt::Result
where
  F: Write,
{
  f.write_str("#ifndef ")?;
  show_identifier(f, &pind.ident, s)?;
  f.write_str("\n")
}

pub fn show_preprocessor_include<F>(
  f: &mut F,
  pi: &syntax::PreprocessorInclude,
  s: &mut FormattingState<'_>,
) -> std::fmt::Result
where
  F: Write,
{
  f.write_str("#include ")?;
  show_path(f, &pi.path, s)?;
  f.write_str("\n")
}

pub fn show_preprocessor_line<F>(
  f: &mut F,
  pl: &syntax::PreprocessorLine,
  _: &mut FormattingState<'_>,
) -> std::fmt::Result
where
  F: Write,
{
  write!(f, "#line {}", pl.line)?;
  if let Some(source_string_number) = pl.source_string_number {
    write!(f, " {}", source_string_number)?;
  }
  f.write_str("\n")
}

pub fn show_preprocessor_pragma<F>(
  f: &mut F,
  pp: &syntax::PreprocessorPragma,
  _: &mut FormattingState<'_>,
) -> std::fmt::Result
where
  F: Write,
{
  writeln!(f, "#pragma {}", pp.command)
}

pub fn show_preprocessor_undef<F>(
  f: &mut F,
  pud: &syntax::PreprocessorUndef,
  s: &mut FormattingState<'_>,
) -> std::fmt::Result
where
  F: Write,
{
  f.write_str("#undef ")?;
  show_identifier(f, &pud.name, s)?;
  f.write_str("\n")
}

pub fn show_preprocessor_version<F>(
  f: &mut F,
  pv: &syntax::PreprocessorVersion,
  _: &mut FormattingState<'_>,
) -> std::fmt::Result
where
  F: Write,
{
  write!(f, "#version {}", pv.version)?;

  if let Some(ref profile) = pv.profile {
    match *profile {
      syntax::PreprocessorVersionProfile::Core => {
        f.write_str(" core")?;
      }
      syntax::PreprocessorVersionProfile::Compatibility => {
        f.write_str(" compatibility")?;
      }
      syntax::PreprocessorVersionProfile::ES => {
        f.write_str(" es")?;
      }
    }
  }

  f.write_str("\n")
}

pub fn show_preprocessor_extension<F>(
  f: &mut F,
  pe: &syntax::PreprocessorExtension,
  _: &mut FormattingState<'_>,
) -> std::fmt::Result
where
  F: Write,
{
  f.write_str("#extension ")?;

  match pe.name {
    syntax::PreprocessorExtensionName::All => {
      f.write_str("all")?;
    }
    syntax::PreprocessorExtensionName::Specific(ref n) => {
      f.write_str(n)?;
    }
  }

  if let Some(ref behavior) = pe.behavior {
    match *behavior {
      syntax::PreprocessorExtensionBehavior::Require => {
        f.write_str(" : require")?;
      }
      syntax::PreprocessorExtensionBehavior::Enable => {
        f.write_str(" : enable")?;
      }
      syntax::PreprocessorExtensionBehavior::Warn => {
        f.write_str(" : warn")?;
      }
      syntax::PreprocessorExtensionBehavior::Disable => {
        f.write_str(" : disable")?;
      }
    }
  }

  f.write_str("\n")
}

pub fn show_external_declaration<F>(
  f: &mut F,
  ed: &syntax::ExternalDeclaration,
  mut s: &mut FormattingState<'_>,
) -> std::fmt::Result
where
  F: Write,
{
  s.flush_line(f)?;

  match **ed {
    syntax::ExternalDeclarationData::Preprocessor(ref pp) => show_preprocessor(f, pp, &mut s),
    syntax::ExternalDeclarationData::FunctionDefinition(ref fd) => {
      show_function_definition(f, fd, &mut s)
    }
    syntax::ExternalDeclarationData::Declaration(ref d) => show_declaration(f, d, &mut s),
  }
}

pub fn show_translation_unit<F>(
  f: &mut F,
  tu: &syntax::TranslationUnit,
  mut s: FormattingState<'_>,
) -> std::fmt::Result
where
  F: Write,
{
  for ed in &(tu.0).0 {
    show_external_declaration(f, ed, &mut s)?;
  }

  Ok(())
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::assert_ceq;
  use crate::parser::Parse;
  use crate::parsers::expr;

  fn to_string(e: &syntax::Expr) -> String {
    let mut s = String::new();
    show_expr(&mut s, e, &mut FormattingState::default()).unwrap();
    s
  }

  #[test]
  fn unary_parentheses() {
    assert_eq!(to_string(&expr("-a".into()).unwrap().1), "-a");
    assert_eq!(to_string(&expr("-(a + b)".into()).unwrap().1), "-(a + b)");
    assert_eq!(to_string(&expr("-a.x".into()).unwrap().1), "-a.x");

    assert_eq!(to_string(&expr("-(-a)".into()).unwrap().1), "-(-a)");
    assert_eq!(to_string(&expr("+(+a)".into()).unwrap().1), "+(+a)");
    assert_eq!(to_string(&expr("~~a".into()).unwrap().1), "~~a");
    assert_eq!(to_string(&expr("--a".into()).unwrap().1), "--a");
    assert_eq!(to_string(&expr("++a".into()).unwrap().1), "++a");
    assert_eq!(to_string(&expr("+-a".into()).unwrap().1), "+-a");
  }

  #[test]
  fn binary_parentheses() {
    assert_eq!(to_string(&expr("a + b".into()).unwrap().1), "a + b");
    assert_eq!(to_string(&expr("a * b + c".into()).unwrap().1), "a * b + c");
    assert_eq!(to_string(&expr("(a + b) * c".into()).unwrap().1), "(a + b) * c");
    assert_eq!(to_string(&expr("a + (b * c)".into()).unwrap().1), "a + b * c");
    assert_eq!(to_string(&expr("a * (b + c)".into()).unwrap().1), "a * (b + c)");
    assert_eq!(to_string(&expr("(a * b) * c".into()).unwrap().1), "a * b * c");
    assert_eq!(to_string(&expr("a * (b * c)".into()).unwrap().1), "a * (b * c)");
    assert_eq!(to_string(&expr("a&&b&&c".into()).unwrap().1), "a && b && c");
    assert_eq!(
      to_string(&expr("n - p > 0. && u.y < n && u.y > p".into()).unwrap().1),
      "n - p > 0. && u.y < n && u.y > p"
    );
  }

  #[test]
  fn ternary_parentheses() {
    assert_eq!(
      to_string(&expr("a ? b : c ? d : e".into()).unwrap().1),
      "a ? b : c ? d : e"
    );
    assert_eq!(
      to_string(&expr("(a ? b : c) ? d : e".into()).unwrap().1),
      "(a ? b : c) ? d : e"
    );
  }

  #[test]
  fn assignment_parentheses() {
    assert_eq!(to_string(&expr("a = b = c".into()).unwrap().1), "a = b = c");
    assert_eq!(to_string(&expr("(a = b) = c".into()).unwrap().1), "(a = b) = c");
  }

  #[test]
  fn dot_parentheses() {
    assert_eq!(to_string(&expr("a.x".into()).unwrap().1), "a.x");
    assert_eq!(to_string(&expr("(a + b).x".into()).unwrap().1), "(a + b).x");
  }

  #[test]
  fn test_parentheses() {
    use crate::parsers::function_definition;

    const SRC: &'static str = r#"vec2 main() {
float n = 0.;
float p = 0.;
float u = vec2(0., 0.);
if (n-p>0.&&u.y<n&&u.y>p) {
}
return u;
}
"#;

    // Ideally we would use SRC as the expected, but there's a bug in block braces generation
    const DST: &'static str = r#"vec2 main() {
    float n = 0.;
    float p = 0.;
    float u = vec2(0., 0.);
    if (n - p > 0. && u.y < n && u.y > p) {
    }
    return u;
}
"#;

    let mut s = String::new();
    show_function_definition(
      &mut s,
      &function_definition(SRC.into()).unwrap().1,
      &mut FormattingState::default(),
    )
    .unwrap();

    assert_eq!(s, DST);
  }

  #[test]
  fn roundtrip_glsl_complex_expr() {
    let zero = syntax::Expr::DoubleConst(0.);
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
    let input = syntax::Expr::FunCall(
      syntax::FunIdentifier::Identifier("normalize".into()),
      vec![xyz],
    );

    let mut output = String::new();
    show_expr(&mut output, &input, &mut FormattingState::default()).unwrap();
    output.write_str(";").unwrap();

    let back = syntax::Expr::parse(&output);

    assert_ceq!(back, Ok(input), "intermediate source '{}'", output);
  }
}
