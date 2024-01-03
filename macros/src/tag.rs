pub use macros_impl::Tag;

/// Derivable for any enum
/// Creates a new enum with unit variants. 
/// Creates a new struct for each struct variant.
/// ```
/// #[derive(Tag)]
/// enum Foo {
///     Bar { a: usize, b: String }
/// }
///
/// // Generates:
///
/// enum FooTag {
///     Bar
/// }
///
/// impl Tag for Foo {
///     type Tag = FooTag;
///     fn tag(&self) -> FooTag {
///         match self {
///             Bar { .. } => FooTag::Bar
///         }
///     }
/// }
///
///
/// struct BarArgs {
///     a: usize,
///     b: String,
/// }
///
/// impl TagArgs for BarArgs {
///     type Target = Foo;
///     fn build(self) -> Foo {
///         Foo::Bar { a: self.a, b: self.b }
///     }
/// }
/// ```
///
pub trait Tag: Sized {
    type Tag: std::fmt::Debug + Clone + Copy + PartialEq + Eq;
    fn tag(&self) -> Self::Tag;
}

pub trait TagArgs {
    type Target;
    fn build(self) -> Self::Target;
}

// TODO: use from_tag
macro_rules! build {
    () => {
        
    };
}
