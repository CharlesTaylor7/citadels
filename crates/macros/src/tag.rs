pub use macros_impl::Tag;

/// Derivable for any enum
/// Creates a new enum with unit variants.
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
/// ```
pub trait Tag: Sized {
    type Tag: std::fmt::Debug + Clone + Copy + PartialEq + Eq;

    fn tag(&self) -> Self::Tag;
}
