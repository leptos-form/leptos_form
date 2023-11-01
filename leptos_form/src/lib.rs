#![doc = include_str!("../README.md")]
#![cfg_attr(CHANNEL_NIGHTLY, feature(doc_auto_cfg))]
#![forbid(unsafe_code)]

pub use ::leptos_form_core::*;

#[doc = include_str!("../docs/Form.md")]
pub use ::leptos_form_proc_macros::Form;

/// Case conversion. Used in the [`macro@Form#label-attributes`] macro.
pub use ::leptos_form_core::LabelCase;

/// Closure types which can be passed as the `map_submit` argument to the
/// [`macro@Form#component-attributes`] macro.
pub use ::leptos_form_core::MapSubmit;

/// Provides utilities for a data type's form field representation.
/// Must be implemented for a type to be used as a field in a struct which derives the [`macro@Form`] macro.
pub use ::leptos_form_core::FormField;

/// Specifies a data type's default html element. Must be implemented if the field attribute in the [`macro@Form#field-attributes`] macro
/// is called without providing the `el` argument.
pub use ::leptos_form_core::DefaultHtmlElement;

pub mod prelude {
    #[cfg(feature = "chrono")]
    pub use super::config::chrono::*;
    pub use super::config::collections::*;
    pub use super::*;
}

#[doc(hidden)]
pub mod internal {
    pub use leptos;
    pub use leptos_router;
    pub use wasm_bindgen;
}
