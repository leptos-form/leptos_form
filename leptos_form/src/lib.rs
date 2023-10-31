//! # Derive leptos forms from rust structs
//!
//! <div align="center">
//! <!-- CI -->
//! <img src="https://github.com/tlowerison/leptos_form/CI/badge.svg" />
//! <!-- codecov -->
//! <img src="https://codecov.io/gh/tlowerison/leptos_form/branch/main/graph/badge.svg" />
//! <!-- Crates version -->
//! <a href="https://crates.io/crates/leptos_form">
//! <img src="https://img.shields.io/crates/v/leptos_form.svg?style=flat-square"
//! alt="Crates.io version" />
//! </a>
//! <!-- Downloads -->
//! <a href="https://crates.io/crates/leptos_form">
//! <img src="https://img.shields.io/crates/d/leptos_form.svg?style=flat-square"
//! alt="Download" />
//! </a>
//! <!-- docs.rs docs -->
//! <a href="https://docs.rs/leptos_form">
//! <img src="https://img.shields.io/badge/docs-latest-blue.svg?style=flat-square"
//! alt="docs.rs docs" />
//! </a>
//! <a href="https://github.com/rust-secure-code/safety-dance/">
//! <img src="https://img.shields.io/badge/unsafe-forbidden-success.svg?style=flat-square"
//! alt="Unsafe Rust forbidden" />
//! </a>
//! </div>
//!
//! ## Documentation
//!
//! * [Docs](https://docs.rs/leptos_form)
//! * [GitHub repository](https://github.com/tlowerison/leptos_form)
//!
//! ## Features
//!
//! * Automatic form parsing -- focus on how your data is represented and not on how to get it in and out of html
//! * Easy specification of label and input classes, great for Tailwind integration
//! * Labels are derived from struct fields, and can be set to a form-wide casing
//! * Integration with popular crates
//!
//! ## Crate features
//!
//! This crate offers the following features, all of which are not activated by default:
//!
//! - `bigdecimal`: Provides impls for [`BigDecimal`](bigdecimal::BigDecimal)
//! - `chrono`: Provides impls for [`DateTime`](chrono::DateTime), [`NaiveDate`](chrono::NaiveDate), [`NaiveDateTime`](chrono::NaiveDateTime)
//! - `num-bigint`: Provides impls for [`BigInt`](num_bigint::BigInt) and [`BigUint`](num_bigint::BigUint)
//! - `uuid`: Provides impls for [`Uuid`](uuid::Uuid)

#![forbid(unsafe_code)]

pub use ::leptos_form_core::*;

#[doc = include_str!("../../proc_macros/docs/Form.md")]
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
