#![forbid(unsafe_code)]

use proc_macro::TokenStream;

#[proc_macro_derive(Form, attributes(form))]
pub fn derive_form(tokens: TokenStream) -> TokenStream {
    match leptos_form_proc_macros_core::derive_form(tokens.into()) {
        Ok(tokens) => tokens.into(),
        Err(err) => err.into_compile_error().into(),
    }
}
