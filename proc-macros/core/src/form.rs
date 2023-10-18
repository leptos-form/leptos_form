#![allow(unused)]

use ::convert_case::*;
use ::derive_more::*;
use ::itertools::Itertools;
use ::proc_macro2::TokenStream;
use ::quote::{format_ident, quote};
use ::syn::parse::{Error, Parse, ParseStream, Result};
use ::syn::parse2;
use ::syn::punctuated::Punctuated;

static ATTR: &str = "form";

#[derive(Clone, Debug, IsVariant)]
enum ContainerArg {
    Container(TokenStream),
    Internal,
    NoLabels,
    RenameLabels(Case),
}

#[derive(Clone, IsVariant)]
enum FieldArg {
    Class(syn::LitStr),
    Config(syn::Expr),
    El(syn::Type),
    IdPrefix(syn::LitStr),
    Label(syn::LitStr),
    LabelClass(syn::LitStr),
    NoLabel,
}

impl ContainerArg {
    fn as_container(&self) -> Option<&TokenStream> {
        match self {
            Self::Container(container) => Some(container),
            _ => None,
        }
    }
    fn as_rename_labels(&self) -> Option<&Case> {
        match self {
            Self::RenameLabels(case) => Some(case),
            _ => None,
        }
    }
}

impl FieldArg {
    fn as_class(&self) -> Option<&syn::LitStr> {
        match self {
            Self::Class(class) => Some(class),
            _ => None,
        }
    }
    fn as_config(&self) -> Option<&syn::Expr> {
        match self {
            Self::Config(config) => Some(config),
            _ => None,
        }
    }
    fn as_el(&self) -> Option<&syn::Type> {
        match self {
            Self::El(el) => Some(el),
            _ => None,
        }
    }
    fn as_id_prefix(&self) -> Option<&syn::LitStr> {
        match self {
            Self::IdPrefix(id_prefix) => Some(id_prefix),
            _ => None,
        }
    }
    fn as_label(&self) -> Option<&syn::LitStr> {
        match self {
            Self::Label(label) => Some(label),
            _ => None,
        }
    }
    fn as_label_class(&self) -> Option<&syn::LitStr> {
        match self {
            Self::LabelClass(label_class) => Some(label_class),
            _ => None,
        }
    }
}

impl Parse for ContainerArg {
    fn parse(input: ParseStream<'_>) -> Result<Self> {
        let lookahead = input.lookahead1();
        if lookahead.peek(syn::Ident) {
            let ident = input.parse::<syn::Ident>()?;
            match &*format!("{ident}") {
                "container" => {
                    // base line syntax check that the provided input stream
                    // looks approximately like an html element
                    let fork = input.fork();
                    fork.parse::<syn::Token![<]>()?;
                    fork.parse::<syn::Ident>()?;

                    Ok(Self::Container(input.parse()?))
                }
                "internal" => Ok(Self::Internal),
                "no_labels" => Ok(Self::NoLabels),
                "rename_labels" => {
                    input.parse::<syn::Token![=]>()?;
                    let case = input.parse::<syn::LitStr>()?;
                    Ok(Self::RenameLabels(parse_case(case)?))
                }
                _ => Err(Error::new_spanned(
                    &ident,
                    format!("unrecognized form container attribute: `{ident}`"),
                )),
            }
        } else {
            Err(lookahead.error())
        }
    }
}

impl Parse for FieldArg {
    fn parse(input: ParseStream<'_>) -> Result<Self> {
        let lookahead = input.lookahead1();
        if lookahead.peek(syn::Ident) {
            let ident = input.parse::<syn::Ident>()?;
            match &*format!("{ident}") {
                "class" => {
                    input.parse::<syn::Token![=]>()?;
                    Ok(Self::Class(input.parse()?))
                }
                "config" => {
                    input.parse::<syn::Token![=]>()?;
                    Ok(Self::Config(input.parse()?))
                }
                "el" => {
                    input.parse::<syn::Token![=]>()?;
                    Ok(Self::El(input.parse()?))
                }
                "id_prefix" => {
                    input.parse::<syn::Token![=]>()?;
                    Ok(Self::IdPrefix(input.parse()?))
                }
                "label" => {
                    input.parse::<syn::Token![=]>()?;
                    Ok(Self::Label(input.parse()?))
                }
                "label_class" => {
                    input.parse::<syn::Token![=]>()?;
                    Ok(Self::LabelClass(input.parse()?))
                }
                "no_label" => Ok(Self::NoLabel),
                _ => Err(Error::new_spanned(
                    &ident,
                    format!("unrecognized form field attribute: `{ident}`"),
                )),
            }
        } else {
            Err(lookahead.error())
        }
    }
}

struct PunctWrap<T, P>(Punctuated<T, P>);

impl<T: Parse, P: Parse> Parse for PunctWrap<T, P> {
    fn parse(input: ParseStream<'_>) -> Result<Self> {
        Ok(Self(Punctuated::<T, P>::parse_terminated(input)?))
    }
}

pub fn derive_form(tokens: TokenStream) -> Result<TokenStream> {
    let ast: syn::DeriveInput = parse2(tokens)?;
    let ident = &ast.ident;
    let vis = &ast.vis;
    let signal_ident = format_ident!("__{}SignalType", ast.ident);

    let container_args = ast
        .attrs
        .iter()
        .filter(|&attr| attr.path().is_ident(ATTR))
        .map(|attr| attr.parse_args::<PunctWrap<ContainerArg, syn::Token![,]>>())
        .map(|x| x.map(|x| x.0))
        .collect::<Result<Vec<_>>>()?
        .into_iter()
        .flat_map(|x| x.into_iter())
        .collect_vec();

    let container = container_args.iter().find_map(ContainerArg::as_container);
    let is_internal = container_args.iter().any(|carg| carg.is_internal());
    let is_no_labels = container_args.iter().any(|carg| carg.is_no_labels());
    let rename_label_case = container_args.iter().find_map(ContainerArg::as_rename_labels);

    let leptos_krate: syn::Path = parse2(match is_internal {
        true => quote!(::leptos),
        false => quote!(::leptos_form::internal::leptos),
    })?;
    let leptos_form_krate: syn::Path = parse2(match is_internal {
        true => quote!(crate),
        false => quote!(::leptos_form),
    })?;

    let data_struct = match &ast.data {
        syn::Data::Struct(data_struct) => data_struct,
        _ => return Err(Error::new_spanned(ast, "Form can only be derived for structs")),
    };

    let (fields, field_axs): (Vec<_>, Vec<_>) = data_struct
        .fields
        .iter()
        .enumerate()
        .map(|(i, field)| {
            (
                field,
                field
                    .ident
                    .as_ref()
                    .map(|x| quote!(#x))
                    .unwrap_or_else(|| i.to_string().parse().unwrap()),
            )
        })
        .unzip();

    let all_field_args = fields
        .iter()
        .map(|field| {
            Ok(field
                .attrs
                .iter()
                .filter(|&attr| attr.path().is_ident(ATTR))
                .map(|attr| attr.parse_args::<PunctWrap<FieldArg, syn::Token![,]>>())
                .map(|x| x.map(|x| x.0))
                .collect::<Result<Vec<_>>>()?
                .into_iter()
                .flat_map(|x| x.into_iter())
                .collect_vec())
        })
        .collect::<Result<Vec<_>>>()?;

    let field_el_tys = fields
        .iter()
        .enumerate()
        .map(|(i, field)| field_el_ty(&leptos_form_krate, field, &all_field_args[i]))
        .collect::<Result<Vec<_>>>()?;

    let signal_fields = fields
        .iter()
        .enumerate()
        .map(|(i, field)| {
            Ok(syn::Field {
                attrs: vec![],
                vis: syn::Visibility::Inherited,
                mutability: syn::FieldMutability::None,
                ident: field.ident.clone(),
                colon_token: field.colon_token,
                ty: signal_field_ty(&leptos_krate, &leptos_form_krate, field, &field_el_tys[i])?,
            })
        })
        .collect::<Result<Vec<_>>>()?;

    let signal_ty_def = match data_struct.fields {
        syn::Fields::Named(_) => quote!(
            #[derive(Clone, Debug, Default)]
            #vis struct #signal_ident {
                #(#signal_fields,)*
            }
        ),
        syn::Fields::Unnamed(_) => quote!(
            #[derive(Clone, Debug, Default)]
            #vis struct #signal_ident(
                #(#signal_fields,)*
            )
        ),
        syn::Fields::Unit => return Err(Error::new_spanned(ast, "Form cannot be derived on unit types")),
    };

    let props_ident = format_ident!("props");
    let signal_ty_param = format_ident!("__SignalType");

    let (build_props_idents, field_id_idents, field_view_idents, build_props): (Vec<_>, Vec<_>, Vec<_>, Vec<_>) = fields
        .iter()
        .enumerate()
        .map(|(i, field)| {
            let field_ty = &fields[i].ty;
            let field_el_ty = &field_el_tys[i];

            let id = all_field_args[i]
                .iter()
                .find_map(FieldArg::as_id_prefix)
                .map(|x| x.value());

            let class = all_field_args[i]
                .iter()
                .find_map(FieldArg::as_class)
                .into_iter();

            let config = all_field_args[i]
                .iter()
                .find_map(FieldArg::as_config)
                .cloned()
                .unwrap_or_else(|| parse2(quote!(
                    <#field_ty as #leptos_form_krate::FormSignalType<#field_el_ty>>::Config::default()
                )).unwrap());

            let field_ax = &field_axs[i];
            let field_name = field_ax.to_string();
            let field_id = id.as_ref().unwrap_or(&field_name);

            let field_id = if id.is_none() { field_id.to_case(Case::Kebab) } else { field_id.to_string() };

            // TODO: implement serde derived name case conversion

            let build_props_ident = format_ident!("_{field_ax}_props");
            let field_id_ident = format_ident!("_{field_ax}_id");
            let field_name_ident = format_ident!("_{field_ax}_name");
            let field_view_ident = format_ident!("_{field_ax}_view");

            (
                build_props_ident.clone(),
                field_id_ident.clone(),
                field_view_ident.clone(),
                quote!(
                    let #field_id_ident = #leptos_form_krate::format_form_id(
                        #props_ident.id_prefix.as_ref(),
                        #field_id
                    );
                    let #field_name_ident = #leptos_form_krate::format_form_name(
                        #props_ident.name_prefix.as_ref(),
                        #field_name
                    );
                    let #build_props_ident = #leptos_form_krate::RenderProps::builder()
                        .id(#field_id_ident.clone())
                        .name(#field_name_ident.clone())
                        .id_prefix(#field_id_ident.clone())
                        .name_prefix(#field_name_ident)
                        #(.class(#class))*
                        .signal(#props_ident.signal)
                        .ref_ax(#leptos_form_krate::ref_ax_factory(move |t: &#signal_ty_param| &(#props_ident.ref_ax)(t).#field_ax))
                        .mut_ax(#leptos_form_krate::mut_ax_factory(move |t: &mut #signal_ty_param| &mut (#props_ident.mut_ax)(t).#field_ax))
                        .config(#config)
                        .build();

                    let #field_view_ident = <#field_ty as #leptos_form_krate::FormComponent<#signal_ty_param, #field_el_ty>>::render(#build_props_ident);
                ),
            )
        })
        .multiunzip();

    let labels = field_axs
        .iter()
        .enumerate()
        .map(|(i, field_ax)| {
            if is_no_labels {
                return None;
            }
            if all_field_args[i].iter().any(FieldArg::is_no_label) {
                return None;
            }
            all_field_args[i]
                .iter()
                .find_map(FieldArg::as_label)
                .map(|x| x.value())
                .or_else(|| {
                    Some(match rename_label_case {
                        Some(case) => field_ax.to_string().to_case(*case),
                        None => field_ax.to_string(),
                    })
                })
        })
        .collect_vec();

    let label_classes = labels
        .iter()
        .enumerate()
        .map(|(i, label)| {
            label
                .as_ref()
                .and_then(|label| all_field_args[i].iter().find_map(FieldArg::as_label_class))
        })
        .collect_vec();

    let wrapped_field_views = (0..fields.len()).map(|i| {
        let label = &labels[i];
        let label_class = label_classes[i].as_ref().into_iter();
        let field_id_ident = &field_id_idents[i];

        let build_props_ident = &build_props_idents[i];
        let field_el_ty = &field_el_tys[i];

        let field_view_ident = &field_view_idents[i];

        match label {
            Some(label) => quote!(
                <label for={#field_id_ident} #(class=#label_class)*>
                    #label
                    {#field_view_ident}
                </label>
            ),
            None => quote!({#field_view_ident}),
        }
    });

    let tokens = quote!(
        #signal_ty_def

        impl ::core::convert::AsRef<#signal_ident> for #signal_ident {
            fn as_ref(&self) -> &Self {
                self
            }
        }

        impl ::core::convert::AsMut<#signal_ident> for #signal_ident {
            fn as_mut(&mut self) -> &mut Self {
                self
            }
        }

        impl #leptos_form_krate::DefaultHtmlElement for #ident {
            type El = #leptos_krate::View;
        }

        impl<El> #leptos_form_krate::FormSignalType<El> for #ident {
            type Config = ();
            type SignalType = #signal_ident;
            fn into_signal_type(self, config: Self::Config) -> Self::SignalType {
                todo!()
            }
            fn try_from_signal_type(signal_type: Self::SignalType, config: Self::Config) -> Result<Self, FormError> {
                todo!()
            }
        }

        impl<#signal_ty_param: 'static> #leptos_form_krate::FormComponent<#signal_ty_param, #leptos_krate::View> for #ident {
            fn render(#props_ident: #leptos_form_krate::RenderProps<
                #signal_ty_param,
                impl #leptos_form_krate::RefAccessor<#signal_ty_param, Self::SignalType>,
                impl #leptos_form_krate::MutAccessor<#signal_ty_param, Self::SignalType>,
                Self::Config,
            >) -> impl #leptos_krate::IntoView {
                use #leptos_krate::{IntoAttribute, IntoView};

                #(#build_props)*

                #leptos_krate::view! {
                    #(#wrapped_field_views)*
                }
            }
        }
    );

    Ok(tokens)
}

fn field_el_ty(leptos_form_krate: &syn::Path, field: &syn::Field, field_args: &[FieldArg]) -> Result<syn::Type> {
    let ty = &field.ty;
    field_args
        .iter()
        .find_map(FieldArg::as_el)
        .map(|x| Ok(x.clone()))
        .unwrap_or_else(|| parse2(quote!(<#ty as #leptos_form_krate::DefaultHtmlElement>::El)))
}

fn signal_field_ty(
    leptos_krate: &syn::Path,
    leptos_form_krate: &syn::Path,
    field: &syn::Field,
    el_ty: &syn::Type,
) -> Result<syn::Type> {
    let ty = &field.ty;
    parse2(quote!(<#ty as #leptos_form_krate::FormSignalType<#el_ty>>::SignalType))
}

fn parse_case(lit_str: syn::LitStr) -> Result<Case> {
    Ok(match &*lit_str.value() {
        "UPPER CASE" => Case::Upper,
        "lower case" => Case::Lower,
        "Title Case" => Case::Title,
        "tOGGLE cASE" => Case::Toggle,
        "camelCase" => Case::Camel,
        "PascalCase" | "UpperCamelCase" => Case::Pascal,
        "snake_case" => Case::Snake,
        "UPPER_SNAKE_CASE" | "SCREAMING_SNAKE_CASE" => Case::UpperSnake,
        "kebab-case" => Case::Kebab,
        "COBOL-CASE" | "UPPER-KEBAB-CASE" | "SCREAMING-KEBAB-CASE" => Case::Cobol,
        "Train-Case" => Case::Train,
        "flatcase" => Case::Flat,
        "UPPERFLATCASE" | "SCREAMINGFLATCASE" => Case::UpperFlat,
        "aLtErNaTiNg CaSe" => Case::Alternating,
        _ => return Err(Error::new_spanned(lit_str, "unsupported label case")),
    })
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::assert_eq_text;

    #[test]
    fn test1() -> Result<()> {
        let input = quote!(
            #[derive(Form)]
            #[form(rename_labels = "Title Case")]
            pub struct MyFormData {
                pub id: Uuid,
                pub slug: String,
                pub created_at: chrono::NaiveDateTime,
                pub count: u8,
            }
        );

        let leptos_krate = quote!(::leptos_form::internal::leptos);
        let leptos_form_krate = quote!(::leptos_form);

        let expected = quote!(
            #[derive(Clone, Debug, Default)]
            pub struct __MyFormDataSignalType {
                id: <Uuid as #leptos_form_krate::FormSignalType<<Uuid as #leptos_form_krate::DefaultHtmlElement>::El>>::SignalType,
                slug: <String as #leptos_form_krate::FormSignalType<<String as #leptos_form_krate::DefaultHtmlElement>::El>>::SignalType,
                created_at: <chrono::NaiveDateTime as #leptos_form_krate::FormSignalType<<chrono::NaiveDateTime as #leptos_form_krate::DefaultHtmlElement>::El>>::SignalType,
                count: <u8 as #leptos_form_krate::FormSignalType<<u8 as #leptos_form_krate::DefaultHtmlElement>::El>>::SignalType,
            }

            impl ::core::convert::AsRef<__MyFormDataSignalType> for __MyFormDataSignalType {
                fn as_ref(&self) -> &Self {
                    self
                }
            }

            impl ::core::convert::AsMut<__MyFormDataSignalType> for __MyFormDataSignalType {
                fn as_mut(&mut self) -> &mut Self {
                    self
                }
            }

            impl #leptos_form_krate::DefaultHtmlElement for MyFormData {
                type El = #leptos_krate::View;
            }

            impl<El> #leptos_form_krate::FormSignalType<El> for MyFormData {
                type Config = ();
                type SignalType = __MyFormDataSignalType;
                fn into_signal_type(self, config: Self::Config) -> Self::SignalType {
                    todo!()
                }
                fn try_from_signal_type(signal_type: Self::SignalType, config: Self::Config) -> Result<Self, FormError> {
                    todo!()
                }
            }

            impl<__SignalType: 'static> #leptos_form_krate::FormComponent<__SignalType, #leptos_krate::View> for MyFormData {
                fn render(props: #leptos_form_krate::RenderProps<
                    __SignalType,
                    impl #leptos_form_krate::RefAccessor<__SignalType, Self::SignalType>,
                    impl #leptos_form_krate::MutAccessor<__SignalType, Self::SignalType>,
                    Self::Config,
                >) -> impl #leptos_krate::IntoView {
                    use #leptos_krate::{IntoAttribute, IntoView};

                    let _id_id = #leptos_form_krate::format_form_id(props.id_prefix.as_ref(), "id");
                    let _id_name = #leptos_form_krate::format_form_name(props.name_prefix.as_ref(), "id");
                    let _id_props = #leptos_form_krate::RenderProps::builder()
                        .id(_id_id.clone())
                        .name(_id_name.clone())
                        .id_prefix(_id_id.clone())
                        .name_prefix(_id_name)
                        .signal(props.signal)
                        .ref_ax(#leptos_form_krate::ref_ax_factory(move |t: &__SignalType| &(props.ref_ax)(t).id))
                        .mut_ax(#leptos_form_krate::mut_ax_factory(move |t: &mut __SignalType| &mut (props.mut_ax)(t).id))
                        .config(<Uuid as #leptos_form_krate::FormSignalType<<Uuid as #leptos_form_krate::DefaultHtmlElement>::El>>::Config::default())
                        .build();
                    let _id_view = <Uuid as #leptos_form_krate::FormComponent<__SignalType, <Uuid as #leptos_form_krate::DefaultHtmlElement>::El>>::render(_id_props);

                    let _slug_id = #leptos_form_krate::format_form_id(props.id_prefix.as_ref(), "slug");
                    let _slug_name = #leptos_form_krate::format_form_name(props.name_prefix.as_ref(), "slug");
                    let _slug_props = #leptos_form_krate::RenderProps::builder()
                        .id(_slug_id.clone())
                        .name(_slug_name.clone())
                        .id_prefix(_slug_id.clone())
                        .name_prefix(_slug_name)
                        .signal(props.signal)
                        .ref_ax(#leptos_form_krate::ref_ax_factory(move |t: &__SignalType| &(props.ref_ax)(t).slug))
                        .mut_ax(#leptos_form_krate::mut_ax_factory(move |t: &mut __SignalType| &mut (props.mut_ax)(t).slug))
                        .config(<String as #leptos_form_krate::FormSignalType<<String as #leptos_form_krate::DefaultHtmlElement>::El>>::Config::default())
                        .build();
                    let _slug_view = <String as #leptos_form_krate::FormComponent<__SignalType, <String as #leptos_form_krate::DefaultHtmlElement>::El>>::render(_slug_props);

                    let _created_at_id = #leptos_form_krate::format_form_id(props.id_prefix.as_ref(), "created-at");
                    let _created_at_name = #leptos_form_krate::format_form_name(props.name_prefix.as_ref(), "created_at");
                    let _created_at_props = #leptos_form_krate::RenderProps::builder()
                        .id(_created_at_id.clone())
                        .name(_created_at_name.clone())
                        .id_prefix(_created_at_id.clone())
                        .name_prefix(_created_at_name)
                        .signal(props.signal)
                        .ref_ax(#leptos_form_krate::ref_ax_factory(move |t: &__SignalType| &(props.ref_ax)(t).created_at))
                        .mut_ax(#leptos_form_krate::mut_ax_factory(move |t: &mut __SignalType| &mut (props.mut_ax)(t).created_at))
                        .config(<chrono::NaiveDateTime as #leptos_form_krate::FormSignalType<<chrono::NaiveDateTime as #leptos_form_krate::DefaultHtmlElement>::El>>::Config::default())
                        .build();
                    let _created_at_view = <chrono::NaiveDateTime as #leptos_form_krate::FormComponent<__SignalType, <chrono::NaiveDateTime as #leptos_form_krate::DefaultHtmlElement>::El>>::render(_created_at_props);

                    let _count_id = #leptos_form_krate::format_form_id(props.id_prefix.as_ref(), "count");
                    let _count_name = #leptos_form_krate::format_form_name(props.name_prefix.as_ref(), "count");
                    let _count_props = #leptos_form_krate::RenderProps::builder()
                        .id(_count_id.clone())
                        .name(_count_name.clone())
                        .id_prefix(_count_id.clone())
                        .name_prefix(_count_name)
                        .signal(props.signal)
                        .ref_ax(#leptos_form_krate::ref_ax_factory(move |t: &__SignalType| &(props.ref_ax)(t).count))
                        .mut_ax(#leptos_form_krate::mut_ax_factory(move |t: &mut __SignalType| &mut (props.mut_ax)(t).count))
                        .config(<u8 as #leptos_form_krate::FormSignalType<<u8 as #leptos_form_krate::DefaultHtmlElement>::El>>::Config::default())
                        .build();
                    let _count_view = <u8 as #leptos_form_krate::FormComponent<__SignalType, <u8 as #leptos_form_krate::DefaultHtmlElement>::El>>::render(_count_props);

                    #leptos_krate::view! {
                        <label for={_id_id}>
                            "Id"
                            {_id_view}
                        </label>
                        <label for={_slug_id}>
                            "Slug"
                            {_slug_view}
                        </label>
                        <label for={_created_at_id}>
                            "Created At"
                            {_created_at_view}
                        </label>
                        <label for={_count_id}>
                            "Count"
                            {_count_view}
                        </label>
                    }
                }
            }
        );

        let output = derive_form(input)?;

        let expected = cleanup(&expected);
        let output = cleanup(&output);

        let expected = pretty(expected)?;
        let output = pretty(output)?;

        assert_eq_text!(expected, output);

        Ok(())
    }

    #[test]
    fn test2() -> Result<()> {
        let input = quote!(
            #[derive(Form)]
            pub struct MyFormData {
                #[form(class = "hi", id_prefix = "hello-there", label = "AYO", label_class = "test")]
                pub abc_123: Uuid,
                #[form(no_label)]
                pub zz: u8,
            }
        );

        let leptos_krate = quote!(::leptos_form::internal::leptos);
        let leptos_form_krate = quote!(::leptos_form);

        let expected = quote!(
            #[derive(Clone, Debug, Default)]
            pub struct __MyFormDataSignalType {
                abc_123: <Uuid as #leptos_form_krate::FormSignalType<<Uuid as #leptos_form_krate::DefaultHtmlElement>::El>>::SignalType,
                zz: <u8 as #leptos_form_krate::FormSignalType<<u8 as #leptos_form_krate::DefaultHtmlElement>::El>>::SignalType,
            }

            impl ::core::convert::AsRef<__MyFormDataSignalType> for __MyFormDataSignalType {
                fn as_ref(&self) -> &Self {
                    self
                }
            }

            impl ::core::convert::AsMut<__MyFormDataSignalType> for __MyFormDataSignalType {
                fn as_mut(&mut self) -> &mut Self {
                    self
                }
            }

            impl #leptos_form_krate::DefaultHtmlElement for MyFormData {
                type El = #leptos_krate::View;
            }

            impl<El> #leptos_form_krate::FormSignalType<El> for MyFormData {
                type Config = ();
                type SignalType = __MyFormDataSignalType;
                fn into_signal_type(self, config: Self::Config) -> Self::SignalType {
                    todo!()
                }
                fn try_from_signal_type(signal_type: Self::SignalType, config: Self::Config) -> Result<Self, FormError> {
                    todo!()
                }
            }

            impl<__SignalType: 'static> #leptos_form_krate::FormComponent<__SignalType, #leptos_krate::View> for MyFormData {
                fn render(props: #leptos_form_krate::RenderProps<
                    __SignalType,
                    impl #leptos_form_krate::RefAccessor<__SignalType, Self::SignalType>,
                    impl #leptos_form_krate::MutAccessor<__SignalType, Self::SignalType>,
                    Self::Config,
                >) -> impl #leptos_krate::IntoView {
                    use #leptos_krate::{IntoAttribute, IntoView};

                    let _abc_123_id = #leptos_form_krate::format_form_id(props.id_prefix.as_ref(), "hello-there");
                    let _abc_123_name = #leptos_form_krate::format_form_name(props.name_prefix.as_ref(), "abc_123");
                    let _abc_123_props = #leptos_form_krate::RenderProps::builder()
                        .id(_abc_123_id.clone())
                        .name(_abc_123_name.clone())
                        .id_prefix(_abc_123_id.clone())
                        .name_prefix(_abc_123_name)
                        .class("hi")
                        .signal(props.signal)
                        .ref_ax(#leptos_form_krate::ref_ax_factory(move |t: &__SignalType| &(props.ref_ax)(t).abc_123))
                        .mut_ax(#leptos_form_krate::mut_ax_factory(move |t: &mut __SignalType| &mut (props.mut_ax)(t).abc_123))
                        .config(<Uuid as #leptos_form_krate::FormSignalType<<Uuid as #leptos_form_krate::DefaultHtmlElement>::El>>::Config::default())
                        .build();
                    let _abc_123_view = <Uuid as #leptos_form_krate::FormComponent<__SignalType, <Uuid as #leptos_form_krate::DefaultHtmlElement>::El>>::render(_abc_123_props);

                    let _zz_id = #leptos_form_krate::format_form_id(props.id_prefix.as_ref(), "zz");
                    let _zz_name = #leptos_form_krate::format_form_name(props.name_prefix.as_ref(), "zz");
                    let _zz_props = #leptos_form_krate::RenderProps::builder()
                        .id(_zz_id.clone())
                        .name(_zz_name.clone())
                        .id_prefix(_zz_id.clone())
                        .name_prefix(_zz_name)
                        .signal(props.signal)
                        .ref_ax(#leptos_form_krate::ref_ax_factory(move |t: &__SignalType| &(props.ref_ax)(t).zz))
                        .mut_ax(#leptos_form_krate::mut_ax_factory(move |t: &mut __SignalType| &mut (props.mut_ax)(t).zz))
                        .config(<u8 as #leptos_form_krate::FormSignalType<<u8 as #leptos_form_krate::DefaultHtmlElement>::El>>::Config::default())
                        .build();
                    let _zz_view = <u8 as #leptos_form_krate::FormComponent<__SignalType, <u8 as #leptos_form_krate::DefaultHtmlElement>::El>>::render(_zz_props);

                    #leptos_krate::view! {
                        <label for={_abc_123_id} class="test">
                            "AYO"
                            {_abc_123_view}
                        </label>
                        {_zz_view}
                    }
                }
            }
        );

        let output = derive_form(input)?;

        let expected = cleanup(&expected);
        let output = cleanup(&output);

        let expected = pretty(expected)?;
        let output = pretty(output)?;

        assert_eq_text!(expected, output);

        Ok(())
    }

    pub fn cleanup(tokens: &TokenStream) -> String {
        let tokens = tokens.to_string();

        tokens.replace("< <", "<<").replace("> >", ">>")
    }

    pub fn pretty(cleaned_up: String) -> Result<String> {
        let syntax_tree = syn::parse_file(&cleaned_up)?;
        Ok(prettyplease::unparse(&syntax_tree))
    }
}
