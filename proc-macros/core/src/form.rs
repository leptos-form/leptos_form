#![allow(unused)]

use ::convert_case::*;
use ::derive_more::*;
use ::itertools::Itertools;
use ::proc_macro2::{Span, TokenStream};
use ::quote::{format_ident, quote, ToTokens, TokenStreamExt};
use ::syn::parse::{Error, Parse, ParseStream, Result};
use ::syn::parse2;
use ::syn::punctuated::Punctuated;

static ATTR: &str = "form";

#[derive(Clone, IsVariant)]
enum ContainerArg {
    Component(Component),
    Container(TokenStream),
    Internal,
    NoLabels,
    RenameLabels(Case),
}

#[derive(Clone)]
enum Action {
    Path(syn::Path),
    Url(syn::LitStr),
}

#[derive(Clone)]
struct Component {
    action: Action,
    id: Option<syn::LitStr>,
    class: Option<syn::LitStr>,
    kind: ComponentKind,
}

#[derive(Clone, Debug, IsVariant)]
enum ComponentKind {
    Component,
    Island,
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

#[derive(Clone)]
struct PunctWrap<T, P>(Punctuated<T, P>);

pub fn derive_form(tokens: TokenStream) -> Result<TokenStream> {
    let ast: syn::DeriveInput = parse2(tokens)?;
    let ident = &ast.ident;
    let vis = &ast.vis;
    let signal_ident = format_ident!("__{ident}SignalType");
    let config_ident = format_ident!("__{ident}Config");

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

    let component = container_args.iter().find_map(ContainerArg::as_component);
    let container = container_args.iter().find_map(ContainerArg::as_container);
    let is_internal = container_args.iter().any(|carg| carg.is_internal());
    let is_no_labels = container_args.iter().any(|carg| carg.is_no_labels());
    let rename_label_case = container_args.iter().find_map(ContainerArg::as_rename_labels);

    let leptos_krate: syn::Path = parse2(match is_internal {
        true => quote!(::leptos),
        false => quote!(::leptos_form::internal::leptos),
    })?;
    let leptos_router_krate: syn::Path = parse2(match is_internal {
        true => quote!(::leptos_router),
        false => quote!(::leptos_form::internal::leptos_router),
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

    let field_tys = fields.iter().map(|field| &field.ty).collect_vec();

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
                vis: syn::Visibility::Public(Default::default()),
                mutability: syn::FieldMutability::None,
                ident: field.ident.clone(),
                colon_token: field.colon_token,
                ty: signal_field_ty(&leptos_krate, &leptos_form_krate, field, &field_el_tys[i])?,
            })
        })
        .collect::<Result<Vec<_>>>()?;

    let configs = all_field_args
        .iter()
        .enumerate()
        .map(|(i, field_args)| {
            field_args
                .iter()
                .find_map(FieldArg::as_config)
                .cloned()
                .unwrap_or_else(|| {
                    let field_ty = &field_tys[i];
                    let field_el_ty = &field_el_tys[i];
                    parse2(quote!(
                        <#field_ty as #leptos_form_krate::FormSignalType<#field_el_ty>>::Config::default()
                    ))
                    .unwrap()
                })
        })
        .collect_vec();

    let config_var_ident = format_ident!("config");
    let (config_def, config_value, signal_ty_def, signal_constructor, try_constructor) = match data_struct.fields {
        syn::Fields::Named(_) => (
            quote!(
                #[derive(Clone, Debug, Default)]
                pub struct #config_ident {
                    #(pub #field_axs: <#field_tys as #leptos_form_krate::FormSignalType<#field_el_tys>>::Config,)*
                }
            ),
            quote!(
                #config_ident {
                    #(#field_axs: #configs,)*
                }
            ),
            quote!(
                #[derive(Clone, Debug, Default)]
                #vis struct #signal_ident {
                    #(#signal_fields,)*
                }
            ),
            quote!(
                #signal_ident {
                    #(#field_axs: <#field_tys as #leptos_form_krate::FormSignalType<#field_el_tys>>::into_signal_type(self.#field_axs, &#config_var_ident.#field_axs) ,)*
                }
            ),
            quote!(
                Ok(#ident {
                    #(#field_axs: <#field_tys as #leptos_form_krate::FormSignalType<#field_el_tys>>::try_from_signal_type(signal_type.#field_axs, &#config_var_ident.#field_axs)? ,)*
                })
            ),
        ),
        syn::Fields::Unnamed(_) => (
            quote!(
                #[derive(Clone, Debug, Default)]
                pub struct #config_ident(
                    #(pub <#field_tys as #leptos_form_krate::FormSignalType<#field_el_tys>>::Config,)*
                );
            ),
            quote!(
                #config_ident(#(#configs,)*)
            ),
            quote!(
                #[derive(Clone, Debug, Default)]
                #vis struct #signal_ident(
                    #(#signal_fields,)*
                )
            ),
            quote!(
                #signal_ident(
                    #(<#field_tys as #leptos_form_krate::FormSignalType<#field_el_tys>>::into_signal_type(self.#field_axs, &#config_var_ident.#field_axs) ,)*
                )
            ),
            quote!(
                Ok(#ident(
                    #(<#field_tys as #leptos_form_krate::FormSignalType<#field_el_tys>>::try_from_signal_type(signal_type.#field_axs, &#config_var_ident.#field_axs)? ,)*
                ))
            ),
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
                        #props_ident.id.as_ref(),
                        #field_id
                    );
                    let #field_name_ident = #leptos_form_krate::format_form_name(
                        Some(&#props_ident.name),
                        #field_name
                    );
                    let #build_props_ident = #leptos_form_krate::RenderProps::builder()
                        .id(#field_id_ident.clone())
                        .name(#field_name_ident.clone())
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

    let component_tokens = component.map(|Component { action, kind, id, class }| {
        let id = id.iter();
        let class = class.iter();

        let (tag_import, action_def, open_tag, close_tag) = match action {
            Action::Path(action) => (
                quote!(use #leptos_router_krate::ActionForm;),
                Some(quote!(let action = #leptos_krate::create_server_action::<#action>();)),
                quote!(<ActionForm action=action #(id=#id)* #(class=#class)*>),
                quote!(</ActionForm>),
            ),
            Action::Url(action) => (
                quote!(use #leptos_router_krate::Form;),
                None,
                quote!(<Form action=#action #(id=#id)* #(class=#class)*>),
                quote!(</Form>),
            ),
        };
        let action_def = action_def.into_iter();

        let mod_ident = format_ident!("{}", format!("leptos_form_component_{ident}").to_case(Case::Snake));

        let pound = "#".parse::<TokenStream>().unwrap();
        let tokens = quote!(
            #vis use #mod_ident::*;

            #[allow(unused_imports)]
            mod #mod_ident {
                use super::*;
                use #leptos_form_krate::FormSignalType;
                use #leptos_krate::IntoView;
                #tag_import

                #pound[#leptos_krate::#kind]
                #vis fn #ident(initial: #ident) -> impl IntoView {
                    #(#action_def)*

                    let config = #config_value;

                    let signal = #leptos_krate::create_rw_signal(initial.into_signal_type(&config));
                    let props = #leptos_form_krate::RenderProps::builder()
                        .id(None)
                        .name("")
                        .signal(signal)
                        .ref_ax(#leptos_form_krate::ref_ax_factory(|x| x))
                        .mut_ax(#leptos_form_krate::mut_ax_factory(|x| x))
                        .config(config)
                        .build();

                    let view = <#ident as #leptos_form_krate::FormComponent<#signal_ident, #leptos_krate::View>>::render(props);

                    #leptos_krate::view! {
                        #open_tag
                            {view}
                        #close_tag
                    }
                }
            }
        );
        tokens
    });

    let tokens = quote!(
        #signal_ty_def

        #config_def

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

        impl #leptos_form_krate::FormSignalType<#leptos_krate::View> for #ident {
            type Config = #config_ident;
            type SignalType = #signal_ident;
            fn into_signal_type(self, #config_var_ident: &Self::Config) -> Self::SignalType {
                #signal_constructor
            }
            fn try_from_signal_type(signal_type: Self::SignalType, #config_var_ident: &Self::Config) -> Result<Self, #leptos_form_krate::FormError> {
                #try_constructor
            }
        }

        impl<#signal_ty_param: 'static> #leptos_form_krate::FormComponent<#signal_ty_param, #leptos_krate::View> for #ident {
            #[allow(unused_imports)]
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

        #component_tokens
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

impl ContainerArg {
    fn as_component(&self) -> Option<&Component> {
        match self {
            Self::Component(component) => Some(component),
            _ => None,
        }
    }
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
            Ok(match &*ident.to_string() {
                "component" => Self::Component(parse_component_args(input, ComponentKind::Component)?),
                "container" => {
                    // base line syntax check that the provided input stream
                    // looks approximately like an html element
                    let fork = input.fork();
                    fork.parse::<syn::Token![<]>()?;
                    fork.parse::<syn::Ident>()?;
                    Self::Container(input.parse()?)
                }
                "island" => Self::Component(parse_component_args(input, ComponentKind::Island)?),
                "internal" => Self::Internal,
                "no_labels" => Self::NoLabels,
                "rename_labels" => {
                    input.parse::<syn::Token![=]>()?;
                    let case = input.parse::<syn::LitStr>()?;
                    Self::RenameLabels(parse_case(case)?)
                }
                _ => {
                    return Err(Error::new_spanned(
                        &ident,
                        format!("unrecognized form container attribute: `{ident}`"),
                    ))
                }
            })
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

impl Parse for Action {
    fn parse(input: ParseStream<'_>) -> Result<Self> {
        let lookahead = input.lookahead1();
        if lookahead.peek(syn::LitStr) {
            Ok(Self::Url(input.parse()?))
        } else if lookahead.peek(syn::Ident) || lookahead.peek(syn::Token![::]) {
            Ok(Self::Path(input.parse()?))
        } else {
            Err(lookahead.error())
        }
    }
}

impl ToTokens for ComponentKind {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            Self::Component => tokens.append(proc_macro2::Ident::new("component", Span::call_site())),
            Self::Island => tokens.append(proc_macro2::Ident::new("island", Span::call_site())),
        }
    }
}

enum ComponentArg {
    Action(Action),
    Id(syn::LitStr),
    Class(syn::LitStr),
}

impl Parse for ComponentArg {
    fn parse(input: ParseStream<'_>) -> Result<Self> {
        let lookahead = input.lookahead1();
        if lookahead.peek(syn::Ident) {
            let ident = input.parse::<syn::Ident>()?;
            Ok(match &*ident.to_string() {
                "action" => {
                    input.parse::<syn::Token![=]>()?;
                    Self::Action(input.parse()?)
                }
                "class" => {
                    input.parse::<syn::Token![=]>()?;
                    Self::Class(input.parse()?)
                }
                "id" => {
                    input.parse::<syn::Token![=]>()?;
                    Self::Id(input.parse()?)
                }
                _ => return Err(Error::new_spanned(ident, "unrecognized form component arg")),
            })
        } else {
            Err(lookahead.error())
        }
    }
}

fn parse_component_args(input: ParseStream<'_>, kind: ComponentKind) -> Result<Component> {
    let error_span = input.span();
    let content;
    syn::parenthesized!(content in input);

    let mut action = None;
    let mut id = None;
    let mut class = None;

    for component_arg in content.parse_terminated(ComponentArg::parse, syn::Token![,])? {
        match component_arg {
            ComponentArg::Action(x) => action = Some(x),
            ComponentArg::Id(lit_str) => id = Some(lit_str),
            ComponentArg::Class(lit_str) => class = Some(lit_str),
        }
    }

    Ok(Component {
        kind,
        id,
        class,
        action: action.ok_or_else(|| Error::new(error_span, "action must be specified"))?,
    })
}

impl<T: Parse, P: Parse> Parse for PunctWrap<T, P> {
    fn parse(input: ParseStream<'_>) -> Result<Self> {
        Ok(Self(Punctuated::<T, P>::parse_terminated(input)?))
    }
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
                pub id: <Uuid as #leptos_form_krate::FormSignalType<<Uuid as #leptos_form_krate::DefaultHtmlElement>::El>>::SignalType,
                pub slug: <String as #leptos_form_krate::FormSignalType<<String as #leptos_form_krate::DefaultHtmlElement>::El>>::SignalType,
                pub created_at: <chrono::NaiveDateTime as #leptos_form_krate::FormSignalType<<chrono::NaiveDateTime as #leptos_form_krate::DefaultHtmlElement>::El>>::SignalType,
                pub count: <u8 as #leptos_form_krate::FormSignalType<<u8 as #leptos_form_krate::DefaultHtmlElement>::El>>::SignalType,
            }

            #[derive(Clone, Debug, Default)]
            pub struct __MyFormDataConfig {
                pub id: <Uuid as #leptos_form_krate::FormSignalType<<Uuid as #leptos_form_krate::DefaultHtmlElement>::El>>::Config,
                pub slug: <String as #leptos_form_krate::FormSignalType<<String as #leptos_form_krate::DefaultHtmlElement>::El>>::Config,
                pub created_at: <chrono::NaiveDateTime as #leptos_form_krate::FormSignalType<<chrono::NaiveDateTime as #leptos_form_krate::DefaultHtmlElement>::El>>::Config,
                pub count: <u8 as #leptos_form_krate::FormSignalType<<u8 as #leptos_form_krate::DefaultHtmlElement>::El>>::Config,
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

            impl #leptos_form_krate::FormSignalType<#leptos_krate::View> for MyFormData {
                type Config = __MyFormDataConfig;
                type SignalType = __MyFormDataSignalType;
                fn into_signal_type(self, config: &Self::Config) -> Self::SignalType {
                    __MyFormDataSignalType {
                        id: <Uuid as #leptos_form_krate::FormSignalType<<Uuid as #leptos_form_krate::DefaultHtmlElement>::El>>::into_signal_type(self.id, &config.id),
                        slug: <String as #leptos_form_krate::FormSignalType<<String as #leptos_form_krate::DefaultHtmlElement>::El>>::into_signal_type(self.slug, &config.slug),
                        created_at: <chrono::NaiveDateTime as #leptos_form_krate::FormSignalType<<chrono::NaiveDateTime as #leptos_form_krate::DefaultHtmlElement>::El>>::into_signal_type(self.created_at, &config.created_at),
                        count: <u8 as #leptos_form_krate::FormSignalType<<u8 as #leptos_form_krate::DefaultHtmlElement>::El>>::into_signal_type(self.count, &config.count),
                    }
                }
                fn try_from_signal_type(signal_type: Self::SignalType, config: &Self::Config) -> Result<Self, #leptos_form_krate::FormError> {
                    Ok(MyFormData {
                        id: <Uuid as #leptos_form_krate::FormSignalType<<Uuid as #leptos_form_krate::DefaultHtmlElement>::El>>::try_from_signal_type(signal_type.id, &config.id)?,
                        slug: <String as #leptos_form_krate::FormSignalType<<String as #leptos_form_krate::DefaultHtmlElement>::El>>::try_from_signal_type(signal_type.slug, &config.slug)?,
                        created_at: <chrono::NaiveDateTime as #leptos_form_krate::FormSignalType<<chrono::NaiveDateTime as #leptos_form_krate::DefaultHtmlElement>::El>>::try_from_signal_type(signal_type.created_at, &config.created_at)?,
                        count: <u8 as #leptos_form_krate::FormSignalType<<u8 as #leptos_form_krate::DefaultHtmlElement>::El>>::try_from_signal_type(signal_type.count, &config.count)?,
                    })
                }
            }

            impl<__SignalType: 'static> #leptos_form_krate::FormComponent<__SignalType, #leptos_krate::View> for MyFormData {
                #[allow(unused_imports)]
                fn render(props: #leptos_form_krate::RenderProps<
                    __SignalType,
                    impl #leptos_form_krate::RefAccessor<__SignalType, Self::SignalType>,
                    impl #leptos_form_krate::MutAccessor<__SignalType, Self::SignalType>,
                    Self::Config,
                >) -> impl #leptos_krate::IntoView {
                    use #leptos_krate::{IntoAttribute, IntoView};

                    let _id_id = #leptos_form_krate::format_form_id(props.id.as_ref(), "id");
                    let _id_name = #leptos_form_krate::format_form_name(Some(&props.name), "id");
                    let _id_props = #leptos_form_krate::RenderProps::builder()
                        .id(_id_id.clone())
                        .name(_id_name.clone())
                        .signal(props.signal)
                        .ref_ax(#leptos_form_krate::ref_ax_factory(move |t: &__SignalType| &(props.ref_ax)(t).id))
                        .mut_ax(#leptos_form_krate::mut_ax_factory(move |t: &mut __SignalType| &mut (props.mut_ax)(t).id))
                        .config(<Uuid as #leptos_form_krate::FormSignalType<<Uuid as #leptos_form_krate::DefaultHtmlElement>::El>>::Config::default())
                        .build();
                    let _id_view = <Uuid as #leptos_form_krate::FormComponent<__SignalType, <Uuid as #leptos_form_krate::DefaultHtmlElement>::El>>::render(_id_props);

                    let _slug_id = #leptos_form_krate::format_form_id(props.id.as_ref(), "slug");
                    let _slug_name = #leptos_form_krate::format_form_name(Some(&props.name), "slug");
                    let _slug_props = #leptos_form_krate::RenderProps::builder()
                        .id(_slug_id.clone())
                        .name(_slug_name.clone())
                        .signal(props.signal)
                        .ref_ax(#leptos_form_krate::ref_ax_factory(move |t: &__SignalType| &(props.ref_ax)(t).slug))
                        .mut_ax(#leptos_form_krate::mut_ax_factory(move |t: &mut __SignalType| &mut (props.mut_ax)(t).slug))
                        .config(<String as #leptos_form_krate::FormSignalType<<String as #leptos_form_krate::DefaultHtmlElement>::El>>::Config::default())
                        .build();
                    let _slug_view = <String as #leptos_form_krate::FormComponent<__SignalType, <String as #leptos_form_krate::DefaultHtmlElement>::El>>::render(_slug_props);

                    let _created_at_id = #leptos_form_krate::format_form_id(props.id.as_ref(), "created-at");
                    let _created_at_name = #leptos_form_krate::format_form_name(Some(&props.name), "created_at");
                    let _created_at_props = #leptos_form_krate::RenderProps::builder()
                        .id(_created_at_id.clone())
                        .name(_created_at_name.clone())
                        .signal(props.signal)
                        .ref_ax(#leptos_form_krate::ref_ax_factory(move |t: &__SignalType| &(props.ref_ax)(t).created_at))
                        .mut_ax(#leptos_form_krate::mut_ax_factory(move |t: &mut __SignalType| &mut (props.mut_ax)(t).created_at))
                        .config(<chrono::NaiveDateTime as #leptos_form_krate::FormSignalType<<chrono::NaiveDateTime as #leptos_form_krate::DefaultHtmlElement>::El>>::Config::default())
                        .build();
                    let _created_at_view = <chrono::NaiveDateTime as #leptos_form_krate::FormComponent<__SignalType, <chrono::NaiveDateTime as #leptos_form_krate::DefaultHtmlElement>::El>>::render(_created_at_props);

                    let _count_id = #leptos_form_krate::format_form_id(props.id.as_ref(), "count");
                    let _count_name = #leptos_form_krate::format_form_name(Some(&props.name), "count");
                    let _count_props = #leptos_form_krate::RenderProps::builder()
                        .id(_count_id.clone())
                        .name(_count_name.clone())
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
                pub abc_123: <Uuid as #leptos_form_krate::FormSignalType<<Uuid as #leptos_form_krate::DefaultHtmlElement>::El>>::SignalType,
                pub zz: <u8 as #leptos_form_krate::FormSignalType<<u8 as #leptos_form_krate::DefaultHtmlElement>::El>>::SignalType,
            }

            #[derive(Clone, Debug, Default)]
            pub struct __MyFormDataConfig {
                pub abc_123: <Uuid as #leptos_form_krate::FormSignalType<<Uuid as #leptos_form_krate::DefaultHtmlElement>::El>>::Config,
                pub zz: <u8 as #leptos_form_krate::FormSignalType<<u8 as #leptos_form_krate::DefaultHtmlElement>::El>>::Config,
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

            impl #leptos_form_krate::FormSignalType<#leptos_krate::View> for MyFormData {
                type Config = __MyFormDataConfig;
                type SignalType = __MyFormDataSignalType;
                fn into_signal_type(self, config: &Self::Config) -> Self::SignalType {
                    __MyFormDataSignalType {
                        abc_123: <Uuid as #leptos_form_krate::FormSignalType<<Uuid as #leptos_form_krate::DefaultHtmlElement>::El>>::into_signal_type(self.abc_123, &config.abc_123),
                        zz: <u8 as #leptos_form_krate::FormSignalType<<u8 as #leptos_form_krate::DefaultHtmlElement>::El>>::into_signal_type(self.zz, &config.zz),
                    }
                }
                fn try_from_signal_type(signal_type: Self::SignalType, config: &Self::Config) -> Result<Self, #leptos_form_krate::FormError> {
                    Ok(MyFormData {
                        abc_123: <Uuid as #leptos_form_krate::FormSignalType<<Uuid as #leptos_form_krate::DefaultHtmlElement>::El>>::try_from_signal_type(signal_type.abc_123, &config.abc_123)?,
                        zz: <u8 as #leptos_form_krate::FormSignalType<<u8 as #leptos_form_krate::DefaultHtmlElement>::El>>::try_from_signal_type(signal_type.zz, &config.zz)?,
                    })
                }
            }

            impl<__SignalType: 'static> #leptos_form_krate::FormComponent<__SignalType, #leptos_krate::View> for MyFormData {
                #[allow(unused_imports)]
                fn render(props: #leptos_form_krate::RenderProps<
                    __SignalType,
                    impl #leptos_form_krate::RefAccessor<__SignalType, Self::SignalType>,
                    impl #leptos_form_krate::MutAccessor<__SignalType, Self::SignalType>,
                    Self::Config,
                >) -> impl #leptos_krate::IntoView {
                    use #leptos_krate::{IntoAttribute, IntoView};

                    let _abc_123_id = #leptos_form_krate::format_form_id(props.id.as_ref(), "hello-there");
                    let _abc_123_name = #leptos_form_krate::format_form_name(Some(&props.name), "abc_123");
                    let _abc_123_props = #leptos_form_krate::RenderProps::builder()
                        .id(_abc_123_id.clone())
                        .name(_abc_123_name.clone())
                        .class("hi")
                        .signal(props.signal)
                        .ref_ax(#leptos_form_krate::ref_ax_factory(move |t: &__SignalType| &(props.ref_ax)(t).abc_123))
                        .mut_ax(#leptos_form_krate::mut_ax_factory(move |t: &mut __SignalType| &mut (props.mut_ax)(t).abc_123))
                        .config(<Uuid as #leptos_form_krate::FormSignalType<<Uuid as #leptos_form_krate::DefaultHtmlElement>::El>>::Config::default())
                        .build();
                    let _abc_123_view = <Uuid as #leptos_form_krate::FormComponent<__SignalType, <Uuid as #leptos_form_krate::DefaultHtmlElement>::El>>::render(_abc_123_props);

                    let _zz_id = #leptos_form_krate::format_form_id(props.id.as_ref(), "zz");
                    let _zz_name = #leptos_form_krate::format_form_name(Some(&props.name), "zz");
                    let _zz_props = #leptos_form_krate::RenderProps::builder()
                        .id(_zz_id.clone())
                        .name(_zz_name.clone())
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

    #[test]
    fn test3() -> Result<()> {
        let input = quote!(
            #[derive(Form)]
            #[form(component(action = "/api/my-form-data"))]
            pub struct MyFormData {
                pub ayo: u8,
            }
        );

        let leptos_krate = quote!(::leptos_form::internal::leptos);
        let leptos_router_krate = quote!(::leptos_form::internal::leptos_router);
        let leptos_form_krate = quote!(::leptos_form);

        let expected = quote!(
            #[derive(Clone, Debug, Default)]
            pub struct __MyFormDataSignalType {
                pub ayo: <u8 as #leptos_form_krate::FormSignalType<<u8 as #leptos_form_krate::DefaultHtmlElement>::El>>::SignalType,
            }

            #[derive(Clone, Debug, Default)]
            pub struct __MyFormDataConfig {
                pub ayo: <u8 as #leptos_form_krate::FormSignalType<<u8 as #leptos_form_krate::DefaultHtmlElement>::El>>::Config,
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

            impl #leptos_form_krate::FormSignalType<#leptos_krate::View> for MyFormData {
                type Config = __MyFormDataConfig;
                type SignalType = __MyFormDataSignalType;
                fn into_signal_type(self, config: &Self::Config) -> Self::SignalType {
                    __MyFormDataSignalType {
                        ayo: <u8 as #leptos_form_krate::FormSignalType<<u8 as #leptos_form_krate::DefaultHtmlElement>::El>>::into_signal_type(self.ayo, &config.ayo),
                    }
                }
                fn try_from_signal_type(signal_type: Self::SignalType, config: &Self::Config) -> Result<Self, #leptos_form_krate::FormError> {
                    Ok(MyFormData {
                        ayo: <u8 as #leptos_form_krate::FormSignalType<<u8 as #leptos_form_krate::DefaultHtmlElement>::El>>::try_from_signal_type(signal_type.ayo, &config.ayo)?,
                    })
                }
            }

            impl<__SignalType: 'static> #leptos_form_krate::FormComponent<__SignalType, #leptos_krate::View> for MyFormData {
                #[allow(unused_imports)]
                fn render(props: #leptos_form_krate::RenderProps<
                    __SignalType,
                    impl #leptos_form_krate::RefAccessor<__SignalType, Self::SignalType>,
                    impl #leptos_form_krate::MutAccessor<__SignalType, Self::SignalType>,
                    Self::Config,
                >) -> impl #leptos_krate::IntoView {
                    use #leptos_krate::{IntoAttribute, IntoView};

                    let _ayo_id = #leptos_form_krate::format_form_id(props.id.as_ref(), "ayo");
                    let _ayo_name = #leptos_form_krate::format_form_name(Some(&props.name), "ayo");
                    let _ayo_props = #leptos_form_krate::RenderProps::builder()
                        .id(_ayo_id.clone())
                        .name(_ayo_name.clone())
                        .signal(props.signal)
                        .ref_ax(#leptos_form_krate::ref_ax_factory(move |t: &__SignalType| &(props.ref_ax)(t).ayo))
                        .mut_ax(#leptos_form_krate::mut_ax_factory(move |t: &mut __SignalType| &mut (props.mut_ax)(t).ayo))
                        .config(<u8 as #leptos_form_krate::FormSignalType<<u8 as #leptos_form_krate::DefaultHtmlElement>::El>>::Config::default())
                        .build();
                    let _ayo_view = <u8 as #leptos_form_krate::FormComponent<__SignalType, <u8 as #leptos_form_krate::DefaultHtmlElement>::El>>::render(_ayo_props);

                    #leptos_krate::view! {
                        <label for={_ayo_id}>
                            "ayo"
                            {_ayo_view}
                        </label>
                    }
                }
            }

            pub use leptos_form_component_my_form_data::*;

            #[allow(unused_imports)]
            mod leptos_form_component_my_form_data {
                use super::*;
                use #leptos_form_krate::FormSignalType;
                use #leptos_krate::IntoView;
                use #leptos_router_krate::Form;

                #[#leptos_krate::component]
                pub fn MyFormData(initial: MyFormData) -> impl IntoView {
                    let config = __MyFormDataConfig {
                        ayo: <u8 as #leptos_form_krate::FormSignalType<<u8 as #leptos_form_krate::DefaultHtmlElement>::El>>::Config::default(),
                    };

                    let signal = #leptos_krate::create_rw_signal(initial.into_signal_type(&config));
                    let props = #leptos_form_krate::RenderProps::builder()
                        .id(None)
                        .name("")
                        .signal(signal)
                        .ref_ax(#leptos_form_krate::ref_ax_factory(|x| x))
                        .mut_ax(#leptos_form_krate::mut_ax_factory(|x| x))
                        .config(config)
                        .build();
                    let view = <MyFormData as #leptos_form_krate::FormComponent<__MyFormDataSignalType, #leptos_krate::View>>::render(props);
                    #leptos_krate::view! {
                        <Form action="/api/my-form-data">
                            {view}
                        </Form>
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
