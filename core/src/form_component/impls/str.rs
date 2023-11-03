use crate::*;
use ::leptos::html::*;
use ::leptos::*;
use ::std::borrow::Cow;
use ::wasm_bindgen::JsValue;

macro_rules! str_impl {
    ($($ty:ty $({ $from_signal:expr })?),*$(,)?) => { $(
        str_impl! { @ $ty, Input $({ $from_signal })? }
        str_impl! { @ $ty, Textarea $({ $from_signal })? }

        impl DefaultHtmlElement for $ty {
            type El = HtmlElement<Input>;
        }
    )* };

    (@ $ty:ty, $el:ident $({ $from_signal:expr })?) => { paste! {
        impl FormField<HtmlElement<$el>> for $ty {
            type Config = ();
            type Signal = FormFieldSignal<String>;

            fn default_signal() -> Self::Signal {
                Default::default()
            }
            fn is_default_value(signal: &Self::Signal) -> bool {
                signal.with(|x| x.value.is_empty())
            }
            fn into_signal(self, _: &Self::Config) -> Self::Signal {
                Self::Signal::from(self.to_string())
            }
            fn try_from_signal(signal: Self::Signal, _: &Self::Config) -> Result<Self, FormError> {
                Ok(str_impl!(@from signal $($from_signal)?))
            }
            fn reset_initial_value(signal: &Self::Signal) {
                signal.update(|sig| sig.initial = Some(sig.value.clone()));
            }
            fn with_error<O>(signal: &Self::Signal, f: impl FnOnce(Option<&FormError>) -> O) -> O {
                signal.with(|x| f(x.error.as_ref()))
            }
        }

        impl FormComponent<HtmlElement<$el>> for $ty {
            fn render(props: RenderProps<Self::Signal, Self::Config>) -> impl IntoView {
                let class = props.class_signal();
                view! {
                    <[<$el:lower>]
                        type="text"
                        class={class}
                        id={props.id.unwrap_or_else(|| props.name.clone())}
                        name={props.name}
                        on:input=move |ev| props.signal.0.update(|x| x.value = event_target_value(&ev))
                        on:change=move |_| {
                            if let Err(form_error) = <Self as FormField<HtmlElement<Input>>>::try_from_signal(props.signal, &props.config) {
                                props.signal.update(|x| x.error = Some(form_error));
                            } else if props.signal.with_untracked(|x| x.error.is_some()) {
                                props.signal.update(|x| x.error = None);
                            }
                        }
                        prop:class={move || class.with(|x| x.as_ref().map(|x| JsValue::from_str(&*x)))}
                        prop:value={props.signal.0}
                        style={props.style}
                        value=move || props.signal.get().value
                    />
                }
            }
        }
    } };

    (@from $signal:ident) => { $signal.get().value };
    (@from $signal:ident $from_signal:expr) => { $from_signal($signal.get().value) };
}

str_impl!(
    String,
    Cow<'_, str> { Cow::Owned },
    Oco<'static, str> { Oco::Owned },
);
