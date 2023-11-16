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

            fn default_signal(_: &Self::Config, initial: Option<Self>) -> Self::Signal {
                FormFieldSignal::new_with_default_value(initial.map(|x| x.to_string()))
            }
            fn is_initial_value(signal: &Self::Signal) -> bool {
                signal.value.with(|value| signal.initial.with(|initial| {
                    match initial {
                        Some(initial) => initial == value,
                        None => value.is_empty(),
                    }
                }))
            }
            fn into_signal(self, _: &Self::Config, initial: Option<Self>) -> Self::Signal {
                FormFieldSignal::new(self.to_string(), initial.map(|x| x.to_string()))
            }
            fn try_from_signal(signal: Self::Signal, _: &Self::Config) -> Result<Self, FormError> {
                Ok(str_impl!(@from signal $($from_signal)?))
            }
            fn recurse(signal: &Self::Signal) {
                signal.value.with(|_| {})
            }
            fn reset_initial_value(signal: &Self::Signal) {
                signal.value.with(|value| signal.initial.update(|initial| *initial = Some(value.clone())));
            }
            fn with_error<O>(signal: &Self::Signal, f: impl FnOnce(Option<&FormError>) -> O) -> O {
                signal.error.with(|error| f(error.as_ref()))
            }
        }

        impl FormComponent<HtmlElement<$el>> for $ty {
            fn render(props: RenderProps<Self::Signal, Self::Config>) -> impl IntoView {
                let class = props.class_signal();
                view! {
                    <[<$el:lower>]
                        type="text"
                        class={class}
                        id={props.id.or_else(|| props.name.clone())}
                        name={props.name}
                        on:input=move |ev| props.signal.value.update(|value| *value = event_target_value(&ev))
                        on:change=move |_| {
                            if let Err(form_error) = <Self as FormField<HtmlElement<Input>>>::try_from_signal(props.signal, &props.config) {
                                props.signal.error.update(|error| *error = Some(form_error));
                            } else if props.signal.error.with_untracked(|error| error.is_some()) {
                                props.signal.error.update(|error| *error = None);
                            }
                        }
                        prop:class={move || class.with(|x| x.as_ref().map(|x| JsValue::from_str(&*x)))}
                        prop:value={props.signal.value}
                        style={props.style}
                        value=props.signal.value
                    />
                }
            }
        }
    } };

    (@from $signal:ident) => { $signal.value.get() };
    (@from $signal:ident $from_signal:expr) => { $from_signal($signal.value.get()) };
}

str_impl!(
    String,
    Cow<'_, str> { Cow::Owned },
    Oco<'static, str> { Oco::Owned },
);
