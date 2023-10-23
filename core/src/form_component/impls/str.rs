use crate::*;
use ::leptos::html::*;
use ::leptos::*;
use ::std::borrow::Cow;

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
            type Signal = RwSignal<String>;

            fn default_signal() -> Self::Signal {
                RwSignal::new(Default::default())
            }
            fn is_default_value(signal: &Self::Signal) -> bool {
                signal.with(|value| value.is_empty())
            }
            fn into_signal(self, _: &Self::Config) -> Self::Signal {
                RwSignal::new(self.into())
            }
            fn try_from_signal(signal: Self::Signal, _: &Self::Config) -> Result<Self, FormError> {
                Ok(str_impl!(@from signal $($from_signal)?))
            }
        }

        impl FormComponent<HtmlElement<$el>> for $ty {
            fn render(props: RenderProps<Self::Signal, Self::Config>) -> impl IntoView {
                props.signal.with(|value| {
                    view! {
                        <[<$el:lower>]
                            id={props.id.unwrap_or_else(|| props.name.clone())}
                            name={props.name}
                            class={props.class}
                            type="text"
                            value={value}
                            on:change=setter(props.signal, |x| x.as_string())
                        />
                    }
                })
            }
        }
    } };

    (@from $signal:ident) => { $signal.get() };
    (@from $signal:ident $from_signal:expr) => { $from_signal($signal.get()) };
}

str_impl!(
    String,
    Cow<'_, str> { Cow::Owned },
    Oco<'static, str> { Oco::Owned },
);
