#![allow(unused)]

use crate::*;
use ::leptos::html::*;
use ::leptos::*;
use ::wasm_bindgen::JsValue;

#[cfg(feature = "uuid")]
mod uuid {
    use super::*;

    impl DefaultHtmlElement for ::uuid::Uuid {
        type El = HtmlElement<Input>;
    }

    impl FormField<HtmlElement<Input>> for ::uuid::Uuid {
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
            use std::str::FromStr;
            signal
                .with(|x| ::uuid::Uuid::from_str(&x.value))
                .map_err(FormError::parse)
        }
        fn reset_initial_value(signal: &Self::Signal) {
            signal.update(|sig| sig.initial = Some(sig.value.clone()));
        }
        fn with_error<O>(signal: &Self::Signal, f: impl FnOnce(Option<&FormError>) -> O) -> O {
            signal.with(|x| f(x.error.as_ref()))
        }
    }

    impl FormComponent<HtmlElement<Input>> for ::uuid::Uuid {
        fn render(props: RenderProps<Self::Signal, Self::Config>) -> impl IntoView {
            let class = props.class_signal();
            view! {
                <input
                    type="text"
                    class={class}
                    id={props.id.or_else(|| props.name.clone())}
                    name={props.name}
                    on:input=move |ev| props.signal.0.update(|x| x.value = event_target_value(&ev))
                    on:change=move |_| {
                        if let Err(form_error) = <Self as FormField<HtmlElement<Input>>>::try_from_signal(props.signal, &props.config) {
                            props.signal.update(|x| x.error = Some(form_error));
                        } else if props.signal.with_untracked(|x| x.error.is_some()) {
                            props.signal.update(|x| x.error = None);
                        }
                    }
                    prop:class={move || class.with(|x| x.as_ref().map(|x| JsValue::from_str(x)))}
                    prop:value={props.signal.0}
                    style={props.style}
                    value=move || props.signal.get().value
                />
            }
        }
    }
}

#[cfg(feature = "chrono")]
/// Configuration utilities for using [`chrono`](::chrono) types in form types.
pub mod chrono {
    use super::*;
    use ::chrono::*;

    #[derive(Clone, Debug)]
    pub struct NaiveDateConfig {
        pub format: &'static str,
    }
    #[derive(Clone, Debug)]
    pub struct NaiveDateTimeConfig {
        pub format: &'static str,
    }
    #[derive(Clone, Debug)]
    pub struct FixedOffsetDateTimeConfig {
        pub format: &'static str,
    }
    #[derive(Clone, Debug)]
    pub struct UtcDateTimeConfig {
        pub format: &'static str,
    }
    #[derive(Clone, Debug)]
    pub struct LocalDateTimeConfig {
        pub format: &'static str,
    }

    impl Default for NaiveDateConfig {
        fn default() -> Self {
            Self { format: "%x" }
        }
    }
    impl Default for NaiveDateTimeConfig {
        fn default() -> Self {
            Self { format: "%c" }
        }
    }
    impl Default for FixedOffsetDateTimeConfig {
        fn default() -> Self {
            Self { format: "%+" }
        }
    }
    impl Default for UtcDateTimeConfig {
        fn default() -> Self {
            Self { format: "%c" }
        }
    }
    impl Default for LocalDateTimeConfig {
        fn default() -> Self {
            Self { format: "%c" }
        }
    }

    macro_rules! chrono_impl {
        ($($ty:ty, $config:ty { $($from:tt)* }),*$(,)?) => { $(
            chrono_impl! { @ $ty, $config { $($from)* } }
        )* };

        (@ $ty:ty, $config:ty { $($from:tt)* }) => { paste! {
            impl DefaultHtmlElement for $ty {
                type El = HtmlElement<Input>;
            }

            impl FormField<HtmlElement<Input>> for $ty {
                type Config = $config;
                type Signal = FormFieldSignal<String>;

                fn default_signal() -> Self::Signal {
                    Default::default()
                }
                fn is_default_value(signal: &Self::Signal) -> bool {
                    signal.with(|x| x.value.is_empty())
                }
                fn into_signal(self, config: &Self::Config) -> Self::Signal {
                    Self::Signal::from(self.format(config.format).to_string())
                }
                fn reset_initial_value(signal: &Self::Signal) {
                    signal.update(|sig| sig.initial = Some(sig.value.clone()));
                }
                fn with_error<O>(signal: &Self::Signal, f: impl FnOnce(Option<&FormError>) -> O) -> O {
                    signal.with(|x| f(x.error.as_ref()))
                }

                $($from)*
            }

            impl FormComponent<HtmlElement<Input>> for $ty {
                fn render(props: RenderProps<Self::Signal, Self::Config>) -> impl IntoView {
                    let class = props.class_signal();
                    view! {
                        <input
                            type="text"
                            class={class}
                            id={props.id.or_else(|| props.name.clone())}
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
    }

    chrono_impl!(
        NaiveDate, NaiveDateConfig {
            fn try_from_signal(signal: Self::Signal, config: &Self::Config) -> Result<Self, FormError> {
                signal.with(|x| Self::parse_from_str(&x.value, config.format)).map_err(FormError::parse)
            }
        },
        NaiveDateTime, NaiveDateTimeConfig {
            fn try_from_signal(signal: Self::Signal, config: &Self::Config) -> Result<Self, FormError> {
                signal.with(|x| Self::parse_from_str(&x.value, config.format)).map_err(FormError::parse)
            }
        },
        DateTime<FixedOffset>, FixedOffsetDateTimeConfig {
            fn try_from_signal(signal: Self::Signal, config: &Self::Config) -> Result<Self, FormError> {
                signal.with(|x| DateTime::parse_from_str(&x.value, config.format)).map_err(FormError::parse)
            }
        },
        DateTime<Utc>, UtcDateTimeConfig {
            fn try_from_signal(signal: Self::Signal, config: &Self::Config) -> Result<Self, FormError> {
                signal.with(|x| DateTime::parse_from_str(&x.value, config.format).map(|x| x.with_timezone(&Utc))).map_err(FormError::parse)
            }
        },
        DateTime<Local>, LocalDateTimeConfig {
            fn try_from_signal(signal: Self::Signal, config: &Self::Config) -> Result<Self, FormError> {
                signal.with(|x| DateTime::parse_from_str(&x.value, config.format).map(|x| x.with_timezone(&Local))).map_err(FormError::parse)
            }
        },
    );
}
