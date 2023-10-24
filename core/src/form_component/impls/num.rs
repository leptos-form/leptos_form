use crate::*;
use ::leptos::html::*;
use ::leptos::*;

macro_rules! int_impl {
    ($($ty:ty),*$(,)?) => { $(
        impl DefaultHtmlElement for $ty {
            type El = HtmlElement<Input>;
        }

        impl FormField<HtmlElement<Input>> for $ty {
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
                signal.with(|x| x.value.parse()).map_err(FormError::parse)
            }
            fn with_error<O>(signal: &Self::Signal, f: impl FnOnce(Option<&FormError>) -> O) -> O {
                signal.with(|x| f(x.error.as_ref()))
            }
        }

        impl FormComponent<HtmlElement<Input>> for $ty {
            fn render(props: RenderProps<Self::Signal, Self::Config>) -> impl IntoView {
                view! {
                    <input
                        type="number"
                        id={props.id.unwrap_or_else(|| props.name.clone())}
                        name={props.name}
                        class={props.class}
                        min={$ty::MIN}
                        max={$ty::MAX}
                        value=move || props.signal.get().value
                        prop:value={props.signal.0}
                        on:input=move |ev| props.signal.0.update(|x| x.value = event_target_value(&ev))
                        on:change=move |_| {
                            if let Err(form_error) = <Self as FormField<HtmlElement<Input>>>::try_from_signal(props.signal, &props.config) {
                                props.signal.update(|x| x.error = Some(form_error));
                            } else if props.signal.with_untracked(|x| x.error.is_some()) {
                                props.signal.update(|x| x.error = None);
                            }
                        }
                    />
                }
            }
        }
    )* };
}

int_impl!(u8, u16, u32, u64, u128, usize, i8, i16, i32, i64, i128, isize);
