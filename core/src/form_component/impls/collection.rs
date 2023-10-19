use crate::*;
use ::core::ops::*;
use ::leptos::*;

fn render_collection<El, T, U, V>(
    props: RenderProps<T, impl RefAccessor<T, V>, impl MutAccessor<T, V>, <U as FormSignalType<El>>::Config>,
) -> impl IntoView
where
    U: FormSignalType<El> + FormComponent<T, El> + Sized,
    V: Index<usize, Output = <U as FormSignalType<El>>::SignalType> + IndexMut<usize> + 'static,
    for<'a> &'a V: IntoIterator<Item = &'a <U as FormSignalType<El>>::SignalType>,
{
    props.signal.with(|t| {
        let value = (props.ref_ax)(t);
        value
            .into_iter()
            .enumerate()
            .map(|(i, _)| {
                let props = RenderProps::builder()
                    .id(crate::format_form_id(props.id.as_ref(), i.to_string()))
                    .name(crate::format_form_name(Some(&props.name), i.to_string()))
                    .ref_ax(ref_ax_factory(move |t| &(props.ref_ax)(t)[i]))
                    .mut_ax(mut_ax_factory(move |t| &mut (props.mut_ax)(t)[i]))
                    .signal(props.signal)
                    .config(props.config.clone())
                    .build();
                <U as FormComponent<T, El>>::render(props)
            })
            .collect::<Vec<_>>()
    })
}

impl<U: DefaultHtmlElement> DefaultHtmlElement for Vec<U> {
    type El = Vec<U::El>;
}

impl<U, El> FormSignalType<Vec<El>> for Vec<U>
where
    U: FormSignalType<El>,
{
    type Config = <U as FormSignalType<El>>::Config;
    type SignalType = Vec<<U as FormSignalType<El>>::SignalType>;

    fn into_signal_type(self, config: &Self::Config) -> Self::SignalType {
        self.into_iter().map(|u| u.into_signal_type(config)).collect()
    }
    fn try_from_signal_type(signal_type: Self::SignalType, config: &Self::Config) -> Result<Self, FormError> {
        signal_type
            .into_iter()
            .map(|inner_singal_type| U::try_from_signal_type(inner_singal_type, config))
            .collect()
    }
}

impl<U, T: 'static, El> FormComponent<T, Vec<El>> for Vec<U>
where
    U: FormComponent<T, El>,
{
    fn render(
        props: RenderProps<
            T,
            impl RefAccessor<T, Self::SignalType>,
            impl MutAccessor<T, Self::SignalType>,
            Self::Config,
        >,
    ) -> impl IntoView {
        render_collection::<El, T, U, Self::SignalType>(props)
    }
}
