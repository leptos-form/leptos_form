use crate::components::*;
use crate::*;
use ::core::ops::*;
use ::indexmap::IndexMap;
use ::std::rc::Rc;

/// Configuration for a Vec of FormFields
#[derive(Clone, Default, Derivative, TypedBuilder)]
#[builder(field_defaults(default, setter(into)))]
#[derivative(Debug)]
pub struct VecConfig<Config: Default> {
    /// item level configuration
    /// "item" refers to the inner type of Vec for which this config is being applied
    pub item: Config,
    /// class to be placed on container element which wraps both the label and the item
    #[builder(setter(strip_option))]
    pub item_container_class: Option<Oco<'static, str>>,
    /// custom class to be passed into each item's FormField props
    #[builder(setter(strip_option))]
    pub item_class: Option<Oco<'static, str>>,
    /// custom style to be passed into each item's FormField props
    #[builder(setter(strip_option))]
    pub item_style: Option<Oco<'static, str>>,
    /// label configuration for each item
    #[builder(setter(strip_option))]
    pub item_label: Option<VecItemLabel>,
    /// Vec size control for this form
    pub size: VecConfigSize,
    /// configuration for an add button at the end of the listed items
    pub add: Adornment,
    /// configuration for the remove buttons adorning each field
    pub remove: Adornment,
}

/// Label configuration which will be set for each item
/// in a Vec of FormFields
#[derive(Clone, Debug, Default, TypedBuilder)]
#[builder(field_defaults(default, setter(into)))]
pub struct VecItemLabel {
    /// custom class to set for each label
    #[builder(setter(strip_option))]
    pub class: Option<Oco<'static, str>>,
    /// notation variant for the item label (determines the label content)
    pub notation: Option<VecItemLabelNotation>,
    /// punctuation style for the item label
    pub punctuation: Option<VecItemLabelPunctuation>,
    /// custom style to set for each label
    #[builder(setter(strip_option))]
    pub style: Option<Oco<'static, str>>,
}

/// When a label is configured to be set for a Vec of form fields,
/// the label used will be produced using one of the below notations.
#[derive(Clone, Copy, Debug, Default)]
pub enum VecItemLabelNotation {
    CapitalLetter,
    Letter,
    #[default]
    Number,
}

/// Punctuation applied to the vec item label. Note that this configuration
/// is a no-op wihtout the use of VecItemLabelNotation.
#[derive(Clone, Copy, Debug, Default)]
pub enum VecItemLabelPunctuation {
    Parenthesis,
    #[default]
    Period,
}

#[derive(Clone, Copy, Debug, Default)]
pub enum VecConfigSize {
    Bounded {
        min: Option<usize>,
        max: Option<usize>,
    },
    Const(usize),
    #[default]
    Unbounded,
}

#[derive(Clone, Default, Derivative)]
#[derivative(Debug)]
pub enum Adornment {
    None,
    #[default]
    Default,
    Component(#[derivative(Debug = "ignore")] AdornmentComponent),
    Spec(AdornmentSpec),
}

/// A Component type which accepts two arguments:
/// - an on:click callback which accepts a MouseEvent argument
/// - a derived signal returning a style string which should
///   be placed on the top level component's `style:opacity` prop
pub type AdornmentComponent = Rc<dyn Fn(Rc<dyn Fn(web_sys::MouseEvent)>, StyleSignal) -> View + 'static>;

#[derive(Clone, Debug, TypedBuilder)]
#[builder(field_defaults(default, setter(into)))]
pub struct AdornmentSpec {
    #[builder(setter(strip_option))]
    pub class: Option<Oco<'static, str>>,
    #[builder(default = 24)]
    pub height: usize,
    #[builder(setter(strip_option))]
    pub style: Option<Oco<'static, str>>,
    pub text: Option<Oco<'static, str>>,
    #[builder(default = 24)]
    pub width: usize,
}

impl Default for AdornmentSpec {
    fn default() -> Self {
        Self {
            class: None,
            height: 24,
            style: None,
            text: None,
            width: 24,
        }
    }
}

impl<T: DefaultHtmlElement> DefaultHtmlElement for Vec<T> {
    type El = Vec<T::El>;
}

#[derive(Clone, Copy, Debug)]
pub struct VecSignalItem<Signal> {
    id: usize,
    signal: Signal,
}

impl<T, El> FormField<Vec<El>> for Vec<T>
where
    T: Clone + FormField<El>,
    <T as FormField<El>>::Signal: Clone + std::fmt::Debug,
{
    type Config = VecConfig<<T as FormField<El>>::Config>;
    type Signal = FormFieldSignal<IndexMap<usize, VecSignalItem<<T as FormField<El>>::Signal>>>;

    fn default_signal(config: &Self::Config, initial: Option<Self>) -> Self::Signal {
        FormFieldSignal::new_with_default_value(initial.map(|x| {
            x.into_iter()
                .enumerate()
                .map(|(i, initial)| {
                    (
                        i,
                        VecSignalItem {
                            id: i,
                            signal: T::default_signal(&config.item, Some(initial)),
                        },
                    )
                })
                .collect::<IndexMap<_, _>>()
        }))
    }
    fn is_default_value(signal: &Self::Signal) -> bool {
        signal.value.with(|value| value.is_empty())
    }
    fn into_signal(self, config: &Self::Config, initial: Option<Self>) -> Self::Signal {
        let has_initial = initial.is_some();
        let mut initial = initial
            .map(|x| x.into_iter().map(Some).collect::<Vec<_>>())
            .unwrap_or_default();
        if initial.len() < self.len() {
            initial.append(&mut vec![None; self.len() - initial.len()]);
        }
        let value = self
            .into_iter()
            .zip(initial)
            .enumerate()
            .map(|(i, (item, initial))| {
                (
                    i,
                    VecSignalItem {
                        id: i,
                        signal: item.into_signal(&config.item, initial),
                    },
                )
            })
            .collect::<IndexMap<_, _>>();
        let initial = has_initial.then(|| value.clone());
        FormFieldSignal::new(value, initial)
    }
    fn try_from_signal(signal: Self::Signal, config: &Self::Config) -> Result<Self, FormError> {
        signal.with(|value| {
            value
                .iter()
                .map(|(_, item)| T::try_from_signal(item.signal.clone(), &config.item))
                .collect()
        })
    }
    fn recurse(signal: &Self::Signal) {
        signal.with(|sig| sig.iter().for_each(|(_, sig)| T::recurse(&sig.signal)))
    }
    fn reset_initial_value(signal: &Self::Signal) {
        signal.value.update(|value| {
            value.iter().for_each(|(_, item)| T::reset_initial_value(&item.signal));
        });
    }
    fn with_error<O>(_: &Self::Signal, f: impl FnOnce(Option<&FormError>) -> O) -> O {
        f(None)
    }
}

impl<T, El, S> FormComponent<Vec<El>> for Vec<T>
where
    T: Clone + FormComponent<El, Signal = FormFieldSignal<S>>,
    S: Clone + Eq + 'static + std::fmt::Debug,
    <T as FormField<El>>::Config: std::fmt::Debug,
{
    fn render(props: RenderProps<Self::Signal, Self::Config>) -> impl IntoView {
        let (min_items, max_items) = props.config.size.split();

        let next_id =
            create_rw_signal(
                props
                    .signal
                    .with(|items| if items.is_empty() { 1 } else { items[items.len() - 1].id + 1 }),
            );

        if min_items.is_some() || max_items.is_some() {
            props.signal.update(|items| {
                let num_items = items.len();
                if let Some(min_items) = min_items {
                    if items.len() < min_items {
                        items.reserve(min_items - num_items);
                        while items.len() < min_items {
                            let id = next_id.get_untracked();
                            items.insert(
                                id,
                                VecSignalItem {
                                    id,
                                    signal: T::default_signal(&props.config.item, None),
                                },
                            );
                            next_id.update(|x| *x += 1);
                        }
                    }
                }
                if let Some(max_items) = max_items {
                    while max_items < items.len() {
                        items.pop();
                    }
                }
            });
        }

        let VecConfig {
            item: item_config,
            item_container_class,
            item_class,
            item_label,
            item_style,
            size,
            add,
            remove,
        } = props.config;

        let item_config_clone = item_config.clone();
        view! {
            <div id={props.id} class={props.class} style={props.style}>
                <For
                    key=|(_, (key, _))| *key
                    each=move || props.signal.value.get().into_iter().enumerate()
                    children=move |(index, (key, item))| {
                        let id = || index.to_string();

                        let item_props = RenderProps::builder()
                            .id(Oco::Owned(id()))
                            .name(crate::format_form_name(props.name.as_ref(), id()))
                            .class(item_class.clone())
                            .style(item_style.clone())
                            .field_changed_class(props.field_changed_class.clone())
                            .signal(item.signal)
                            .config(item_config.clone())
                            .build();

                        VecConfig::<<T as FormField<El>>::Config>::wrap(
                            &size,
                            item_container_class.clone(),
                            item_label.as_ref(),
                            &remove,
                            props.signal,
                            key,
                            Oco::Owned(id()),
                            <T as FormComponent<El>>::render(item_props),
                        ).into_view()
                    }
                />
                {
                    let num_items_is_max = move || {
                        let num_items = props.signal.with(|items| items.len());
                        num_items >= max_items.unwrap_or(usize::MAX)
                    };

                    let cursor = move || if num_items_is_max() { None } else { Some("pointer") };
                    let opacity = move || if num_items_is_max() { Some("0.5") } else { None };

                    let on_add = move |_| {
                        if !num_items_is_max() {
                            props.signal.update(|items| {
                                let id = next_id.get_untracked();
                                items.insert(id, VecSignalItem { id, signal: T::default_signal(&item_config_clone, None) });
                                next_id.update(|x| *x = id + 1);
                            });
                        }
                    };

                    match (&size, &add) {
                        (VecConfigSize::Const(_), _)|(_, Adornment::None) => View::default(),
                        (_, Adornment::Component(component)) => component(Rc::new(on_add), Rc::new(opacity)),
                        (_, Adornment::Default) => view! {
                            <input
                                type="button"
                                on:click=on_add
                                style:cursor=cursor
                                style:margin-top="0.5 rem"
                                style:opacity=opacity
                                value="Add"
                            />
                        }
                        .into_view(),
                        (_, Adornment::Spec(adornment_spec)) => {
                            let style = (adornment_spec.class.is_none() && adornment_spec.style.is_none()).then_some("margin-top: 0.5rem;");
                            view! {
                                <input
                                    type="button"
                                    class={adornment_spec.class.clone()}
                                    cursor=cursor
                                    on:click=on_add
                                    style:opacity=opacity
                                    style=style
                                    value={adornment_spec.text.clone().unwrap_or(Oco::Borrowed("Add"))}
                                />
                            }
                            .into_view()
                        }
                    }
                }
            </div>
        }
    }
}

impl From<usize> for VecConfigSize {
    fn from(value: usize) -> Self {
        Self::Const(value)
    }
}

impl From<(usize, usize)> for VecConfigSize {
    fn from(value: (usize, usize)) -> Self {
        Self::Bounded {
            min: Some(value.0),
            max: Some(value.1),
        }
    }
}

static ASCII_LOWER: [char; 26] = [
    'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w',
    'x', 'y', 'z',
];

static ASCII_UPPER: [char; 26] = [
    'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W',
    'X', 'Y', 'Z',
];

impl<Config: Default> VecConfig<Config> {
    #[allow(clippy::too_many_arguments)]
    fn wrap<Signal: std::fmt::Debug>(
        size: &VecConfigSize,
        item_container_class: Option<Oco<'static, str>>,
        item_label: Option<&VecItemLabel>,
        remove_adornment: &Adornment,
        signal: FormFieldSignal<IndexMap<usize, VecSignalItem<Signal>>>,
        key: usize,
        id: Oco<'static, str>,
        item: impl IntoView,
    ) -> impl IntoView {
        // use leptos::ev::MouseEvent;
        // use wasm_bindgen::{JsCast, UnwrapThrowExt};

        let (min_items, _) = size.split();
        let num_items_is_min = move || {
            let num_items = signal.with(|items| items.len());
            num_items <= min_items.unwrap_or_default()
        };

        let cursor: StyleSignal = Rc::new(move || if num_items_is_min() { None } else { Some("pointer") });
        let opacity: StyleSignal = Rc::new(move || if num_items_is_min() { Some("0.5") } else { None });

        let on_remove = move |_| {
            if !num_items_is_min() {
                signal.update(|items| {
                    items.remove(&key);
                });
            }
        };

        let remove_component = match (size, remove_adornment) {
            (VecConfigSize::Const(_), _) | (_, Adornment::None) => View::default(),
            (_, Adornment::Component(component)) => component(Rc::new(on_remove), opacity),
            (_, Adornment::Default) => {
                view! {
                    <MaterialClose
                        cursor=cursor
                        on:click=on_remove
                        opacity=opacity
                        style=Oco::Borrowed("margin-left: 0.5rem !important;")
                    />
                }
            }
            (_, Adornment::Spec(adornment_spec)) => {
                let adornment_style = (adornment_spec.class.is_none() && adornment_spec.style.is_none())
                    .then_some("margin-left: 0.5rem;");
                view! {
                    <MaterialClose
                        class=adornment_spec.class.clone()
                        cursor=cursor
                        height=adornment_spec.height
                        on:click=on_remove
                        opacity=opacity
                        style=adornment_style.map(Oco::from)
                        width=adornment_spec.width
                    />
                }
            }
        };

        view! {
            <div class={item_container_class} style="display: flex; flex-direction: row; align-items: center; margin-bottom: 0.5rem">
                {match item_label {
                    Some(item_label) => item_label.wrap_label(key, id, item, signal),
                    None => item.into_view(),
                }}
                {remove_component}
            </div>
        }
    }
}

impl VecItemLabel {
    fn wrap_label<Signal>(
        &self,
        key: usize,
        id: Oco<'static, str>,
        item: impl IntoView,
        signal: FormFieldSignal<IndexMap<usize, VecSignalItem<Signal>>>,
    ) -> View {
        let notation = self.notation;
        let punctuation = self.punctuation;
        let prefix = move || {
            signal
                .with(|items| items.get_index_of(&key))
                .and_then(|index| match (notation, punctuation) {
                    (Some(notation), punctuation) => {
                        Some(notation.render(index) + punctuation.map(|x| x.render()).unwrap_or_default())
                    }
                    _ => None,
                })
                .map(|prefix| view! { <div>{prefix}</div> }.into_view())
                .unwrap_or_default()
        };
        view! {
            <label for={id} class={self.class.clone()} style={self.style.clone()}>
                {prefix}
                {item}
            </label>
        }
        .into_view()
    }
}

impl VecConfigSize {
    fn split(&self) -> (Option<usize>, Option<usize>) {
        match *self {
            VecConfigSize::Bounded { min, max } => (min, max),
            VecConfigSize::Const(num_items) => (Some(num_items), Some(num_items)),
            VecConfigSize::Unbounded => (None, None),
        }
    }
}

impl VecItemLabelNotation {
    fn render(&self, index: usize) -> String {
        let display_index = index + 1;
        let ascii_set = match self {
            Self::CapitalLetter => &ASCII_UPPER,
            Self::Letter => &ASCII_LOWER,
            Self::Number => return display_index.to_string(),
        };
        let n = (display_index.ilog(ascii_set.len()) + 1) as usize;
        let mut chars = vec![' '; n];
        let mut num = index;
        for j in 0..n {
            chars[n - 1 - j] = ascii_set[num % ascii_set.len()];
            num /= ascii_set.len();
        }
        chars.into_iter().collect()
    }
}

impl VecItemLabelPunctuation {
    fn render(&self) -> &'static str {
        match self {
            Self::Parenthesis => ")",
            Self::Period => ".",
        }
    }
}
