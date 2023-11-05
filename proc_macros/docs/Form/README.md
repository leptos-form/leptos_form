Derives forms from structs.

This derive macro provides a way of generating [`Form`](leptos_router::Form) components and configuring their styling and behavior.
It creates a struct of signals, one per field, where each signal which contains the appropriate html type
(essentially [`String`], although specialized behavior is provided for collections). Upon form submission, the values in the form
are all parsed back into the type this macro is derived on and then submitted to the suitable endpoint / server function.

Note that serde integration is not completed, so in order for graceful degradation to work the deriving type must not change the
serialized names of any of its fields.

See an [extended example](#example) below.

# Struct attributes

| Attribute   | description                                                                                                                                      | Type                                       | Optional |
|-------------|--------------------------------------------------------------------------------------------------------------------------------------------------|--------------------------------------------|----------|
| component   | Derive a component for this type using [`leptos::component`]                                                                                     | [component](#component-attributes)         | Y        |
| error       | Specify error rendering behavior which will be used as a default for all fields; defaults to `default`                                           | [error handler](#error-handler-attributes) | Y        |
| field_class | Class property set on the wrapping element for each field by default                                                                             | string                                     | Y        |
| groups      | A list of all groups within the form                                                                                                             | list\<[container](#container-attributes)\> | Y        |
| id          | `id` property set on the wrapping \<Form\> element. Note that this id will prefixed by other ids if this type is used as a field in another form | string                                     | Y        |
| label       | Default label configuration used for all fields                                                                                                  | [label](#label-attributes)                 | Y        |
| wrapper     | Can only be used when derived on a newtype -- required to correctly produce the `name` attribute on fields                                       | none                                       | Y        |


# Field attributes

Any type which implements [`trait@FormField`] can be used as a field in a struct which derives Form.

| Attribute | description                                                                                                                                      | Type                                       | Optional |
|-----------|--------------------------------------------------------------------------------------------------------------------------------------------------|--------------------------------------------|----------|
| class     | `class` property set on this field's wrapping element                                                                                            | string                                     | Y        |
| config    | A Rust expression whose type is the [`FormField::Config`] type of this field's type                                                              | expr                                       | Y        |
| el        | The Rust type representing the html tag used to encode this field (note that the field type must implement `FormField<$el>`)                     | type                                       | Y        |
| error     | Specify error rendering behavior for this field, falling back on the container default where needed; defaults to `default`                       | [error handler](#error-handler-attributes) | Y        |
| group     | Group number if this field should be included in a group (0-indexed)                                                                             | usize                                      | Y        |
| id        | `id` property set on the wrapping \<Form\> element. Note that this id will prefixed by other ids if this type is used as a field in another form | string                                     | Y        |
| label     | Label configuration used for this field, falling back on the container default where needed                                                      | [label](#label-attributes)                 | Y        |
| style     | `style` property set on this field's wrapping element                                                                                            | string                                     | Y        |

## Component attributes
If specified, a leptos component can be produced for this type which will render a form derived from this type's fields.

| Attribute           | description                                                                                                                                             | Type                                 | Optional |
|---------------------|---------------------------------------------------------------------------------------------------------------------------------------------------------|--------------------------------------|----------|
| action              | Leptos action configuration for this form                                                                                                               | [action](#action-attribute)          | Y        |
| class               | `class` property set on the wrapping \<Form\> element                                                                                                   | string                               | Y        |
| field_changed_class | An additional class to be appended to the containing element of any field whose value has changed                                                       | string                               | Y        |
| map_submit          | Maps this type given its initial and current values into another type which will then be passed to the provided action                                  | [`MapSubmit`]                        | Y        |
| name                | The name of the component function produced; if this type is a tuple struct, name cannot be the type name or the type name prepended with an underscore | ident                                | Y        |
| on_error            | A callback which is called after a form submission error; called with the action's error and the action signal                                          | [`OnError`](components::OnError)     | Y        |
| on_success          | A callback which is called after a successful form submission; called with the successful action outcome and the action signal                          | [`OnSuccess`](components::OnSuccess) | Y        |
| reset_on_success    | Configures whether the form's fields should be reset to the form's initial values upon successful submission; defaults to false                         | bool                                 | Y        |
| submit              | Submit element, typically should be a button                                                                                                            | `impl IntoView`                      | Y        |
| style               | `style` property set on the wrapping \<Form\> element                                                                                                   | string                               | Y        |

## Action attribute
If specified, an action will be attached to the rendered [`Form`](leptos_router::Form) component.

An action can be specified one of two ways:
- a string literal, representing the url the form should be submitted to; in this case, quality of life attributes like `component.map_submit`, `component.on_error`, `component.on_success`, and `component.reset_on_success` cannot be used because the page will immediately reload upon form submission
- a path to a server function specified in a particular way:
    ```rust
    mod my_mod {
        #[derive(Clone, Debug, leptos_form::Form, serde::Deserialize, serde::Serialize)]
        #[form(component(action = my_server_fn(my_data)))]
        struct MyForm {}

        #[leptos::server]
        async fn my_server_fn(my_data: MyForm) -> Result<(), leptos::ServerFnError> {
            Ok(())
        }
    }
    ```

  leptos_form *does not* use [`ActionForm`](leptos_router::ActionForm), and therefore must produce the props to the server function itself which requires knowing the argument names of the server function.
  Thus the peculiar way of specifying server functions.

  ActionForm is avoided because it does not provide hooks for parsing the form prior to form submission.
  We parse the form first into the deriving struct (i.e. the type [`macro@Form`] is derived on) and then either submit it to the server function or pass it to `map_submit` if specified.
  This allows us to keeps the internals of state managament in the form completely separate from representation of the form's data.

## Container attributes
Specifies a containing element to contain wrap children.

| Attribute | description                                       | Optional                                                                                               |
| ----------|---------------------------------------------------|--------------------------------------------------------------------------------------------------------|
| tag       | The html tag to use for the wrapping html element | N (Y if used in the context of an adjacent field label and the form also has a default adjacent label) |
| id        | `id` property set on the wrapping html element    | Y                                                                                                      |
| class     | `class` property set on the wrapping html element | Y                                                                                                      |
| style     | `style` property set on the wrapping html element | Y                                                                                                      |

## Error handler attributes
When focus is removed from a field's input element, the input's value is parsed into this deriving struct's field's type.
If there is an error while parsing the input, it can be displayed around the input.
This attribute configures how that error is displayed.
Only one of the below attributes can be used at a time.

| Attribute | description                                                                     | Type                               |
|-----------|---------------------------------------------------------------------------------|------------------------------------|
| component | A leptos component which takes a single prop `error` with type [`FormError`]    | ident                              |
| container | A containing element which will render the stringified error as a child element | [container](#container-attributes) |
| default   | Renders the stringified error in a `<span style="color: red;">` element         | none                               |
| none      | No errors are ever rendered                                                     | none                               |
| raw       | The stringified error is rendered without any containing element                | none                               |

## Label attributes
Configuration for how to render a field's label.
Only one of the below attributes can be used at a time.

| Attribute | description                                                                                                                | Type                                         |
|-----------|----------------------------------------------------------------------------------------------------------------------------|----------------------------------------------|
| adjacent  | Renders the field label adjacent to the field input within a containing element (adjacent in the sense of the DOM tree)    | [adjacent label](#adjacent-label-attributes) |
| default   | Renders the field label adjacent to the field input without any containing element (adjacent in the sense of the DOM tree) | none                                         |
| none      | No label is rendered                                                                                                       | none                                         |
| wrap      | Wraps the field label string and input in a \<label\> element                                                              | [wrapped label](#wrapped-label-attributes)   |

## Adjacent label attributes
Configuration for an adjacent label.
Rendered adjacent labels have the following html structure:
```html
<attr.container.tag>
    <label for="my-field" id={attr.id} class={attr.class}>
        {attr.value}
    </label>
    <input id="my-field" type="text" value="foo" />
</attr.container.tag>
```

| Attribute  | description                                        | Type                               | Optional                            |
| -----------|----------------------------------------------------|------------------------------------|-------------------------------------|
| container  | A containing element which will wrap the \<label\> | [container](#container-attributes) | N if struct-level, Y if field-level |
| id         | `id` property set on the wrapping html element     | string                             | Y                                   |
| class      | `class` property set on the wrapping html element  | string                             | Y                                   |
| rename_all | Rename all labels to a particular case             | string, see [`LabelCase`]          | Y (only allowed at struct-level)    |
| style      | `style` property set on the wrapping html element  | string                             | Y                                   |
| value      | A literal string to override the label value       | string                             | Y (only allowed at field-level)     |

## Wrapped label attributes
Configuration for a wrapped label.
Rendered wrapped labels have the following html structure:
```html
<label for="my-field" id={attr.id} class={attr.class}>
    <div>{attr.value}</div>
    <input id="my-field" type="text" value="foo" />
</label>
```

| Attribute  | description                                        | Type                      | Optional                            |
| -----------|----------------------------------------------------|---------------------------|-------------------------------------|
| id         | `id` property set on the wrapping html element     | string                    | Y                                   |
| class      | `class` property set on the wrapping html element  | string                    | Y                                   |
| rename_all | Rename all labels to a particular case             | string, see [`LabelCase`] | Y (only allowed at struct-level)    |
| style      | `style` property set on the wrapping html element  | string                    | Y                                   |
| value      | A literal string to override the label value       | string                    | Y (only allowed at field-level)     |


# Example
This example derives two forms: one for creating blog posts and another for updating them.
Note that this example assumes makes use of Tailwind classes.

See additional [examples](https://github.com/tlowerison/leptos_form/tree/main/examples).

```rust
mod blog_post {
    use ::leptos::*;
    use ::leptos::html::Textarea;
    use ::leptos_form::prelude::*;
    use ::leptos_router::*;
    use ::serde::*;
    use ::typed_builder::*;
    use ::uuid::Uuid;

    /// db model of a blog post -- this type is here just to facilitate
    /// the use of server functions and is not particularly relevant to
    /// this example
    #[derive(Clone, Debug, Deserialize, Serialize, TypedBuilder)]
    pub struct DbBlogPost {
        pub id: Uuid,
        pub slug: String,
        pub title: String,
        pub summary: String,
        pub tags: Vec<String>,
        pub content: String,
    }

    /// ui model of a blog post -- it contains all the data necessary to create a blog post
    /// from the front end and also uses accurate field types like `Uuid` and `Vec<String>` rather than
    /// the stringified types placed inside html (leptos_form handles that conversion!)
    #[derive(Clone, Debug, Deserialize, Form, Serialize, TypedBuilder)]
    #[form(
        field_class = "appearance-none block w-full bg-gray-200 text-gray-700 border border-gray-200 rounded py-3 px-4 leading-tight focus:outline-none focus:bg-white focus:border-gray-500",
        component(
            action = create_blog_post(data),
            class = "w-full max-w-lg",
            reset_on_success,
            on_success = |value, _| (view! { <div>{move || format!(r#"Created blog post with id "{}"."#, value.id)}</div> }.into_view()),
            submit = view! { <input class="cursor-pointer" type="submit" value="Create" />},
        ),
        groups(
            container(tag = "div", class="flex flex-wrap -mx-3 mb-6", style = "color: red;"),
            container(tag = "div", class="flex flex-wrap -mx-3 mb-6"),
            container(tag = "div", class="flex flex-wrap -mx-3 mb-2"),
        ),
        label(adjacent(container(tag = "div", class="w-full md:w-1/2 px-3"), class = "block uppercase tracking-wide text-gray-700 text-xs font-bold mb-2")),
    )]
    pub struct BlogPost {
        #[form(class = "hidden", label = "none")]
        pub id: Uuid,
        #[form(group = 0)]
        pub slug: String,
        #[builder(default)]
        #[form(group = 0)]
        pub title: String,
        #[builder(default)]
        #[form(group = 1)]
        pub summary: String,
        #[builder(default)]
        #[form(group = 1, config = vec_config())]
        #[serde(default)]
        pub tags: Vec<String>,
        #[builder(default)]
        #[form(
            el(HtmlElement<Textarea>),
            group = 2,
            label(adjacent(container(tag = "div", class="w-full px-3"))),
        )]
        pub content: String,
    }

    #[component]
    pub fn BlogPostCreate() -> impl IntoView {
        let id = Uuid::new_v4();
        let initial = BlogPost::builder().id(id).slug(id.to_string()).build();

        view! {
            <div class="w-full flex flex-row items-center justify-center">
                <BlogPost initial={initial.clone()} />
            </div>
        }
    }

    #[server]
    async fn create_blog_post(data: BlogPost) -> Result<DbBlogPost, ServerFnError> { todo!() }

    /// configures how items in the `tags` field appear in the form
    fn vec_config<C: Default>() -> VecConfig<C> {
        VecConfig::builder()
            .item_label(VecItemLabel::builder()
                .class("flex flex-row items-center space-x-2")
                .notation(VecItemLabelNotation::Number)
                .build())
            .size(VecConfigSize::Bounded { min: Some(1), max: None })
            .remove(Adornment::Spec(AdornmentSpec::builder()
                .class("ml-2 text-white")
                .build()))
            .build()
    }


    // -------- UPDATE UI ------------


    /// url params used in update form
    #[derive(Clone, Debug, Default, Eq, Params, PartialEq)]
    pub struct BlogPostParams {
        pub slug: Option<String>,
    }

    /// Form struct which contains the data we want to submit to our backend
    /// for updating a blog post. Note that this is not the struct which `Form`
    /// is derived on, but it is the type we submit to `update_blog_post`.
    /// This type is extracted from the form data prior to submission by using
    /// the `map_submit` attribute when deriving `Form` on `BlogPostPatchForm`
    #[derive(Clone, Debug, Deserialize, Serialize, TypedBuilder)]
    #[builder(field_defaults(default))]
    pub struct BlogPostPatch {
        #[builder(!default)]
        pub id: Uuid,
        pub slug: Option<String>,
        pub title: Option<String>,
        pub summary: Option<String>,
        pub tags: Option<Vec<String>>,
        pub content: Option<String>,
    }

    /// ui model of making an update to a blog post. Note that the actual type
    /// is just a newtype around `BlogPost` -- this gives two huge benefits:
    ///  1. our form is editing the exact same ui data type so creates and updates
    ///     are autatically in sync in terms of the fields available to edit
    ///  2. we inherit all of the styling that we want from the create form while
    ///     retaining the ability to add different interactivity through attributes
    ///     like `field_changed_class`, `map_submit`, `on_success` and `submit`
    #[derive(Clone, Debug, Form)]
    #[form(
        component(
            action = update_blog_post(data),
            class = "w-full max-w-lg",
            field_changed_class = "border border-1 border-green-500 focus:border-green-500",
            // map_submit accepts any valid expression which implements `MapSubmit`a
            // see map_blog_patch_submit further down
            map_submit = map_blog_patch_submit,
            name = BlogPostPatchForm_,
            on_success = |_, _| (view! { <div>{move || "Updated blog post."}</div> }.into_view()),
            submit = view! { <input class="cursor-pointer border border-1 rounded px-2 py-1" type="submit" value="Update" />},
        ),
    )]
    pub struct BlogPostPatchForm(#[form(label = "none")] pub BlogPost);

    #[component]
    pub fn BlogPostUpdate() -> impl IntoView {
        let params = use_params::<BlogPostParams>();
        let params = move || params.get().unwrap_or_default();
        let blog_post = create_resource(move || params().slug.unwrap(), get_blog_post_by_slug);

        let eb_fallback = |errs: RwSignal<Errors>| view! {{move || errs.get().into_iter().map(|(_, err)| format!("{err}")).collect::<Vec<_>>().join(", ")}};

        view! {
            <div class="flex flex-col items-center">
                <a href={move || format!("/blog/{}", params().slug.unwrap_or_else(|| "".to_string()))}>
                    <div class="border border-1 rounded px-2 py-1" style="width: fit-content;">
                        "Go to post"
                    </div>
                </a>
                <Suspense>
                    {move || blog_post.get().map(|res| view! {
                        <ErrorBoundary fallback=eb_fallback>
                            {res.map(|blog_post| view! {
                                <div class="w-full flex flex-row items-center justify-center">
                                    <BlogPostPatchForm_ initial={BlogPostPatchForm::from(blog_post)} />
                                </div>
                            })}
                        </ErrorBoundary>
                    })}
                </Suspense>
            </div>
        }
    }

    #[server]
    async fn get_blog_post_by_slug(slug: String) -> Result<DbBlogPost, ServerFnError> { todo!() }

    #[server]
    async fn update_blog_post(data: BlogPostPatch) -> Result<DbBlogPost, ServerFnError> { todo!() }

    fn map_blog_patch_submit(FormDiff { initial, current }: FormDiff<BlogPostPatchForm>) -> BlogPostPatch {
        BlogPostPatch::builder()
            .id(initial.0.id)
            .slug((initial.0.slug != current.0.slug).then_some(current.0.slug))
            .title((initial.0.title != current.0.title).then_some(current.0.title))
            .summary((initial.0.summary != current.0.summary).then_some(current.0.summary))
            .tags((initial.0.tags != current.0.tags).then_some(current.0.tags))
            .content((initial.0.content != current.0.content).then_some(current.0.content))
            .build()
    }

    impl From<DbBlogPost> for BlogPostPatchForm {
        fn from(value: DbBlogPost) -> Self {
            Self(BlogPost::builder()
                .id(value.id)
                .slug(value.slug)
                .title(value.title)
                .summary(value.summary)
                .content(value.content)
                .tags(value.tags)
                .build())
        }
    }
}
```

##### Create blog post UI - Filling out form
![Create blog post UI - Filling out form](https://raw.githubusercontent.com/tlowerison/leptos_form/main/proc_macros/docs/Form/example1_create1.png)

##### Create blog post UI - Submitted
![Create blog post UI - Submitted](https://raw.githubusercontent.com/tlowerison/leptos_form/main/proc_macros/docs/Form/example1_create2.png)

##### Update blog post UI - Filling out form
![Update blog post UI - Filling out form](https://raw.githubusercontent.com/tlowerison/leptos_form/main/proc_macros/docs/Form/example1_update1.png)
