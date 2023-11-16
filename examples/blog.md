```rust
use super::{BlogPostContent, BlogPostParams, create::*};
use crate::prelude::*;
use leptos::html::*;
use leptos_form::prelude::*;

#[derive(Clone, Debug, Deserialize, Form, Serialize, TypedBuilder)]
#[form(
    class = "w-full max-w-lg",
    component(
        action = create_blog_post(data),
        reset_on_success,
        on_success = |value, _| view!(<div>{move || format!(r#"Created blog post with id "{}"."#, value.id)}</div>).into_view(),
    ),
    field_class = "appearance-none block w-full bg-gray-200 text-gray-700 border border-gray-200 rounded py-3 px-4 leading-tight focus:outline-none focus:bg-white focus:border-gray-500",
    groups(
        container(tag = "div", class="flex flex-wrap -mx-3 mb-6"),
        container(tag = "div", class="flex flex-wrap -mx-3 mb-6"),
        container(tag = "div", class="flex flex-wrap -mx-3 mb-2"),
    ),
    label(adjacent(container(tag = "div", class="w-full md:w-1/2 px-3"), class = "block uppercase tracking-wide text-gray-700 text-xs font-bold mb-2")),
)]
pub struct BlogPostPost {
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

#[derive(Clone, Debug, Form)]
#[form(
    component(
        action = update_blog_post(data),
        field_changed_class = "border border-1 border-green-500 focus:border-green-500",
        map_submit = |FormDiff { initial, current }: FormDiff<BlogPostPatchForm>| {
            BlogPostPatch::builder()
                .id(initial.0.id)
                .slug((initial.0.slug != current.0.slug).then_some(current.0.slug))
                .title((initial.0.title != current.0.title).then_some(current.0.title))
                .summary((initial.0.summary != current.0.summary).then_some(current.0.summary))
                .tags((initial.0.tags != current.0.tags).then_some(current.0.tags))
                .content((initial.0.content != current.0.content).then_some(current.0.content))
                .build()
        },
        name = BlogPostPatchForm_,
        on_success = |_, _| (view!(<div>{move || "Updated blog post."}</div>).into_view()),
    ),
)]
pub struct BlogPostPatchForm(#[form(label = "none")] pub BlogPostPost);

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

fn vec_config<C: Default>() -> VecConfig<C> {
    VecConfig {
        remove: Adornment::Spec(AdornmentSpec::builder()
            .class("cursor-pointer ml-2 text-white")
            .build()),
        ..Default::default()
    }
}

#[component]
pub fn BlogPostCreate() -> impl IntoView {
    let id = Uuid::new_v4();
    let initial = BlogPostPost::builder().id(id).slug(id.to_string()).build();

    view! {
        <AdminGated
            is_not_admin=router_fallback
            is_admin=move || view! {
                <div class="w-full flex flex-row items-center justify-center">
                    <BlogPostPost
                        initial={initial.clone()}
                        top=|| view!(<input class="cursor-pointer" type="submit" value="Create" />)
                    />
                </div>
            }
        />
    }
}

#[server]
async fn create_blog_post(data: BlogPostPost) -> Result<DbBlogPost, ServerFnError> {
    use super::BlogPostContent;

    let summary = data.summary.trim();
    let blog_post_content = BlogPostContent { summary: (!summary.is_empty()).then_some(summary.to_string()), content: data.content };

    let content = serde_json::to_value(&blog_post_content).map_err(Log::Debug(AppError::BadRequest("invalid blog post content")))?;

    let db_blog_post = DbBlogPost::builder()
        .id(data.id)
        .slug(data.slug.to_string())
        .title(data.title)
        .content(content)
        .tags(data.tags)
        .build();

    let db_blog_post = DbBlogPost::insert_one(&use_db()?, db_blog_post).await.map_err(Log::Debug(AppError::BadRequest("Cannot create post")))?;

    Ok(db_blog_post)
}

impl From<DbBlogPost> for BlogPostPatchForm {
    fn from(value: DbBlogPost) -> Self {
        let blog_post_content = serde_json::from_value::<BlogPostContent>(value.content).unwrap();
        Self(BlogPostPost::builder()
            .id(value.id)
            .slug(value.slug)
            .title(value.title)
            .summary(blog_post_content.summary.unwrap_or_default())
            .content(blog_post_content.content)
            .tags(value.tags)
            .build())
    }
}

#[component]
pub fn BlogPostUpdate() -> impl IntoView {
    let params = use_params::<BlogPostParams>();
    let params = move || params.get().unwrap_or_default();
    let blog_post = create_resource(move || params().slug, get_blog_post_by_slug);

    view! {
        <div class="flex flex-col items-center">
            <a href=move || format!("/blog/{}", params().slug)>
                <div class="border border-1 rounded px-2 py-1" style="width: fit-content;">
                    "Go to post"
                </div>
            </a>
            <AdminGated
                is_not_admin=router_fallback
                is_admin=move || view! {
                    <Suspense>
                        {move || blog_post.get().map(|res| view! {
                            <ErrorBoundary fallback=eb_fallback>
                                {res.map(|blog_post| view! {
                                    <div class="w-full flex flex-row items-center justify-center">
                                        <BlogPostPatchForm_
                                            initial={BlogPostPatchForm::from(blog_post)}
                                            top=|| view!(<input class="cursor-pointer border border-1 rounded px-2 py-1" type="submit" value="Update" />)
                                        />
                                    </div>
                                })}
                            </ErrorBoundary>
                        })}
                    </Suspense>
                }
            />
        </div>
    }
}

#[server]
async fn get_blog_post_by_slug(slug: String) -> Result<DbBlogPost, ServerFnError> {
    let db = use_context::<DbPool>().ok_or_else(|| AppError::Internal)?;
    Ok(DbBlogPost::get_by_column(&db, schema::blog_post::slug, [slug])
        .await
        .map_err(Log::Debug(AppError::Internal))?
        .pop()
        .ok_or_else(|| AppError::NotFound)?)
}

#[server]
async fn update_blog_post(data: BlogPostPatch) -> Result<DbBlogPost, ServerFnError> {
    use super::BlogPostContent;

    let db_blog_post = DbBlogPost::get_one(&use_db()?, data.id).await.map_err(Log::Debug(AppError::BadRequest("Cannot find post.")))?;

    let content = if data.summary.is_some() || data.content.is_some() {
        let blog_post_content = serde_json::from_value::<BlogPostContent>(db_blog_post.content).map_err(Log::Debug(AppError::BadRequest("Internal Server Error")))?;

        let blog_post_content = BlogPostContent {
            content: data.content.unwrap_or(blog_post_content.content),
            summary: data.summary.or(blog_post_content.summary).and_then(|summary| {
                let summary = summary.trim().to_string();
                if summary.is_empty() { None } else { Some(summary) }
            }),
        };

        Some(serde_json::to_value(&blog_post_content).map_err(Log::Debug(AppError::BadRequest("Invalid blog post content.")))?)
    } else {
        None
    };

    let db_blog_post_patch = DbBlogPostPatch::builder()
        .id(data.id)
        .slug(data.slug)
        .title(data.title)
        .content(content)
        .tags(data.tags)
        .build();

    let db_blog_post = DbBlogPost::update_one(&use_db()?, db_blog_post_patch).await.map_err(Log::Debug(AppError::BadRequest("Cannot update post.")))?;

    Ok(db_blog_post)
}
```
