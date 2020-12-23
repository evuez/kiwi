# kiwi

A (dirty) Haskell blog generator.

I'm using this for my [own site](https://liftm.io/) (repo here: https://github.com/evuez/evuez.github.io).

Setting it up is pretty simple, install it with `stack install`, then create a `kiwi.toml` file:

```toml
[templates]
index = "templates/index.html"
pages = "templates/page.html"

[[source]]
name = "Posts"
path = "posts"

[[source]]
name = "Projects"
path = "projects"
```

The `index` template is used to generate the... index of the site, and `pages` is used for any other page. These use [mustache](https://mustache.github.io/). Templates examples are available here: https://github.com/evuez/evuez.github.io/tree/main/templates

Each `source` must have a `path` matching an existing directory where the markdown files for this source live. Here, we would have the following directory structure:

```
kiwi.toml
templates
  index.html
  page.html
posts
  post-1.md
  ...
projects
  project-1.md
  ...
```

Finally, each markdown file must include metadata:

 - A `title` (required)
 - A `date` (optional)
 - A list of `tags` (optional)

Metadata are written in [TOML](https://toml.io/) and must be at the top of the file. A line containing only `%%%` is used to separate the metadata from the actual contents. See https://raw.githubusercontent.com/evuez/evuez.github.io/main/posts/currying.md for an example.
