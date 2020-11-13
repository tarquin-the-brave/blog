+++
title = "Using Rust Doc to Generate a Config File Reference"
date = 2020-07-15T14:58:19+01:00
images = []
tags = []
categories = []
draft = true
+++

There's something missing from the documentation of CLI tools.

They often have home pages that do a great job of explaining the core concepts,
giving a "Quick Start" guide, and demonstrating some use cases.

Where I find tools' documentation is missing something, is when you're already
well acquainted with the tool, and you want to know some specific detail about
a single field like what possible values it can have or exactly where it sits in the
structure of the config. I find myself really in need of a "config file
reference" akin to the ones you see for APIs which get generated from API Specs
or code.

Having worked with the Kubernetes API a fair bit over the last few years I find
myself going straight to [the API reference][k8sapi] whenever I need to look something
up, as I'm normally looking up the specifics of where a field lives or what it
does. Having documentation that allows you to follow links to navigate through
the structure of the API objects is really helpful. Kubernetes also has a load
of concept guides and walkthroughs in [its documentation][k8sdocs]. It's something
I'd hold up as a generally well documented thing.

[k8sapi]: https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.19/
[k8sdocs]: https://kubernetes.io/docs/home/

It's often hard to find out the specifics of a config field if it
doesn't appear in examples or some explanation. Projects that make an attempt
to provide a reference for config seem to follow the same pattern.  For
relatively simple config files they provide an example with all the fields,
optional and not, provided, and then comment the example to say what's option,
what the possible values are.

![](/images/rust-docs-config-ref/helm-chart-file.png)

This works OK while the config is small and simple.  But as config grows in
size and becomes more complex in terms of what fields are needed, where
sections of config could have alternatives, or be required only if some other
config is set, it starts to get out of hand. That example above is the documentation
for [Helm charts' `Chart.yaml` file](https://helm.sh/docs/topics/charts/#the-chartyaml-file).

It seems beyond a certain point projects, that do provide a config reference,
resort to a more protracted form of a web page that lists each field and
provides the information on it.  This produces something that is complete, but
isn't always that easy to navigate, scrolling is often your only resort, and I
often find I lose track of where a field is nested within the structure.

In both approaches mentioned there, there is the problem that the config file
reference is maintained separately from the code that defines it.  For
maintainability and correctness' sake, we really want the code that defines
config, and the documentation that attempts to provide a complete and correct
reference for it, to be together.

So what I'm looking for is config file documentation that:

- Reflects the structure of the config,
- Is navigable by following links into and back out of config substructure, and
- Defined in code.

# Enter Rust Docs

I've been developing a CLI tool at work recently.  It's written in
Rust.  I'm not sure I fully understand why, but Rust is a excellent language
for writing CLI tools in.  I don't think there's much about the core tenets
of the language that necessarily make that so.  The representation
of errors in sum types, `Result<T, E>`, does strongly encourage the errors to be handled
right back to `main` and the user, but the `?` operator makes it all to
easy to throw out a cryptic error message from a library you've called with
no context relevant to your application. I think really it's just that there's
some really well designed libraries to make CLI tools with: [`clap`][clap],
[`structopt`][structopt], and [`serde`][serde] (for parsing config files).

[clap]: https://docs.rs/clap/2.33.3/clap/
[structopt]: https://docs.rs/structopt/0.3.20
[serde]: https://serde.rs/

I've noticed the Rust docs going largely unused as the crate is a binary...

Perhaps they could fulfil what I'm looking for.  Rust docs reflect the structure
of structures. If you're using [`structopt`][structopt]
then the tool's config is defined in a Rust structure. Rust docs are navigable by
following links.  The source for them is defined in code. It's sounding like a
strong candidate, on paper at least...

I've made this dummy CLI tool to see how this looks.

Defining a reasonably simplistic config structure:

```rust
#[derive(serde::Deserialize)]
pub struct Config {
    name: String,
    version: semver::Version,
    description: String,
    source: Source,
    target: std::path::PathBuf,
}

#[derive(serde::Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum Source {
    File(std::path::PathBuf),
    Url(String),
}
```

The Rust docs looks like:

![](/images/rust-docs-config-ref/config1.png)

We can see the fields and their types. Not all the types are types in the
format language of the config file, but with a comment we can clarify that.
Where the type doesn't map to a type of the config file format people can
follow the link to see what it is.

This is in keeping with API documentations that may show a fields type to be
another object.  Take the documentation for a [Kubernetes
Pod](https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.19/#pod-v1-core)
for example:

![](/images/rust-docs-config-ref/k8s1.png)

We can follow the link to `Source` to see what that is:

![](/images/rust-docs-config-ref/source1.png)

Granted, there's some obvious problems emerging already.

If a reader follows
a link to a foreign type there's no guarantee that that type's documentation
will helpfully portray how that type is serialized in our chosen config
file format.

For the enum above, the representation in the Rust docs is quite diverged from
how it would appear in the config file.  And perhaps some Rust literacy
is going to be needed to know that an enum is something that could be one
variant, or could be another.

Any difference between the Rust structure and config file representation that's
given by attributes, e.g. the:

```rust
#[serde(rename_all = "snake_case")]
```

isn't accounted for.

But maybe these are things we can fix up with comments.  Let's assume for
now the config file format is YAML.

![](/images/rust-docs-config-ref/config2.png)

Following the link to `Source` again:

![](/images/rust-docs-config-ref/source2.png)

This doesn't look so bad now.  There's even a nice link to go back up to
top level config, which when you follow it, takes you to the field this
data is under in the parent.

![](/images/rust-docs-config-ref/config2source.png)

You can also expand and collapse the descriptions of each field as you
browse around.

I admit, I am massively skimming over the face that in both cases of `Source`
and `Config` what I'm showing above [isn't all that appears on the page][config2]. There's
methods and trait implementations below and a sidebar that links you to them
with a big Rust symbol in it.  This is definitely a problem, but one that
I'm going to come back to.

Let's make this config more complex and see what happens!

```rust
#[derive(serde::Deserialize)]
pub struct Config {
    name: String,
    version: semver::Version,
    description: Option<String>,
    #[serde(flatten)]
    data: AppData,
}

#[derive(serde::Deserialize)]
pub struct AppData {
    source: Source,
    target: Target,
    actions: Vec<Actions>,
}

#[derive(serde::Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum Source {
    File(std::path::PathBuf),
    Url(String),
}

#[derive(serde::Deserialize)]
pub struct Target {
    path: std::path::PathBuf,
}

#[derive(serde::Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum Actions {
    Foo,
    Bar,
    Baz,
    FooBar,
    BarBaz,
    #[serde(rename = "fbb")]
    FooBarBaz,
}
```

So now our top level `Config` object defines some metadata fields: `name`, `version`,
and a now optional `description`, and flattens in the configuration data for the
application.  This might take a bit more explaining in the comments...




# Alternative Approaches

Define config file/CLI in OpenAPI spec and use an existing OpenAPI -> HTML tool?

# Potential Project

- Is there anything out there that does this?
  + perhaps for another langauge
