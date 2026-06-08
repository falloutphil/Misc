# Build Effect v3 Documentation as EPUB/HTML/Markdown

This repository/script builds an offline book version of the stable Effect v3 documentation from the Effect website source.

It generates:

```text
effect-v3-docs.md
effect-v3-docs.html
effect-v3-docs.epub
```

The script is designed for the stable v3 docs source layout used by the Effect website repository:

```text
website-main/content/src/content/docs/docs
website-main/content/astro.config.ts
```

It reads the Starlight sidebar configuration from `astro.config.ts`, orders pages using the docs frontmatter, converts the MDX documentation into book-friendly Markdown, and then uses Pandoc to generate HTML and EPUB.

## Files

The main script is:

```text
build_effect_books_fixed.py
```

The generated output directory contains:

```text
effect-v3-docs.md
effect-v3-docs.html
effect-v3-docs.epub
assets/
book.css
```

## What the script does

The script performs the following conversion steps:

* reads the Effect website docs from `content/src/content/docs/docs`
* reads Starlight sidebar ordering from `content/astro.config.ts`
* sorts pages by `sidebar.order`
* strips MDX frontmatter
* removes MDX-only `import` / `export` lines
* converts Starlight/MDX components into static book-friendly output
* flattens tabs into normal subsections
* converts callouts into styled static boxes
* copies local images into an `assets/` directory
* rewrites local image paths
* normalises code fences such as:

````text
```ts twoslash showLineNumbers=false
````

into:

````text
```ts
````

* uses a real Markdown fence parser so code blocks do not accidentally consume later Markdown content
* generates Markdown, HTML, and EPUB using Pandoc

## Requirements

### System requirements

Install Pandoc.

On macOS:

```bash
brew install pandoc
```

On Ubuntu/Debian:

```bash
sudo apt update
sudo apt install pandoc
```

### Python requirements

The script itself only uses the Python standard library.

A virtual environment is still recommended so that any future additions or local tooling stay isolated.

Create and activate a virtual environment:

```bash
python3 -m venv .venv
source .venv/bin/activate
```

Upgrade packaging tools:

```bash
python -m pip install --upgrade pip setuptools wheel
```

No Python packages are required for the current script.

## Input source

You need an extracted or cloned Effect website source tree.

For example, if using the downloaded GitHub archive:

```text
website-main/
```

The script expects to find:

```text
website-main/content/astro.config.ts
website-main/content/src/content/docs/docs
```

If cloning manually:

```bash
git clone https://github.com/Effect-TS/website.git website-main
```

## Basic usage

Run:

```bash
python build_effect_books_fixed.py \
  --website-dir ./website-main \
  --out-dir ./effect-book-fixed
```

This generates:

```text
effect-book-fixed/effect-v3-docs.md
effect-book-fixed/effect-v3-docs.html
effect-book-fixed/effect-v3-docs.epub
effect-book-fixed/assets/
effect-book-fixed/book.css
```

## Command used to generate the current fixed files

The current fixed files were generated with:

```bash
python build_effect_books_fixed.py \
  --website-dir /mnt/data/effect_src/website-main \
  --out-dir /mnt/data/effect_book_fixed
```

For a normal local checkout, the equivalent command would be:

```bash
python build_effect_books_fixed.py \
  --website-dir ./website-main \
  --out-dir ./effect_book_fixed
```

## Options

### `--website-dir`

Required.

Path to the Effect website source checkout or extracted GitHub archive.

Example:

```bash
--website-dir ./website-main
```

The script accepts either of these layouts:

```text
website-main/content/astro.config.ts
```

or:

```text
website-main/astro.config.ts
```

but for the current Effect website archive, the correct path is usually:

```text
website-main/content
```

inside the repo/archive.

You should still pass the outer repo/archive directory:

```bash
--website-dir ./website-main
```

### `--out-dir`

Optional.

Output directory.

Default:

```text
effect-book-fixed
```

Example:

```bash
--out-dir ./dist/effect-book
```

The output directory is deleted and recreated on each run.

### `--skip-epub`

Optional.

Generate only Markdown and HTML, skipping EPUB generation.

Example:

```bash
python build_effect_books_fixed.py \
  --website-dir ./website-main \
  --out-dir ./effect-book-fixed \
  --skip-epub
```

This is useful while debugging the Markdown/HTML conversion.

### `--include-unstable`

Optional.

By default, the script excludes sidebar sections labelled:

```text
AI
Micro
Platform
```

Those sections are treated as unstable or less central to the stable v3 book build.

To include them:

```bash
python build_effect_books_fixed.py \
  --website-dir ./website-main \
  --out-dir ./effect-book-fixed \
  --include-unstable
```

## Recommended workflow

First generate Markdown and HTML only:

```bash
python build_effect_books_fixed.py \
  --website-dir ./website-main \
  --out-dir ./effect-book-fixed \
  --skip-epub
```

Open:

```text
effect-book-fixed/effect-v3-docs.html
```

Check that headings, code blocks, callouts, and examples look correct.

Then generate the EPUB:

```bash
python build_effect_books_fixed.py \
  --website-dir ./website-main \
  --out-dir ./effect-book-fixed
```

Open:

```text
effect-book-fixed/effect-v3-docs.epub
```

in your EPUB reader.

## Verifying the EPUB

If you have `epubcheck` installed, validate the generated EPUB:

```bash
epubcheck effect-book-fixed/effect-v3-docs.epub
```

On macOS:

```bash
brew install epubcheck
```

## Known behaviour

### MDX components are flattened

The Effect website is an Astro/Starlight MDX site. Some components are interactive on the website but cannot be represented directly in an EPUB.

The script converts these into static equivalents.

For example:

```text
<Tabs>
<TabItem label="...">
```

becomes normal Markdown subsections.

```text
<Aside>
```

becomes a styled static callout box.

### Code block handling

The fixed script uses a proper Markdown fence parser.

This matters because many Effect docs examples contain output comments, embedded Markdown-looking text, and Twoslash/Starlight code fence metadata.

The script distinguishes between:

````text
```ts twoslash showLineNumbers=false
````

and:

```text
```

````

so that a code block does not accidentally absorb the rest of the page.

This fixed a previous EPUB issue where raw Markdown such as headings and `**Example**` text appeared inside code blocks.

### PDF generation

This fixed script generates Markdown, HTML, and EPUB.

It does not currently generate PDF directly.

For a PDF, use the generated HTML as the source.

For example, using a browser:

1. Open `effect-book-fixed/effect-v3-docs.html`
2. Use Print
3. Choose Save as PDF

Or use a dedicated HTML-to-PDF tool such as WeasyPrint:

```bash
pipx install weasyprint
weasyprint effect-book-fixed/effect-v3-docs.html effect-book-fixed/effect-v3-docs.pdf
````

For very large documentation books, browser or WeasyPrint PDF generation can take a while.

## Troubleshooting

### `pandoc: command not found`

Install Pandoc:

```bash
brew install pandoc
```

or:

```bash
sudo apt install pandoc
```

### `could not find website content root`

Check that `--website-dir` points to the Effect website source tree.

The following files/directories must exist somewhere under that path:

```text
content/astro.config.ts
content/src/content/docs/docs
```

or:

```text
astro.config.ts
src/content/docs/docs
```

### EPUB shows raw Markdown inside code blocks

Make sure you are using the fixed script:

```text
build_effect_books_fixed.py
```

The earlier script had overly loose code-fence handling. The fixed script uses a proper fenced-code parser and should prevent raw Markdown from being swallowed into code blocks.

### Output directory already exists

The script deletes and recreates the output directory every time it runs.

Do not point `--out-dir` at a directory containing files you want to keep.

## Example full run

```bash
python3 -m venv .venv
source .venv/bin/activate
python -m pip install --upgrade pip setuptools wheel

brew install pandoc

python build_effect_books_fixed.py \
  --website-dir ./website-main \
  --out-dir ./effect_book_fixed
```

Generated files:

```text
effect_book_fixed/effect-v3-docs.md
effect_book_fixed/effect-v3-docs.html
effect_book_fixed/effect-v3-docs.epub
```
