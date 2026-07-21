#!/usr/bin/env python3
"""
Build a good-quality offline EPUB/PDF from the public Next.js Learn pages.

This is intentionally a public-page converter, not a GitHub-source converter:
`vercel/next-learn` contains starter/final code, while the Learn prose is served
from nextjs.org/learn.

Outputs:
  nextjs-learn.md
  nextjs-learn.html
  nextjs-learn.epub  (requires pandoc or pypandoc_binary)
  nextjs-learn.pdf   (optional: requires weasyprint or chromium)
  quality-report.txt

Dependencies:
  python3 -m pip install -r requirements.txt
  brew install pandoc                 # optional on macOS; pypandoc_binary also works
  python3 -m pip install weasyprint   # optional PDF engine if not using requirements.txt

Example:
  python3 build_nextjs_learn_book.py \
    --course app \
    --out-dir ./nextjs-app-router-course \
    --skip-pdf
"""
from __future__ import annotations

import argparse
import dataclasses
import hashlib
import html
import os
import re
import shutil
import subprocess
import sys
import textwrap
import time
import urllib.parse
import zipfile
import tempfile
from pathlib import Path
from typing import Iterable

import requests
from bs4 import BeautifulSoup, Tag
from markdownify import markdownify as markdownify

BASE = "https://nextjs.org"
DEFAULT_UA = "nextjs-learn-book-builder/2.3 (+personal offline reading; contact: local-script)"

APP_ROUTER_SEEDS = [
    "/learn/dashboard-app",
    "/learn/dashboard-app/getting-started",
    "/learn/dashboard-app/css-styling",
    "/learn/dashboard-app/optimizing-fonts-images",
    "/learn/dashboard-app/creating-layouts-and-pages",
    "/learn/dashboard-app/navigating-between-pages",
    "/learn/dashboard-app/setting-up-your-database",
    "/learn/dashboard-app/fetching-data",
    "/learn/dashboard-app/static-and-dynamic-rendering",
    "/learn/dashboard-app/streaming",
    "/learn/dashboard-app/adding-search-and-pagination",
    "/learn/dashboard-app/mutating-data",
    "/learn/dashboard-app/error-handling",
    "/learn/dashboard-app/improving-accessibility",
    "/learn/dashboard-app/adding-authentication",
    "/learn/dashboard-app/adding-metadata",
    "/learn/dashboard-app/next-steps",
]

REACT_FOUNDATIONS_SEEDS = ["/learn/react-foundations"]
PAGES_ROUTER_SEEDS = ["/learn/pages-router"]

COURSE_SEEDS = {
    "app": APP_ROUTER_SEEDS,
    "react": REACT_FOUNDATIONS_SEEDS,
    "pages": PAGES_ROUTER_SEEDS,
    "all": APP_ROUTER_SEEDS + REACT_FOUNDATIONS_SEEDS + PAGES_ROUTER_SEEDS,
}

NOISE_SELECTORS = [
    "script", "style", "noscript", "template",
    "nav", "header", "footer", "form[role='search']",
    "button", "input", "textarea", "select",
    "svg[aria-hidden='true']", "[data-nextjs-data-runtime-error-collapsed]",
    "[aria-label='Breadcrumb']", "[aria-label='Pagination']",
]

NOISE_TEXT_MARKERS = [
    "Was this helpful?",
    "Sign in to save progress",
    "Progress isn",
    "Subscribe to our newsletter",
    "Cookie Preferences",
    "Vercel, Inc.",
]

@dataclasses.dataclass(frozen=True)
class Page:
    url: str
    title: str
    markdown: str


def run(cmd: list[str], cwd: Path | None = None, check: bool = True) -> subprocess.CompletedProcess[str]:
    print("+", " ".join(cmd), file=sys.stderr)
    return subprocess.run(cmd, cwd=cwd, text=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, check=check)


def which(name: str) -> str | None:
    return shutil.which(name)


def pandoc_path() -> str | None:
    direct = which("pandoc")
    if direct:
        return direct

    try:
        import pypandoc  # type: ignore
    except ImportError:
        return None

    try:
        candidate = pypandoc.get_pandoc_path()
    except OSError:
        return None
    return candidate if candidate and Path(candidate).exists() else None


def ensure_dir(path: Path) -> None:
    path.mkdir(parents=True, exist_ok=True)


def normalize_url(href: str, current_url: str = BASE) -> str | None:
    if not href:
        return None
    href = href.strip()
    if href.startswith("#") or href.startswith("mailto:") or href.startswith("javascript:"):
        return None
    url = urllib.parse.urljoin(current_url, href)
    parsed = urllib.parse.urlparse(url)
    clean = parsed._replace(query="", fragment="")
    return urllib.parse.urlunparse(clean)


def is_learn_page(url: str, course: str) -> bool:
    parsed = urllib.parse.urlparse(url)
    if parsed.netloc and parsed.netloc != "nextjs.org":
        return False
    path = parsed.path.rstrip("/")
    if course == "app":
        return path == "/learn/dashboard-app" or path.startswith("/learn/dashboard-app/")
    if course == "react":
        return path == "/learn/react-foundations" or path.startswith("/learn/react-foundations/")
    if course == "pages":
        return path == "/learn/pages-router" or path.startswith("/learn/pages-router/")
    return path.startswith("/learn/")


def slugify(text: str) -> str:
    text = text.lower()
    text = re.sub(r"[^a-z0-9]+", "-", text).strip("-")
    return text or "item"


def local_asset_name(url: str, content_type: str | None = None) -> str:
    parsed = urllib.parse.urlparse(url)
    qs = urllib.parse.parse_qs(parsed.query)
    source_url = qs.get("url", [url])[0]
    decoded = urllib.parse.unquote(source_url)
    ext = Path(urllib.parse.urlparse(decoded).path).suffix.lower()
    if not ext or len(ext) > 8:
        ctype = (content_type or "").lower()
        if "png" in ctype:
            ext = ".png"
        elif "jpeg" in ctype or "jpg" in ctype:
            ext = ".jpg"
        elif "svg" in ctype:
            ext = ".svg"
        elif "webp" in ctype:
            ext = ".webp"
        else:
            ext = ".png"
    digest = hashlib.sha256(url.encode("utf-8")).hexdigest()[:16]
    base = slugify(Path(urllib.parse.urlparse(decoded).path).stem)[:48] or "asset"
    return f"{base}-{digest}{ext}"


def fetch(session: requests.Session, url: str, *, timeout: int = 30, retries: int = 3) -> requests.Response:
    last_exc: Exception | None = None
    for attempt in range(retries):
        try:
            resp = session.get(url, timeout=timeout)
            if resp.status_code == 429:
                wait = 2 + attempt * 3
                print(f"Rate limited; sleeping {wait}s", file=sys.stderr)
                time.sleep(wait)
                continue
            resp.raise_for_status()
            return resp
        except Exception as exc:
            last_exc = exc
            if attempt + 1 < retries:
                time.sleep(1 + attempt)
    raise RuntimeError(f"Failed to fetch {url}: {last_exc}")


def download_asset(session: requests.Session, src: str, current_url: str, assets_dir: Path) -> str:
    abs_url = urllib.parse.urljoin(current_url, src)
    parsed = urllib.parse.urlparse(abs_url)
    if parsed.path.startswith("/_next/image"):
        qs = urllib.parse.parse_qs(parsed.query)
        if "url" in qs:
            abs_url = urllib.parse.urljoin(BASE, urllib.parse.unquote(qs["url"][0]))
    try:
        resp = fetch(session, abs_url, timeout=45, retries=2)
    except Exception as exc:
        print(f"WARN: could not download image {abs_url}: {exc}", file=sys.stderr)
        return abs_url
    name = local_asset_name(abs_url, resp.headers.get("content-type"))
    target = assets_dir / name
    if not target.exists():
        target.write_bytes(resp.content)
    return f"assets/{name}"


def find_main_content(soup: BeautifulSoup) -> Tag:
    for selector in ["main article", "article", "main"]:
        found = soup.select_one(selector)
        if found:
            return found
    body = soup.body
    if body:
        return body
    raise RuntimeError("Could not locate page body")


def remove_noise(root: Tag) -> None:
    for selector in NOISE_SELECTORS:
        for node in list(root.select(selector)):
            node.decompose()

    for node in list(root.find_all(True)):
        text = " ".join(node.get_text(" ", strip=True).split())
        if not text:
            continue
        if len(text) < 500 and any(marker in text for marker in NOISE_TEXT_MARKERS):
            node.decompose()


def unwrap_pictures(root: Tag) -> None:
    for picture in list(root.find_all("picture")):
        img = picture.find("img")
        if img:
            picture.replace_with(img)


def remove_probable_duplicate_theme_images(root: Tag) -> None:
    """Remove adjacent light/dark duplicate images before attributes are stripped.

    Next's pages often render paired images for light and dark themes. For a static
    EPUB, keeping both doubles screenshots and looks broken. We dedupe adjacent
    images with the same alt text, keeping the first one. This intentionally runs
    before local asset download.
    """
    imgs = list(root.find_all("img"))
    previous_alt: str | None = None
    previous_parent: Tag | None = None
    for img in imgs:
        if img.parent is None:
            continue
        alt = (img.get("alt") or "").strip()
        parent = img.parent if isinstance(img.parent, Tag) else None
        if alt and previous_alt == alt and parent is previous_parent:
            img.decompose()
            continue
        previous_alt = alt or None
        previous_parent = parent


def strip_line_number_gutter(pre: Tag) -> None:
    for selector in [
        ".line-number", ".line-numbers-rows", ".linenumber", ".lineNumber",
        "[data-line-number]", "[aria-hidden='true']",
    ]:
        for node in list(pre.select(selector)):
            node.decompose()


def infer_code_language(pre: Tag, code: Tag | None, label: str) -> str:
    attrs: list[str] = []
    for node in [code, pre, pre.parent if isinstance(pre.parent, Tag) else None]:
        if not node:
            continue
        for key, value in node.attrs.items():
            if isinstance(value, list):
                attrs.extend(str(v) for v in value)
            else:
                attrs.append(str(value))
    haystack = " ".join(attrs)
    for pat in [r"language-([A-Za-z0-9_+.-]+)", r"lang(?:uage)?[=: ]+([A-Za-z0-9_+.-]+)"]:
        m = re.search(pat, haystack)
        if m:
            return m.group(1).lower()
    lower_label = label.lower()
    suffix_map = {
        ".tsx": "tsx", ".ts": "ts", ".jsx": "jsx", ".js": "js",
        ".css": "css", ".json": "json", ".md": "md", ".sql": "sql",
        ".env": "bash", ".sh": "bash", ".mjs": "js", ".cjs": "js",
    }
    for suffix, lang in suffix_map.items():
        if suffix in lower_label:
            return lang
    if any(word in lower_label for word in ["terminal", "bash", "shell", "command"]):
        return "bash"
    return ""


def nearby_code_label(pre: Tag) -> str:
    labels: list[str] = []
    for sib in list(pre.previous_siblings)[0:4]:
        if isinstance(sib, Tag):
            text = " ".join(sib.get_text(" ", strip=True).split())
            if text and len(text) < 100:
                labels.append(text)
        elif str(sib).strip():
            txt = " ".join(str(sib).strip().split())
            if txt and len(txt) < 100:
                labels.append(txt)
    return " ".join(reversed(labels))


def clean_code_text(text: str) -> str:
    text = text.replace("\xa0", " ")
    text = text.replace("\r\n", "\n").replace("\r", "\n")
    # Remove excessive blank lines caused by nested highlighted spans.
    text = re.sub(r"\n{3,}", "\n\n", text)
    return text.strip("\n")


def clean_code_blocks(root: Tag) -> None:
    factory = BeautifulSoup("", "lxml")
    for pre in list(root.find_all("pre")):
        strip_line_number_gutter(pre)
        code = pre.find("code")
        label = nearby_code_label(pre)
        lang = infer_code_language(pre, code, label)

        # Important: use separator="". Using "\n" inserts a newline between
        # every syntax-highlighting token/span, producing unusable code blocks.
        source = code if code else pre
        text = clean_code_text(source.get_text("", strip=False))

        new_pre = factory.new_tag("pre")
        new_code = factory.new_tag("code")
        if lang:
            new_code["class"] = [f"language-{lang}"]
        new_code.string = text
        new_pre.append(new_code)
        pre.replace_with(new_pre)


def rewrite_images(session: requests.Session, root: Tag, current_url: str, assets_dir: Path) -> None:
    for img in list(root.find_all("img")):
        src = img.get("src") or img.get("data-src")
        if not src:
            srcset = img.get("srcset") or ""
            if srcset:
                # Prefer the first srcset candidate; it is usually enough for EPUB.
                src = srcset.split(",")[0].strip().split(" ")[0]
        if not src:
            img.decompose()
            continue
        img["src"] = download_asset(session, src, current_url, assets_dir)
        if not img.get("alt"):
            img["alt"] = "Image"
        for attr in list(img.attrs):
            if attr not in {"src", "alt", "title"}:
                del img[attr]


def rewrite_links(root: Tag, current_url: str) -> None:
    for a in list(root.find_all("a")):
        href = a.get("href")
        url = normalize_url(href, current_url) if href else None
        if not url:
            a.unwrap()
            continue
        parsed = urllib.parse.urlparse(url)
        if parsed.netloc == "nextjs.org" or not parsed.netloc:
            a["href"] = urllib.parse.urlunparse(parsed._replace(scheme="https", netloc="nextjs.org"))
        else:
            a["href"] = url


def html_to_markdown(fragment_html: str) -> str:
    text = markdownify(
        fragment_html,
        heading_style="ATX",
        bullets="-",
        strip=["span"],
    )
    text = html.unescape(text)
    text = re.sub(r"\n{4,}", "\n\n", text)
    text = re.sub(r"[ \t]+\n", "\n", text)
    return text.strip()


def strip_leading_chapter_chrome(markdown: str, title: str) -> str:
    title_re = re.escape(title.strip())
    # The rendered Learn pages commonly start as:
    #   # Title
    #   1
    #   Chapter 1
    #   # Title
    # build_markdown adds the chapter h1 itself, so strip all of that chrome.
    changed = True
    while changed:
        before = markdown
        markdown = re.sub(r"^\s*#\s+" + title_re + r"\s*\n+", "", markdown, flags=re.I)
        markdown = re.sub(r"^\s*\d+\s*\n\s*Chapter\s+\d+\s*\n+", "", markdown, flags=re.I)
        changed = markdown != before
    return markdown.lstrip()


def remove_completion_and_nextup(markdown: str) -> str:
    # Remove the course UI block at chapter endings. It is useful online but noisy in a book.
    markdown = re.sub(
        r"\n+##\s+You've Completed Chapter[\s\S]*?(?=\n+<p class=\"source-url\"|\Z)",
        "\n",
        markdown,
        flags=re.I,
    )
    markdown = re.sub(
        r"\n+Next Up\s*\n+\d+:[\s\S]*?\n+\[Start Chapter[^\]]*\]\([^)]*\)\s*(?=\n+<p class=\"source-url\"|\Z)",
        "\n",
        markdown,
        flags=re.I,
    )
    return markdown


def dedupe_adjacent_markdown_images(markdown: str) -> str:
    # Remove light/dark duplicate images after markdown conversion too.
    image_re = re.compile(r"!\[([^\]]*)\]\(([^)]+)\)")
    lines = markdown.splitlines()
    out: list[str] = []
    prev_alt: str | None = None
    for line in lines:
        matches = list(image_re.finditer(line))
        if len(matches) > 1:
            kept: list[str] = []
            seen_alts: set[str] = set()
            pos = 0
            rebuilt = ""
            for m in matches:
                rebuilt += line[pos:m.start()]
                alt = m.group(1).strip()
                if alt and alt in seen_alts:
                    pass
                else:
                    rebuilt += m.group(0)
                    if alt:
                        seen_alts.add(alt)
                pos = m.end()
            rebuilt += line[pos:]
            line = rebuilt
        m = image_re.fullmatch(line.strip())
        if m:
            alt = m.group(1).strip()
            if alt and alt == prev_alt:
                continue
            prev_alt = alt or None
        elif line.strip():
            prev_alt = None
        out.append(line)
    return "\n".join(out)


def collapse_duplicate_headings(markdown: str) -> str:
    lines = markdown.splitlines()
    out: list[str] = []
    last_heading: str | None = None
    blank_since = False
    for line in lines:
        stripped = line.strip()
        if stripped.startswith("#"):
            norm = re.sub(r"\s+", " ", stripped.lower())
            if norm == last_heading and blank_since:
                continue
            last_heading = norm
            blank_since = False
        elif stripped == "":
            blank_since = True
        else:
            last_heading = None
            blank_since = False
        out.append(line)
    return "\n".join(out)




def demote_body_headings(markdown: str) -> str:
    """Make fetched page headings subordinate to the generated chapter title.

    Each chapter is wrapped by build_markdown() with its own `# Chapter Title`.
    Any headings extracted from the page body should therefore start at `##`.
    This also prevents UI/tab labels such as `# macOS` from becoming EPUB
    chapters when the source site renders them with large heading styles.
    Fenced code blocks are left untouched.
    """
    out: list[str] = []
    in_fence = False
    for line in markdown.splitlines():
        if re.match(r"^\s*```", line):
            in_fence = not in_fence
            out.append(line)
            continue
        if not in_fence:
            m = re.match(r"^(#{1,5})(\s+.+)$", line)
            if m:
                line = "#" + line
        out.append(line)
    return "\n".join(out)


def normalize_platform_heading_noise(markdown: str) -> str:
    """Clean a couple of platform-tab labels that render awkwardly in static books."""
    lines = markdown.splitlines()
    out: list[str] = []
    in_fence = False
    for line in lines:
        if re.match(r"^\s*```", line):
            in_fence = not in_fence
            out.append(line)
            continue
        if not in_fence:
            stripped = line.strip()
            if re.match(r"^#{2,6}\s+Windows can use https?://", stripped, re.I):
                text = re.sub(r"^#{2,6}\s+", "", stripped)
                line = f"> {text}"
            elif re.match(r"^#{2,6}\s+(macOS|Windows|Linux)\s*$", stripped, re.I):
                label = re.sub(r"^#{2,6}\s+", "", stripped)
                line = f"### {label}"
        out.append(line)
    return "\n".join(out)


def remove_isolated_chapter_numbers(markdown: str) -> str:
    """Remove site progress/chapter numbers that are not content.

    The Learn pages render a bare number near the end of many chapters
    (for example `13` before the next-chapter card). In Markdown/EPUB this
    is just noise. Fenced code blocks are preserved exactly.
    """
    out: list[str] = []
    in_fence = False
    for line in markdown.splitlines():
        if re.match(r"^\s*```", line):
            in_fence = not in_fence
            out.append(line)
            continue
        if not in_fence and re.match(r"^\s*(?:[1-9]|1[0-9]|2[0-9])\s*$", line):
            continue
        out.append(line)
    return "\n".join(out)



def pandoc_language_for_fence(label: str, code: str) -> str:
    """Infer a Pandoc-supported language for a Markdown code fence.

    The Next.js Learn site renders nice labelled code blocks, but after HTML to
    Markdown conversion those labels become nearby plain text (for example
    `Terminal` or `/app/page.tsx`) and the fence itself may be plain ```.
    Pandoc only syntax-highlights when the fence has a supported language.
    """
    label_l = label.lower().strip()
    code_l = code.strip().lower()

    if any(word in label_l for word in ["terminal", "shell", "bash", "command line"]):
        return "bash"
    if label_l.endswith((".tsx", ".jsx")) or ".tsx" in label_l or ".jsx" in label_l:
        # Pandoc does not reliably highlight `tsx`, but its `jsx` highlighter
        # gives useful colour for TSX/JSX examples.
        return "jsx"
    if label_l.endswith(".ts") or ".ts" in label_l:
        return "ts"
    if label_l.endswith((".js", ".mjs", ".cjs")) or any(x in label_l for x in [".js", ".mjs", ".cjs"]):
        return "javascript"
    if label_l.endswith(".css") or ".css" in label_l:
        return "css"
    if label_l.endswith(".json") or ".json" in label_l:
        return "json"
    if label_l.endswith(".sql") or ".sql" in label_l:
        return "sql"
    if label_l.endswith((".env", ".env.local")) or ".env" in label_l:
        return "bash"

    stripped = code.strip()
    if not stripped:
        return ""
    first = stripped.splitlines()[0].strip()
    if first.startswith(("npm ", "npx ", "pnpm ", "yarn ", "cd ", "git ", "openssl ", "vercel ", "node ")):
        return "bash"
    if re.search(r"^\s*(import|export)\s+", code, re.M) or " from '" in code or ' from "' in code:
        if re.search(r"<[/A-Za-z][^>]*>", code) or "className=" in code:
            return "jsx"
        return "ts"
    if re.search(r"^\s*(const|let|var|async function|function|type|interface)\b", code, re.M):
        if re.search(r"<[/A-Za-z][^>]*>", code) or "className=" in code:
            return "jsx"
        return "ts"
    if stripped.startswith(("{", "[")) and re.search(r'"[^"\n]+"\s*:', stripped):
        return "json"
    if re.search(r"\b(select|insert|update|delete|create table|alter table)\b", code_l):
        return "sql"
    return ""


def add_markdown_code_fence_languages(markdown: str) -> str:
    """Add useful language tags to plain Markdown fences for Pandoc highlighting.

    Leaves existing language tags and fenced code contents untouched.
    """
    lines = markdown.splitlines()
    out: list[str] = []
    i = 0
    last_nonempty = ""
    while i < len(lines):
        line = lines[i]
        m = re.match(r"^```\s*([A-Za-z0-9_+.-]*)\s*$", line)
        if not m:
            out.append(line)
            if line.strip():
                last_nonempty = line.strip()
            i += 1
            continue

        existing_lang = m.group(1).strip()
        fence_start = i
        code_lines: list[str] = []
        i += 1
        while i < len(lines) and not re.match(r"^```\s*$", lines[i]):
            code_lines.append(lines[i])
            i += 1

        code = "\n".join(code_lines)
        lang = existing_lang or pandoc_language_for_fence(last_nonempty, code)
        out.append(f"```{lang}" if lang else "```")
        out.extend(code_lines)
        if i < len(lines):
            out.append(lines[i])
            i += 1
        else:
            # Unclosed fence; preserve content and let Pandoc complain if needed.
            pass
        last_nonempty = ""
    return "\n".join(out)


def polish_markdown(markdown: str, title: str) -> str:
    markdown = strip_leading_chapter_chrome(markdown, title)
    markdown = remove_completion_and_nextup(markdown)
    markdown = demote_body_headings(markdown)
    markdown = normalize_platform_heading_noise(markdown)
    markdown = remove_isolated_chapter_numbers(markdown)
    markdown = dedupe_adjacent_markdown_images(markdown)
    markdown = collapse_duplicate_headings(markdown)
    markdown = add_markdown_code_fence_languages(markdown)
    markdown = re.sub(r"\n{4,}", "\n\n", markdown)
    return markdown.strip()


def extract_page(session: requests.Session, url: str, assets_dir: Path) -> tuple[Page, list[str]]:
    resp = fetch(session, url)
    soup = BeautifulSoup(resp.text, "lxml")
    main = find_main_content(soup)
    h1 = main.find("h1")
    title = h1.get_text(" ", strip=True) if h1 else ""
    if not title and soup.title:
        title = soup.title.get_text(" ", strip=True).replace(" | Next.js", "")
    title = title or url.rstrip("/").split("/")[-1].replace("-", " ").title()

    links: list[str] = []
    for a in main.find_all("a"):
        next_url = normalize_url(a.get("href", ""), url)
        if next_url:
            links.append(next_url)

    content = BeautifulSoup(str(main), "lxml")
    root = content.body or content
    remove_noise(root)
    unwrap_pictures(root)
    remove_probable_duplicate_theme_images(root)
    clean_code_blocks(root)
    rewrite_images(session, root, url, assets_dir)
    rewrite_links(root, url)

    body_md = html_to_markdown(str(root))
    body_md = polish_markdown(body_md, title)
    source = f"\n\n<p class=\"source-url\">Source: <a href=\"{url}\">{url}</a></p>\n"
    return Page(url=url, title=title, markdown=body_md + source), links


def crawl_course(session: requests.Session, course: str, assets_dir: Path, max_pages: int) -> list[Page]:
    seeds = [urllib.parse.urljoin(BASE, p) for p in COURSE_SEEDS[course]]
    seen: set[str] = set()
    queued: list[str] = []
    pages: list[Page] = []

    def enqueue(url: str) -> None:
        parsed = urllib.parse.urlparse(url)
        clean = urllib.parse.urlunparse(parsed._replace(query="", fragment="")).rstrip("/")
        if clean not in seen and clean not in queued and is_learn_page(clean, course):
            queued.append(clean)

    for seed in seeds:
        enqueue(seed)

    while queued and len(pages) < max_pages:
        url = queued.pop(0)
        if url in seen:
            continue
        seen.add(url)
        print(f"Fetching {url}", file=sys.stderr)
        try:
            page, links = extract_page(session, url, assets_dir)
            if len(page.markdown) > 200:
                pages.append(page)
        except Exception as exc:
            print(f"WARN: failed {url}: {exc}", file=sys.stderr)
            continue
        for link in links:
            enqueue(link)
        time.sleep(0.25)

    known_order = {urllib.parse.urljoin(BASE, p).rstrip("/"): i for i, p in enumerate(COURSE_SEEDS[course])}
    pages.sort(key=lambda p: (known_order.get(p.url.rstrip("/"), 10_000), p.url))
    return pages


def write_css(out_dir: Path) -> tuple[Path, Path]:
    html_css = out_dir / "book.css"
    epub_css = out_dir / "epub.css"
    css = r"""
:root { color-scheme: light; }
body {
  font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", system-ui, sans-serif;
  line-height: 1.62;
  color: #171717;
  max-width: 880px;
  margin: 0 auto;
  padding: 2.5rem 1.5rem;
}
h1, h2, h3, h4 { line-height: 1.22; margin-top: 1.8em; }
h1 { font-size: 2.05rem; border-bottom: 1px solid #e5e5e5; padding-bottom: .35rem; }
h2 { font-size: 1.48rem; }
h3 { font-size: 1.22rem; }
a { color: #0645ad; text-decoration: none; }
a:hover { text-decoration: underline; }
pre {
  padding: 1rem;
  border-radius: 10px;
  background: #0b1020;
  color: #f8fafc;
  overflow-x: auto;
  white-space: pre-wrap;
  word-break: break-word;
  font-size: .88rem;
  line-height: 1.45;
}
code {
  font-family: "SFMono-Regular", Consolas, "Liberation Mono", Menlo, monospace;
  font-size: .92em;
}
:not(pre) > code {
  background: #f2f4f8;
  padding: .12rem .28rem;
  border-radius: 4px;
}
blockquote {
  border-left: 4px solid #d0d7de;
  margin: 1.2rem 0;
  padding: .25rem 1rem;
  background: #f6f8fa;
}
table { border-collapse: collapse; width: 100%; margin: 1.2rem 0; font-size: .92rem; }
th, td { border: 1px solid #d0d7de; padding: .45rem .6rem; vertical-align: top; }
th { background: #f6f8fa; }
img { max-width: 100%; height: auto; display: block; margin: 1rem auto; }
.chapter-source, .source-url { color: #666; font-size: .82rem; }
hr { border: 0; border-top: 1px solid #e5e5e5; margin: 2.5rem 0; }
/* Pandoc syntax-highlighting classes. Pandoc emits these spans in EPUB/HTML
   when fenced code blocks have a recognised language such as ts, jsx, bash,
   css, json, or sql. Keep these colours explicit because passing --css to
   Pandoc prevents its default highlight stylesheet from being included. */
pre.sourceCode, pre code.sourceCode { color: #f8fafc; }
.sourceCode .kw { color: #ff7b72; font-weight: 600; }
.sourceCode .dt, .sourceCode .cf { color: #ffa657; font-weight: 600; }
.sourceCode .fu, .sourceCode .ex { color: #d2a8ff; }
.sourceCode .st, .sourceCode .ch, .sourceCode .ss { color: #a5d6ff; }
.sourceCode .co { color: #8b949e; font-style: italic; }
.sourceCode .im, .sourceCode .pp { color: #ff7b72; }
.sourceCode .op { color: #f8fafc; }
.sourceCode .dv, .sourceCode .bn, .sourceCode .fl { color: #79c0ff; }
.sourceCode .bu, .sourceCode .cn { color: #79c0ff; }
.sourceCode .va, .sourceCode .ot { color: #ffa657; }
.sourceCode .at { color: #7ee787; }
.sourceCode .al, .sourceCode .er { color: #ff7b72; font-weight: 700; }
.sourceCode a { color: inherit; text-decoration: none; }
@media print {
  body { max-width: none; padding: 0; }
  h1 { break-before: page; }
  h1:first-child { break-before: auto; }
  pre, blockquote, table, img { break-inside: avoid; }
}
""".strip()
    html_css.write_text(css, encoding="utf-8")
    epub_css.write_text(css, encoding="utf-8")
    return html_css, epub_css


def build_markdown(pages: list[Page], out_dir: Path, title: str, fetched_at: str) -> Path:
    md_path = out_dir / "nextjs-learn.md"
    parts = [
        f"% {title}\n% Vercel / Next.js public Learn pages\n% Generated {fetched_at}\n",
        "# About this book\n",
        textwrap.dedent(f"""
        This book was generated from the public Next.js Learn pages for personal offline reading.
        The starter/example code is available separately in the `vercel/next-learn` repository; the curriculum text is fetched from `nextjs.org/learn`.

        Generated: {fetched_at}
        """).strip(),
    ]
    for page in pages:
        parts.append("\n\n<hr/>\n\n")
        parts.append(f"# {page.title}\n\n")
        parts.append(f"<p class=\"chapter-source\">Original: <a href=\"{page.url}\">{page.url}</a></p>\n\n")
        parts.append(page.markdown)
    final = "\n".join(parts).strip() + "\n"
    md_path.write_text(final, encoding="utf-8")
    return md_path


def pandoc_available() -> bool:
    return pandoc_path() is not None


def build_html(md_path: Path, out_dir: Path, css: Path, title: str) -> Path:
    html_path = out_dir / "nextjs-learn.html"
    pandoc = pandoc_path()
    if pandoc:
        run([
            pandoc, str(md_path),
            "--from", "gfm+yaml_metadata_block",
            "--to", "html5",
            "--standalone",
            "--toc", "--toc-depth=3",
            "--metadata", f"title={title}",
            "--css", css.name,
            "--highlight-style", "tango",
            "-o", str(html_path),
        ])
    else:
        body = "<pre>" + html.escape(md_path.read_text(encoding="utf-8")) + "</pre>"
        html_path.write_text(f"<!doctype html><meta charset='utf-8'><title>{html.escape(title)}</title>{body}", encoding="utf-8")
    return html_path


def build_epub(md_path: Path, out_dir: Path, css: Path, title: str) -> Path | None:
    pandoc = pandoc_path()
    if not pandoc:
        print("WARN: pandoc not found; skipping EPUB", file=sys.stderr)
        return None
    epub_path = out_dir / "nextjs-learn.epub"
    run([
        pandoc, str(md_path),
        "--from", "gfm+yaml_metadata_block",
        "--to", "epub3",
        "--standalone",
        "--toc", "--toc-depth=3",
        "--metadata", f"title={title}",
        "--metadata", "author=Vercel / Next.js public Learn pages",
        "--resource-path", str(out_dir),
        "--css", str(css),
        "--highlight-style", "tango",
        "-o", str(epub_path),
    ])
    return epub_path


def build_pdf(html_path: Path, out_dir: Path, engine: str) -> Path | None:
    pdf_path = out_dir / "nextjs-learn.pdf"
    selected = engine
    if engine == "auto":
        if which("weasyprint"):
            selected = "weasyprint"
        elif which("chromium") or which("google-chrome") or which("chrome") or which("chromium-browser"):
            selected = "chromium"
        else:
            print("WARN: no PDF engine found; install weasyprint or chromium", file=sys.stderr)
            return None

    if selected == "weasyprint":
        if not which("weasyprint"):
            print("WARN: weasyprint not found; skipping PDF", file=sys.stderr)
            return None
        run(["weasyprint", str(html_path), str(pdf_path)])
        return pdf_path

    if selected == "chromium":
        browser = which("chromium") or which("google-chrome") or which("chrome") or which("chromium-browser")
        if not browser:
            print("WARN: chromium/google-chrome not found; skipping PDF", file=sys.stderr)
            return None
        run([
            browser,
            "--headless", "--disable-gpu", "--no-sandbox",
            f"--print-to-pdf={pdf_path}",
            html_path.resolve().as_uri(),
        ])
        return pdf_path

    print(f"WARN: unknown PDF engine {engine!r}; skipping PDF", file=sys.stderr)
    return None



TOKEN_INLINE_STYLES = {
    "kw": "color:#ff7b72;font-weight:600;",
    "dt": "color:#ffa657;font-weight:600;",
    "cf": "color:#ffa657;font-weight:600;",
    "fu": "color:#d2a8ff;",
    "ex": "color:#d2a8ff;",
    "st": "color:#a5d6ff;",
    "ch": "color:#a5d6ff;",
    "ss": "color:#a5d6ff;",
    "co": "color:#8b949e;font-style:italic;",
    "im": "color:#ff7b72;",
    "pp": "color:#ff7b72;",
    "op": "color:#f8fafc;",
    "dv": "color:#79c0ff;",
    "bn": "color:#79c0ff;",
    "fl": "color:#79c0ff;",
    "bu": "color:#79c0ff;",
    "cn": "color:#79c0ff;",
    "va": "color:#ffa657;",
    "ot": "color:#ffa657;",
    "at": "color:#7ee787;",
    "al": "color:#ff7b72;font-weight:700;",
    "er": "color:#ff7b72;font-weight:700;",
}

CODE_CONTAINER_STYLE = (
    "background-color:#0b1020;color:#f8fafc;"
    "border-radius:10px;padding:1em;"
    "white-space:pre-wrap;word-break:break-word;"
)


def merge_style(existing: str | None, addition: str) -> str:
    existing = (existing or "").strip()
    if existing and not existing.endswith(";"):
        existing += ";"
    return (existing + addition).strip()


def inline_epub_syntax_styles(epub_path: Path) -> None:
    """Inline syntax colours inside the generated EPUB.

    Pandoc creates proper token spans for recognised fenced-code languages, but
    some EPUB readers, including some Boox configurations, ignore or partially
    override publisher CSS. Inline styles are less elegant but much more robust
    for offline technical reading.
    """
    if not epub_path.exists():
        return

    with tempfile.TemporaryDirectory() as tmp_name:
        tmp = Path(tmp_name)
        with zipfile.ZipFile(epub_path, "r") as zin:
            zin.extractall(tmp)

        for xhtml in (tmp / "EPUB").rglob("*.xhtml"):
            text = xhtml.read_text(encoding="utf-8")
            if "sourceCode" not in text:
                continue
            soup = BeautifulSoup(text, "xml")

            for pre in soup.find_all("pre"):
                classes = pre.get("class") or []
                if "sourceCode" in classes:
                    pre["style"] = merge_style(pre.get("style"), CODE_CONTAINER_STYLE)
            for code in soup.find_all("code"):
                classes = code.get("class") or []
                if "sourceCode" in classes:
                    code["style"] = merge_style(code.get("style"), "color:#f8fafc;background-color:#0b1020;")
            for span in soup.find_all("span"):
                classes = span.get("class") or []
                if isinstance(classes, str):
                    classes = classes.split()
                for cls in classes:
                    style = TOKEN_INLINE_STYLES.get(cls)
                    if style:
                        span["style"] = merge_style(span.get("style"), style)
                        break
            for anchor in soup.find_all("a"):
                parent = anchor.parent
                if parent and parent.name == "span" and (parent.get("id") or "").startswith("cb"):
                    anchor["style"] = merge_style(anchor.get("style"), "color:inherit;text-decoration:none;")
            xhtml.write_text(str(soup), encoding="utf-8")

        tmp_epub = epub_path.with_suffix(".tmp.epub")
        if tmp_epub.exists():
            tmp_epub.unlink()
        with zipfile.ZipFile(tmp_epub, "w") as zout:
            mimetype = tmp / "mimetype"
            if mimetype.exists():
                zout.write(mimetype, "mimetype", compress_type=zipfile.ZIP_STORED)
            for file in sorted(tmp.rglob("*")):
                if file.is_dir() or file == mimetype:
                    continue
                arc = file.relative_to(tmp).as_posix()
                zout.write(file, arc, compress_type=zipfile.ZIP_DEFLATED)
        tmp_epub.replace(epub_path)


def maybe_clone_starter_code(work_dir: Path) -> Path | None:
    if not which("git"):
        print("WARN: git not found; cannot clone vercel/next-learn", file=sys.stderr)
        return None
    repo = work_dir / "next-learn"
    if repo.exists():
        run(["git", "-C", str(repo), "pull", "--ff-only"], check=False)
    else:
        ensure_dir(work_dir)
        run(["git", "clone", "--depth", "1", "https://github.com/vercel/next-learn.git", str(repo)], check=False)
    return repo if repo.exists() else None


def quality_report(md_path: Path, out_dir: Path) -> Path:
    text = md_path.read_text(encoding="utf-8")
    report_path = out_dir / "quality-report.txt"
    lines = text.splitlines()
    h1s: list[tuple[int, str]] = []
    possible_bad_code = []
    in_fence = False
    fence_start = 0
    fence_lines: list[str] = []
    for idx, line in enumerate(lines, 1):
        if line.startswith("```"):
            if not in_fence:
                in_fence = True
                fence_start = idx
                fence_lines = []
            else:
                if len(fence_lines) >= 5:
                    shortish = sum(1 for x in fence_lines if 0 < len(x.strip()) <= 2)
                    if shortish / max(len(fence_lines), 1) > 0.45:
                        possible_bad_code.append((fence_start, len(fence_lines)))
                in_fence = False
            continue
        if in_fence:
            fence_lines.append(line)
        elif line.startswith("# "):
            h1s.append((idx, line))

    fence_langs: dict[str, int] = {}
    for m in re.finditer(r"^```([^`\s]*)", text, re.M):
        lang = m.group(1) or "plain"
        fence_langs[lang] = fence_langs.get(lang, 0) + 1

    warnings = []
    for pattern in ["NoneType", "Traceback", "Was this helpful?", "Sign in to save progress", "Progress isn"]:
        if pattern in text:
            warnings.append(f"Found leftover marker: {pattern}")
    suspicious_h1s = [h for _, h in h1s if re.search(r"macOS|Windows can use|Practice:", h, re.I)]
    if suspicious_h1s:
        warnings.append(f"Suspicious top-level headings: {suspicious_h1s[:10]}")
    if possible_bad_code:
        warnings.append(f"Possible tokenized code fences: {possible_bad_code[:10]}")

    report = [
        f"Markdown: {md_path}",
        f"Lines: {len(lines)}",
        f"Top-level headings: {len(h1s)}",
        "",
        "First top-level headings:",
        *[f"  {line_no}: {heading}" for line_no, heading in h1s[:40]],
        "",
        "Code fence languages:",
        *(f"  {lang}: {count}" for lang, count in sorted(fence_langs.items())),
        "",
        "Warnings:",
        *(warnings or ["  none"]),
    ]
    report_path.write_text("\n".join(report) + "\n", encoding="utf-8")
    return report_path


def main(argv: list[str]) -> int:
    parser = argparse.ArgumentParser(description="Build EPUB/PDF from Next.js Learn pages")
    parser.add_argument("--course", choices=sorted(COURSE_SEEDS), default="app", help="which Learn course to crawl")
    parser.add_argument("--out-dir", type=Path, default=Path("nextjs-learn-book"))
    parser.add_argument("--work-dir", type=Path, default=Path(".nextjs-learn-book-work"))
    parser.add_argument("--title", default=None)
    parser.add_argument("--max-pages", type=int, default=80)
    parser.add_argument("--skip-epub", action="store_true")
    parser.add_argument("--skip-pdf", action="store_true")
    parser.add_argument("--pdf-engine", choices=["auto", "weasyprint", "chromium"], default="auto")
    parser.add_argument("--clone-starter-code", action="store_true", help="also clone vercel/next-learn for starter/final examples")
    parser.add_argument("--user-agent", default=DEFAULT_UA)
    parser.add_argument("--clean", action="store_true", help="remove out-dir before building")
    args = parser.parse_args(argv)

    if args.clean and args.out_dir.exists():
        shutil.rmtree(args.out_dir)
    ensure_dir(args.out_dir)
    assets_dir = args.out_dir / "assets"
    ensure_dir(assets_dir)

    if args.clone_starter_code:
        maybe_clone_starter_code(args.work_dir)

    session = requests.Session()
    session.headers.update({"User-Agent": args.user_agent})

    title = args.title or {
        "app": "Next.js Learn: App Router Course",
        "react": "Next.js Learn: React Foundations",
        "pages": "Next.js Learn: Pages Router Course",
        "all": "Next.js Learn Courses",
    }[args.course]

    pages = crawl_course(session, args.course, assets_dir, args.max_pages)
    if not pages:
        raise SystemExit("No pages fetched; cannot build book")

    fetched_at = time.strftime("%Y-%m-%d %H:%M:%S %Z")
    html_css, epub_css = write_css(args.out_dir)
    md_path = build_markdown(pages, args.out_dir, title, fetched_at)
    report_path = quality_report(md_path, args.out_dir)
    html_path = build_html(md_path, args.out_dir, html_css, title)

    epub_path = None
    pdf_path = None
    if not args.skip_epub:
        epub_path = build_epub(md_path, args.out_dir, epub_css, title)
        if epub_path:
            inline_epub_syntax_styles(epub_path)
    if not args.skip_pdf:
        pdf_path = build_pdf(html_path, args.out_dir, args.pdf_engine)

    print("\nDone.", file=sys.stderr)
    print(f"Markdown: {md_path}", file=sys.stderr)
    print(f"HTML:     {html_path}", file=sys.stderr)
    print(f"Report:   {report_path}", file=sys.stderr)
    if epub_path:
        print(f"EPUB:     {epub_path}", file=sys.stderr)
    if pdf_path:
        print(f"PDF:      {pdf_path}", file=sys.stderr)
    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv[1:]))
