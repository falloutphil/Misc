## Setup

Create a virtual environment and install the Python dependencies:
```bash
python3 -m venv venv
source ./venv/bin/activate
./venv/bin/pip install -r requirements.txt
```

## Basic Build

Build the course content as Markdown and HTML only:
```bash
rm -rf ./nextjs-app-router-course
python3 build_nextjs_learn_book.py --course app --out-dir ./nextjs-app-router-course --skip-pdf --clean
```

## PDF Output

PDF generation works with `weasyprint`, which is included in `requirements.txt`:
```bash
python3 build_nextjs_learn_book.py --course app --out-dir ./nextjs-app-router-course --skip-epub --clean
```

The script can also use a local Chromium/Chrome install when selected explicitly:
```bash
python3 build_nextjs_learn_book.py \
  --course app \
  --out-dir ./nextjs-app-router-course \
  --skip-epub \
  --pdf-engine chromium \
  --clean
```

## EPUB Output

EPUB generation requires a `pandoc` binary. This repo supports either of these paths:

1. Install system `pandoc`.
2. Use the `pypandoc_binary` package already listed in `requirements.txt`.

Build EPUB only:
```bash
python3 build_nextjs_learn_book.py --course app --out-dir ./nextjs-app-router-course --skip-pdf --clean
```

## Full Build

Build Markdown, HTML, EPUB, and PDF together:
```bash
python3 build_nextjs_learn_book.py --course app --out-dir ./nextjs-app-router-course --clean
```

## Outputs

The build writes these files to the chosen `--out-dir`:
- `nextjs-learn.md`
- `nextjs-learn.html`
- `nextjs-learn.epub`
- `nextjs-learn.pdf`
- `quality-report.txt`
