#!/usr/bin/env bash
# kb-tool.sh — Project-scoped Chroma MCP toolkit for Claude Code
#
# Features:
#   - install : pipx-install chroma-mcp + PDF dependencies (PyMuPDF)
#   - init    : create project structure (.chroma, textbooks/, bin/chroma-mcp-here)
#   - ingest  : parse PDFs in textbooks/ and upsert chunks into project-local Chroma DB
#   - add-mcp : register a Claude MCP that uses the project-local DB (wrapper)
#   - status  : show current config & sanity checks
#   - all     : install + init + ingest + add-mcp
#
# Design principles:
#   - Per-project isolation by default (DB lives in $PROJECT_DIR/.chroma)
#   - Same path shared by ingest + MCP via CHROMA_MCP_DIR (wrapper enforces it)
#   - Idempotent operations (safe to re-run)
#   - Clear logs and helpful errors
#
# Requirements:
#   - bash, python3, pipx, claude CLI (for add-mcp), and system libs needed by PyMuPDF
#   - chroma-mcp will be installed by `install`
#
# Usage examples:
#   ./kb-tool.sh all
#   ./kb-tool.sh init --project-dir ~/git/myproj
#   ./kb-tool.sh ingest --chunk-size 950 --chunk-overlap 220
#   ./kb-tool.sh add-mcp --mcp-name textbooks-myproj
#
# Exit codes:
#   0 on success, non-zero on error.

set -Eeuo pipefail

# ---------- logging & helpers ----------

bold() { printf '\033[1m%s\033[0m' "$*"; }
green() { printf '\033[32m%s\033[0m' "$*"; }
yellow() { printf '\033[33m%s\033[0m' "$*"; }
red() { printf '\033[31m%s\033[0m' "$*"; }

log() { echo "$(bold "[kb]") $*"; }
ok() { log "$(green "✓") $*"; }
warn() { log "$(yellow "⚠") $*"; }
err() { log "$(red "✗") $*"; }

die() { err "$*"; exit 1; }

trap 'err "unexpected error (line $LINENO)"; exit 2' ERR

# ---------- defaults (overridable via flags) ----------

PROJECT_DIR="$PWD"
TEXTBOOKS_DIR=""
DB_DIR=""
COLLECTION=""
MCP_NAME=""
CHUNK_SIZE=900
CHUNK_OVERLAP=150
BATCH=256
GC_ORPHANS=1
EMBED_MODEL=""
FILE_PATH=""

# Derived defaults happen later (need PROJECT_DIR)

# ---------- usage ----------

usage() {
  cat <<'USAGE'
kb-tool.sh — Project-scoped Chroma MCP toolkit

Commands:
  install           Install chroma-mcp via pipx and inject PDF deps (PyMuPDF).
  init              Create project layout and MCP wrapper in ./bin/.
  ingest            Ingest PDFs from textbooks/ to the project-local Chroma DB.
  add-mcp           Register a Claude MCP that points at the project DB (wrapper).
  status            Print resolved paths and environment checks.
  export            Create a compressed tarball of the project-local DB for transfer.
  import            Import a previously exported tarball into the project DB dir.
  all               Run: install -> init -> ingest -> add-mcp.
  help              Show this help.

Global options (apply to most commands):
  --project-dir DIR       Project root (default: current directory)
  --textbooks-dir DIR     Directory of PDFs (default: $PROJECT_DIR/textbooks)
  --db-dir DIR            Chroma DB dir (default: $PROJECT_DIR/.chroma)
  --collection NAME       Chroma collection (default: textbooks-<project>)
  --mcp-name NAME         Claude MCP tool name (default: textbooks-<project>)
  --chunk-size N          Chunk size (default: 900)
  --chunk-overlap N       Chunk overlap (default: 150)
  --batch N               Upsert batch size (default: 256)
  --gc-orphans 0|1        Remove DB docs for missing PDFs (default: 1)
  --embed-model NAME      Embedding model selector, e.g. "sentence:BAAI/bge-small-en-v1.5"
  --file PATH             For export/import: output or input tar.gz path

Examples:
  ./kb-tool.sh all
  ./kb-tool.sh ingest --project-dir ~/git/myproj --chunk-size 950 --chunk-overlap 220
  ./kb-tool.sh add-mcp --mcp-name textbooks-ml-papers
  ./kb-tool.sh export --file /tmp/myproj-kb.tar.gz
  ./kb-tool.sh import --file /tmp/myproj-kb.tar.gz

Notes:
  - "install" uses pipx: pipx install chroma-mcp && pipx runpip chroma-mcp install PyMuPDF chromadb>=1.0.10
  - The wrapper bin/chroma-mcp-here exports CHROMA_MCP_DIR to ensure the MCP uses the same DB.
  - The ingest step writes a manifest.json file in the DB dir for selective updates and orphan GC.

USAGE
}

# ---------- argument parsing ----------

COMMAND="${1:-help}"
shift || true

while [[ $# -gt 0 ]]; do
  case "$1" in
    --project-dir) PROJECT_DIR="${2:?}"; shift 2;;
    --textbooks-dir) TEXTBOOKS_DIR="${2:?}"; shift 2;;
    --db-dir) DB_DIR="${2:?}"; shift 2;;
    --collection) COLLECTION="${2:?}"; shift 2;;
    --mcp-name) MCP_NAME="${2:?}"; shift 2;;
    --chunk-size) CHUNK_SIZE="${2:?}"; shift 2;;
    --chunk-overlap) CHUNK_OVERLAP="${2:?}"; shift 2;;
    --batch) BATCH="${2:?}"; shift 2;;
    --gc-orphans) GC_ORPHANS="${2:?}"; shift 2;;
    --embed-model) EMBED_MODEL="${2:?}"; shift 2;;
    --file) FILE_PATH="${2:?}"; shift 2;;
    -h|--help) usage; exit 0;;
    *) die "Unknown option: $1";;
  esac
done

# ---------- derive paths / names ----------

# Normalise PROJECT_DIR
PROJECT_DIR="$(cd "$PROJECT_DIR" && pwd)"

PROJECT_BASENAME="$(basename "$PROJECT_DIR")"

: "${TEXTBOOKS_DIR:="$PROJECT_DIR/textbooks"}"
: "${DB_DIR:="$PROJECT_DIR/.chroma"}"
: "${COLLECTION:="textbooks-$PROJECT_BASENAME"}"
: "${MCP_NAME:="textbooks-$PROJECT_BASENAME"}"

BIN_DIR="$PROJECT_DIR/bin"
WRAPPER="$BIN_DIR/chroma-mcp-here"

# ---------- environment checks ----------

need_cmd() {
  command -v "$1" >/dev/null 2>&1 || die "Missing command: $1"
}

pipx_venv_python() {
  # Prefer the standard pipx path, fallback to system python
  local p="$HOME/.local/pipx/venvs/chroma-mcp/bin/python"
  if [[ -x "$p" ]]; then
    echo "$p"
  else
    echo "python3"
  fi
}

assert_pdf_deps() {
  local pybin; pybin="$(pipx_venv_python)"
  if ! "$pybin" - >/dev/null 2>&1 <<'PY'
import sys
ok = True
try:
  import chromadb
except Exception:
  ok = False
try:
  import fitz  # PyMuPDF
except Exception:
  ok = False
try:
  import sentence_transformers  # required if using SentenceTransformerEmbeddingFunction
except Exception:
  ok = False
sys.exit(0 if ok else 1)
PY
  then
    die "Python deps not found. Run: ./kb-tool.sh install"
  fi
}

# ---------- commands ----------

cmd_install() {
  need_cmd pipx
  log "Installing chroma-mcp with pipx…"
  if ! pipx list | grep -q '^package chroma-mcp'; then
    pipx install chroma-mcp || die "pipx install chroma-mcp failed"
  else
    ok "chroma-mcp already installed"
  fi
  log "Injecting/Upgrading Python deps into chroma-mcp venv…"
  # Base deps (no GPU stuff here)
  pipx runpip chroma-mcp install --upgrade "chromadb>=1.0.10" PyMuPDF || die "pipx runpip failed (base deps)"
  # Install CPU-only PyTorch FIRST so sentence-transformers reuses it (no CUDA/NVIDIA wheels)
  pipx runpip chroma-mcp install --upgrade --index-url https://download.pytorch.org/whl/cpu torch || die "pipx runpip failed (torch cpu)"
  # Now sentence-transformers (will not pull CUDA because torch is already satisfied)
  pipx runpip chroma-mcp install --upgrade sentence-transformers || die "pipx runpip failed (sentence-transformers)"
  ok "Python deps present (chromadb, PyMuPDF, torch CPU, sentence-transformers)"
}

cmd_init() {
  log "Initialising project at $(bold "$PROJECT_DIR")"
  mkdir -p "$DB_DIR" "$TEXTBOOKS_DIR" "$BIN_DIR"

  # Wrapper that ensures the MCP server reads the correct DB path
  if [[ ! -x "$WRAPPER" ]]; then
    cat > "$WRAPPER" <<SH
#!/usr/bin/env bash
set -Eeuo pipefail
# Force MCP server to use this project's DB
export CHROMA_MCP_DIR="\${CHROMA_MCP_DIR:-$DB_DIR}"
exec chroma-mcp "\$@"
SH
    chmod +x "$WRAPPER"
    ok "Wrote wrapper: $WRAPPER"
  else
    ok "Wrapper exists: $WRAPPER"
  fi

  # .gitignore hints (safe append)
  if [[ -d "$PROJECT_DIR/.git" ]]; then
    {
      echo "# kb-tool"
      echo "/.chroma/"
      echo "/bin/chroma-mcp-here"
      echo "/textbooks/*.pdf"
      echo "/textbooks/*/*.pdf"
    } | awk 'NF' | sort -u >> "$PROJECT_DIR/.gitignore" 2>/dev/null || true
    ok "Updated .gitignore (if present)"
  fi

  ok "Init complete. Put PDFs into: $TEXTBOOKS_DIR"
}

cmd_ingest() {
  assert_pdf_deps
  mkdir -p "$DB_DIR" "$TEXTBOOKS_DIR"

  # Warn if textbooks dir empty
  if ! ls "$TEXTBOOKS_DIR"/*.pdf >/dev/null 2>&1; then
    warn "No PDFs found in $TEXTBOOKS_DIR — continuing (ingest will be a no-op)"
  fi

  local pybin; pybin="$(pipx_venv_python)"
  log "Ingesting PDFs → Chroma"
  log "  DB:        $DB_DIR"
  log "  Textbooks: $TEXTBOOKS_DIR"
  log "  Coll:      $COLLECTION"
  log "  Chunks:    size=$CHUNK_SIZE overlap=$CHUNK_OVERLAP batch=$BATCH"
  log "  GC orphans: $GC_ORPHANS"

  CHROMA_MCP_DIR="$DB_DIR" \
  TEXTBOOKS_DIR="$TEXTBOOKS_DIR" \
  CHROMA_COLLECTION="$COLLECTION" \
  CHUNK="$CHUNK_SIZE" \
  OVERLAP="$CHUNK_OVERLAP" \
  BATCH="$BATCH" \
  GC_ORPHANS="$GC_ORPHANS" \
  EMBED_MODEL="${EMBED_MODEL:-sentence:BAAI/bge-small-en-v1.5}" \
  "$pybin" - <<'PY'
import os, json, hashlib, time
import chromadb, fitz
from chromadb.utils import embedding_functions as ef

def xdg_data_home():
    return os.environ.get("XDG_DATA_HOME", os.path.expanduser("~/.local/share"))

DATA_DIR = os.environ.get("CHROMA_MCP_DIR") or os.path.join(xdg_data_home(), "chroma_mcp")
DOC_DIR  = os.environ.get("TEXTBOOKS_DIR")  or os.path.join(os.getcwd(), "textbooks")
COLL     = os.environ.get("CHROMA_COLLECTION") or "textbooks"

CHUNK    = int(os.environ.get("CHUNK", "900"))
OVERLAP  = int(os.environ.get("OVERLAP", "150"))
BATCH    = int(os.environ.get("BATCH", "256"))
GC_ORPHANS = os.environ.get("GC_ORPHANS", "1") not in ("0", "false", "False")
EMBED_MODEL = os.environ.get("EMBED_MODEL", "default").strip()
MANIFEST = os.path.join(DATA_DIR, "manifest.json")

os.makedirs(DATA_DIR, exist_ok=True)
client = chromadb.PersistentClient(path=DATA_DIR)

# Embedding selection:
# Default here is set by the caller ABOVE to "sentence:BAAI/bge-small-en-v1.5" for STEM (CS/Math/Eng) textbooks:
# - better recall/precision on technical jargon & definitions than MiniLM default
# - modest CPU/RAM overhead vs MiniLM
# NOTE: If the call did not set EMBED_MODEL (it does here!) then you get the default ChromaDB model as per below
# which is MiniLM via ONNX.
# NOTE: The caller of the caller - i.e. you can set the default or any model using EMBED_MODEL=default kb-init .......
# Note: RAM/latency is paid mainly during *ingest* (embedding PDFs) and *query embedding* at retrieval time.
# Generation cost isn't relevant here (we're not generating text in this script).
embed_fn = None
if EMBED_MODEL == "default":
    embed_fn = ef.DefaultEmbeddingFunction()
elif EMBED_MODEL.startswith("sentence:"):
    model_name = EMBED_MODEL.split(":", 1)[1]
    embed_fn = ef.SentenceTransformerEmbeddingFunction(model_name=model_name)

coll = client.get_or_create_collection(COLL, embedding_function=embed_fn)

def file_digest(path, block=1024*1024):
    h = hashlib.sha1()
    with open(path, "rb") as f:
        while True:
            b = f.read(block)
            if not b: break
            h.update(b)
    return h.hexdigest()

def chunks(text, size=CHUNK, overlap=OVERLAP):
    step = max(1, size-overlap)
    for i in range(0, len(text), step):
        yield text[i:i+size], i // step

def make_id(digest, page, idx):
    return f"{digest[:12]}:{page}:{idx}"

def load_manifest():
    try:
        with open(MANIFEST, "r") as f:
            return json.load(f)
    except Exception:
        return {}

def save_manifest(m):
    os.makedirs(DATA_DIR, exist_ok=True)
    tmp = MANIFEST + ".tmp"
    with open(tmp, "w") as f:
        json.dump(m, f, indent=2, sort_keys=True)
    os.replace(tmp, MANIFEST)

manifest = load_manifest()

pdfs = []
if os.path.isdir(DOC_DIR):
    for f in sorted(os.listdir(DOC_DIR)):
        if f.lower().endswith(".pdf"):
            pdfs.append(os.path.join(DOC_DIR, f))

print("Ingesting:", [os.path.basename(p) for p in pdfs])

if GC_ORPHANS and manifest:
    missing = [src for src in list(manifest.keys()) if not os.path.exists(src)]
    for src in missing:
        base = os.path.basename(src)
        print(f"🗑  removing stale entries for missing file: {base}")
        try:
            coll.delete(where={"source": src})
        except Exception as e:
            print(f"! delete failed for {base}: {e}")
        manifest.pop(src, None)
    if missing:
        save_manifest(manifest)

total_added = 0

for path in pdfs:
    base = os.path.basename(path)
    t0 = time.time()
    try:
        digest = file_digest(path)
    except Exception as e:
        print(f"! digest failed: {base}: {e}")
        continue

    prev = manifest.get(path)
    if prev and prev.get("digest") == digest and prev.get("complete", False):
        print(f"↷ {base}: unchanged (digest={digest[:8]}), skipping")
        continue

    if prev and prev.get("digest") != digest:
        print(f"↻ {base}: changed (old {prev.get('digest','')[:8]} → new {digest[:8]}), removing old chunks")
        try:
            coll.delete(where={"source": path})
        except Exception as e:
            print(f"! delete failed: {base}: {e}")

    try:
        doc = fitz.open(path)
    except Exception as e:
        print(f"! open failed: {base}: {e}")
        continue

    ids, docs, metas = [], [], []
    added_for_file = 0
    for pno in range(len(doc)):
        text = doc[pno].get_text("text")
        if not text.strip():
            continue
        for ch, idx in chunks(text):
            cid = make_id(digest, pno+1, idx)
            ids.append(cid)
            docs.append(ch)
            metas.append({
                "source": path,
                "source_basename": base,
                "page": pno+1,
                "file_digest": digest,
                "chunk_size": CHUNK,
                "overlap": OVERLAP
            })
            if len(ids) >= BATCH:
                coll.upsert(ids=ids, documents=docs, metadatas=metas)
                added_for_file += len(ids)
                print(f"  +{added_for_file}", end="\r")
                ids.clear(); docs.clear(); metas.clear()
    if ids:
        coll.upsert(ids=ids, documents=docs, metadatas=metas)
        added_for_file += len(ids)

    manifest[path] = {"digest": digest, "complete": True, "chunks": added_for_file, "mtime": os.path.getmtime(path)}
    save_manifest(manifest)

    total_added += added_for_file
    dt = time.time()-t0
    print(f"\n✓ {base}: +{added_for_file} chunks in {dt:.1f}s (digest {digest[:8]})")

try:
    cnt = coll.count()
except Exception:
    cnt = "unknown"
print("Total chunks in collection (approx):", cnt)
print("Done. Added/updated:", total_added)

# Persist minimal metadata for export/import & audit
try:
    meta = {
        "collection": COLL,
        "embed_model": EMBED_MODEL,
        "chunk": CHUNK,
        "overlap": OVERLAP,
        "batch": BATCH,
        "created_or_updated": time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime()),
    }
    with open(os.path.join(DATA_DIR, "kb-meta.json"), "w") as f:
        json.dump(meta, f, indent=2, sort_keys=True)
except Exception as e:
    print(f"! failed to write kb-meta.json: {e}")
PY

  ok "Ingest finished"
}

cmd_add_mcp() {
  need_cmd claude
  [[ -x "$WRAPPER" ]] || die "Wrapper not found: $WRAPPER (run init first)"

  log "Ensuring no stale MCP named '$MCP_NAME' remains…"
  claude mcp remove "$MCP_NAME" >/dev/null 2>&1 || true

  log "Registering Claude MCP: $(bold "$MCP_NAME")"
  claude mcp add "$MCP_NAME" "$WRAPPER"
  ok "MCP registered: $MCP_NAME (command: $WRAPPER)"
}

cmd_status() {
  cat <<EOF
$(bold "kb-tool status")
  Project Dir    : $PROJECT_DIR
  Textbooks Dir  : $TEXTBOOKS_DIR
  DB Dir         : $DB_DIR
  Collection     : $COLLECTION
  MCP Name       : $MCP_NAME
  Wrapper        : $WRAPPER
  Chunks         : size=$CHUNK_SIZE overlap=$CHUNK_OVERLAP batch=$BATCH
  GC orphans     : $GC_ORPHANS
EOF

  printf "\n"; log "Checking commands…"
  for c in bash python3; do
    if command -v "$c" >/dev/null 2>&1; then ok "found $c"; else warn "missing $c"; fi
  done
  if command -v pipx >/dev/null 2>&1; then ok "found pipx"; else warn "missing pipx (install from https://pypa.github.io/pipx/)"; fi
  if command -v claude >/dev/null 2>&1; then ok "found claude CLI"; else warn "missing claude CLI (required for add-mcp)"; fi

  printf "\n"; log "Checking Python deps (chromadb, PyMuPDF)…"
  if assert_pdf_deps 2>/dev/null; then ok "deps OK"; else warn "deps missing—run: ./kb-tool.sh install"; fi

  printf "\n"; log "Filesystem checks…"
  [[ -d "$TEXTBOOKS_DIR" ]] && ok "textbooks dir exists" || warn "textbooks dir missing (will create on init): $TEXTBOOKS_DIR"
  [[ -d "$DB_DIR" ]] && ok "DB dir exists" || warn "DB dir missing (will create on init): $DB_DIR"
  [[ -x "$WRAPPER" ]] && ok "wrapper exists" || warn "wrapper missing (run init): $WRAPPER"
}

cmd_export() {
  need_cmd tar
  mkdir -p "$PROJECT_DIR"
  local out="${FILE_PATH:-$PROJECT_DIR/kb-export-$PROJECT_BASENAME.tar.gz}"
  log "Creating export archive: $out"
  [[ -d "$DB_DIR" ]] || die "DB dir not found: $DB_DIR (run ingest first)"
  # Pack the contents of DB_DIR so import can restore directly into $DB_DIR
  tar -C "$DB_DIR" -czf "$out" .
  ok "Export complete → $out"
}

cmd_import() {
  need_cmd tar
  [[ -n "${FILE_PATH:-}" ]] || die "--file PATH is required for import"
  [[ -f "$FILE_PATH" ]] || die "No such file: $FILE_PATH"
  if [[ -d "$DB_DIR" ]]; then
    local bak="${DB_DIR}.bak-$(date +%Y%m%d-%H%M%S)"
    log "Existing DB found; backing up to $bak"
    mv "$DB_DIR" "$bak"
  fi
  mkdir -p "$DB_DIR"
  log "Importing archive $FILE_PATH → $DB_DIR"
  tar -xzf "$FILE_PATH" -C "$DB_DIR"
  ok "Import complete"
}

cmd_all() {
  cmd_install
  cmd_init
  cmd_ingest
  cmd_add_mcp
}

# ---------- dispatch ----------

case "$COMMAND" in
  install)  cmd_install ;;
  init)     cmd_init ;;
  ingest)   cmd_ingest ;;
  add-mcp)  cmd_add_mcp ;;
  status)   cmd_status ;;
  export)   cmd_export ;;
  import)   cmd_import ;;
  all)      cmd_all ;;
  help|*)   usage ;;
esac
