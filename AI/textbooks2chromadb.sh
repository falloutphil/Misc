# Run this to sync the textbooks in textbooks/ with chromadb
# It not re-add books already there
# And I believe it will delete textbooks you remove from the dir
# It creates a manifest.json file

# NOTE: It assumes you've installed chroma-mcp via pipx
# And added to it's venv the following PDF parser afterwards:
# pipx runpip chroma-mcp install --upgrade "chromadb>=1.0.10" PyMuPDF

# TODO: Just make it regular python script one day!

VENV="$HOME/.local/pipx/venvs/chroma-mcp"
"$VENV/bin/python" - <<'PY'
import os, io, json, hashlib, time, chromadb, fitz, math

DATA_DIR = os.path.expanduser("~/.local/share/chroma_mcp")
DOC_DIR  = os.path.expanduser("~/textbooks")
COLL     = "textbooks"
CHUNK, OVERLAP = 900, 150
BATCH    = 256
MANIFEST = os.path.join(DATA_DIR, "manifest.json")
GC_ORPHANS = True  # set False to skip deletion of entries for missing files

client = chromadb.PersistentClient(path=DATA_DIR)
coll = client.get_or_create_collection(COLL)

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
    # digest prefix avoids collisions; page/idx are stable
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

# Discover PDFs
pdfs = [os.path.join(DOC_DIR,f) for f in sorted(os.listdir(DOC_DIR)) if f.lower().endswith(".pdf")]
print("Ingesting:", [os.path.basename(p) for p in pdfs])

# Optional: remove DB docs for files that no longer exist
if GC_ORPHANS:
    # Find basenames present in DB but not on disk and delete them
    # We iterate by sources we've seen in manifest to avoid full DB scan
    missing = [src for src in manifest.keys() if not os.path.exists(src)]
    for src in missing:
        base = os.path.basename(src)
        print(f"🗑  removing stale entries for missing file: {base}")
        coll.delete(where={"source": src})
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
        # Up-to-date: skip reading/processing
        print(f"↷ {base}: unchanged (digest={digest[:8]}), skipping")
        continue

    # If file changed, drop only that file's old chunks (by exact source)
    if prev and prev.get("digest") != digest:
        print(f"↻ {base}: changed (old {prev.get('digest','')[:8]} → new {digest[:8]}), removing old chunks")
        coll.delete(where={"source": path})

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
                # upsert = insert or update; safe if rerun during edits
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
    print(f"\n✓ {base}: +{added_for_file} chunks in {time.time()-t0:.1f}s (digest {digest[:8]})")

print("Total chunks in collection (approx):", coll.count())
print("Done. Added/updated:", total_added)
PY
