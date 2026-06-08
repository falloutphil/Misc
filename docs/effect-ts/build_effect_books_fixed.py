#!/usr/bin/env python3
from __future__ import annotations
import argparse, re, shutil, subprocess, sys, html
from pathlib import Path

BOOK_TITLE='Effect Documentation — Stable v3'
BOOK_AUTHOR='Effect-TS'

def die(msg):
    print('error:', msg, file=sys.stderr); raise SystemExit(1)

def run(cmd, cwd=None):
    print('+',' '.join(map(str,cmd)))
    subprocess.run(list(map(str,cmd)), cwd=str(cwd) if cwd else None, check=True)

def scalar(v):
    v=v.strip()
    if len(v)>=2 and v[0] in '"\'' and v[-1]==v[0]: return v[1:-1]
    if re.fullmatch(r'-?\d+',v): return int(v)
    return v

def split_fm(s):
    if not s.startswith('---\n'): return {}, s
    end=s.find('\n---',4)
    if end<0: return {}, s
    raw=s[4:end]; body=s[end+4:].lstrip('\n')
    meta={}; lines=raw.splitlines(); i=0
    while i<len(lines):
        m=re.match(r'^([\w-]+):\s*(.*)$',lines[i])
        if not m: i+=1; continue
        k,v=m.group(1),m.group(2).strip()
        if v=='':
            nested={}; i+=1
            while i<len(lines) and (lines[i].startswith('  ') or not lines[i].strip()):
                nm=re.match(r'^\s+([\w-]+):\s*(.*)$', lines[i])
                if nm: nested[nm.group(1)]=scalar(nm.group(2))
                i+=1
            meta[k]=nested; continue
        meta[k]=scalar(v); i+=1
    return meta, body

def find_root(website):
    for c in [website/'content', website]:
        if (c/'astro.config.ts').exists() and (c/'src/content/docs/docs').exists(): return c
    die('could not find website content root')

def strip_comments(s):
    s=re.sub(r'//.*','',s)
    return re.sub(r'/\*.*?\*/','',s,flags=re.S)

def sidebar_entries(config):
    t=strip_comments(config.read_text())
    idx=t.find('sidebar:'); start=t.find('[',idx)
    if idx<0 or start<0: die('no sidebar')
    depth=0; end=None
    for i,ch in enumerate(t[start:], start):
        if ch=='[': depth+=1
        elif ch==']':
            depth-=1
            if depth==0: end=i+1; break
    arr=t[start:end]
    out=[]
    for m in re.finditer(r'\{(?P<body>.*?)(?=\n\s*\},|\n\s*\}\s*\])', arr, re.S):
        b=m.group('body')
        lm=re.search(r'label:\s*["\']([^"\']+)["\']', b)
        if not lm: continue
        label=lm.group(1)
        am=re.search(r'autogenerate:\s*\{\s*directory:\s*["\']([^"\']+)["\']',b,re.S)
        sm=re.search(r'slug:\s*["\']([^"\']+)["\']',b)
        if am: out.append(('dir',label,am.group(1)))
        elif sm: out.append(('slug',label,sm.group(1)))
    return out

def page_info(content, rel, label):
    base=content/'src/content/docs'
    p=base/(rel+'.mdx')
    if not p.exists(): p=base/rel/'index.mdx'
    if not p.exists():
        print('warning missing',rel,file=sys.stderr); return None
    meta,_=split_fm(p.read_text(encoding='utf-8'))
    sidebar=meta.get('sidebar') if isinstance(meta.get('sidebar'),dict) else {}
    order=sidebar.get('order',9999) if isinstance(sidebar,dict) else 9999
    return {'path':p,'rel':rel,'label':label,'title':str(meta.get('title') or p.stem.replace('-',' ').title()),'desc':str(meta.get('description') or ''),'order':int(order)}

def collect(content, include_unstable=False):
    entries=sidebar_entries(content/'astro.config.ts')
    base=content/'src/content/docs'
    pages=[]
    for kind,label,target in entries:
        if not include_unstable and label in {'AI','Micro','Platform'}: continue
        if kind=='slug':
            pi=page_info(content,target,label)
            if pi: pages.append(pi)
        else:
            d=base/target
            sect=[]
            for p in sorted(d.glob('*.mdx')):
                rel=str(p.relative_to(base).with_suffix(''))
                pi=page_info(content,rel,label)
                if pi: sect.append(pi)
            pages += sorted(sect,key=lambda x:(x['order'],x['title'].lower()))
    return pages

def slug(s):
    return re.sub(r'[^a-z0-9]+','-',s.lower()).strip('-') or 'section'

def clean_info(info):
    info=info.strip()
    if not info: return ''
    # keep only the first language token; strip Twoslash/Starlight display options.
    first=info.split()[0]
    aliases={'typescript':'ts','javascript':'js','shell':'sh','bash':'sh','yml':'yaml','plaintext':'text'}
    return aliases.get(first,first)

def process_mdx(body):
    """Convert outside-code MDX; normalize fenced code with a real fence parser."""
    lines=body.splitlines()
    out=[]; in_code=False; fence_char=''; fence_len=0
    for line in lines:
        # Correct Markdown fence recognition: up to 3 leading spaces; closer must be fence-only.
        if not in_code:
            m=re.match(r'^( {0,3})(`{3,}|~{3,})([^`]*)$', line)
            if m:
                indent,fence,info=m.groups()
                in_code=True; fence_char=fence[0]; fence_len=len(fence)
                out.append(f'{indent}{fence_char*fence_len}{clean_info(info)}')
                continue
            # Drop MDX imports/exports outside code.
            if re.match(r'^\s*(import|export)\s+', line): continue
            # MDX comments.
            line=re.sub(r'\{/\*.*?\*/\}','',line)
            # Callouts.
            m=re.match(r'\s*<Aside\b([^>]*)>\s*$',line)
            if m:
                typ=attr(m.group(1),'type') or 'note'; title=attr(m.group(1),'title') or typ.title()
                out.append(f'<div class="aside {html.escape(typ)}"><p class="aside-title">{html.escape(title)}</p>')
                continue
            if re.match(r'\s*</Aside>\s*$',line): out.append('</div>'); continue
            # Flatten tabs; opening/closing Tabs disappear, TabItem becomes a heading.
            if re.match(r'\s*</?Tabs\b[^>]*>\s*$',line): continue
            m=re.match(r'\s*<TabItem\b([^>]*)>\s*$',line)
            if m:
                out.append('')
                out.append('#### '+(attr(m.group(1),'label') or attr(m.group(1),'title') or 'Tab'))
                out.append('')
                continue
            if re.match(r'\s*</TabItem>\s*$',line): continue
            # Steps wrapper only.
            if re.match(r'\s*</?Steps\b[^>]*>\s*$',line): continue
            # Replace one-line Badge.
            line=re.sub(r'<Badge\b([^>]*)\s*/>',lambda mm:f'<span class="badge">{html.escape(attr(mm.group(1),"text") or "Badge")}</span>',line)
            # Remove simple layout tags, preserving children.
            line=re.sub(r'</?(?:Card|CardGrid|CardGroup|LinkCard|Icon)\b[^>]*>','',line)
            out.append(line)
        else:
            m=re.match(rf'^( {{0,3}}){re.escape(fence_char)}{{{fence_len},}}\s*$', line)
            if m:
                out.append(m.group(1)+fence_char*fence_len)
                in_code=False; fence_char=''; fence_len=0
            else:
                # Strip hazardous control chars but do not touch Markdown-looking text inside examples.
                out.append(''.join(ch for ch in line if ch in '\t' or ord(ch)>=32))
    if in_code:
        # Better to close a damaged source block than poison the rest of the book.
        out.append(fence_char*fence_len)
    return '\n'.join(out)+'\n'

def attr(s,name):
    m=re.search(rf'\b{name}=(["\'])(.*?)\1',s)
    return html.unescape(m.group(2)) if m else None

def rewrite_assets(text,page,content,out):
    assets=out/'assets'; assets.mkdir(exist_ok=True)
    def img(m):
        alt,url=m.group(1),m.group(2).strip()
        if re.match(r'^[a-z]+://',url) or url.startswith('#'): return m.group(0)
        path=url.strip('<>').split()[0].split('#')[0].split('?')[0]
        candidates=[]
        if path.startswith('/'):
            candidates.append(content/'public'/path.lstrip('/'))
            candidates.append(content/'src/content/docs'/path.lstrip('/'))
        else:
            candidates.append(page['path'].parent/path)
            candidates.append(content/'src/content/docs'/path)
        for c in candidates:
            if c.exists():
                name=slug(page['rel'])+'-'+c.name
                dst=assets/name
                if not dst.exists(): shutil.copy2(c,dst)
                return f'![{alt}](assets/{name})'
        print('warning image not found', page['path'], url, file=sys.stderr)
        return m.group(0)
    return re.sub(r'!\[([^\]]*)\]\(([^)]+)\)',img,text)

def css(out):
    p=out/'book.css'
    p.write_text('''
body{font-family:-apple-system,BlinkMacSystemFont,"Segoe UI",Roboto,Helvetica,Arial,sans-serif;line-height:1.58;color:#172033;max-width:880px;margin:0 auto;padding:2.5rem 3rem}h1,h2,h3,h4{line-height:1.25}h1{font-size:2.05rem;margin-top:3rem;border-bottom:1px solid #d9deea;padding-bottom:.45rem}h2{font-size:1.5rem;margin-top:2rem}h3{font-size:1.18rem;margin-top:1.5rem}pre,code{font-family:"JetBrains Mono","Fira Code","SFMono-Regular",Consolas,Menlo,monospace}code{background:#f6f8fa;border-radius:4px;padding:.08rem .24rem;font-size:.92em}pre{background:#f6f8fa;border:1px solid #d9deea;border-radius:10px;padding:.9rem 1rem;overflow-x:auto;white-space:pre-wrap;break-inside:avoid}pre code{background:transparent;padding:0;font-size:.84rem}blockquote{border-left:4px solid #d9deea;padding-left:1rem;color:#566381;margin-left:0}.aside{background:#f7f9ff;border:1px solid #d9deea;border-left:5px solid #5b7cfa;border-radius:9px;padding:.85rem 1rem;margin:1rem 0;break-inside:avoid}.aside-title{font-weight:700;margin-top:0}.aside.tip{border-left-color:#278a4b}.aside.caution,.aside.warning{border-left-color:#c47f00}.badge{display:inline-block;border:1px solid #d9deea;border-radius:999px;padding:0 .45rem;font-size:.78em;color:#566381}table{border-collapse:collapse;width:100%;font-size:.92rem}th,td{border:1px solid #d9deea;padding:.45rem .6rem;vertical-align:top}th{background:#f0f3f8}img,svg{max-width:100%;height:auto}@page{size:A4;margin:16mm 15mm 18mm}@media print{body{max-width:none;padding:0}h1{break-before:page}h1:first-of-type{break-before:auto}}
'''.strip()+'\n')
    return p

def build(args):
    website=Path(args.website_dir).resolve()
    content=find_root(website)
    out=Path(args.out_dir).resolve()
    if out.exists(): shutil.rmtree(out)
    out.mkdir(parents=True)
    pages=collect(content,args.include_unstable)
    chunks=[f'% {BOOK_TITLE}\n% {BOOK_AUTHOR}\n\n# About this book\n\nGenerated from the stable Effect website documentation source. Interactive MDX components are flattened for EPUB/PDF reading.\n\n']
    cur=None
    for pg in pages:
        if pg['label']!=cur:
            cur=pg['label']; chunks.append(f'\n# {cur} {{#{slug("section-"+cur)}}}\n')
        meta,body=split_fm(pg['path'].read_text(encoding='utf-8'))
        body=process_mdx(body)
        body=rewrite_assets(body,pg,content,out)
        desc=f'\n\n_{pg["desc"]}_\n' if pg['desc'] else ''
        chunks.append(f'\n# {pg["title"]} {{#{slug(pg["rel"])}}}\n{desc}\n{body}\n')
    md=out/'effect-v3-docs.md'; md.write_text('\n'.join(chunks),encoding='utf-8')
    style=css(out)
    run(['pandoc',md,'--from','markdown+raw_html+fenced_code_attributes+pipe_tables','--to','html5','--standalone','--toc','--toc-depth','3','--metadata',f'title={BOOK_TITLE}','--css',style.name,'--highlight-style','pygments','-o',out/'effect-v3-docs.html'],cwd=out)
    if not args.skip_epub:
        run(['pandoc',md,'--from','markdown+raw_html+fenced_code_attributes+pipe_tables','--to','epub3','--standalone','--toc','--toc-depth','3','--metadata',f'title={BOOK_TITLE}','--metadata',f'author={BOOK_AUTHOR}','--css',style,'--highlight-style','pygments','--resource-path',out,'-o',out/'effect-v3-docs.epub'])
    print('generated',out)

def main():
    ap=argparse.ArgumentParser()
    ap.add_argument('--website-dir',required=True)
    ap.add_argument('--out-dir',default='effect-book-fixed')
    ap.add_argument('--include-unstable',action='store_true')
    ap.add_argument('--skip-epub',action='store_true')
    args=ap.parse_args(); build(args)
if __name__=='__main__': main()
