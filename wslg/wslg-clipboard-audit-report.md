# WSLg Clipboard Audit: Wayland vs X11 (Emacs + CLI)

## TL;DR

**WSLg does not expose image data to X11 applications—this is a WSLg architectural limitation, not an Emacs bug.**

- ✅ **Wayland/pgtk Emacs:** Successfully retrieves images from Windows clipboard (typically `image/bmp`)
- ❌ **X11 Emacs:** Cannot retrieve images from Windows clipboard (no image targets advertised)
- ✅ **Both:** Text works reliably (UTF8_STRING, TEXT, and on Wayland also text/html)

This is confirmed by Microsoft maintainers and proven through **three independent layers of evidence** using different tools.

---

## What this report includes

This report documents a reproducible audit of **Windows ⇄ WSLg** clipboard behavior for **text** and **images**, comparing **Wayland (pgtk)** vs **X11 (Xwayland)** clients.

### Three layers of proof (eliminating variables):

1. **Shell watchers (console-only, no Emacs):**
   - `watch-wayland.sh` — monitors Wayland clipboard using `wl-paste`
   - `watch-x11.sh` — monitors X11 clipboard using `xclip`
   - **Purpose:** Prove the limitation exists at the WSLg level, independent of Emacs

2. **Emacs audit (GUI application test):**
   - `wslg-clipboard-audit.el` — uses only Emacs native APIs
   - **Purpose:** Demonstrate the issue affects real GUI applications (not just CLI tools)

3. **Control experiments (Linux-to-Linux clipboard):**
   - Direct `wl-copy` and `xclip` injection tests
   - **Purpose:** Prove X11 tools CAN handle images when WSLg isn't involved

### Evidence screenshots (Windows → WSLg):
  - `00 empty windows clipboard.png` — Baseline
  - `01 image on windows clipboard.png` — First image copy
  - `02 html text on windows clipboard.png` — HTML text copy
  - `03 another image on windows clipboard.png` — Second image copy
  - `04 plain text on windows clipboard.png` — Plain text copy

### Control experiment screenshots (Linux → Linux):
  - `05 wl-copy owl png.png` — PNG via wl-copy (Wayland)
  - `06 wl-copy owl bmp.png` — BMP via wl-copy (Wayland)
  - `07 xclip owl png.png` — PNG via xclip (X11)
  - `08 xclip owl bmp.png` — BMP via xclip (X11)
  - `09 windows clipboard console version.png` — Shell watchers (Windows → WSLg)
  - `10 xclip console version success.png` — Shell watcher detecting xclip injection (Linux → X11)

All files referenced here live alongside this Markdown file for easy viewing in GitHub.

---

## Summary (what we found)

- **Images:** Wayland/pgtk Emacs successfully pulls image bytes from the clipboard (often `image/bmp` when copied from Windows). X11 Emacs does **not** see image targets and retrieves **no image bytes**.  
  → This matches WSLg’s current behavior: **Windows⇄Wayland supports bitmap/image**, **Windows⇄X11 does not**. (see: [WSLg #236 maintainer comment](https://github.com/microsoft/wslg/issues/236) and [discussion in #642](https://github.com/microsoft/wslg/issues/642))

- **Text:** Both Wayland and X11 paths retrieve text reliably (e.g., `UTF8_STRING`, `TEXT`, and on Wayland also `text/html`).

- **Target enumeration quirk on Wayland:** Sometimes `TARGETS` is empty in Emacs/pgtk even though a direct request for `image/bmp` (or `image/png`/`image/jpeg`) **succeeds**. Probing known image types directly works and is recommended as a robustness tweak.

These observations are consistent across multiple runs and independently corroborated by the shell watchers.

---

## LAYER 1: Console watchers (proving WSLg limitation, independent of Emacs)

**Key point:** These shell scripts use standard Linux clipboard tools (`wl-paste` for Wayland, `xclip` for X11) and prove the limitation exists at the WSLg bridge level—no Emacs involved.

The following **console-only** snapshot shows the two shell watchers running side‑by‑side while copying data in Windows. This demonstrates the same pattern we later observed inside Emacs:

- **Left (Wayland `watch-wayland.sh`):** Detects `text/html` and `text/plain;charset=utf-8`, pulls them (reporting lengths), and then detects **`image/bmp`** and saves the image (reporting size in bytes).
- **Right (X11 `watch-x11.sh`):** Lists typical text targets (`TARGETS`, `TEXT`, `TIMESTAMP`, `UTF8_STRING`) and successfully pulls text when present; when no text is offered it reports "No text available…". It **never shows any `image/*` targets** in this snapshot.

![Console watchers – Wayland vs X11](<09 windows clipboard console version.png>)

**Takeaway:** The Wayland clipboard presents image data (`image/bmp` here) to Linux clients; the X11 clipboard does not expose image targets under WSLg. This console evidence is independent of Emacs and proves the limitation is in WSLg's clipboard bridge, not in any particular GUI application.

---

## LAYER 2: Emacs audit (proving real GUI apps see the same behavior)

**Key point:** This demonstrates that the WSLg limitation affects actual GUI applications (Emacs), not just command-line tools. We use Emacs's native clipboard APIs to query and retrieve clipboard data.

### How the audit was conducted

We ran **both** Emacs builds (Wayland/pgtk and X11) side-by-side in separate terminals:

**Wayland/pgtk (Emacs 30.1):**
```bash
/usr/local/bin/emacs -Q -l ./wslg-clipboard-audit.el
```

**X11 (Xwayland, Emacs 28.2):**
```bash
env GDK_BACKEND=x11 WAYLAND_DISPLAY= DISPLAY=:0 \
  /usr/bin/emacs -Q -l ./wslg-clipboard-audit.el
```

After loading the script, we repeatedly ran `M-x ph/clipboard-audit-run` with different items copied to the Windows clipboard. Each run captured a snapshot of the clipboard state. The Windows Snipping Tool was used to capture side-by-side screenshots of both Emacs windows for evidence.

> **Note:** If Emacs reports "Render error: Invalid image type 'bmp'", that only means your Emacs was built without a BMP loader. The **raw bytes were still retrieved and saved** to `/tmp/wslg-cliptest/`; you can convert with `convert file.bmp file.png` if needed.

### Repro sequence and results

For each step, we ran `M-x ph/clipboard-audit-run` in **both** Emacs builds after copying to the Windows clipboard.

1. **Baseline (empty clipboard)**
   ![Baseline – both show no text or images](<00 empty windows clipboard.png>)

2. **Copy an image in Windows** (e.g., Snipping Tool → Copy)
   - **Wayland/pgtk**: `Found image/bmp (…)` and saved file to `/tmp/wslg-cliptest/...bmp`
   - **X11**: No image targets; no image retrieved
   ![After copying an image – Wayland pulls BMP, X11 does not](<01 image on windows clipboard.png>)

3. **Copy text in Windows (HTML)**
   - **Both**: Pulled text successfully (Wayland often sees `text/html` + `text/plain;charset=utf-8`)
   ![After copying text – both pull text](<02 html text on windows clipboard.png>)

4. **Copy another image**
   - **Wayland/pgtk**: Again retrieves image bytes and saves to file
   - **X11**: Still no image targets
   ![Second image – Wayland pulls, X11 does not](<03 another image on windows clipboard.png>)

5. **Copy more text (Plain)**
   - **Both**: Pulled text successfully
   ![Second text – both pull text](<04 plain text on windows clipboard.png>)

### Notes from the runs
- If an image is on the Windows clipboard **before** starting Emacs, the first pgtk audit sometimes shows `TARGETS: (none reported)` and no image fetch; **recopying** the image after Emacs is running consistently succeeds. This suggests a **targets/notification quirk** rather than missing data. (Our audit mitigates this by probing common `image/*` types directly.)

- The shell watchers corroborate Emacs:
  - Wayland: `wl-paste` lists `image/bmp` (and saves it).
  - X11: `xclip` shows only textish targets (`UTF8_STRING`, `TEXT`, `STRING`); it never reported `image/*` in our trials.

---

## LAYER 3: Control experiments (proving X11 tools CAN handle images)

**Key point:** X11 clipboard tools (`xclip`) and X11 Emacs are fully capable of handling image data—they just don't receive it from WSLg. These experiments prove that by bypassing WSLg and using Linux-to-Linux clipboard operations.

### Wayland control: injecting images directly with wl-copy

To prove Wayland clients (pgtk Emacs) consume image payloads independently of Windows, we injected test image files directly into the Wayland clipboard (bypassing WSLg entirely):

```bash
# PNG
wl-copy < owl.png
# BMP
wl-copy < owl.bmp
```

After each injection, we ran `M-x ph/clipboard-audit-run` in both Emacs windows. Results show inline renders and saved artifacts—as expected, the X11 Emacs build on the right picks up nothing from `wl-copy` because `wl-copy` targets the Wayland clipboard only.

**PNG via wl-copy (Linux → Wayland clipboard):**
![Wayland injection – PNG](<05 wl-copy owl png.png>)

**BMP via wl-copy (Linux → Wayland clipboard):**
![Wayland injection – BMP](<06 wl-copy owl bmp.png>)

### X11 control: injecting images directly with xclip

The above proves Wayland works, but perhaps X11 just isn't capable of any image copying? To verify that:
1. Our X11 tools (`watch-x11.sh`, xclip, X11 Emacs) CAN detect and handle image data
2. The limitation is specifically in WSLg's Windows→X11 bridge

We **manually injected** images onto the X11 clipboard using `xclip` (Linux→X11, bypassing WSLg):

```bash
# PNG injection (Linux → X11 clipboard)
xclip -selection clipboard -t image/png -i '09 windows clipboard console version.png' -loops 1 &

# BMP injection (Linux → X11 clipboard)
xclip -selection clipboard -t image/bmp -i 'owl.bmp' -loops 1 &
```

- `-t image/png` or `-t image/bmp` declares the correct MIME type
- `-loops 1` keeps `xclip` alive as the X11 **selection owner** for one consumer (our watcher)

#### Console proof: watch-x11.sh detects xclip injection

**Result:** `watch-x11.sh` immediately reported the `image/png` target and attempted to pull it (see log line: *"Pulled image/png via X11..."*). This demonstrates our X11 watcher script **does** detect image targets and retrieves image bytes when they're actually present on the X11 clipboard.

Meanwhile, as expected, the Wayland watcher didn't see this injection because it was targeted at the **X11** clipboard only.

![Console proof – X11 image injected with xclip](<10 xclip console version success.png>)

**Conclusion from this test:** The X11 tools work perfectly for detecting and retrieving images—the limitation is specifically in WSLg not exposing image data to the X11 clipboard.

#### Emacs proof: X11 Emacs can render images from xclip

We also ran `M-x ph/clipboard-audit-run` in both Emacs windows after the xclip injections to confirm X11 Emacs can handle images when they're actually available on the X11 clipboard.

**PNG via xclip (Linux → X11 clipboard):**
![Emacs proof – X11 PNG injected](<07 xclip owl png.png>)

**BMP via xclip (Linux → X11 clipboard):**
![Emacs proof – X11 BMP injected](<08 xclip owl bmp.png>)

As expected, the Wayland/pgtk Emacs (left side in screenshots) doesn't see anything because it doesn't listen to the X11 clipboard.

**Conclusion from this test:** X11 Emacs is fully capable of retrieving and rendering images (including BMP with external converter)—it just doesn't receive them from WSLg's Windows→X11 bridge.

---

## Why this matches WSLg design

- WSLg’s architecture bridges clipboard data over RDP and **supports text/HTML/bitmap** for Wayland clients. ([WSLg architecture blog](https://devblogs.microsoft.com/commandline/wslg-architecture/))
- Microsoft maintainers explicitly note: **“copy/paste of image data is only supported between Windows and Wayland native application, but not X11 application.”** (emphasis added). ([WSLg #236](https://github.com/microsoft/wslg/issues/236))
- Another maintainer comment clarifies that **X11↔Windows is text-only (UTF‑8)**, while **Wayland** gets limited HTML and bitmap formats. ([WSLg #642](https://github.com/microsoft/wslg/issues/642))

These statements line up exactly with what we saw: Wayland/pgtk can fetch image bytes; X11 cannot because no image targets are offered to Xwayland apps under WSLg.

---

## What (if anything) Emacs could improve

- **X11 Emacs:** Nothing to do—**image targets are not offered** to X11 apps under WSLg, so Emacs can’t fetch what isn’t there.
- **Wayland/pgtk Emacs:** Consider a small robustness tweak: when `TARGETS` is empty, **probe common image types directly** (`image/png`, `image/bmp`, `image/jpeg`). Our audit showed this succeeds even when enumeration fails, improving UX on WSLg.

---

## Caveats & open questions

- We tested **Windows → WSLg** direction. The reverse direction wasn’t part of this report.
- The occasional empty `TARGETS` on Wayland may indicate a GTK/Wayland/WSLg **enumeration timing** issue; adding a short retry or direct probes works around it.
- The BMP inline render error in Emacs is a **loader absence**, not a transfer failure.

---

## Appendix: How the watchers work

### Wayland watcher (`watch-wayland.sh`)
- Polls `wl-paste --list-types` and normalizes types (sorted/unique).
- On change, prints the list and **pulls** a fixed set of types to files under `/tmp/wslg-cliptest`.
- `wl-clipboard` is the supported tool for Wayland clipboards; it accepts exact MIME types. — see [wl-clipboard GitHub](https://github.com/bugaevc/wl-clipboard)

### X11 watcher (`watch-x11.sh`)
- Polls `xclip -selection clipboard -t TARGETS -o`.
- Proactively **requests text** each tick to ensure conversion happens (some bridges lazily convert on demand).
- If `image/*` ever appears in `TARGETS`, tries to pull it and records size (we never saw this occur under WSLg).

---

## References

- **WSLg architecture:** overall clipboard support (text/HTML/bitmap via RDP). ([WSLg architecture blog](https://devblogs.microsoft.com/commandline/wslg-architecture/))  
- **WSLg GitHub #236:** “image data only supported between Windows and Wayland, not X11.” (Maintainer comment). ([WSLg #236](https://github.com/microsoft/wslg/issues/236))  
- **WSLg GitHub #642 (maintainer):** X11 is *text (UTF‑8) only*; Wayland supports limited HTML & bitmap in addition to text. ([WSLg #642](https://github.com/microsoft/wslg/issues/642))  
- **`wl-clipboard` / `wl-paste` manual:** Wayland clipboard CLI and MIME handling. — see [wl-clipboard GitHub](https://github.com/bugaevc/wl-clipboard) and the [`wl-paste` man page](https://man.archlinux.org/man/wl-paste.1.en)  
- **Superuser thread (X11 clipboard under WSLg):** reports X11 ↔ Windows inconsistencies. (see discussion: https://superuser.com/questions/1723016/clipboard-**issues-with-windows-wsl2-gui-apps-wslg)

---

## Reproduction instructions

Follow these steps to reproduce all three layers of evidence:

### Setup (one-time)

Install dependencies in WSL:
```bash
sudo apt update
sudo apt install -y wl-clipboard xclip coreutils
# Optional (for BMP/image conversions):
sudo apt install -y imagemagick
```

### LAYER 1: Console watchers

Open two terminal windows and run the shell watchers:

**Terminal 1 (Wayland watcher):**
```bash
./watch-wayland.sh
```

**Terminal 2 (X11 watcher):**
```bash
./watch-x11.sh
```

Now copy various items in Windows (images from Snipping Tool, text from browser, etc.). Observe the watchers detect and pull clipboard data in real-time. You'll see Wayland detects images; X11 does not.

### LAYER 2: Emacs audit

Open two more terminal windows and run both Emacs builds:

**Terminal 3 (Wayland/pgtk Emacs):**
```bash
/usr/local/bin/emacs -Q -l ./wslg-clipboard-audit.el
```

**Terminal 4 (X11 Emacs):**
```bash
env GDK_BACKEND=x11 WAYLAND_DISPLAY= DISPLAY=:0 \
  /usr/bin/emacs -Q -l ./wslg-clipboard-audit.el
```

After both Emacs instances load:
1. Copy something to the Windows clipboard (image, text, HTML, etc.)
2. Run `M-x ph/clipboard-audit-run` in **both** Emacs windows
3. Observe the `*clipboard-audit*` buffer in each showing what was retrieved
4. Use Windows Snipping Tool to capture side-by-side screenshots of both Emacs windows

Repeat steps 1-4 with different clipboard content to build your evidence set.

All artifacts are saved to `/tmp/wslg-cliptest/` with timestamps.

### LAYER 3: Control experiments

With both Emacs instances still running and both shell watchers still running:

**Test wl-copy (Wayland control):**
```bash
wl-copy < owl.png
```
Then run `M-x ph/clipboard-audit-run` in both Emacs windows. Observe that pgtk Emacs retrieves the image; X11 Emacs does not (expected—they use different clipboards).

**Test xclip (X11 control):**
```bash
xclip -selection clipboard -t image/png -i 'owl.png' -loops 1 &
```
Then run `M-x ph/clipboard-audit-run` in both Emacs windows. Observe that X11 Emacs retrieves the image; pgtk Emacs does not (expected—they use different clipboards).

Watch the shell watchers during these experiments to see them detect the injections.

### Optional: Clear Windows clipboard before starting or between tests

```bash
/mnt/c/Windows/System32/WindowsPowerShell/v1.0/powershell.exe -NoProfile -STA -Command \
  "[void][Reflection.Assembly]::LoadWithPartialName('System.Windows.Forms'); [System.Windows.Forms.Clipboard]::Clear()"
```
