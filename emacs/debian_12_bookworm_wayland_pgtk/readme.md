# Emacs 30.2 + WebKit (xwidgets) on Wayland — an end-to-end, portable recipe

This is a complete, “do it once, run anywhere” guide. You’ll build a **compatible WebKitGTK** (once, on a capable box), build **Emacs 30.2** with **PGTK + xwidgets** against that WebKit, package both, and then **install on other laptops** with a few commands. Finally, you’ll wire a **Zsh launcher** so Emacs always loads the right libraries at runtime (the crucial bit).

> Target OS used here: **Debian 12 (bookworm)** on Wayland (including WSLg). The same outline works on other distros; adapt package names and paths.

---

## 0) Why this extra work?

* Emacs xwidgets embeds **WebKitGTK**. Emacs 30.2 expects WebKitGTK **4.0/4.1** in a range it knows is stable. On Debian 12, the security repo currently ships **2.50.x** which is **newer than Emacs 30.2’s upper bound**, so `./configure` may refuse or you’ll hit runtime mismatches.
* Solution: supply a **portable WebKitGTK 2.40.x** in `/opt/wk240` and build Emacs against it.
* Critical lesson: building Emacs against `/opt/wk240` is **not enough**. At **runtime** you must ensure:

  * Emacs’ **UI process** uses the `/opt/wk240` libs,
  * WebKit’s **helper processes** come from the same prefix,
  * **GIO/TLS** modules (glib-networking, certificates) come from the **system**, not `/opt`.

Mess those up and you’ll see a **blank white canvas** or “TLS/SSL support not available”.

---

## 1) Build a portable WebKitGTK bundle (do this once)

> If you already have `/opt/wk240` from a previous build, skip to §3.

### 1.1 Install build prerequisites (on a powerful machine)

```bash
sudo apt update
sudo apt install -y build-essential ninja-build cmake python3 \
  libgtk-3-dev libgtk-4-dev libsoup2.4-dev libsoup-3.0-dev \
  libjpeg-dev libpng-dev libwebp-dev libwoff2dec-dev libicu-dev \
  libgstreamer1.0-dev libgstreamer-plugins-base1.0-dev \
  libgles2-mesa-dev libegl1-mesa-dev libgbm-dev libxcomposite-dev \
  libxdamage-dev libxrandr-dev libxt-dev libharfbuzz-dev \
  bison flex ruby
```

> These are typical; WebKit’s build system will tell you if anything is missing.

### 1.2 Fetch and build WebKitGTK 2.40.x

Two common ways; pick one.

**A) WebKit’s own scripts (simplest):**

```bash
git clone https://github.com/WebKit/WebKit.git
cd WebKit
git checkout webkit-2.40.5  # or another 2.40.x tag

# Configure + build for GTK port, Release mode
Tools/Scripts/build-webkit --gtk --release

# Install into a private prefix
sudo Tools/Scripts/build-webkit --gtk --release --install --cmakeargs -DCMAKE_INSTALL_PREFIX=/opt/wk240
```

**B) CMake manually (equivalent):**

```bash
mkdir -p ~/src/webkit-2.40 && cd ~/src/webkit-2.40
# … place WebKit 2.40.x sources here …
cmake -S . -B build -G Ninja \
  -DPORT=GTK -DCMAKE_BUILD_TYPE=Release \
  -DCMAKE_INSTALL_PREFIX=/opt/wk240
ninja -C build
sudo ninja -C build install
```

After install you should have:

```
/opt/wk240/
  bin/                      # (may be empty for GTK port)
  include/
  lib/                      # libwebkit2gtk-4.0.so.*, libjavascriptcoregtk-4.0.so.*
  libexec/webkit2gtk-4.0/   # WebKitWebProcess, WebKitNetworkProcess, …
  share/
```

> We call this folder “wk240”.

---

## 2) Build Emacs 30.2 with PGTK + xwidgets against wk240

### 2.1 Prereqs for Emacs build

```bash
sudo apt install -y git libgtk-3-dev libtree-sitter-dev \
  libgnutls28-dev libjpeg-dev libpng-dev libtiff-dev libgif-dev \
  libjansson-dev libharfbuzz-dev libwebp-dev libxpm-dev \
  libgccjit-12-dev libasound2-dev
```

`libasound2-dev` matters even if your main goal is xwidgets. On Linux, native
Emacs sound uses ALSA. If that package is missing, `./configure` can still
succeed overall while silently leaving `HAVE_ALSA` unset, which later breaks
`play-sound-file`.

### 2.2 Ensure pkg-config can find `/opt/wk240`

```bash
export PKG_CONFIG_PATH=/opt/wk240/lib/pkgconfig${PKG_CONFIG_PATH+:$PKG_CONFIG_PATH}
```

### 2.3 Configure Emacs

We’ll install Emacs into a **Stow tree** so we can move/symlink it cleanly.

```bash
# Sources
cd ~/installs
curl -LO https://ftp.gnu.org/gnu/emacs/emacs-30.2.tar.xz
tar -xf emacs-30.2.tar.xz
cd emacs-30.2

# Configure for Wayland PGTK + xwidgets; install into a Stow dir
./configure \
  --prefix=/usr/local/stow/emacs-30.2-wk \
  --with-pgtk \
  --with-tree-sitter \
  --with-json \
  --with-modules \
  --with-xwidgets

make -j$(nproc)
sudo make install
```

Quick checks:

```bash
/usr/local/stow/emacs-30.2-wk/bin/emacs --version  # should be 30.2
/bin/grep '^#define HAVE_ALSA 1$' src/config.h     # should match exactly
/usr/local/stow/emacs-30.2-wk/bin/emacs -Q --batch \
  --eval '(princ (featurep '"'"'xwidget-internal))'   # should print: t
```

### 2.4 Stow it into place

```bash
# (Optional) remove stale .desktop files that commonly conflict
sudo rm -f /usr/local/share/applications/emacs*.desktop \
           /usr/local/share/applications/emacsclient*.desktop

# Dry run first
sudo stow -n -v -d /usr/local/stow -t /usr/local emacs-30.2-wk

# If clean, do it for real:
sudo stow -R -d /usr/local/stow -t /usr/local emacs-30.2-wk
```

If Stow reports conflicts such as `share/info/dir` or `share/emacs/site-lisp/subdirs.el`, back them up or delete them (they’re auto-generated) and re-run the Stow command.

---

## 3) Package for other laptops

### 3.1 Tar the Stow’d Emacs

```bash
sudo tar -C /usr/local/stow -czf ~/emacs-30.2-wk-stow.tgz emacs-30.2-wk
```

### 3.2 Tar the WebKit bundle

```bash
sudo tar -C /opt -czf ~/wk240-$(uname -m).tgz wk240
```

Copy both tars to the target machine.

### 3.3 Install on the target (low-power) laptop

```bash
# 1) Unpack WebKit bundle
sudo tar -C /opt -xzf wk240-*.tgz

# 2) Ensure Stow root exists
sudo mkdir -p /usr/local/stow

# 3) Unpack Emacs
sudo tar -C /usr/local/stow -xzf emacs-30.2-wk-stow.tgz

# 4) Clean up possible conflicts (same as §2.4)
sudo rm -f /usr/local/share/applications/emacs*.desktop \
           /usr/local/share/applications/emacsclient*.desktop

# 5) Stow Emacs into place (dry run, then real)
sudo stow -n -v -d /usr/local/stow -t /usr/local emacs-30.2-wk
sudo stow -R    -d /usr/local/stow -t /usr/local emacs-30.2-wk

# 6) Runtime dependencies (TLS/GIO + GPU stack)
sudo apt install -y glib-networking ca-certificates \
  libgl1-mesa-dri libegl1 libgbm1 mesa-vulkan-drivers
```

---

## 4) The Zsh launcher (the key to making it actually work)

This wrapper makes Emacs **always** load the right WebKit pieces at runtime:

* Emacs UI: `/opt/wk240/lib` (via `LD_LIBRARY_PATH` and `GI_TYPELIB_PATH`)
* WebKit helpers: `/opt/wk240/libexec/webkit2gtk-4.0` (via `WEBKIT_EXEC_PATH`)
* GIO/TLS plugins: **system** dir (via `GIO_EXTRA_MODULES`, *not* `GIO_MODULE_DIR`)
* GSettings schemas: include `/opt/wk240/share` **plus** system dirs (via `XDG_DATA_DIRS`)
* Wayland/WSLg stability: `WEBKIT_DISABLE_DMABUF_RENDERER=1` (harmless on X11)

Put this in your `~/.zshrc`:

```zsh
# Portable Emacs launcher with WebKit wiring + Wayland niceties
emacs() {
  local bin
  if bin="$(whence -p emacs 2>/dev/null)"; then :; else bin="/usr/local/bin/emacs"; fi
  [[ -n "$EMACS_BIN" ]] && bin="$EMACS_BIN"

  # Foreground if it's a help/version/non-GUI flow or when stdout/stderr aren't TTYs
  local fg=0
  for a in "$@"; do
    case "$a" in
      --help|-h|-\?|--version|--fingerprint|\
      --batch|--script|-nw|--no-window-system|-t|--terminal|\
      --fg-daemon|--fg-daemon=*)
        fg=1; break;;
    esac
  done
  [[ ! -t 1 || ! -t 2 || -n "$EMACS_FOREGROUND" ]] && fg=1

  # WebKitGTK bundle present? wire it up
  if [[ -d /opt/wk240/lib && -d /opt/wk240/libexec/webkit2gtk-4.0 ]]; then
    export LD_LIBRARY_PATH=/opt/wk240/lib${LD_LIBRARY_PATH+:$LD_LIBRARY_PATH}
    export GI_TYPELIB_PATH=/opt/wk240/lib/girepository-1.0${GI_TYPELIB_PATH+:$GI_TYPELIB_PATH}
    export WEBKIT_EXEC_PATH=/opt/wk240/libexec/webkit2gtk-4.0
    # TLS/proxies from system GIO modules (DO NOT set GIO_MODULE_DIR)
    local _gio_mod="/usr/lib/x86_64-linux-gnu/gio/modules"
    [[ -d "$_gio_mod" ]] && export GIO_EXTRA_MODULES="$_gio_mod"
    # Include wk240 schemas without hiding system schemas
    export XDG_DATA_DIRS="/opt/wk240/share:${XDG_DATA_DIRS:-/usr/local/share:/usr/share}"
  fi

  # Wayland/WSLg stability: disable DMA-BUF path for WebKitGTK (safe everywhere)
  export WEBKIT_DISABLE_DMABUF_RENDERER=1

  # Prefer Wayland on WSLg unless explicitly forcing X11
  if [[ -z "${EMACS_FORCE_X11:-}" ]]; then
    if [[ -S /mnt/wslg/runtime-dir/wayland-0 || -n "${EMACS_FORCE_WAYLAND:-}" ]]; then
      export XDG_RUNTIME_DIR=/mnt/wslg/runtime-dir
      export WAYLAND_DISPLAY=wayland-0
      export GDK_BACKEND=wayland
      # If you need to *force* no X fallback:  unset DISPLAY
    fi
  fi

  if (( fg )); then
    command "$bin" "$@"
  else
    # Background GUI launch with small rolling stderr log
    local logdir="$HOME/.cache"; mkdir -p "$logdir"
    local log="${EMACS_LAUNCH_LOG:-$logdir/emacs-launch.err}"
    local max=$((512 * 1024))
    if [[ -f "$log" && $(stat -c%s "$log" 2>/dev/null || echo 0) -ge $max ]]; then
      mv -f "$logdir/emacs-launch.err.2" "$logdir/emacs-launch.err.3" 2>/dev/null || true
      mv -f "$logdir/emacs-launch.err.1" "$logdir/emacs-launch.err.2" 2>/dev/null || true
      mv -f "$log" "$logdir/emacs-launch.err.1" 2>/dev/null || true
      : > "$log"
    fi
    command "$bin" "$@" > /dev/null 2>>"$log" &!
  fi
}
```

> Want a simple shell script instead? Save the following as `~/.local/bin/emacs-wk` and use that to launch Emacs:
>
> ```bash
> #!/usr/bin/env bash
> export LD_LIBRARY_PATH=/opt/wk240/lib${LD_LIBRARY_PATH+:$LD_LIBRARY_PATH}
> export GI_TYPELIB_PATH=/opt/wk240/lib/girepository-1.0${GI_TYPELIB_PATH+:$GI_TYPELIB_PATH}
> export WEBKIT_EXEC_PATH=/opt/wk240/libexec/webkit2gtk-4.0
> unset GIO_MODULE_DIR
> export GIO_EXTRA_MODULES=/usr/lib/x86_64-linux-gnu/gio/modules
> export XDG_DATA_DIRS=/opt/wk240/share:${XDG_DATA_DIRS:-/usr/local/share:/usr/share}
> export WEBKIT_DISABLE_DMABUF_RENDERER=1
> exec /usr/local/bin/emacs "$@"
> ```

---

## 5) Verification checklist (takes 30 seconds)

1. **xwidgets present**

   ```bash
   emacs -Q --batch --eval '(princ (featurep '"'"'xwidget-internal))'
   # → t
   ```

2. **WebKit can draw**

   ```bash
   emacs -Q --eval '(progn (require '"'"'xwidget)
                           (xwidget-webkit-browse-url "https://example.org"))'
   # Should show the Example Domain page
   ```

3. **Helpers are from `/opt/wk240`**

   ```bash
   readlink -f /proc/$(pgrep -nf WebKitWebProcess)/exe
   # → /opt/wk240/libexec/webkit2gtk-4.0/WebKitWebProcess
   ```

4. **UI process uses /opt webkit libs, but system GIO modules**

   ```bash
   lsof -p $(pgrep -n emacs) | egrep 'libwebkit2gtk|libjavascriptcoregtk|gio/modules' | awk '{print $9}' | sort -u
   # Expect /opt/wk240/lib/libwebkit2gtk-4.0.so... and /usr/lib/.../gio/modules/* from system
   ```

---

## 6) Wayland & WSLg notes

* **PGTK** is Emacs’ pure-GTK build that talks Wayland directly. If you run it under X11 you’ll see a warning; it still works, but stay on Wayland for best results.
* On **WSLg**, `WEBKIT_DISABLE_DMABUF_RENDERER=1` avoids a flaky GPU path (DMA-BUF). Keeping it set is fine on bare metal too.
* If you deliberately want X11, set `EMACS_FORCE_X11=1` and start an X session or ensure `DISPLAY` is set.

---

## 7) Common pitfalls & fixes

* **Blank white xwidget canvas**
  Almost always means the runtime wiring is wrong. Use the wrapper and re-check §5. The biggest offenders:

  * You exported `GIO_MODULE_DIR=/opt/wk240/...` (don’t). That hides **glib-networking**, so TLS fails.
  * You didn’t set `WEBKIT_EXEC_PATH`, so helpers came from the distro and mismatched your UI libs.
* **“TLS/SSL support not available; install glib-networking”**
  You’re loading the wrong GIO modules. Remove `GIO_MODULE_DIR`, set `GIO_EXTRA_MODULES` to the system path, and ensure `sudo apt install -y glib-networking ca-certificates`.
* **`stow` conflicts**
  Remove conflicting stale files under `/usr/local/share/applications/` and re-run Stow (see §2.4).
* **`featurep 'xwidget-internal` is `nil`**
  You forgot `--with-xwidgets` (or used a non-PGTK build). Reconfigure & rebuild Emacs.

---

## 8) Optional: Grip/Markdown preview inside Emacs

Once xwidgets/WebKit render correctly, you can use **grip-mode** for GitHub-style previews:

```elisp
;; ~/.doom.d/init.el → :lang (markdown +grip)
;; ~/.doom.d/config.el
(with-eval-after-load 'grip-mode
  (setq grip-command 'go-grip
        grip-preview-use-webkit t
        grip-theme 'auto))
```

Key to start/stop: `C-c l p`. If you prefer an external browser, set `grip-preview-use-webkit` to `nil`.

---

### That’s it

* You’ve **built** a compatible WebKitGTK once, **built** Emacs 30.2 with **PGTK+xwidgets**, **packaged** both, and **installed** them elsewhere with **Stow**.
* The **Zsh wrapper** guarantees the right libraries and helper processes are used and that TLS works — that was the missing piece causing the blank page earlier.

If you ever need to refresh WebKit, rebuild into `/opt/wk240`, re-tar, and redeploy; your Emacs build and launcher remain the same.
