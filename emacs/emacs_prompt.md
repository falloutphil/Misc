Boom—nice sleuthing. Here’s exactly what changed, why it fixed the blank xwidget, and a cleaned-up `emacs()` that bakes it all in.

# What was wrong (and what we fixed)

1. **Version mismatch between UI libs and helper processes**
   Emacs (UI process) was loading your WebKitGTK from `/opt/wk240/lib`, but the spawned helpers (`WebKitWebProcess` / `WebKitNetworkProcess`) were coming from the *system* path. That ABI/protocol mismatch produces the “Received invalid message …” spam and a white canvas.
   **Fix:** set `WEBKIT_EXEC_PATH=/opt/wk240/libexec/webkit2gtk-4.0` so the helpers match the UI libs.

2. **TLS plugins weren’t being found**
   When `GIO_MODULE_DIR` pointed at `/opt`, Emacs couldn’t see GLib’s TLS modules (provided by `glib-networking`), so HTTPS failed (“TLS/SSL support not available”).
   **Fix:** **don’t** override `GIO_MODULE_DIR`; instead, **unset** it and set `GIO_EXTRA_MODULES` to your system’s GIO module dir (Debian: `/usr/lib/x86_64-linux-gnu/gio/modules`). Keep `ca-certificates` installed. Now HTTPS works.

3. **Wayland/WSLg stability**
   DMA-BUF on WSLg can be flaky.
   **Fix:** keep `WEBKIT_DISABLE_DMABUF_RENDERER=1` (safe on X11 too).

4. **Keep shares in sync, but don’t override schemas**
   Add `/opt/wk240/share` to `XDG_DATA_DIRS`, but don’t set `GSETTINGS_SCHEMA_DIR`. That avoids schema/version mismatches.

# Drop-in zsh function (robust & conditional)

```zsh
# Robust Emacs launcher for zsh (works with or without /opt/wk240)
emacs() {
  # Resolve real binary (prefer Stow on PATH)
  local bin
  if bin="$(whence -p emacs 2>/dev/null)"; then :; else bin="/usr/local/bin/emacs"; fi
  [[ -n "$EMACS_BIN" ]] && bin="$EMACS_BIN"

  # Decide foreground:
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

  # Prefer Wayland on WSLg unless explicitly forcing X11
  if [[ -z "${EMACS_FORCE_X11:-}" ]]; then
    if [[ -n "$WAYLAND_DISPLAY" || -S /mnt/wslg/runtime-dir/wayland-0 || -n "${EMACS_FORCE_WAYLAND:-}" ]]; then
      if [[ -S /mnt/wslg/runtime-dir/wayland-0 ]]; then
        export XDG_RUNTIME_DIR=/mnt/wslg/runtime-dir
        export WAYLAND_DISPLAY=wayland-0
      fi
      export GDK_BACKEND=wayland
      # Disable DMA-BUF on Wayland unless user opts back in
      [[ -z "$EMACS_ENABLE_DMABUF" ]] && export WEBKIT_DISABLE_DMABUF_RENDERER=1
    fi
  fi

  # If your private WebKit is installed, wire it up (unless opted out)
  if [[ -z "$EMACS_NO_WK240" && -d /opt/wk240 ]]; then
    # UI process uses /opt WebKitGTK
    export LD_LIBRARY_PATH="/opt/wk240/lib${LD_LIBRARY_PATH+:$LD_LIBRARY_PATH}"
    export GI_TYPELIB_PATH="/opt/wk240/lib/girepository-1.0${GI_TYPELIB_PATH+:$GI_TYPELIB_PATH}"
    # Helpers (WebProcess/NetworkProcess) from the same prefix
    export WEBKIT_EXEC_PATH="/opt/wk240/libexec/webkit2gtk-4.0"
    # Don’t shadow system GIO modules; ensure TLS/proxy plugins are visible
    unset GIO_MODULE_DIR
    local sys_gio=""
    for d in /usr/lib/x86_64-linux-gnu/gio/modules /usr/lib/gio/modules; do
      [[ -d "$d" ]] && { sys_gio="$d"; break; }
    done
    [[ -n "$EMACS_GIO_MODULES" ]] && sys_gio="$EMACS_GIO_MODULES"
    [[ -n "$sys_gio" ]] && export GIO_EXTRA_MODULES="$sys_gio"
    # Add our share dir without clobbering system dirs
    export XDG_DATA_DIRS="/opt/wk240/share:${XDG_DATA_DIRS:-/usr/local/share:/usr/share}"
    # IMPORTANT: do not set GSETTINGS_SCHEMA_DIR
  fi

  if (( fg )); then
    command "$bin" "$@"
  else
    # Background GUI launch with tiny rolling stderr log
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

## Notes / switches you can use

* `EMACS_NO_WK240=1 emacs …` → ignore `/opt/wk240` entirely (use system WebKit).
* `EMACS_ENABLE_DMABUF=1 emacs …` → try DMA-BUF on Wayland.
* `EMACS_FORCE_X11=1 emacs …` → skip the Wayland shim.
* `EMACS_GIO_MODULES=/custom/dir emacs …` → override the auto-detected GIO modules path.

With this, your `xwidget-webkit` renders reliably (helpers match the UI libs; TLS works; DMA-BUF is tamed).




-------------------



In Conclusion - The WebKit build was fine—the breakage was at **runtime** because Emacs mixed your `/opt/wk240` libs with Debian’s system ones. Xwidgets needs the **UI process**, the **helper processes**, and the **GIO/TLS plugins** to be from compatible locations. When those didn’t line up you got a white canvas and “Received invalid message …”.

### What must be set at runtime (the short list)

* Point Emacs’ **UI process** at your private WebKitGTK:

  * `LD_LIBRARY_PATH=/opt/wk240/lib`
  * `GI_TYPELIB_PATH=/opt/wk240/lib/girepository-1.0`
* Force the **helpers** to match the same build:

  * `WEBKIT_EXEC_PATH=/opt/wk240/libexec/webkit2gtk-4.0`
* **Do not** replace system GIO modules (that broke TLS):

  * `unset GIO_MODULE_DIR`
  * `GIO_EXTRA_MODULES=/usr/lib/x86_64-linux-gnu/gio/modules`
  * Ensure `glib-networking` and `ca-certificates` are installed.
* Let GSettings find both your and system schemas without overriding:

  * `XDG_DATA_DIRS=/opt/wk240/share:/usr/local/share:/usr/share`
  * (Do **not** set `GSETTINGS_SCHEMA_DIR`.)
* Wayland/WSLg stability toggle (safe on X11):

  * `WEBKIT_DISABLE_DMABUF_RENDERER=1`

### Minimal wrapper (drop-in)

```bash
#!/usr/bin/env bash
export LD_LIBRARY_PATH=/opt/wk240/lib${LD_LIBRARY_PATH+:$LD_LIBRARY_PATH}
export GI_TYPELIB_PATH=/opt/wk240/lib/girepository-1.0${GI_TYPELIB_PATH+:$GI_TYPELIB_PATH}
export WEBKIT_EXEC_PATH=/opt/wk240/libexec/webkit2gtk-4.0
unset GIO_MODULE_DIR
export GIO_EXTRA_MODULES=/usr/lib/x86_64-linux-gnu/gio/modules
export XDG_DATA_DIRS=/opt/wk240/share:${XDG_DATA_DIRS:-/usr/local/share:/usr/share}
export WEBKIT_DISABLE_DMABUF_RENDERER=1
exec /usr/local/bin/emacs "$@"
```

If you keep those directives (already folded into the zsh `emacs()` I gave you), Emacs won’t “stray” onto Debian’s defaults and xwidget-webkit will render reliably.

