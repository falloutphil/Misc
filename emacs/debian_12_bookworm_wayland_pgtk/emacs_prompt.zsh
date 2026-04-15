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
