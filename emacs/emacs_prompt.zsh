
# Robust Emacs launcher for zsh - stick it in .zshrc etc
emacs() {
  # Resolve real binary (prefer Stow on PATH)
  local bin
  if bin="$(whence -p emacs 2>/dev/null)"; then :; else bin="/usr/local/bin/emacs"; fi
  [[ -n "$EMACS_BIN" ]] && bin="$EMACS_BIN"

  # Foreground if:
  #  - print-and-exit: --help, --version, --fingerprint
  #  - non-GUI/TTY flows: --batch, --script, -nw, --no-window-system, -t/--terminal
  #  - explicit foreground daemon: --fg-daemon[=NAME]
  #  - stdout/stderr not TTY (pipes/scripts) or EMACS_FOREGROUND=1
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
    if [[ -S /mnt/wslg/runtime-dir/wayland-0 || -n "${EMACS_FORCE_WAYLAND:-}" ]]; then
      export XDG_RUNTIME_DIR=/mnt/wslg/runtime-dir
      export WAYLAND_DISPLAY=wayland-0
      export GDK_BACKEND=wayland
      # If you *must* hard-block X fallback, uncomment:
      # unset DISPLAY
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
