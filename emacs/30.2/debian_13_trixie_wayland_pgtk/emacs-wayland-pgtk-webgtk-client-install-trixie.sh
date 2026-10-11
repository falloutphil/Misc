#!/usr/bin/env bash
set -euo pipefail

# -----------------------------------------------------------------------------
# Client-side installer for the portable Emacs 30.2 PGTK/Wayland xwidgets bundle
# on Debian 13.x (Trixie) under Windows WSL/WSLg.
#
# Expected files in the same directory as this script:
#   - icu75.txz
#   - wk240.txz
#   - emacs-30.2-pgtkwk.txz   (or whatever EMACS_STOW_NAME is set to)
# Optional:
#   - SHA256SUMS
#
# What this script does:
#   - installs runtime packages only
#   - extracts private ICU into /opt/icu75
#   - extracts private WebKitGTK into /opt/wk240
#   - extracts Emacs into /usr/local/stow/<package>
#   - activates Emacs via GNU Stow into /usr/local
#   - installs WSLg-aware launchers:
#       /usr/local/bin/emacs-pgtkwk
#       /usr/local/bin/emacs-pgtkwk-safe
# -----------------------------------------------------------------------------

EMACS_STOW_NAME="${EMACS_STOW_NAME:-emacs-30.2-pgtkwk}"
ICU_PREFIX="${ICU_PREFIX:-/opt/icu75}"
WK_PREFIX="${WK_PREFIX:-/opt/wk240}"
STOW_DIR="${STOW_DIR:-/usr/local/stow}"
WEBKIT_TARGET_ID="${WEBKIT_TARGET_ID:-gtk3-wayland-soup2-icu75}"
ALLOW_REPLACE_MISMATCHED_WEBKIT="${ALLOW_REPLACE_MISMATCHED_WEBKIT:-no}"

SCRIPT_DIR="$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)"
ARTIFACT_DIR="${ARTIFACT_DIR:-$SCRIPT_DIR}"

ICU_ARCHIVE="${ICU_ARCHIVE:-$ARTIFACT_DIR/icu75.txz}"
WK_ARCHIVE="${WK_ARCHIVE:-$ARTIFACT_DIR/wk240.txz}"
EMACS_ARCHIVE="${EMACS_ARCHIVE:-$ARTIFACT_DIR/${EMACS_STOW_NAME}.txz}"

EMACS_STOW_PATH="${STOW_DIR}/${EMACS_STOW_NAME}"
WAYLAND_WRAPPER="/usr/local/bin/emacs-pgtkwk"
SAFE_WRAPPER="/usr/local/bin/emacs-pgtkwk-safe"

need_file() {
  [ -f "$1" ] || {
    echo "Missing required file: $1" >&2
    exit 1
  }
}

need_cmd() {
  command -v "$1" >/dev/null 2>&1 || {
    echo "Required command not found: $1" >&2
    exit 1
  }
}

log() {
  printf '\n[%s] %s\n' "$(date +%H:%M:%S)" "$*"
}

die() {
  printf '\nERROR: %s\n' "$*" >&2
  exit 1
}

cleanup_sudo_timestamp() {
  sudo -k || true
}
trap cleanup_sudo_timestamp EXIT

configure_alsa_pulse_bridge() {
  local asoundrc="$HOME/.asoundrc"
  local asoundrc_block
  local updated_asoundrc="no"

  log "13) Configuring ALSA default routing for native Emacs sound"
  log "Why: Emacs uses ALSA, while WSLg exposes PulseAudio as the useful runtime sink"

  asoundrc_block=$(cat <<'EOF'
# Added for Emacs ALSA sound on WSLg/PulseAudio.
pcm.!default {
  type pulse
}
ctl.!default {
  type pulse
}
EOF
)

  if [[ ! -e "$asoundrc" ]]; then
    printf '%s\n' "$asoundrc_block" > "$asoundrc"
    updated_asoundrc="yes"
  else
    if grep -Eq '^[[:space:]]*(pcm|ctl)\.!default[[:space:]]*\{' "$asoundrc"; then
      log "Existing ALSA default config found in $asoundrc; leaving it unchanged"
      printf '\nPlease merge this stanza into %s manually if Emacs sound still fails:\n%s\n' "$asoundrc" "$asoundrc_block"
    else
      printf '\n%s\n' "$asoundrc_block" >> "$asoundrc"
      updated_asoundrc="yes"
    fi
  fi

  if [[ "$updated_asoundrc" == "yes" ]]; then
    grep -q '^  type pulse$' "$asoundrc" || die "Pulse routing stanza missing from $asoundrc"
    log "Installed ALSA-to-Pulse routing in $asoundrc"
  else
    log "ALSA-to-Pulse routing was not written automatically; see the manual merge note above if needed"
  fi
}

find_sound_test_file() {
  find /usr/share/sounds -type f -name '*.wav' 2>/dev/null | head -n 1
}

verify_emacs_sound() {
  local sound_file
  sound_file="$(find_sound_test_file)"
  if [[ -z "$sound_file" ]]; then
    log "No WAV test file found under /usr/share/sounds; skipping Emacs sound smoke test"
    return 0
  fi

  log "14) Running Emacs sound smoke test with $sound_file"
  /usr/local/bin/emacs -Q --batch \
    --eval "(condition-case err (progn (play-sound-file \"$sound_file\") (princ \"ok\")) (error (princ (error-message-string err)) (kill-emacs 12)))" \
    | grep -q '^ok$' || die "Emacs sound smoke test failed"
}

webkit_target_marker() {
  printf '%s\n' "${WK_PREFIX}/.emacs-webkit-target"
}

webkit_private_install_present() {
  [[ -f "${WK_PREFIX}/lib/pkgconfig/webkit2gtk-4.0.pc" ]] \
    || [[ -f "${WK_PREFIX}/lib/pkgconfig/webkit2gtk-4.1.pc" ]] \
    || [[ -f "${WK_PREFIX}/lib/libwebkit2gtk-4.0.so" ]] \
    || [[ -f "${WK_PREFIX}/lib/libwebkit2gtk-4.1.so" ]]
}

webkit_wayland_present() {
  webkit_private_install_present \
    && [[ -f "$(webkit_target_marker)" ]] \
    && grep -qx "$WEBKIT_TARGET_ID" "$(webkit_target_marker)"
}

refuse_mismatched_webkit_replacement() {
  if webkit_private_install_present && ! webkit_wayland_present; then
    if [[ "$ALLOW_REPLACE_MISMATCHED_WEBKIT" == "yes" ]]; then
      log "Existing WebKitGTK at ${WK_PREFIX} is not marked as ${WEBKIT_TARGET_ID}; replacing because ALLOW_REPLACE_MISMATCHED_WEBKIT=yes"
      return
    fi

    die "Existing WebKitGTK at ${WK_PREFIX} is not marked as this recipe's GTK3/Wayland build. This avoids accidentally replacing the X11-only Trixie WebKit. Use a different WK_PREFIX, or set ALLOW_REPLACE_MISMATCHED_WEBKIT=yes if replacing it is intentional."
  fi
}

install_wrappers() {
  log "11) Installing WSLg-aware launchers"

  sudo tee "$WAYLAND_WRAPPER" >/dev/null <<'EOF'
#!/usr/bin/env bash
set -euo pipefail

if [[ -z "${EMACS_FORCE_X11:-}" ]]; then
  if [[ -S /mnt/wslg/runtime-dir/wayland-0 ]]; then
    export XDG_RUNTIME_DIR=/mnt/wslg/runtime-dir
    export WAYLAND_DISPLAY="${WAYLAND_DISPLAY:-wayland-0}"
  fi

  if [[ -n "${WAYLAND_DISPLAY:-}" || -n "${EMACS_FORCE_WAYLAND:-}" ]]; then
    export GDK_BACKEND=wayland
    [[ -z "${EMACS_ENABLE_DMABUF:-}" ]] && export WEBKIT_DISABLE_DMABUF_RENDERER=1
  fi
fi

if [[ -d /opt/wk240 ]]; then
  if [[ -d /opt/icu75/lib ]]; then
    export LD_LIBRARY_PATH="/opt/wk240/lib:/opt/icu75/lib${LD_LIBRARY_PATH+:$LD_LIBRARY_PATH}"
  else
    export LD_LIBRARY_PATH="/opt/wk240/lib${LD_LIBRARY_PATH+:$LD_LIBRARY_PATH}"
  fi

  if [[ -d /opt/wk240/lib/girepository-1.0 ]]; then
    export GI_TYPELIB_PATH="/opt/wk240/lib/girepository-1.0${GI_TYPELIB_PATH+:$GI_TYPELIB_PATH}"
  fi

  export WEBKIT_EXEC_PATH="/opt/wk240/libexec/webkit2gtk-4.0"

  unset GIO_MODULE_DIR
  sys_gio=""
  for d in /usr/lib/x86_64-linux-gnu/gio/modules /usr/lib/gio/modules; do
    if [[ -d "$d" ]]; then
      sys_gio="$d"
      break
    fi
  done
  [[ -n "${EMACS_GIO_MODULES:-}" ]] && sys_gio="$EMACS_GIO_MODULES"
  [[ -n "$sys_gio" ]] && export GIO_EXTRA_MODULES="$sys_gio"

  export XDG_DATA_DIRS="/opt/wk240/share:${XDG_DATA_DIRS:-/usr/local/share:/usr/share}"
fi

exec /usr/local/bin/emacs "$@"
EOF
  sudo chmod 755 "$WAYLAND_WRAPPER"

  sudo tee "$SAFE_WRAPPER" >/dev/null <<'EOF'
#!/usr/bin/env bash
set -euo pipefail
exec env \
  WEBKIT_DISABLE_COMPOSITING_MODE=1 \
  LIBGL_ALWAYS_SOFTWARE=1 \
  GSK_RENDERER=cairo \
  /usr/local/bin/emacs-pgtkwk "$@"
EOF
  sudo chmod 755 "$SAFE_WRAPPER"
}

# Use a predictable umask so extracted trees remain readable.
umask 022

need_cmd sudo
need_cmd apt-get
need_cmd tar
need_cmd sha256sum
need_cmd grep

need_file "$ICU_ARCHIVE"
need_file "$WK_ARCHIVE"
need_file "$EMACS_ARCHIVE"

log "1) Verifying sudo access"
sudo -v

if [ -f "$ARTIFACT_DIR/SHA256SUMS" ]; then
  log "2) Verifying checksums from SHA256SUMS"
  (
    cd "$ARTIFACT_DIR"
    sha256sum -c SHA256SUMS
  )
else
  log "2) No SHA256SUMS found; skipping checksum verification"
fi

log "3) Installing client runtime packages"
sudo apt-get update
sudo apt-get install -y \
  stow \
  xz-utils \
  ca-certificates \
  glib-networking \
  bubblewrap \
  xdg-dbus-proxy \
  libgccjit0 \
  imagemagick \
  libgtk-3-0t64 \
  libwayland-client0 \
  libwayland-cursor0 \
  libwayland-egl1 \
  libxkbcommon0 \
  libwpe-1.0-1 \
  libwpebackend-fdo-1.0-1 \
  libgl1-mesa-dri \
  libegl1 \
  libgbm1 \
  mesa-vulkan-drivers \
  gstreamer1.0-plugins-good \
  gstreamer1.0-plugins-bad \
  gstreamer1.0-plugins-ugly \
  gstreamer1.0-libav \
  gstreamer1.0-alsa \
  libasound2-plugins \
  alsa-utils \
  libsoup2.4-1 \
  libsecret-1-0 \
  libenchant-2-2 \
  libhyphen0 \
  liblcms2-2

need_cmd stow
need_cmd readlink

log "4) Creating destination directories"
sudo mkdir -p /opt "$STOW_DIR"

log "5) Cleaning prior installs for a safe rerun"
refuse_mismatched_webkit_replacement
sudo stow -D -d "$STOW_DIR" -t /usr/local "$EMACS_STOW_NAME" 2>/dev/null || true
sudo rm -rf "$ICU_PREFIX" "$WK_PREFIX" "$EMACS_STOW_PATH"
sudo rm -f "$WAYLAND_WRAPPER" "$SAFE_WRAPPER"

log "6) Extracting private ICU into /"
sudo tar -C / -xJf "$ICU_ARCHIVE"

log "7) Extracting private WebKitGTK into /"
sudo tar -C / -xJf "$WK_ARCHIVE"

log "8) Extracting Emacs package into $STOW_DIR"
sudo tar -C "$STOW_DIR" -xJf "$EMACS_ARCHIVE"

[ -d "$EMACS_STOW_PATH" ] || die "Expected extracted Emacs package dir missing: $EMACS_STOW_PATH"
[ -x "$EMACS_STOW_PATH/bin/emacs" ] || die "Expected Emacs binary missing: $EMACS_STOW_PATH/bin/emacs"

log "9) Removing potentially conflicting desktop files"
sudo rm -f \
  /usr/local/share/applications/emacsclient.desktop \
  /usr/local/share/applications/emacsclient-mail.desktop \
  /usr/local/share/applications/emacs-mail.desktop \
  /usr/local/share/applications/emacs.desktop || true

log "10) Activating Emacs through GNU Stow"
sudo stow -d "$STOW_DIR" -t /usr/local "$EMACS_STOW_NAME"

install_wrappers

log "12) Running installation checks"

[ -x /usr/local/bin/emacs ] || die "Installed emacs launcher not found at /usr/local/bin/emacs"
[ -x "$WAYLAND_WRAPPER" ] || die "Wayland wrapper missing: $WAYLAND_WRAPPER"
[ -x "$SAFE_WRAPPER" ] || die "Safe wrapper missing: $SAFE_WRAPPER"

resolved_emacs="$(readlink -f /usr/local/bin/emacs)"
printf 'Resolved /usr/local/bin/emacs -> %s\n' "$resolved_emacs"

case "$resolved_emacs" in
  "$EMACS_STOW_PATH"/bin/emacs|"$EMACS_STOW_PATH"/bin/emacs-*)
    :
    ;;
  *)
    die "/usr/local/bin/emacs does not point into ${EMACS_STOW_PATH}"
    ;;
esac

/usr/local/bin/emacs --version | head -n 1

/usr/local/bin/emacs -Q --batch \
  --eval "(princ (if (featurep 'xwidget-internal) 't 'nil))" \
  | grep -q '^t$' \
  || die "xwidgets feature check failed"

/usr/local/bin/emacs -Q --batch \
  --eval "(princ (if (image-type-available-p 'imagemagick) 't 'nil))" \
  | grep -q '^t$' \
  || die "ImageMagick support check failed"

command -v magick >/dev/null 2>&1 || command -v convert >/dev/null 2>&1 \
  || die "Neither magick nor convert found on PATH"

configure_alsa_pulse_bridge
verify_emacs_sound

log "15) Final notes"
cat <<EOF

Install complete.

Activated package:
  ${EMACS_STOW_PATH}

Standard stowed launcher:
  /usr/local/bin/emacs

WSLg Wayland launcher:
  ${WAYLAND_WRAPPER}

Conservative rendering launcher:
  ${SAFE_WRAPPER}

What to use:
  - For normal WSLg Wayland use:
      ${WAYLAND_WRAPPER}
  - If xwidget pages open but draw blank or incorrectly:
      ${SAFE_WRAPPER} -Q

Checks passed:
  - Emacs is stowed into /usr/local
  - xwidgets support is present
  - ImageMagick support is present
  - managed ALSA-to-Pulse routing stanza is installed or already present
  - Emacs native sound smoke test was run or skipped because no WAV file exists

Suggested first test:
  ${WAYLAND_WRAPPER} -Q
  Then:
    M-x xwidget-webkit-browse-url RET https://www.bbc.com RET

EOF
