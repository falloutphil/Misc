#!/usr/bin/env bash
set -euo pipefail

# -----------------------------------------------------------------------------
# Client-side installer for the portable Emacs 30.2 xwidgets bundle on
# Debian 13.x (Trixie).
#
# Expected files in the same directory as this script:
#   - icu75.txz
#   - wk240.txz
#   - emacs-30.2-x11wk.txz   (or whatever EMACS_STOW_NAME is set to)
# Optional:
#   - SHA256SUMS
#
# What this script does:
#   - installs runtime packages only
#   - extracts private ICU into /opt/icu75
#   - extracts private WebKitGTK into /opt/wk240
#   - extracts Emacs into /usr/local/stow/<package>
#   - activates Emacs via GNU Stow into /usr/local
#   - installs a dedicated xwidget-safe wrapper:
#       /usr/local/bin/emacs-x11wk-safe
#
# Important:
#   The xwidget rendering workaround is NOT exported globally.
#   It is confined to the wrapper script only.
# -----------------------------------------------------------------------------

EMACS_STOW_NAME="${EMACS_STOW_NAME:-emacs-30.2-x11wk}"
ICU_PREFIX="${ICU_PREFIX:-/opt/icu75}"
WK_PREFIX="${WK_PREFIX:-/opt/wk240}"
STOW_DIR="${STOW_DIR:-/usr/local/stow}"

SCRIPT_DIR="$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)"
ARTIFACT_DIR="${ARTIFACT_DIR:-$SCRIPT_DIR}"

ICU_ARCHIVE="${ICU_ARCHIVE:-$ARTIFACT_DIR/icu75.txz}"
WK_ARCHIVE="${WK_ARCHIVE:-$ARTIFACT_DIR/wk240.txz}"
EMACS_ARCHIVE="${EMACS_ARCHIVE:-$ARTIFACT_DIR/${EMACS_STOW_NAME}.txz}"

EMACS_STOW_PATH="${STOW_DIR}/${EMACS_STOW_NAME}"
SAFE_WRAPPER="/usr/local/bin/emacs-x11wk-safe"

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
  printf '
[%s] %s
' "$(date +%H:%M:%S)" "$*"
}

configure_alsa_pulse_bridge() {
  local asoundrc="$HOME/.asoundrc"
  local asoundrc_block
  local updated_asoundrc="no"
  log "13) Configuring ALSA default routing for native Emacs sound"
  log "Why: Emacs uses ALSA, but WSL/WSLg typically exposes PulseAudio as the usable runtime sink"

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
  /usr/local/bin/emacs -Q --batch     --eval "(condition-case err (progn (play-sound-file \"$sound_file\") (princ \"ok\")) (error (princ (error-message-string err)) (kill-emacs 12)))"     | grep -q '^ok$' || die "Emacs sound smoke test failed"
}

die() {
  printf '\nERROR: %s\n' "$*" >&2
  exit 1
}

cleanup_sudo_timestamp() {
  sudo -k || true
}
trap cleanup_sudo_timestamp EXIT

# Use a predictable umask so extracted trees remain readable.
umask 022

need_cmd sudo
need_cmd tar
need_cmd sha256sum
need_cmd stow
need_cmd readlink
need_cmd grep

need_file "$ICU_ARCHIVE"
need_file "$WK_ARCHIVE"
need_file "$EMACS_ARCHIVE"

log "1) Verifying sudo access"
sudo -v

# Optional integrity verification if SHA256SUMS is present.
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
  libgl1-mesa-dri \
  libegl1 \
  libgbm1 \
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

log "4) Creating destination directories"
sudo mkdir -p /opt "$STOW_DIR"

log "5) Cleaning prior installs for a safe rerun"
sudo stow -D -d "$STOW_DIR" -t /usr/local "$EMACS_STOW_NAME" 2>/dev/null || true
sudo rm -rf "$ICU_PREFIX" "$WK_PREFIX" "$EMACS_STOW_PATH"

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

log "11) Installing dedicated xwidget-safe wrapper"
sudo tee "$SAFE_WRAPPER" >/dev/null <<EOF
#!/usr/bin/env bash
set -euo pipefail
exec env \
  WEBKIT_DISABLE_COMPOSITING_MODE=1 \
  LIBGL_ALWAYS_SOFTWARE=1 \
  GSK_RENDERER=cairo \
  /usr/local/bin/emacs "\$@"
EOF
sudo chmod 755 "$SAFE_WRAPPER"

log "12) Running installation checks"

[ -x /usr/local/bin/emacs ] || die "Installed emacs launcher not found at /usr/local/bin/emacs"

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

Standard launcher:
  /usr/local/bin/emacs

Xwidget-safe launcher:
  ${SAFE_WRAPPER}

What to use:
  - For normal Emacs use:
      /usr/local/bin/emacs
  - For xwidget/WebKit browsing on machines that show blank or badly rendered pages:
      ${SAFE_WRAPPER} -Q
  - The installer has already written a managed ~/.asoundrc stanza routing ALSA
    default output to Pulse, verified that stanza, and run an Emacs sound smoke test.

Why the extra wrapper exists:
  Some Debian 13 + GTK3/X11 + WebKitGTK setups render xwidgets incorrectly unless
  WebKit compositing is disabled and software rendering is forced. We do NOT set
  those variables globally, because that would be a broad and unnecessary system-
  wide change. Instead, they are confined to the wrapper above.

Checks passed:
  - Emacs is stowed into /usr/local
  - xwidgets support is present
  - ImageMagick support is present
  - managed ALSA-to-Pulse routing stanza is installed in ~/.asoundrc
  - Emacs native sound smoke test passed

Suggested first test:
  ${SAFE_WRAPPER} -Q
  Then:
    M-x xwidget-webkit-browse-url RET https://www.bbc.com RET

EOF
