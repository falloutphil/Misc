#!/usr/bin/env bash
set -euo pipefail

# -----------------------------------------------------------------------------
# Client-side installer for the portable Emacs xwidgets bundle
#
# Expected usage:
#   1. Copy this script into the same directory as:
#        - icu75.txz
#        - wk240.txz
#        - emacs-30.2-x11wk.txz   (or whatever EMACS_STOW_NAME was used)
#   2. Run this script on a Debian 13.x machine.
#
# What this script does:
#   - installs only the *runtime* packages needed on the client machine
#   - extracts the private ICU and WebKitGTK bundles under /opt
#   - extracts the Emacs package under /usr/local/stow
#   - uses GNU Stow to activate Emacs into /usr/local
#   - runs a few checks so you fail early rather than discovering breakage later
#
# Why Stow is used here:
#   GNU Stow is a symlink farm manager. Instead of copying all of Emacs directly
#   into /usr/local/bin, /usr/local/share, etc., we keep the actual package in
#     /usr/local/stow/<package>
#   and let Stow create symlinks into /usr/local.
#   That makes future upgrades and removals clean and reversible.
# -----------------------------------------------------------------------------

EMACS_STOW_NAME="${EMACS_STOW_NAME:-emacs-30.2-x11wk}"
SCRIPT_DIR="$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)"
ARTIFACT_DIR="${ARTIFACT_DIR:-$SCRIPT_DIR}"
ICU_ARCHIVE="${ICU_ARCHIVE:-$ARTIFACT_DIR/icu75.txz}"
WK_ARCHIVE="${WK_ARCHIVE:-$ARTIFACT_DIR/wk240.txz}"
EMACS_ARCHIVE="${EMACS_ARCHIVE:-$ARTIFACT_DIR/${EMACS_STOW_NAME}.txz}"

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

# Use a predictable umask so extracted trees remain readable to normal users.
umask 022

need_cmd sudo
need_cmd tar
need_cmd sha256sum
need_file "$ICU_ARCHIVE"
need_file "$WK_ARCHIVE"
need_file "$EMACS_ARCHIVE"

# Optional integrity verification if SHA256SUMS is present in the artifact dir.
if [ -f "$ARTIFACT_DIR/SHA256SUMS" ]; then
  log "Verifying checksums from SHA256SUMS"
  (
    cd "$ARTIFACT_DIR"
    sha256sum -c SHA256SUMS
  )
fi

log "Installing client runtime packages"
sudo apt update

# Only runtime packages go on the client machine. No compilers, no headers.
sudo apt install -y \
  stow xz-utils \
  glib-networking ca-certificates libgl1-mesa-dri libegl1 libgbm1 \
  bubblewrap xdg-dbus-proxy \
  libsoup2.4-1 libsecret-1-0 libenchant-2-2 libhyphen0 liblcms2-2 \
  gstreamer1.0-plugins-good gstreamer1.0-plugins-ugly \
  gstreamer1.0-plugins-bad gstreamer1.0-libav gstreamer1.0-alsa \
  libgccjit0 imagemagick

log "Creating destination directories"
sudo mkdir -p /opt /usr/local/stow

log "Extracting private ICU into /opt"
sudo tar -C / -xJf "$ICU_ARCHIVE"

log "Extracting private WebKitGTK into /opt"
sudo tar -C / -xJf "$WK_ARCHIVE"

log "Extracting Emacs package into /usr/local/stow"
sudo tar -C /usr/local/stow -xJf "$EMACS_ARCHIVE"

# Remove desktop files that can conflict with the ones from this stowed package.
log "Removing potentially conflicting desktop files"
sudo rm -f \
  /usr/local/share/applications/emacsclient.desktop \
  /usr/local/share/applications/emacsclient-mail.desktop \
  /usr/local/share/applications/emacs-mail.desktop || true

log "Activating Emacs through GNU Stow"
sudo stow -R -d /usr/local/stow -t /usr/local "$EMACS_STOW_NAME"

log "Running installation checks"
readlink -f /usr/local/bin/emacs >/dev/null

/usr/local/stow/"$EMACS_STOW_NAME"/bin/emacs -Q --batch \
  --eval "(princ (featurep 'xwidget-internal))" | grep -q '^t$' \
  || { echo "xwidgets check failed" >&2; exit 1; }

/usr/local/stow/"$EMACS_STOW_NAME"/bin/emacs -Q --batch \
  --eval "(princ (if (image-type-available-p 'imagemagick) 't 'nil))" | grep -q '^t$' \
  || { echo "ImageMagick support check failed" >&2; exit 1; }

command -v magick >/dev/null 2>&1 || command -v convert >/dev/null 2>&1 \
  || { echo "Neither magick nor convert found on PATH" >&2; exit 1; }

cat <<EOF2

Install complete.

Activated package:
  /usr/local/stow/$EMACS_STOW_NAME

Visible launcher:
  /usr/local/bin/emacs

Quick checks passed:
  - xwidgets available
  - ImageMagick available
  - Emacs activated through Stow

You can now start Emacs normally.
EOF2
