#!/usr/bin/env bash
# emacs-webgtk-build.sh
#
# Purpose
#   Build a private WebKitGTK 2.40.x (GTK3 + libsoup2) so Emacs 30.2 can enable
#   xwidgets on Debian 12 (Bookworm) without touching the system WebKit.
#   Then build Emacs 30.2 and (by default) install it under GNU Stow so the
#   whole Emacs install is managed by symlinks.
#
# Defaults (Stow)
#   - Installs Emacs to:   /usr/local/stow/emacs-30.2-wk
#   - Then runs:           sudo stow -R -d /usr/local/stow -t /usr/local emacs-30.2-wk
#   - This keeps /usr/local clean and reversible. Use --no-stow to skip.
#
# Resume-friendly
#   - Re-running continues from where you left off (no full cleans).
#   - Use --clean=light to decide interactively whether to rebuild private deps.
#   - Use --clean=deep to stash/replace source trees and re-download tarballs.
#
# Safety & Security
#   - Uses an old WebKit (2.40.x). Prefer your regular browser for untrusted sites.
#   - rpath is set so Emacs links to the private WebKit in $WK_PREFIX/lib.
#
# Usage
#   ./emacs-webgtk-build.sh                      # default (Stow-enabled) build
#   ./emacs-webgtk-build.sh --clean=light        # ask whether to rebuild private deps
#   ./emacs-webgtk-build.sh --clean=deep         # stash trees & re-fetch sources
#   ./emacs-webgtk-build.sh --no-stow            # install Emacs directly to $EMACS_PREFIX
#   ./emacs-webgtk-build.sh --zap-old-emacs      # remove prior non-Stow 30.2 before stowing
#
# Tunables (env)
#   WK_VER=2.40.5
#   WK_PREFIX=/opt/wk240
#   EMACS_VER=30.2
#   STOW_NAME=emacs-30.2-wk
#   STOW_DIR=/usr/local/stow
#   STOW_TARGET=/usr/local
#   EMACS_PREFIX=/usr/local/stow/emacs-30.2-wk   # auto-derived from STOW by default
#   JOBS=$(nproc)

set -euo pipefail

# ------------------------------ Config ----------------------------------------
WK_VER="${WK_VER:-2.40.5}"
WK_PREFIX="${WK_PREFIX:-/opt/wk240}"

EMACS_VER="${EMACS_VER:-30.2}"
STOW_NAME="${STOW_NAME:-emacs-${EMACS_VER}-wk}"
STOW_DIR="${STOW_DIR:-/usr/local/stow}"
STOW_TARGET="${STOW_TARGET:-/usr/local}"

# Default to Stow-managed install
STOW_ENABLE="yes"
EMACS_PREFIX_DEFAULT="${EMACS_PREFIX:-$STOW_DIR/$STOW_NAME}"
EMACS_PREFIX="${EMACS_PREFIX_DEFAULT}"

BUILDROOT="${BUILDROOT:-$HOME/build-emacs-xw}"
JOBS="${JOBS:-$(nproc)}"
CLEAN="none"  # none|light|deep
ZAP_OLD="no"
REBUILD_PRIVATE_DEPS="auto"  # auto|yes|no

# ---------------------------- Arg parsing -------------------------------------
for arg in "$@"; do
  case "$arg" in
    --clean=none|--clean=light|--clean=deep) CLEAN="${arg#--clean=}";;
    --no-stow) STOW_ENABLE="no";;
    --zap-old-emacs) ZAP_OLD="yes";;
    *) echo "Unknown arg: $arg" >&2; exit 2;;
  esac
done

if [[ "$STOW_ENABLE" == "no" ]]; then
  # If user explicitly disables Stow, fall back to a plain prefix (if provided);
  # otherwise default to /usr/local
  EMACS_PREFIX="${EMACS_PREFIX:-/usr/local}"
fi

# ------------------------------ Helpers ---------------------------------------
ts() { date +"[%H:%M:%S]"; }
log(){ printf "\n%s %s\n" "$(ts)" "$*"; }
die(){ printf "\nERROR: %s\n" "$*" >&2; exit 1; }
trap 'die "Build failed. See logs above."' ERR

stash_dir() {  # move dir to dir.bak.YYYYmmdd-HHMMSS (if exists)
  [[ -d "$1" ]] || return 0
  local dst="${1}.bak.$(date +%Y%m%d-%H%M%S)"
  mv "$1" "$dst"
  log "Stashed '$1' -> '$dst'"
}

require_cmd() {
  command -v "$1" >/dev/null 2>&1 || die "Missing command: $1"
}

webkit_present() {
  [[ -f "$WK_PREFIX/lib/pkgconfig/webkit2gtk-4.0.pc" ]] \
    && [[ -f "$WK_PREFIX/lib/libwebkit2gtk-4.0.so" || -f "$WK_PREFIX/lib/libwebkit2gtk-4.0.so.37" ]]
}

decide_rebuild_private_deps() {
  case "$CLEAN" in
    deep)
      REBUILD_PRIVATE_DEPS="yes"
      ;;
    light)
      if [[ "$REBUILD_PRIVATE_DEPS" == "auto" ]]; then
        if [[ -t 0 ]]; then
          printf "\nRebuild private dependencies under %s (WebKitGTK etc.)? [y/N] " "$WK_PREFIX" >&2
          local reply
          IFS= read -r reply || reply=""
          case "$reply" in
            [Yy]|[Yy][Ee][Ss]) REBUILD_PRIVATE_DEPS="yes" ;;
            *) REBUILD_PRIVATE_DEPS="no" ;;
          esac
        else
          REBUILD_PRIVATE_DEPS="no"
        fi
      fi
      ;;
    *)
      if [[ "$REBUILD_PRIVATE_DEPS" == "auto" ]]; then
        REBUILD_PRIVATE_DEPS="no"
      fi
      ;;
  esac
}

preinstall_note() {
  log "Config:
  WebKit version:       $WK_VER
  WebKit prefix:        $WK_PREFIX
  Build root:           $BUILDROOT
  Stow enabled:         $STOW_ENABLE
  Emacs version:        $EMACS_VER
  Emacs install prefix: $EMACS_PREFIX
  Stow name:            $STOW_NAME
  Stow dir:             $STOW_DIR
  Stow target:          $STOW_TARGET
  Clean mode:           $CLEAN
  Rebuild private deps: $REBUILD_PRIVATE_DEPS
  Jobs:                 $JOBS"
}

# Remove common non-Stow Emacs 30.2 files so stow won’t conflict.
zap_old_emacs() {
  [[ "$ZAP_OLD" == "yes" ]] || return 0
  log "Removing prior non-Stow Emacs $EMACS_VER files that commonly conflict..."
  sudo rm -f  /usr/local/bin/emacs-"$EMACS_VER" \
              /usr/local/bin/emacsclient \
              /usr/local/bin/ebrowse \
              /usr/local/bin/etags \
              /usr/local/bin/ctags \
              /usr/local/share/applications/emacs.desktop \
              /usr/local/share/man/man1/emacs.1 \
              /usr/local/share/man/man1/emacsclient.1 \
              /usr/local/share/man/man1/etags.1 \
              /usr/local/share/man/man1/ctags.1
  sudo rm -rf /usr/local/share/emacs/"$EMACS_VER"
  # Don't remove /usr/local/bin/emacs unless it’s a dangling/conflicting leftover:
  if [[ -L /usr/local/bin/emacs ]]; then
    sudo rm -f /usr/local/bin/emacs
  fi
}

remove_stale_glib_schema_cache() {
  local target="$STOW_TARGET/share/glib-2.0/schemas/gschemas.compiled"
  if [[ -e "$target" && ! -L "$target" ]]; then
    log "Removing stale non-stowed GLib schema cache at $target"
    sudo rm -f "$target"
  fi
}

configure_alsa_pulse_bridge() {
  local asoundrc="$HOME/.asoundrc"
  local asoundrc_block
  local updated_asoundrc="no"
  log "6) Configuring ALSA default routing for native Emacs sound"
  log "Why: this Emacs build uses ALSA, while WSL/WSLg usually exposes PulseAudio as the working runtime sink"

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
  local emacs_bin="$1"
  local sound_file
  sound_file="$(find_sound_test_file)"
  if [[ -z "$sound_file" ]]; then
    log "No WAV test file found under /usr/share/sounds; skipping Emacs sound smoke test"
    return 0
  fi

  log "7) Running Emacs sound smoke test with $sound_file"
  "$emacs_bin" -Q --batch     --eval "(condition-case err (progn (play-sound-file \"$sound_file\") (princ \"ok\")) (error (princ (error-message-string err)) (kill-emacs 12)))"     | grep -q '^ok$' || die "Emacs sound smoke test failed"
}

# --------------------------- 1) System packages --------------------------------
log "1) Installing build dependencies (apt)"
sudo apt update

# Keep as array so comments don’t break continuations.
PKGS=(
  # Toolchain & build tools
  build-essential cmake ninja-build pkg-config curl ca-certificates
  python3 ruby ruby-dev bison flex gperf gettext

  # GTK3 + common graphics/libs
  libgtk-3-dev libglib2.0-dev libepoxy-dev libharfbuzz-dev libpango1.0-dev
  libjpeg-dev libpng-dev libwebp-dev libtiff-dev libgif-dev
  libicu-dev libxml2-dev libxslt1-dev libsqlite3-dev libopenjp2-7-dev
  libatk1.0-dev libatk-bridge2.0-dev libxrandr-dev libxdamage-dev libxcomposite-dev
  libxkbcommon-dev libdrm-dev libwayland-dev wayland-protocols bubblewrap
  libsoup2.4-dev

  # Introspection (needed by WebKitGTK build)
  gobject-introspection libgirepository1.0-dev gi-docgen

  # WPE/Wayland pieces used by WebKitGTK
  libwpe-1.0-dev libwpebackend-fdo-1.0-dev

  # Spell/hyphen/images/sandbox
  libenchant-2-dev libhyphen-dev libavif-dev libseccomp-dev

  # GStreamer stack for media support
  libgstreamer1.0-dev libgstreamer-plugins-base1.0-dev libgstreamer-plugins-bad1.0-dev
  gstreamer1.0-plugins-bad gstreamer1.0-plugins-good gstreamer1.0-plugins-ugly gstreamer1.0-libav

  # WebKit odds & ends hit by CMake checks
  libgcrypt20-dev libgpg-error-dev unifdef libmanette-0.2-dev libevdev-dev gir1.2-manette-0.2

  # **WOFF2 dev pkg on Debian 12 (provides libwoff2dec.pc)**
  libwoff-dev

  # Emacs feature headers
  libgnutls28-dev libjansson-dev libtree-sitter-dev librsvg2-dev libgccjit-12-dev libmailutils-dev
  libasound2-dev libasound2-plugins alsa-utils

  # Stow (default flow depends on it)
  stow
)
sudo apt install -y "${PKGS[@]}"

# Optional: speed up rebuilds with ccache
if ! command -v ccache >/dev/null 2>&1; then
  sudo apt install -y ccache || true
fi

# Sanity for WOFF2 (should succeed after installing libwoff-dev)
pkg-config --exists libwoff2dec || die "pkg-config can't find libwoff2dec (install libwoff-dev)."
pkg-config --exists alsa || die "pkg-config can't find alsa (install libasound2-dev)."

# --------------------- 2) Prepare sources / cleaning ---------------------------
decide_rebuild_private_deps
preinstall_note
mkdir -p "$BUILDROOT"
cd "$BUILDROOT"

SRC_TAR="webkitgtk-$WK_VER.tar.xz"
SRC_DIR="webkitgtk-$WK_VER"
BUILD_DIR="$SRC_DIR/build"

if [[ "$CLEAN" == "deep" ]]; then
  stash_dir "$SRC_DIR"
  [[ -f "$SRC_TAR" ]] && mv "$SRC_TAR" "${SRC_TAR}.bak.$(date +%Y%m%d-%H%M%S)"
fi

[[ -f "$SRC_TAR" ]] || curl -LO "https://webkitgtk.org/releases/$SRC_TAR"
[[ -d "$SRC_DIR" ]] || tar -xf "$SRC_TAR"

mkdir -p "$BUILD_DIR"
cd "$BUILD_DIR"

if [[ "$CLEAN" == "light" && "$REBUILD_PRIVATE_DEPS" == "yes" ]]; then
  ninja -t clean || true
fi

# Ensure WebKit install prefix exists
sudo mkdir -p "$WK_PREFIX"

# ------------------ 3) Configure & build WebKitGTK (GTK3 + Soup2) --------------
if [[ "$REBUILD_PRIVATE_DEPS" == "yes" ]]; then
  log "3) Configuring WebKitGTK $WK_VER (GTK3 + libsoup2) → $WK_PREFIX"
  cmake .. -GNinja \
    -DCMAKE_BUILD_TYPE=Release \
    -DCMAKE_INSTALL_PREFIX="$WK_PREFIX" \
    -DPORT=GTK \
    -DUSE_SOUP2=ON \
    -DCMAKE_C_COMPILER_LAUNCHER=ccache \
    -DCMAKE_CXX_COMPILER_LAUNCHER=ccache

  log "Building WebKitGTK (jobs: $JOBS)"
  ninja -j"$JOBS"
  sudo ninja install
else
  log "3) Skipping WebKitGTK rebuild and reusing existing private install at $WK_PREFIX"
fi

# Verify the private pkg-config file and version
webkit_present || die "Expected an existing WebKitGTK install under $WK_PREFIX, but it was not found."
PKG_CONFIG_PATH="$WK_PREFIX/lib/pkgconfig" pkg-config --exists webkit2gtk-4.0 \
  || die "private webkit2gtk-4.0 .pc not found in $WK_PREFIX/lib/pkgconfig"

WK_BUILT_VER="$(PKG_CONFIG_PATH="$WK_PREFIX/lib/pkgconfig" pkg-config --modversion webkit2gtk-4.0)"
echo "webkit2gtk-4.0 @ $WK_PREFIX → $WK_BUILT_VER"
case "$WK_BUILT_VER" in 2.40.*) : ;; *) die "Expected 2.40.x, got '$WK_BUILT_VER'";; esac

# ------------------ 4) Build Emacs $EMACS_VER with xwidgets --------------------
log "4) Building Emacs $EMACS_VER with xwidgets (rpath -> $WK_PREFIX/lib)"
require_cmd curl
require_cmd tar

# Freshen Emacs sources when deep cleaning
cd /tmp
if [[ "$CLEAN" == "deep" ]]; then
  rm -rf "emacs-$EMACS_VER"
fi
rm -f "emacs-$EMACS_VER.tar.xz"
curl -LO "https://ftp.gnu.org/gnu/emacs/emacs-$EMACS_VER.tar.xz"
rm -rf "emacs-$EMACS_VER"
tar -xf "emacs-$EMACS_VER.tar.xz"
cd "emacs-$EMACS_VER"

export PKG_CONFIG_PATH="$WK_PREFIX/lib/pkgconfig:${PKG_CONFIG_PATH:-}"
export LDFLAGS="-Wl,-rpath,$WK_PREFIX/lib ${LDFLAGS:-}"

./configure \
  --prefix="$EMACS_PREFIX" \
  --with-pgtk \
  --with-native-compilation \
  --with-tree-sitter \
  --with-modules \
  --with-cairo \
  --with-harfbuzz \
  --with-xwidgets \
  --with-mailutils \
  --with-rsvg \
  --with-webp \
  --with-sqlite3 \
  --with-gnutls \
  --with-xml2

grep -q '^#define HAVE_ALSA 1$' src/config.h \
  || die "Emacs configure did not enable ALSA (expected HAVE_ALSA=1 in src/config.h)."

make -j"$JOBS"
sudo make install

# ------------------ 5) Stow (default) & verify xwidgets ------------------------
if [[ "$STOW_ENABLE" == "yes" ]]; then
  require_cmd stow
  # Ensure Stow dir exists and optionally zap old non-Stow files
  sudo mkdir -p "$STOW_DIR"
  zap_old_emacs
  remove_stale_glib_schema_cache

  log "Dry-run Stow to check for conflicts..."
  if ! sudo stow -n -v -d "$STOW_DIR" -t "$STOW_TARGET" "$STOW_NAME"; then
    die "GNU Stow reported conflicts. Re-run with --zap-old-emacs or remove listed files."
  fi

  log "Applying Stow links..."
  sudo stow -R -d "$STOW_DIR" -t "$STOW_TARGET" "$STOW_NAME"
else
  log "Skipping Stow (installed directly to $EMACS_PREFIX)"
fi

log "5) Verifying xwidgets availability"
"$EMACS_PREFIX/bin/emacs" -Q --batch --eval "(princ (featurep 'xwidget-internal))" | grep -q '^t$' \
  && echo "OK: xwidgets available" \
  || die "xwidgets NOT available (check WebKit build/paths)."

cat <<EOF

Success!

Run Emacs:
  $( [[ "$STOW_ENABLE" == "yes" ]] && echo "$STOW_TARGET/bin/emacs" || echo "$EMACS_PREFIX/bin/emacs" )

WSL/WSLg native sound post-build step:
  cat > ~/.asoundrc <<'EOF'
  pcm.!default {
    type pulse
  }
  ctl.!default {
    type pulse
  }
  EOF

Why this is needed:
  Emacs sound on Linux uses ALSA in this build path. On WSLg, PulseAudio is
  typically available but native ALSA hardware is not. The ALSA-to-Pulse bridge
  gives `play-sound-file` a usable default device.

Inside Emacs:
  M-: (featurep 'xwidget-internal)   ;; should print t
  M-: (play-sound-file "/usr/share/sounds/purple/receive.wav")

Doom tip (config.el):
  (if (featurep 'xwidget-internal)
      (setq browse-url-browser-function #'xwidget-webkit-browse-url)
    (setq browse-url-browser-function #'browse-url-default-browser))

Notes:
  - This uses WebKitGTK 2.40.x. Prefer your main browser for untrusted sites.
  - Re-run with --clean=light or --clean=deep if you need to rebuild fresh.
  - Use --no-stow to install Emacs directly to a non-Stow prefix.

EOF
