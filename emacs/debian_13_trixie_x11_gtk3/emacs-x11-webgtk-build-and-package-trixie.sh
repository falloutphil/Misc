#!/usr/bin/env bash
set -euo pipefail

# -----------------------------------------------------------------------------
# Emacs 30.2 for Debian 13 (Trixie)
#
# What this script does:
#   1. Installs build dependencies on the build machine.
#   2. Builds a private ICU under /opt/icu75.
#   3. Builds a private WebKitGTK 2.40.5 under /opt/wk240, linked to that ICU.
#   4. Builds Emacs with GTK3/X11, xwidgets and ImageMagick support.
#   5. Installs Emacs under /usr/local/stow/<name> and activates it via GNU Stow.
#   6. Produces a portable bundle in $HOME/dist containing:
#        - icu75.txz
#        - wk240.txz
#        - <emacs-stow-name>.txz
#        - client installer script
#        - README
#        - combined bundle tar.xz
#
# Why Stow is used for Emacs but not for ICU/WebKitGTK:
#   - Emacs is the user-facing application we want to “install” neatly into
#     /usr/local/bin, /usr/local/share, etc. GNU Stow excels at that job by
#     managing a symlink farm from /usr/local/stow/<package> into /usr/local.
#   - ICU and WebKitGTK are *private bundled dependencies* for this Emacs build,
#     not general system libraries. Keeping them under /opt makes it obvious
#     that they are local, non-Debian-managed builds and reduces the chance that
#     other software accidentally links against them.
#
# Why xwidgets is painful:
#   - Emacs xwidgets on GNU/Linux rely on GTK and WebKitGTK.
#   - The distro version of WebKitGTK may be newer than the version that has
#     historically behaved well with the exact Emacs/xwidgets combination you
#     want to run.
#   - Building a private, known-good WebKitGTK stack is therefore a pragmatic
#     way to stabilize the result.
#
# Why GObject Introspection is disabled here:
#   - WebKitGTK can optionally generate GIR/typelib metadata for language
#     bindings and related tooling.
#   - Emacs xwidgets do not need that metadata in order to *use* WebKitGTK.
#   - In practice, introspection has been a recurring source of build pain,
#     especially when you are trying to keep the build minimal and reproducible.
#   - So we turn it off deliberately and also keep the related apt packages
#     commented out below.
# -----------------------------------------------------------------------------

# ------------------------------
# Tunables
# ------------------------------
WK_VER="${WK_VER:-2.40.5}"
ICU_VER="${ICU_VER:-75_1}"
WK_PREFIX="${WK_PREFIX:-/opt/wk240}"
ICU_PREFIX="${ICU_PREFIX:-/opt/icu75}"
EMACS_VER="${EMACS_VER:-30.2}"
EMACS_STOW_NAME="${EMACS_STOW_NAME:-emacs-30.2-x11wk}"
EMACS_STOW_DIR="/usr/local/stow/${EMACS_STOW_NAME}"
BUILDROOT="${BUILDROOT:-$HOME/build-emacs-xw-x11}"
DISTDIR="${DISTDIR:-$HOME/dist/${EMACS_STOW_NAME}}"
BUNDLEDIR="${BUNDLEDIR:-$HOME/dist}"
JOBS="${JOBS:-$(nproc)}"
CLEAN="${CLEAN:-none}"

# ------------------------------
# Arg parsing
# ------------------------------
for arg in "$@"; do
  case "$arg" in
    --clean=none|--clean=light|--clean=deep)
      CLEAN="${arg#--clean=}"
      ;;
    *)
      echo "Unknown arg: $arg" >&2
      exit 2
      ;;
  esac
done

# ------------------------------
# Helpers
# ------------------------------
ts() { date +"[%H:%M:%S]"; }
log() { printf "\n%s %s\n" "$(ts)" "$*"; }
die() { printf "\nERROR: %s\n" "$*" >&2; exit 1; }
stash_dir() {
  [ -d "$1" ] || return 0
  mv "$1" "${1}.bak.$(date +%Y%m%d-%H%M%S)"
}
need_cmd() {
  command -v "$1" >/dev/null 2>&1 || die "Required command not found: $1"
}
trap 'die "Build failed. See logs above."' ERR

# A predictable umask matters for the final artifacts. 022 means:
#   - owner can write
#   - group/world can read
# This avoids producing tarballs that unpack into weirdly private trees.
umask 022

need_cmd sudo
need_cmd curl
need_cmd tar
need_cmd sha256sum

# ------------------------------
# 1) Build prerequisites
# ------------------------------
log "1) Installing build prerequisites (apt)"
sudo apt update

# Core tooling.
sudo apt install -y \
  build-essential cmake ninja-build pkg-config curl ca-certificates \
  python3 bison flex gperf gettext ruby ruby-dev ccache stow xz-utils file

# GTK / X11 / rendering / media dependencies needed by WebKitGTK and Emacs.
sudo apt install -y \
  libgtk-3-dev libglib2.0-dev libepoxy-dev libharfbuzz-dev libpango1.0-dev \
  libjpeg-dev libpng-dev libwebp-dev libtiff-dev libgif-dev \
  libxml2-dev libxslt1-dev libsqlite3-dev libopenjp2-7-dev \
  libatk1.0-dev libatk-bridge2.0-dev \
  libx11-dev libxext-dev libxrender-dev libxi-dev libxfixes-dev \
  libxinerama-dev libxcursor-dev libxcomposite-dev libxrandr-dev \
  libx11-xcb-dev libxcb1-dev libxcb-shm0-dev \
  libegl-dev libgl-dev libgles-dev libglvnd-dev libdrm-dev \
  bubblewrap xdg-dbus-proxy \
  libsoup2.4-dev libsecret-1-dev libenchant-2-dev libhyphen-dev \
  libavif-dev libseccomp-dev libwoff-dev liblcms2-dev libmanette-0.2-dev \
  libgstreamer1.0-dev libgstreamer-plugins-base1.0-dev \
  libgstreamer-plugins-bad1.0-dev \
  gstreamer1.0-plugins-good gstreamer1.0-plugins-ugly gstreamer1.0-libav

# Emacs-specific build dependencies.
sudo apt install -y \
  libjansson-dev libtree-sitter-dev librsvg2-dev libmailutils-dev \
  libgccjit-14-dev libxpm-dev libncurses-dev libtinfo-dev libxt-dev \
  libacl1-dev libasound2-dev libgpm-dev libcairo2-dev libgnutls28-dev

# ImageMagick support for Emacs image handling and for the external sprite/image
# tooling used by your own Emacs Lisp projects.
sudo apt install -y imagemagick libmagickwand-7.q16-dev

# Optional packages for WebKitGTK introspection and docs.
# They are *not* used by this build because ENABLE_INTROSPECTION=OFF and
# ENABLE_DOCUMENTATION=OFF below. They remain here as commented reference only.
# sudo apt install -y gobject-introspection libgirepository1.0-dev gi-docgen

# ------------------------------
# 2) Runtime prerequisites on the build machine
# ------------------------------
log "2) Ensuring runtime prerequisites (TLS/GL/sandbox/native-comp/ImageMagick)"
sudo apt install -y \
  glib-networking ca-certificates libgl1-mesa-dri libegl1 libgbm1 \
  libsoup2.4-1 libsecret-1-0 libenchant-2-2 libhyphen0 liblcms2-2 \
  gstreamer1.0-plugins-good gstreamer1.0-plugins-ugly \
  gstreamer1.0-plugins-bad gstreamer1.0-libav gstreamer1.0-alsa \
  libgccjit0 imagemagick

# ------------------------------
# 3) Prepare workspace
# ------------------------------
log "3) Preparing workspace at $BUILDROOT (clean=$CLEAN)"
mkdir -p "$BUILDROOT" "$DISTDIR"
cd "$BUILDROOT"

if [ "$CLEAN" = "deep" ]; then
  stash_dir icu
  stash_dir "webkitgtk-${WK_VER}"
  rm -rf "emacs-${EMACS_VER}"
fi

# ccache speeds up rebuilds dramatically on iteration.
export CC="ccache gcc"
export CXX="ccache g++"

# ------------------------------
# 4) Build ICU privately under /opt/icu75
# ------------------------------
log "4) Building ICU ${ICU_VER/_/.} -> $ICU_PREFIX"
ICU_TAG="${ICU_VER/_/-}"
ICU_TARBALL="icu4c-${ICU_VER}-src.tgz"

if [ ! -d icu ]; then
  rm -f "$ICU_TARBALL"
  curl -fL --retry 3 -o "$ICU_TARBALL" \
    "https://github.com/unicode-org/icu/releases/download/release-${ICU_TAG}/${ICU_TARBALL}"
  tar tzf "$ICU_TARBALL" >/dev/null
  tar xzf "$ICU_TARBALL"
fi

pushd icu/source >/dev/null
[ "$CLEAN" = "light" ] && make clean || true
./configure --prefix="$ICU_PREFIX" --disable-samples --disable-tests
make -j"$JOBS"
sudo make install
popd >/dev/null

# ------------------------------
# 5) Fetch WebKitGTK source
# ------------------------------
log "5) Fetching WebKitGTK $WK_VER"
[ -f "webkitgtk-${WK_VER}.tar.xz" ] || \
  curl -fLO --retry 3 "https://webkitgtk.org/releases/webkitgtk-${WK_VER}.tar.xz"
[ -d "webkitgtk-${WK_VER}" ] || tar -xf "webkitgtk-${WK_VER}.tar.xz"

# ------------------------------
# 6) Configure WebKitGTK
# ------------------------------
log "6) Configuring WebKitGTK -> $WK_PREFIX"
mkdir -p "webkitgtk-${WK_VER}/build"
cd "webkitgtk-${WK_VER}/build"
[ "$CLEAN" = "light" ] && ninja -t clean || true
sudo mkdir -p "$WK_PREFIX"

# Force this build to prefer our private ICU instead of the system one.
export CMAKE_PREFIX_PATH="${ICU_PREFIX}${CMAKE_PREFIX_PATH+:$CMAKE_PREFIX_PATH}"
export PKG_CONFIG_PATH="${ICU_PREFIX}/lib/pkgconfig${PKG_CONFIG_PATH+:$PKG_CONFIG_PATH}"
export CFLAGS="-I${ICU_PREFIX}/include${CFLAGS:+ $CFLAGS}"
export CXXFLAGS="-I${ICU_PREFIX}/include${CXXFLAGS:+ $CXXFLAGS}"
export LDFLAGS="-Wl,-rpath,${ICU_PREFIX}/lib${LDFLAGS:+ $LDFLAGS}"

cmake .. -GNinja \
  -DCMAKE_BUILD_TYPE=Release \
  -DCMAKE_INSTALL_PREFIX="$WK_PREFIX" \
  -DCMAKE_PREFIX_PATH="$CMAKE_PREFIX_PATH" \
  -DICU_ROOT="$ICU_PREFIX" \
  -DPORT=GTK \
  -DENABLE_X11_TARGET=ON \
  -DENABLE_WAYLAND_TARGET=OFF \
  -DUSE_GTK4=OFF \
  -DUSE_SOUP2=ON \
  -DUSE_JPEGXL=OFF \
  -DENABLE_INTROSPECTION=OFF \
  -DENABLE_DOCUMENTATION=OFF \
  -DENABLE_MINIBROWSER=OFF

# ------------------------------
# 7) Build and install WebKitGTK
# ------------------------------
log "7) Building WebKitGTK (jobs: $JOBS)"
ninja -j"$JOBS"
sudo ninja install
cd "$BUILDROOT"

# ------------------------------
# 8) Verify WebKitGTK pkg-config version
# ------------------------------
log "8) Verifying WebKitGTK version"
PKG_CONFIG_PATH="$WK_PREFIX/lib/pkgconfig" pkg-config --exists webkit2gtk-4.0
WK_BUILT_VER="$(PKG_CONFIG_PATH="$WK_PREFIX/lib/pkgconfig" pkg-config --modversion webkit2gtk-4.0 || true)"
[ "${WK_BUILT_VER:-}" = "$WK_VER" ] || die "Expected $WK_VER, got '${WK_BUILT_VER:-none}'"

# ------------------------------
# 9) Build Emacs into a Stow package directory
# ------------------------------
log "9) Building Emacs $EMACS_VER -> $EMACS_STOW_DIR"
[ -f "emacs-${EMACS_VER}.tar.xz" ] || \
  curl -fLO --retry 3 "https://ftp.gnu.org/gnu/emacs/emacs-${EMACS_VER}.tar.xz"
rm -rf "emacs-${EMACS_VER}"
tar -xf "emacs-${EMACS_VER}.tar.xz"
cd "emacs-${EMACS_VER}"

export PKG_CONFIG_PATH="$WK_PREFIX/lib/pkgconfig${PKG_CONFIG_PATH+:$PKG_CONFIG_PATH}"
export LDFLAGS="-Wl,-rpath,${WK_PREFIX}/lib -Wl,-rpath,${ICU_PREFIX}/lib${LDFLAGS:+ $LDFLAGS}"

./configure \
  --prefix="$EMACS_STOW_DIR" \
  --with-x=yes \
  --with-x-toolkit=gtk3 \
  --with-xwidgets \
  --with-native-compilation \
  --with-tree-sitter \
  --with-modules \
  --with-cairo \
  --with-harfbuzz \
  --with-mailutils \
  --with-rsvg \
  --with-webp \
  --with-imagemagick \
  --with-sqlite3 \
  --with-gnutls \
  --with-xml2 \
  --with-gpm

make -j"$JOBS"
sudo rm -rf "$EMACS_STOW_DIR"
sudo make install

# ------------------------------
# 10) Stow Emacs into /usr/local
# ------------------------------
log "10) Activating Emacs via GNU Stow"
# GNU Stow is a symlink farm manager. The actual package lives in
#   /usr/local/stow/<package>
# and Stow creates the visible links in
#   /usr/local/bin, /usr/local/share, etc.
# This gives you a very clean upgrade/uninstall story for software that you
# build yourself.
sudo mkdir -p /usr/local/stow
sudo rm -f \
  /usr/local/share/applications/emacsclient.desktop \
  /usr/local/share/applications/emacsclient-mail.desktop \
  /usr/local/share/applications/emacs-mail.desktop || true
sudo stow -R -d /usr/local/stow -t /usr/local "$EMACS_STOW_NAME"

# ------------------------------
# 11) Sanity checks
# ------------------------------
log "11) Running sanity checks"
"$EMACS_STOW_DIR/bin/emacs" -Q --batch \
  --eval "(princ (featurep 'xwidget-internal))" | grep -q '^t$' \
  || die "xwidgets not available"

"$EMACS_STOW_DIR/bin/emacs" -Q --batch \
  --eval "(princ (if (image-type-available-p 'imagemagick) 't 'nil))" | grep -q '^t$' \
  || die "ImageMagick support not available"

readlink -f /usr/local/bin/emacs >/dev/null || die "emacs not on PATH via stow"
command -v magick >/dev/null 2>&1 || command -v convert >/dev/null 2>&1 \
  || die "Neither magick nor convert found on PATH"

echo "OK: Emacs + xwidgets + ImageMagick ready"

# ------------------------------
# 12) Package portable artifacts
# ------------------------------
log "12) Creating portable artifact set under $DISTDIR"
rm -rf "$DISTDIR"
mkdir -p "$DISTDIR"

# The umask is already 022 globally, but make it explicit again for packaging.
umask 022

sudo tar -C / -cJf "$DISTDIR/icu75.txz" opt/icu75
sudo tar -C / -cJf "$DISTDIR/wk240.txz" opt/wk240
sudo tar -C /usr/local/stow -cJf "$DISTDIR/${EMACS_STOW_NAME}.txz" "$EMACS_STOW_NAME"

# Metadata to make auditing and troubleshooting easier later.
cat > "$DISTDIR/BUILD-METADATA.txt" <<META
Build timestamp: $(date -Is)
Build host: $(hostname)
Debian release info:
$(cat /etc/os-release)
Kernel: $(uname -a)
WebKitGTK version: $WK_VER
ICU version: ${ICU_VER/_/.}
Emacs version: $EMACS_VER
Emacs stow name: $EMACS_STOW_NAME
ICU prefix: $ICU_PREFIX
WebKit prefix: $WK_PREFIX
Stow dir: $EMACS_STOW_DIR
META

# Copy the client installer and README into the dist tree if they exist next to
# this script. This makes the builder the single command that emits a complete
# delivery bundle.
SCRIPT_DIR="$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)"
[ -f "$SCRIPT_DIR/emacs-x11-webgtk-client-install-trixie.sh" ] && \
  cp "$SCRIPT_DIR/emacs-x11-webgtk-client-install-trixie.sh" "$DISTDIR/"
[ -f "$SCRIPT_DIR/README.md" ] && cp "$SCRIPT_DIR/README.md" "$DISTDIR/"

(
  cd "$DISTDIR"
  sha256sum icu75.txz wk240.txz "${EMACS_STOW_NAME}.txz" > SHA256SUMS
)

# Build one combined bundle so you can hand over a single artifact tree if you
# want to. The separate txz files remain useful for inspection and re-use.
COMBINED_BUNDLE="$BUNDLEDIR/${EMACS_STOW_NAME}-bundle.tar.xz"
tar -C "$BUNDLEDIR" -cJf "$COMBINED_BUNDLE" "${EMACS_STOW_NAME}"
sha256sum "$COMBINED_BUNDLE" > "$COMBINED_BUNDLE.sha256"

cat <<EOF2

Build complete.

Artifacts directory:
  $DISTDIR

Combined hand-off bundle:
  $COMBINED_BUNDLE
  $COMBINED_BUNDLE.sha256

Contents:
  icu75.txz
  wk240.txz
  ${EMACS_STOW_NAME}.txz
  SHA256SUMS
  BUILD-METADATA.txt
  emacs-x11-webgtk-client-install-trixie.sh   (if present next to this script)
  README.md                                   (if present next to this script)

This build machine now also has Emacs activated locally via GNU Stow.
EOF2
