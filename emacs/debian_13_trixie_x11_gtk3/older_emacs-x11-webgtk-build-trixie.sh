#!/usr/bin/env bash
set -euo pipefail

# ------------------------------------------------------------------
# Stable Emacs 30.2 (GTK3/X11 + xwidgets) on Debian Trixie
# WebKitGTK 2.40.5 linked to private ICU 75 in /opt/icu75
# JPEG XL disabled; GI introspection/docs/minibrowser disabled
# Re-runnable: use --clean={none|light|deep}
# ------------------------------------------------------------------

# Versions / prefixes
WK_VER="${WK_VER:-2.40.5}"
ICU_VER="${ICU_VER:-75_1}"          # builds ICU 75.1
WK_PREFIX="${WK_PREFIX:-/opt/wk240}"
ICU_PREFIX="${ICU_PREFIX:-/opt/icu75}"
EMACS_VER="${EMACS_VER:-30.2}"
EMACS_STOW_NAME="${EMACS_STOW_NAME:-emacs-30.2-x11wk}"
EMACS_STOW_DIR="/usr/local/stow/${EMACS_STOW_NAME}"
BUILDROOT="${BUILDROOT:-$HOME/build-emacs-xw-x11}"
JOBS="${JOBS:-$(nproc)}"
CLEAN="${CLEAN:-none}"

# Arg parsing
for arg in "$@"; do
  case "$arg" in
    --clean=none|--clean=light|--clean=deep) CLEAN="${arg#--clean=}";;
    *) echo "Unknown arg: $arg" >&2; exit 2;;
  esac
done

ts(){ date +"[%H:%M:%S]"; }
log(){ printf "\n%s %s\n" "$(ts)" "$*"; }
die(){ printf "\nERROR: %s\n" "$*" >&2; exit 1; }
stash_dir(){ [ -d "$1" ] || return 0; mv "$1" "${1}.bak.$(date +%Y%m%d-%H%M%S)"; }
trap 'die "Build failed. See logs above."' ERR

# 1) Build prerequisites
log "1) Installing build prerequisites (apt)"
sudo apt update
sudo apt install -y build-essential cmake ninja-build pkg-config curl ca-certificates python3 bison flex gperf gettext ruby ruby-dev ccache
sudo apt install -y gobject-introspection libgirepository1.0-dev gi-docgen libxml2-utils
sudo apt install -y libgtk-3-dev libglib2.0-dev libepoxy-dev libharfbuzz-dev libpango1.0-dev
sudo apt install -y libjpeg-dev libpng-dev libwebp-dev libtiff-dev libgif-dev libxml2-dev libxslt1-dev libsqlite3-dev libopenjp2-7-dev
sudo apt install -y libatk1.0-dev libatk-bridge2.0-dev libx11-dev libxext-dev libxrender-dev libxi-dev libxfixes-dev libxinerama-dev libxcursor-dev libxcomposite-dev libxrandr-dev libx11-xcb-dev libxcb1-dev libxcb-shm0-dev
sudo apt install -y libegl-dev libgl-dev libgles-dev libglvnd-dev libdrm-dev
sudo apt install -y bubblewrap xdg-dbus-proxy
sudo apt install -y libsoup2.4-dev libsecret-1-dev libenchant-2-dev libhyphen-dev libavif-dev libseccomp-dev libwoff-dev liblcms2-dev libmanette-0.2-dev
sudo apt install -y libgstreamer1.0-dev libgstreamer-plugins-base1.0-dev libgstreamer-plugins-bad1.0-dev gstreamer1.0-plugins-good gstreamer1.0-plugins-ugly gstreamer1.0-libav
sudo apt install -y libjansson-dev libtree-sitter-dev librsvg2-dev libmailutils-dev libgccjit-14-dev
sudo apt install -y libxpm-dev libncurses-dev libtinfo-dev libxt-dev libacl1-dev libasound2-dev libxml2-utils libgpm-dev
sudo apt install -y libcairo2-dev libgnutls28-dev
# vterm pre-reqs
sudo apt install \
  cmake \
  libtool-bin \
  libvterm-dev \
  build-essential

# 2) Runtime prerequisites (TLS/GL/sandbox)
log "2) Ensuring runtime prerequisites (TLS/GL/sandbox)"
sudo apt install -y glib-networking ca-certificates libgl1-mesa-dri libegl1 libgbm1
sudo apt install -y \
  libsoup2.4-1 libsecret-1-0 libenchant-2-2 libhyphen0 liblcms2-2 \
  gstreamer1.0-plugins-good gstreamer1.0-plugins-ugly gstreamer1.0-plugins-bad gstreamer1.0-libav \
  gstreamer1.0-alsa

# 3) Prepare workspace
log "3) Preparing workspace at $BUILDROOT (clean=$CLEAN)"
mkdir -p "$BUILDROOT"
cd "$BUILDROOT"
if [ "$CLEAN" = "deep" ]; then
  stash_dir icu
  stash_dir "webkitgtk-${WK_VER}"
  rm -rf "emacs-${EMACS_VER}"
fi

# enable ccache (faster rebuilds)
export CC="ccache gcc"
export CXX="ccache g++"


# 4) Build ICU 75.1 to /opt/icu75
log "4) Building ICU ${ICU_VER/_/.} → $ICU_PREFIX"
ICU_TAG="${ICU_VER/_/-}"
ICU_TARBALL="icu4c-${ICU_VER}-src.tgz"
if [ ! -d icu ]; then
  rm -f "$ICU_TARBALL"
  curl -fL --retry 3 -o "$ICU_TARBALL" "https://github.com/unicode-org/icu/releases/download/release-${ICU_TAG}/${ICU_TARBALL}"
  tar tzf "$ICU_TARBALL" >/dev/null
  tar xzf "$ICU_TARBALL"
fi
pushd icu/source >/dev/null
[ "$CLEAN" = "light" ] && make clean || true
./configure --prefix="$ICU_PREFIX" --disable-samples --disable-tests
make -j"$JOBS"
sudo make install
popd >/dev/null

# 5) WebKitGTK source (2.40.5)
log "5) Fetching WebKitGTK $WK_VER"
[ -f "webkitgtk-${WK_VER}.tar.xz" ] || curl -LO "https://webkitgtk.org/releases/webkitgtk-${WK_VER}.tar.xz"
[ -d "webkitgtk-${WK_VER}" ] || tar -xf "webkitgtk-${WK_VER}.tar.xz"

# 6) Configure WebKitGTK (GTK3 + libsoup2 + X11; no introspection/docs/minibrowser; no JPEGXL)
log "6) Configuring WebKitGTK → $WK_PREFIX"
mkdir -p "webkitgtk-${WK_VER}/build"
cd "webkitgtk-${WK_VER}/build"
[ "$CLEAN" = "light" ] && ninja -t clean || true
sudo mkdir -p "$WK_PREFIX"

# Important: teach CMake to prefer our private ICU
export CMAKE_PREFIX_PATH="${ICU_PREFIX}${CMAKE_PREFIX_PATH+:$CMAKE_PREFIX_PATH}"
export PKG_CONFIG_PATH="${ICU_PREFIX}/lib/pkgconfig${PKG_CONFIG_PATH+:$PKG_CONFIG_PATH}"
export CFLAGS="-I${ICU_PREFIX}/include"
export CXXFLAGS="-I${ICU_PREFIX}/include"
export LDFLAGS="-Wl,-rpath,${ICU_PREFIX}/lib"

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

# 7) Build & install WebKitGTK
log "7) Building WebKitGTK (jobs: $JOBS)"
ninja -j"$JOBS"
sudo ninja install
cd "$BUILDROOT"

# 8) Verify WebKit pkg-config/version
log "8) Verifying WebKitGTK version"
PKG_CONFIG_PATH="$WK_PREFIX/lib/pkgconfig" pkg-config --exists webkit2gtk-4.0
WK_BUILT_VER="$(PKG_CONFIG_PATH="$WK_PREFIX/lib/pkgconfig" pkg-config --modversion webkit2gtk-4.0 || true)"
[ "${WK_BUILT_VER:-}" = "$WK_VER" ] || die "Expected $WK_VER, got '${WK_BUILT_VER:-none}'"

# 9) Build Emacs (GTK3/X11 + xwidgets) into Stow dir
log "9) Building Emacs $EMACS_VER (GTK3/X11 + xwidgets) → $EMACS_STOW_DIR"
[ -f "emacs-${EMACS_VER}.tar.xz" ] || curl -LO "https://ftp.gnu.org/gnu/emacs/emacs-${EMACS_VER}.tar.xz"
rm -rf "emacs-${EMACS_VER}"
tar -xf "emacs-${EMACS_VER}.tar.xz"
cd "emacs-${EMACS_VER}"
export PKG_CONFIG_PATH="$WK_PREFIX/lib/pkgconfig${PKG_CONFIG_PATH+:$PKG_CONFIG_PATH}"
export LDFLAGS="-Wl,-rpath,${WK_PREFIX}/lib -Wl,-rpath,${ICU_PREFIX}/lib"
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
  --with-sqlite3 \
  --with-gnutls \
  --with-xml2 \
  --with-gpm
make -j"$JOBS"
sudo rm -rf "$EMACS_STOW_DIR"
sudo make install

# 10) Stow it
log "10) Stowing Emacs into /usr/local"
sudo mkdir -p /usr/local/stow
sudo rm -f /usr/local/share/applications/emacsclient.desktop /usr/local/share/applications/emacsclient-mail.desktop /usr/local/share/applications/emacs-mail.desktop || true
sudo stow -R -d /usr/local/stow -t /usr/local "$EMACS_STOW_NAME"

# 11) Sanity checks
log "11) Sanity checks"
"$EMACS_STOW_DIR/bin/emacs" -Q --batch --eval "(princ (featurep 'xwidget-internal))" | grep -q '^t$' || die "xwidgets not available"
readlink -f /usr/local/bin/emacs >/dev/null || die "emacs not on PATH via stow"
echo "OK: Emacs + xwidgets ready"

# 12) Portable tarballs
log "12) Making portable tarballs"
mkdir -p "$HOME/dist"
sudo tar -C / -czf "$HOME/dist/icu75.tgz" opt/icu75
sudo tar -C / -czf "$HOME/dist/wk240.tgz" opt/wk240
sudo tar -C /usr/local/stow -czf "$HOME/dist/${EMACS_STOW_NAME}.tgz" "${EMACS_STOW_NAME}"
# after creating the three .tgz files checksum them
( cd "$HOME/dist" && sha256sum icu75.tgz wk240.tgz "${EMACS_STOW_NAME}.tgz" > SHA256SUMS )

cat <<EOF

Build complete.

Tarballs in: $HOME/dist
  icu75.tgz
  wk240.tgz
  ${EMACS_STOW_NAME}.tgz

To install on another Trixie laptop:
  sudo apt update
  sudo apt install -y glib-networking ca-certificates libgl1-mesa-dri libegl1 libgbm1 bubblewrap xdg-dbus-proxy
  sudo mkdir -p /opt /usr/local/stow
  sudo tar -C / -xzf icu75.tgz
  sudo tar -C / -xzf wk240.tgz
  sudo tar -C /usr/local/stow -xzf ${EMACS_STOW_NAME}.tgz
  sudo rm -f /usr/local/share/applications/emacsclient.desktop /usr/local/share/applications/emacsclient-mail.desktop /usr/local/share/applications/emacs-mail.desktop || true
  sudo stow -R -d /usr/local/stow -t /usr/local ${EMACS_STOW_NAME}

Add your X11 launcher wrapper to ~/.zshrc if you haven't already.
EOF
