#!/usr/bin/env bash
set -euo pipefail

# ------------------------------------------------------------------
# Stable Emacs 30.2 (GTK3/X11 + xwidgets) on Debian Trixie
# WebKitGTK 2.40.5 linked to private ICU 75 in /opt/icu75
# JPEG XL disabled; GI introspection/docs/minibrowser disabled
# Re-runnable: use --clean={none|light|deep}
# ------------------------------------------------------------------

WK_VER="${WK_VER:-2.40.5}"
ICU_VER="${ICU_VER:-75_1}"                 # ICU 75.1
EMACS_VER="${EMACS_VER:-30.2}"

WORKROOT="${WORKROOT:-$HOME/build-emacs-xw-x11}"
WK_PREFIX="${WK_PREFIX:-/opt/wk240}"
ICU_PREFIX="${ICU_PREFIX:-/opt/icu75}"
STOW_DIR="${STOW_DIR:-/usr/local/stow}"
EMACS_STOW_NAME="${EMACS_STOW_NAME:-emacs-${EMACS_VER}-x11wk}"
EMACS_PREFIX="${EMACS_PREFIX:-${STOW_DIR}/${EMACS_STOW_NAME}}"

CLEAN_MODE="none"

usage() {
  cat <<EOF
Usage: $0 [--clean=none|light|deep]

  none  - keep everything possible
  light - rebuild WebKit/Emacs build dirs, keep downloaded tarballs and source trees
  deep  - wipe workroot and rebuild from scratch

Environment overrides:
  WK_VER ICU_VER EMACS_VER WORKROOT WK_PREFIX ICU_PREFIX STOW_DIR EMACS_STOW_NAME
EOF
}

for arg in "$@"; do
  case "$arg" in
    --clean=none|--clean=light|--clean=deep)
      CLEAN_MODE="${arg#--clean=}"
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    *)
      printf 'Unknown argument: %s\n' "$arg" >&2
      usage >&2
      exit 1
      ;;
  esac
done

log() {
  printf '\n[%s] %s\n' "$(date +%H:%M:%S)" "$*"
}

die() {
  printf '\nERROR: %s\n' "$*" >&2
  exit 1
}

need_cmd() {
  command -v "$1" >/dev/null 2>&1 || die "Missing required command: $1"
}

sudo_keepalive() {
  sudo -v
}

cleanup_sudo_timestamp() {
  sudo -k || true
}
trap cleanup_sudo_timestamp EXIT

install_build_prereqs() {
  log "1) Installing build prerequisites"

  sudo apt-get update
  sudo apt-get install -y \
    build-essential \
    cmake \
    ninja-build \
    pkg-config \
    curl \
    ca-certificates \
    python3 \
    ruby \
    ruby-dev \
    bison \
    flex \
    gperf \
    gettext \
    xz-utils \
    file \
    rsync \
    stow \
    libcairo2-dev \
    libgtk-3-dev \
    libglib2.0-dev \
    libepoxy-dev \
    libharfbuzz-dev \
    libpango1.0-dev \
    libjpeg-dev \
    libpng-dev \
    libwebp-dev \
    libtiff-dev \
    libgif-dev \
    libxml2-dev \
    libxslt1-dev \
    libsqlite3-dev \
    libopenjp2-7-dev \
    libwoff-dev \
    liblcms2-dev \
    libatk1.0-dev \
    libatk-bridge2.0-dev \
    libx11-dev \
    libxext-dev \
    libxrender-dev \
    libxi-dev \
    libxfixes-dev \
    libxinerama-dev \
    libxcursor-dev \
    libxcomposite-dev \
    libxdamage-dev \
    libxrandr-dev \
    libx11-xcb-dev \
    libxcb1-dev \
    libxcb-shm0-dev \
    libxt-dev \
    libxtst-dev \
    libegl-dev \
    libgl-dev \
    libgles-dev \
    libglvnd-dev \
    libdrm-dev \
    libgbm-dev \
    libsoup2.4-dev \
    libsecret-1-dev \
    libenchant-2-dev \
    libhyphen-dev \
    libavif-dev \
    libseccomp-dev \
    libmanette-0.2-dev \
    libevdev-dev \
    libgcrypt20-dev \
    libgpg-error-dev \
    libgstreamer1.0-dev \
    libgstreamer-plugins-base1.0-dev \
    libgstreamer-plugins-bad1.0-dev \
    gstreamer1.0-plugins-good \
    gstreamer1.0-plugins-bad \
    gstreamer1.0-plugins-ugly \
    gstreamer1.0-libav \
    bubblewrap \
    xdg-dbus-proxy \
    glib-networking \
    libjansson-dev \
    libtree-sitter-dev \
    librsvg2-dev \
    libmailutils-dev \
    libgccjit-14-dev \
    libxpm-dev \
    libncurses-dev \
    libacl1-dev \
    libgpm-dev \
    libgnutls28-dev \
    imagemagick \
    libmagickwand-7.q16-dev
}

prepare_workspace() {
  log "2) Preparing workspace at ${WORKROOT} (clean=${CLEAN_MODE})"

  case "$CLEAN_MODE" in
    deep)
      rm -rf "$WORKROOT"
      ;;
    light)
      rm -rf \
        "$WORKROOT/webkitgtk-${WK_VER}/build" \
        "$WORKROOT/emacs-${EMACS_VER}/build"
      ;;
    none)
      :
      ;;
  esac

  mkdir -p "$WORKROOT"
}

build_private_icu() {
  log "3) Building private ICU 75.1 -> ${ICU_PREFIX}"

  local icu_tar="icu4c-${ICU_VER}-src.tgz"
  local icu_url="https://github.com/unicode-org/icu/releases/download/release-75-1/${icu_tar}"
  local icu_src_root="${WORKROOT}/icu"
  local icu_build_dir="${icu_src_root}/source"

  if [[ "$CLEAN_MODE" == "deep" ]]; then
    sudo rm -rf "$ICU_PREFIX"
  fi

  if [[ -x "${ICU_PREFIX}/bin/icuinfo" ]] && [[ -f "${ICU_PREFIX}/lib/libicuuc.so" ]]; then
    log "3a) Reusing existing ICU at ${ICU_PREFIX}"
    return
  fi

  rm -rf "$icu_src_root"
  mkdir -p "$WORKROOT"

  curl -L "$icu_url" -o "${WORKROOT}/${icu_tar}"
  tar -xzf "${WORKROOT}/${icu_tar}" -C "$WORKROOT"

  [[ -d "$icu_src_root" ]] || die "Expected ICU source dir ${icu_src_root} after extraction"
  [[ -d "$icu_build_dir" ]] || die "Expected ICU build dir ${icu_build_dir} after extraction"

  pushd "$icu_build_dir" >/dev/null
  ./configure --prefix="$ICU_PREFIX"
  make -j"$(nproc)"
  sudo make install
  popd >/dev/null

  [[ -x "${ICU_PREFIX}/bin/icuinfo" ]] || die "ICU install failed: icuinfo missing"
  [[ -f "${ICU_PREFIX}/lib/libicuuc.so" ]] || die "ICU install failed: libicuuc.so missing"
}

fetch_webkit() {
  log "4) Fetching WebKitGTK ${WK_VER}"

  local wk_tar="webkitgtk-${WK_VER}.tar.xz"
  local wk_url="https://webkitgtk.org/releases/${wk_tar}"
  local wk_src_dir="${WORKROOT}/webkitgtk-${WK_VER}"

  if [[ ! -d "$wk_src_dir" ]]; then
    curl -L "$wk_url" -o "${WORKROOT}/${wk_tar}"
    tar -xf "${WORKROOT}/${wk_tar}" -C "$WORKROOT"
  fi
}

configure_and_build_webkit() {
  local wk_src_dir="${WORKROOT}/webkitgtk-${WK_VER}"
  local wk_build_dir="${wk_src_dir}/build"

  [[ -d "$wk_src_dir" ]] || die "WebKit source directory missing: ${wk_src_dir}"

  log "5) Resetting WebKit build directory"
  rm -rf "$wk_build_dir"
  mkdir -p "$wk_build_dir"

  sudo mkdir -p "$WK_PREFIX"

  pushd "$wk_build_dir" >/dev/null

  unset CC CXX CPP CFLAGS CXXFLAGS LDFLAGS
  unset CMAKE_C_COMPILER_LAUNCHER CMAKE_CXX_COMPILER_LAUNCHER
  unset CCACHE_DIR
  export CCACHE_DISABLE=1

  export CC=/usr/bin/gcc
  export CXX=/usr/bin/g++
  export PKG_CONFIG_PATH="${ICU_PREFIX}/lib/pkgconfig${PKG_CONFIG_PATH+:$PKG_CONFIG_PATH}"

  hash -r

  log "6) Configuring WebKitGTK"
  cmake .. -GNinja \
    -DCMAKE_BUILD_TYPE=Release \
    -DCMAKE_INSTALL_PREFIX="$WK_PREFIX" \
    -DCMAKE_PREFIX_PATH="$ICU_PREFIX" \
    -DCMAKE_BUILD_RPATH="$ICU_PREFIX/lib" \
    -DCMAKE_INSTALL_RPATH="$WK_PREFIX/lib;$ICU_PREFIX/lib" \
    -DICU_ROOT="$ICU_PREFIX" \
    -DICU_INCLUDE_DIR="$ICU_PREFIX/include" \
    -DICU_UC_LIBRARY="$ICU_PREFIX/lib/libicuuc.so" \
    -DICU_I18N_LIBRARY="$ICU_PREFIX/lib/libicui18n.so" \
    -DICU_DATA_LIBRARY="$ICU_PREFIX/lib/libicudata.so" \
    -DPORT=GTK \
    -DENABLE_X11_TARGET=ON \
    -DENABLE_WAYLAND_TARGET=OFF \
    -DUSE_GTK4=OFF \
    -DUSE_SOUP2=ON \
    -DUSE_JPEGXL=OFF \
    -DENABLE_INTROSPECTION=OFF \
    -DENABLE_DOCUMENTATION=OFF \
    -DENABLE_MINIBROWSER=OFF \
    -DCMAKE_C_COMPILER=/usr/bin/gcc \
    -DCMAKE_CXX_COMPILER=/usr/bin/g++

  log "6a) Verifying compiler setup before build"

  local cmake_c_compiler
  local cmake_cxx_compiler
  local cmake_c_launcher
  local cmake_cxx_launcher
  local resolved_c
  local resolved_cxx
  local expected_c
  local expected_cxx

  cmake_c_compiler="$(sed -n 's/^CMAKE_C_COMPILER:.*=//p' CMakeCache.txt | head -n1)"
  cmake_cxx_compiler="$(sed -n 's/^CMAKE_CXX_COMPILER:.*=//p' CMakeCache.txt | head -n1)"
  cmake_c_launcher="$(sed -n 's/^CMAKE_C_COMPILER_LAUNCHER:.*=//p' CMakeCache.txt | head -n1)"
  cmake_cxx_launcher="$(sed -n 's/^CMAKE_CXX_COMPILER_LAUNCHER:.*=//p' CMakeCache.txt | head -n1)"

  resolved_c="$(readlink -f "${cmake_c_compiler:-}" 2>/dev/null || true)"
  resolved_cxx="$(readlink -f "${cmake_cxx_compiler:-}" 2>/dev/null || true)"
  expected_c="$(readlink -f /usr/bin/gcc)"
  expected_cxx="$(readlink -f /usr/bin/g++)"

  printf 'C compiler from cache: %s\n' "${cmake_c_compiler:-<unset>}"
  printf 'CXX compiler from cache: %s\n' "${cmake_cxx_compiler:-<unset>}"
  printf 'Resolved C compiler: %s\n' "${resolved_c:-<unset>}"
  printf 'Resolved CXX compiler: %s\n' "${resolved_cxx:-<unset>}"

  [[ -n "${resolved_c:-}" ]] \
    || die "C compiler missing from CMakeCache.txt"

  [[ -n "${resolved_cxx:-}" ]] \
    || die "CXX compiler missing from CMakeCache.txt"

  [[ "$resolved_c" == "$expected_c" ]] \
    || die "C compiler does not resolve to /usr/bin/gcc (got: ${resolved_c:-unset})"

  [[ "$resolved_cxx" == "$expected_cxx" ]] \
    || die "CXX compiler does not resolve to /usr/bin/g++ (got: ${resolved_cxx:-unset})"

  [[ -z "${cmake_c_launcher:-}" ]] \
    || die "C compiler launcher still configured: ${cmake_c_launcher}"

  [[ -z "${cmake_cxx_launcher:-}" ]] \
    || die "CXX compiler launcher still configured: ${cmake_cxx_launcher}"

  log "7) Building WebKitGTK"
  ninja -j"$(nproc)"

  log "8) Installing WebKitGTK -> ${WK_PREFIX}"
  sudo ninja install

  popd >/dev/null

  [[ -f "${WK_PREFIX}/lib/libwebkit2gtk-4.1.so" || -f "${WK_PREFIX}/lib/libwebkit2gtk-4.0.so" ]] \
    || die "WebKit install failed: library not found in ${WK_PREFIX}/lib"
}

fetch_emacs() {
  log "9) Fetching Emacs ${EMACS_VER}"

  local emacs_tar="emacs-${EMACS_VER}.tar.xz"
  local emacs_url="https://ftp.gnu.org/gnu/emacs/${emacs_tar}"
  local emacs_src_dir="${WORKROOT}/emacs-${EMACS_VER}"

  if [[ ! -d "$emacs_src_dir" ]]; then
    curl -L "$emacs_url" -o "${WORKROOT}/${emacs_tar}"
    tar -xf "${WORKROOT}/${emacs_tar}" -C "$WORKROOT"
  fi
}

build_emacs() {
  local emacs_src_dir="${WORKROOT}/emacs-${EMACS_VER}"

  [[ -d "$emacs_src_dir" ]] || die "Emacs source directory missing: ${emacs_src_dir}"

  log "10) Preparing Emacs install prefix at ${EMACS_PREFIX}"
  sudo mkdir -p "$EMACS_PREFIX"
  sudo chown "$(id -u):$(id -g)" "$EMACS_PREFIX"

  pushd "$emacs_src_dir" >/dev/null

  log "11) Cleaning previous Emacs build"
  make distclean >/dev/null 2>&1 || true

  export PKG_CONFIG_PATH="${WK_PREFIX}/lib/pkgconfig:${ICU_PREFIX}/lib/pkgconfig${PKG_CONFIG_PATH+:$PKG_CONFIG_PATH}"
  export CPPFLAGS="-I${WK_PREFIX}/include -I${ICU_PREFIX}/include"
  export LDFLAGS="-Wl,-rpath,${WK_PREFIX}/lib -Wl,-rpath,${ICU_PREFIX}/lib -L${WK_PREFIX}/lib -L${ICU_PREFIX}/lib"

  log "12) Configuring Emacs"
  ./configure \
    --prefix="$EMACS_PREFIX/usr/local" \
    --with-x-toolkit=gtk3 \
    --with-xwidgets \
    --with-cairo \
    --with-native-compilation \
    --with-json \
    --with-tree-sitter \
    --with-mailutils \
    --with-imagemagick \
    --with-rsvg \
    --with-jpeg \
    --with-png \
    --with-tiff \
    --with-gif \
    --with-webp \
    --with-xinput2 \
    --with-harfbuzz \
    --with-gnutls \
    --with-modules

  log "12a) Verifying Emacs configure result"
  if ! grep -q '^HAVE_XWIDGETS=yes' src/config.h 2>/dev/null; then
    grep -n 'xwidget\|webkit\|HAVE_XWIDGETS' config.log || true
    die "Emacs configure did not enable xwidgets"
  fi

  log "13) Building Emacs"
  make -j"$(nproc)"

  log "14) Installing Emacs into stow tree"
  make install

  popd >/dev/null
}

stow_emacs() {
  log "15) Stowing ${EMACS_STOW_NAME} into /usr/local"

  if [[ -d "${STOW_DIR}/${EMACS_STOW_NAME}" ]]; then
    sudo stow -D -d "$STOW_DIR" -t /usr/local "$EMACS_STOW_NAME" 2>/dev/null || true
    sudo stow -d "$STOW_DIR" -t /usr/local "$EMACS_STOW_NAME"
  else
    die "Expected stow directory missing: ${STOW_DIR}/${EMACS_STOW_NAME}"
  fi
}

run_tests() {
  log "16) Running verification tests"

  local emacs_bin="/usr/local/bin/emacs"

  [[ -x "$emacs_bin" ]] || die "Installed emacs binary not found at $emacs_bin"

  "$emacs_bin" --version | head -n 1

  ldd "$emacs_bin" | grep -E 'webkit|icu|xwidget' || true

  "$emacs_bin" -Q --batch \
    --eval "(progn
              (princ (format \"system-configuration=%s\n\" system-configuration))
              (princ (format \"feature-xwidgets=%s\n\" (featurep 'xwidget-internal)))
              (princ (format \"module-file-suffix=%s\n\" module-file-suffix))
              (princ \"ok\n\"))"

  "$emacs_bin" -Q --batch \
    --eval "(unless (featurep 'xwidget-internal) (kill-emacs 11))"

  log "17) Done"
  cat <<EOF
Installed:
  Emacs prefix:   ${EMACS_PREFIX}
  WebKit prefix:  ${WK_PREFIX}
  ICU prefix:     ${ICU_PREFIX}

Smoke test:
  /usr/local/bin/emacs -Q
EOF
}

main() {
  need_cmd sudo
  need_cmd curl
  need_cmd tar
  need_cmd cmake
  need_cmd ninja
  need_cmd gcc
  need_cmd g++
  need_cmd make
  need_cmd stow
  need_cmd readlink
  need_cmd sed
  need_cmd grep

  sudo_keepalive
  install_build_prereqs
  prepare_workspace
  build_private_icu
  fetch_webkit
  configure_and_build_webkit
  fetch_emacs
  build_emacs
  stow_emacs
  run_tests
}

main "$@"
