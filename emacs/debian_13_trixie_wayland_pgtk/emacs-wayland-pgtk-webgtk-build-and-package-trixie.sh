#!/usr/bin/env bash
set -euo pipefail

# ------------------------------------------------------------------
# Emacs 30.2 (PGTK/Wayland + xwidgets) on Debian 13 / Trixie under WSLg
#
# Builds:
#   - private ICU 75.1 under /opt/icu75
#   - private WebKitGTK 2.40.5 under /opt/wk240, GTK3 + Wayland + libsoup2
#   - Emacs 30.2 with --with-pgtk and --with-xwidgets
#   - a GNU Stow package under /usr/local/stow/emacs-30.2-pgtkwk
#
# Re-runnable:
#   --clean=none  : reuse installed ICU/WebKit if present; rebuild Emacs
#   --clean=light : drop build dirs, keep installed prefixes if present
#   --clean=deep  : wipe workroot and installed ICU/WebKit, rebuild from scratch
# ------------------------------------------------------------------

WK_VER="${WK_VER:-2.40.5}"
ICU_VER="${ICU_VER:-75_1}"                 # ICU 75.1
EMACS_VER="${EMACS_VER:-30.2}"

WORKROOT="${WORKROOT:-$HOME/build-emacs-xw-wayland}"
WK_PREFIX="${WK_PREFIX:-/opt/wk240}"
ICU_PREFIX="${ICU_PREFIX:-/opt/icu75}"
STOW_DIR="${STOW_DIR:-/usr/local/stow}"
EMACS_STOW_NAME="${EMACS_STOW_NAME:-emacs-${EMACS_VER}-pgtkwk}"
EMACS_PREFIX="${EMACS_PREFIX:-${STOW_DIR}/${EMACS_STOW_NAME}}"
DIST_DIR="${DIST_DIR:-$HOME/dist/${EMACS_STOW_NAME}-trixie-wslg}"
WEBKIT_TARGET_ID="${WEBKIT_TARGET_ID:-gtk3-wayland-soup2-icu75}"
ALLOW_REPLACE_MISMATCHED_WEBKIT="${ALLOW_REPLACE_MISMATCHED_WEBKIT:-no}"

CLEAN_MODE="none"
PACKAGE_ARTIFACTS="yes"

usage() {
  cat <<EOF
Usage: $0 [--clean=none|light|deep] [--no-package]

  none       keep installed ICU/WebKit if present; rebuild Emacs only
  light      remove build dirs, keep installed prefixes if present
  deep       wipe workroot and installed ICU/WebKit, rebuild from scratch
  no-package skip creation of portable .txz artifacts

Environment overrides:
  WK_VER ICU_VER EMACS_VER WORKROOT WK_PREFIX ICU_PREFIX STOW_DIR
  EMACS_STOW_NAME EMACS_PREFIX DIST_DIR ALLOW_REPLACE_MISMATCHED_WEBKIT
EOF
}

for arg in "$@"; do
  case "$arg" in
    --clean=none|--clean=light|--clean=deep)
      CLEAN_MODE="${arg#--clean=}"
      ;;
    --no-package)
      PACKAGE_ARTIFACTS="no"
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

webkit_pc_path() {
  if [[ -f "${WK_PREFIX}/lib/pkgconfig/webkit2gtk-4.0.pc" ]]; then
    printf '%s\n' "${WK_PREFIX}/lib/pkgconfig/webkit2gtk-4.0.pc"
    return 0
  fi
  if [[ -f "${WK_PREFIX}/lib/pkgconfig/webkit2gtk-4.1.pc" ]]; then
    printf '%s\n' "${WK_PREFIX}/lib/pkgconfig/webkit2gtk-4.1.pc"
    return 0
  fi
  return 1
}

webkit_lib_present() {
  [[ -f "${WK_PREFIX}/lib/libwebkit2gtk-4.0.so" ]] || [[ -f "${WK_PREFIX}/lib/libwebkit2gtk-4.1.so" ]]
}

icu_present() {
  [[ -x "${ICU_PREFIX}/bin/icuinfo" ]] && [[ -f "${ICU_PREFIX}/lib/libicuuc.so" ]]
}

webkit_present() {
  webkit_pc_path >/dev/null 2>&1 && webkit_lib_present
}

webkit_target_marker() {
  printf '%s\n' "${WK_PREFIX}/.emacs-webkit-target"
}

webkit_wayland_present() {
  webkit_present \
    && [[ -f "$(webkit_target_marker)" ]] \
    && grep -qx "$WEBKIT_TARGET_ID" "$(webkit_target_marker)"
}

mark_webkit_wayland() {
  printf '%s\n' "$WEBKIT_TARGET_ID" | sudo tee "$(webkit_target_marker)" >/dev/null
}

refuse_mismatched_webkit_replacement() {
  if webkit_present && ! webkit_wayland_present; then
    if [[ "$ALLOW_REPLACE_MISMATCHED_WEBKIT" == "yes" ]]; then
      log "Existing WebKitGTK at ${WK_PREFIX} is not marked as ${WEBKIT_TARGET_ID}; replacing because ALLOW_REPLACE_MISMATCHED_WEBKIT=yes"
      return
    fi

    die "Existing WebKitGTK at ${WK_PREFIX} is not marked as this recipe's GTK3/Wayland build. This avoids accidentally replacing the X11-only Trixie WebKit. Use a different WK_PREFIX, or set ALLOW_REPLACE_MISMATCHED_WEBKIT=yes if replacing it is intentional."
  fi
}

path_from_root() {
  local path="$1"
  [[ "$path" = /* ]] || die "Expected absolute path, got: $path"
  printf '%s\n' "${path#/}"
}

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
    unifdef \
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
    libxkbcommon-dev \
    libwayland-dev \
    wayland-protocols \
    libegl-dev \
    libgl-dev \
    libgles-dev \
    libglvnd-dev \
    libdrm-dev \
    libgbm-dev \
    libwpe-1.0-dev \
    libwpebackend-fdo-1.0-dev \
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
    gstreamer1.0-alsa \
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
    libasound2-dev \
    libasound2-plugins \
    alsa-utils \
    imagemagick \
    libmagickwand-7.q16-dev
}

prepare_workspace() {
  log "2) Preparing workspace at ${WORKROOT} (clean=${CLEAN_MODE})"

  case "$CLEAN_MODE" in
    deep)
      refuse_mismatched_webkit_replacement
      rm -rf "$WORKROOT"
      sudo rm -rf "$ICU_PREFIX" "$WK_PREFIX"
      ;;
    light)
      rm -rf \
        "$WORKROOT/webkitgtk-${WK_VER}/build" \
        "$WORKROOT/emacs-${EMACS_VER}"
      ;;
    none)
      :
      ;;
  esac

  mkdir -p "$WORKROOT"
}

build_private_icu() {
  log "3) Ensuring private ICU 75.1 -> ${ICU_PREFIX}"

  local icu_tar="icu4c-${ICU_VER}-src.tgz"
  local icu_url="https://github.com/unicode-org/icu/releases/download/release-75-1/${icu_tar}"
  local icu_src_root="${WORKROOT}/icu"
  local icu_build_dir="${icu_src_root}/source"

  if icu_present; then
    log "3a) Reusing existing ICU at ${ICU_PREFIX}"
    return
  fi

  rm -rf "$icu_src_root"
  mkdir -p "$WORKROOT"

  if [[ ! -f "${WORKROOT}/${icu_tar}" ]]; then
    curl -L "$icu_url" -o "${WORKROOT}/${icu_tar}"
  fi

  tar -xzf "${WORKROOT}/${icu_tar}" -C "$WORKROOT"

  [[ -d "$icu_src_root" ]] || die "Expected ICU source dir ${icu_src_root} after extraction"
  [[ -d "$icu_build_dir" ]] || die "Expected ICU build dir ${icu_build_dir} after extraction"

  pushd "$icu_build_dir" >/dev/null
  ./configure --prefix="$ICU_PREFIX" --disable-samples --disable-tests
  make -j"$(nproc)"
  sudo make install
  popd >/dev/null

  icu_present || die "ICU install failed"
}

fetch_webkit() {
  log "4) Ensuring WebKitGTK ${WK_VER} source"

  local wk_tar="webkitgtk-${WK_VER}.tar.xz"
  local wk_url="https://webkitgtk.org/releases/${wk_tar}"
  local wk_src_dir="${WORKROOT}/webkitgtk-${WK_VER}"

  if [[ -d "$wk_src_dir" ]]; then
    return
  fi

  if [[ ! -f "${WORKROOT}/${wk_tar}" ]]; then
    curl -L "$wk_url" -o "${WORKROOT}/${wk_tar}"
  fi

  tar -xf "${WORKROOT}/${wk_tar}" -C "$WORKROOT"

  [[ -d "$wk_src_dir" ]] || die "Expected WebKit source dir ${wk_src_dir} after extraction"
}

configure_and_build_webkit() {
  local wk_src_dir="${WORKROOT}/webkitgtk-${WK_VER}"
  local wk_build_dir="${wk_src_dir}/build"
  local pc_file

  if webkit_wayland_present; then
    pc_file="$(webkit_pc_path)"
    log "5) Reusing existing WebKitGTK install at ${WK_PREFIX}"
    log "5a) Found pkg-config file: ${pc_file}"
    return
  fi

  if webkit_present; then
    die "Existing WebKitGTK at ${WK_PREFIX} is not marked as this recipe's GTK3/Wayland build. This avoids accidentally reusing the X11-only Trixie WebKit. Use --clean=deep with ALLOW_REPLACE_MISMATCHED_WEBKIT=yes to replace it, or set WK_PREFIX to a different empty prefix."
  fi

  [[ -d "$wk_src_dir" ]] || die "WebKit source directory missing: ${wk_src_dir}"

  log "5) Resetting WebKit build directory"
  rm -rf "$wk_build_dir"
  mkdir -p "$wk_build_dir"

  sudo mkdir -p "$WK_PREFIX"

  pushd "$wk_build_dir" >/dev/null

  unset CC CXX CPP CFLAGS CXXFLAGS LDFLAGS PKG_CONFIG_PATH
  unset CMAKE_C_COMPILER_LAUNCHER CMAKE_CXX_COMPILER_LAUNCHER
  unset CCACHE_DIR
  export CCACHE_DISABLE=1

  export CC=/usr/bin/gcc
  export CXX=/usr/bin/g++
  export PKG_CONFIG_PATH="${ICU_PREFIX}/lib/pkgconfig"
  export CPPFLAGS="-I${ICU_PREFIX}/include"
  export LDFLAGS="-Wl,-rpath,${ICU_PREFIX}/lib -L${ICU_PREFIX}/lib"

  hash -r

  log "6) Configuring WebKitGTK for GTK3/Wayland"
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
    -DENABLE_X11_TARGET=OFF \
    -DENABLE_WAYLAND_TARGET=ON \
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

  cmake_c_compiler="$(sed -n 's/^CMAKE_C_COMPILER:.*=//p' CMakeCache.txt | head -n1)"
  cmake_cxx_compiler="$(sed -n 's/^CMAKE_CXX_COMPILER:.*=//p' CMakeCache.txt | head -n1)"
  cmake_c_launcher="$(sed -n 's/^CMAKE_C_COMPILER_LAUNCHER:.*=//p' CMakeCache.txt | head -n1)"
  cmake_cxx_launcher="$(sed -n 's/^CMAKE_CXX_COMPILER_LAUNCHER:.*=//p' CMakeCache.txt | head -n1)"

  resolved_c="$(readlink -f "${cmake_c_compiler:-}" 2>/dev/null || true)"
  resolved_cxx="$(readlink -f "${cmake_cxx_compiler:-}" 2>/dev/null || true)"

  printf 'C compiler from cache: %s\n' "${cmake_c_compiler:-<unset>}"
  printf 'CXX compiler from cache: %s\n' "${cmake_cxx_compiler:-<unset>}"
  printf 'Resolved C compiler: %s\n' "${resolved_c:-<unset>}"
  printf 'Resolved CXX compiler: %s\n' "${resolved_cxx:-<unset>}"

  [[ "$resolved_c" == "$(readlink -f /usr/bin/gcc)" ]] \
    || die "C compiler does not resolve to /usr/bin/gcc"

  [[ "$resolved_cxx" == "$(readlink -f /usr/bin/g++)" ]] \
    || die "CXX compiler does not resolve to /usr/bin/g++"

  [[ -z "${cmake_c_launcher:-}" ]] || die "C compiler launcher still configured: ${cmake_c_launcher}"
  [[ -z "${cmake_cxx_launcher:-}" ]] || die "CXX compiler launcher still configured: ${cmake_cxx_launcher}"

  log "7) Building WebKitGTK"
  ninja -j"$(nproc)"

  log "8) Installing WebKitGTK -> ${WK_PREFIX}"
  sudo ninja install
  mark_webkit_wayland

  popd >/dev/null

  webkit_wayland_present || die "WebKit install failed or target marker was not written"
}

verify_webkit_version() {
  log "9) Verifying private WebKitGTK version"

  local pc_file
  local pc_name
  local wk_built_ver

  pc_file="$(webkit_pc_path)" || die "No private WebKitGTK pkg-config file found"
  pc_name="$(basename "$pc_file" .pc)"
  wk_built_ver="$(PKG_CONFIG_PATH="${WK_PREFIX}/lib/pkgconfig:${ICU_PREFIX}/lib/pkgconfig" pkg-config --modversion "$pc_name")"

  printf '%s @ %s -> %s\n' "$pc_name" "$WK_PREFIX" "$wk_built_ver"

  case "$wk_built_ver" in
    2.40.*)
      :
      ;;
    *)
      die "Expected WebKitGTK 2.40.x, got '${wk_built_ver}'"
      ;;
  esac
}

fetch_emacs() {
  log "10) Ensuring Emacs ${EMACS_VER} source"

  local emacs_tar="emacs-${EMACS_VER}.tar.xz"
  local emacs_url="https://ftp.gnu.org/gnu/emacs/${emacs_tar}"
  local emacs_src_dir="${WORKROOT}/emacs-${EMACS_VER}"

  if [[ -d "$emacs_src_dir" ]]; then
    return
  fi

  if [[ ! -f "${WORKROOT}/${emacs_tar}" ]]; then
    curl -L "$emacs_url" -o "${WORKROOT}/${emacs_tar}"
  fi

  tar -xf "${WORKROOT}/${emacs_tar}" -C "$WORKROOT"

  [[ -d "$emacs_src_dir" ]] || die "Expected Emacs source dir ${emacs_src_dir} after extraction"
}

prepare_emacs_prefix() {
  log "11) Preparing Emacs stow prefix at ${EMACS_PREFIX}"

  case "$EMACS_PREFIX" in
    "$STOW_DIR"/*)
      sudo stow -D -d "$STOW_DIR" -t /usr/local "$EMACS_STOW_NAME" 2>/dev/null || true
      sudo rm -rf "$EMACS_PREFIX"
      ;;
    *)
      die "Refusing to replace non-Stow EMACS_PREFIX: ${EMACS_PREFIX}"
      ;;
  esac

  sudo mkdir -p "$EMACS_PREFIX"
  sudo chown "$(id -u):$(id -g)" "$EMACS_PREFIX"
}

build_emacs() {
  local emacs_src_dir="${WORKROOT}/emacs-${EMACS_VER}"

  prepare_emacs_prefix

  pushd "$emacs_src_dir" >/dev/null

  log "12) Cleaning previous Emacs build"
  make distclean >/dev/null 2>&1 || true

  local wk_pkgconfig
  wk_pkgconfig="${WK_PREFIX}/lib/pkgconfig"
  [[ -d "$wk_pkgconfig" ]] || die "Missing WebKit pkg-config dir: ${wk_pkgconfig}"

  export PKG_CONFIG_PATH="${wk_pkgconfig}:${ICU_PREFIX}/lib/pkgconfig"
  export CPPFLAGS="-I${WK_PREFIX}/include -I${ICU_PREFIX}/include"
  export LDFLAGS="-Wl,-rpath,${WK_PREFIX}/lib -Wl,-rpath,${ICU_PREFIX}/lib -L${WK_PREFIX}/lib -L${ICU_PREFIX}/lib"

  log "13) Configuring Emacs for PGTK/Wayland"
  ./configure \
    --prefix="$EMACS_PREFIX" \
    --with-pgtk \
    --with-xwidgets \
    --with-cairo \
    --with-native-compilation \
    --with-tree-sitter \
    --with-mailutils \
    --with-imagemagick \
    --with-rsvg \
    --with-jpeg \
    --with-png \
    --with-tiff \
    --with-gif \
    --with-webp \
    --with-harfbuzz \
    --with-gnutls \
    --with-modules \
    --with-sqlite3 \
    --with-xml2

  log "13a) Verifying Emacs configure result"

  grep -q '^#define HAVE_ALSA 1$' src/config.h \
    || die "Emacs configure did not enable ALSA (expected HAVE_ALSA=1 in src/config.h)"

  grep -q '^#define HAVE_XWIDGETS 1$' src/config.h \
    || {
      grep -n 'webkit2gtk\|xwidget\|HAVE_XWIDGETS\|WEBKIT_' config.log || true
      die "Emacs configure did not enable xwidgets"
    }

  grep -q '^#define HAVE_PGTK 1$' src/config.h \
    || {
      grep -n 'pgtk\|PGTK' config.log src/config.h || true
      die "Emacs configure did not enable PGTK"
    }

  log "14) Building Emacs"
  make -j"$(nproc)"

  log "15) Installing Emacs into stow tree"
  make install

  popd >/dev/null
}

remove_stale_glib_schema_cache() {
  local target="/usr/local/share/glib-2.0/schemas/gschemas.compiled"
  if [[ -e "$target" && ! -L "$target" ]]; then
    log "Removing stale non-stowed GLib schema cache at $target"
    sudo rm -f "$target"
  fi
}

stow_emacs() {
  log "16) Stowing ${EMACS_STOW_NAME} into /usr/local"

  [[ -d "${STOW_DIR}/${EMACS_STOW_NAME}" ]] \
    || die "Expected stow directory missing: ${STOW_DIR}/${EMACS_STOW_NAME}"

  sudo mkdir -p "$STOW_DIR"
  remove_stale_glib_schema_cache
  sudo rm -f \
    /usr/local/share/applications/emacsclient.desktop \
    /usr/local/share/applications/emacsclient-mail.desktop \
    /usr/local/share/applications/emacs-mail.desktop \
    /usr/local/share/applications/emacs.desktop || true

  if ! sudo stow -n -v -d "$STOW_DIR" -t /usr/local "$EMACS_STOW_NAME"; then
    die "GNU Stow reported conflicts. Remove the listed files and re-run."
  fi

  sudo stow -R -d "$STOW_DIR" -t /usr/local "$EMACS_STOW_NAME"
}

run_tests() {
  log "17) Running verification tests"

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
}

package_artifacts() {
  [[ "$PACKAGE_ARTIFACTS" == "yes" ]] || {
    log "18) Skipping portable artifact packaging"
    return
  }

  log "18) Packaging portable artifacts into ${DIST_DIR}"

  local icu_rel
  local wk_rel
  local script_dir

  icu_rel="$(path_from_root "$ICU_PREFIX")"
  wk_rel="$(path_from_root "$WK_PREFIX")"
  script_dir="$(CDPATH= cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"

  mkdir -p "$DIST_DIR"

  sudo tar -C / -cJf "$DIST_DIR/icu75.txz" "$icu_rel"
  sudo tar -C / -cJf "$DIST_DIR/wk240.txz" "$wk_rel"
  sudo tar -C "$STOW_DIR" -cJf "$DIST_DIR/${EMACS_STOW_NAME}.txz" "$EMACS_STOW_NAME"
  sudo chown "$(id -u):$(id -g)" \
    "$DIST_DIR/icu75.txz" \
    "$DIST_DIR/wk240.txz" \
    "$DIST_DIR/${EMACS_STOW_NAME}.txz"

  (
    cd "$DIST_DIR"
    sha256sum icu75.txz wk240.txz "${EMACS_STOW_NAME}.txz" > SHA256SUMS
  )

  if [[ -f "$script_dir/emacs-wayland-pgtk-webgtk-client-install-trixie.sh" ]]; then
    cp "$script_dir/emacs-wayland-pgtk-webgtk-client-install-trixie.sh" "$DIST_DIR/"
  fi
  if [[ -f "$script_dir/emacs_prompt.zsh" ]]; then
    cp "$script_dir/emacs_prompt.zsh" "$DIST_DIR/"
  fi

  log "19) Done"
  cat <<EOF
Installed:
  Emacs prefix:   ${EMACS_PREFIX}
  WebKit prefix:  ${WK_PREFIX}
  ICU prefix:     ${ICU_PREFIX}

Portable artifacts:
  ${DIST_DIR}/icu75.txz
  ${DIST_DIR}/wk240.txz
  ${DIST_DIR}/${EMACS_STOW_NAME}.txz
  ${DIST_DIR}/SHA256SUMS

WSLg launchers:
  Standard stowed binary:
    /usr/local/bin/emacs
  Install-time wrapper on client machines:
    /usr/local/bin/emacs-pgtkwk
    /usr/local/bin/emacs-pgtkwk-safe

Smoke test after installing the client wrapper:
  /usr/local/bin/emacs-pgtkwk -Q
EOF
}

main() {
  need_cmd sudo
  need_cmd apt-get

  sudo_keepalive
  install_build_prereqs

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
  need_cmd sha256sum

  prepare_workspace
  build_private_icu
  fetch_webkit
  configure_and_build_webkit
  verify_webkit_version
  fetch_emacs
  build_emacs
  stow_emacs
  run_tests
  package_artifacts
}

main "$@"
