#!/usr/bin/env bash
set -euo pipefail

# ------------------------------------------------------------------
# Emacs 31.1 (GTK3/X11 + xwidgets) on Debian 13 / Trixie.
#
# This builds Emacs against Trixie's native WebKitGTK 4.1 package. Emacs 31.1
# removed the older WebKit upper-bound that forced the 30.2 private /opt stack,
# and the Wayland/Pgtk build has been validated with xwidgets on this machine.
# ------------------------------------------------------------------

EMACS_VER="${EMACS_VER:-31.1}"
EMACS_TAR_SHA256="${EMACS_TAR_SHA256:-1da5790d9580c81932b5bf700633114468da7b3412d69faa767daebf974f4586}"
JOBS="${JOBS:-$(nproc)}"

WORKROOT="${WORKROOT:-$HOME/build-emacs-31.1-x11-native-webkit}"
STOW_DIR="${STOW_DIR:-/usr/local/stow}"
EMACS_STOW_NAME="${EMACS_STOW_NAME:-emacs-${EMACS_VER}-x11-nativewk}"
EMACS_PREFIX="${EMACS_PREFIX:-${STOW_DIR}/${EMACS_STOW_NAME}}"

CLEAN_MODE="none"

usage() {
	cat <<EOF
Usage: $0 [--clean=none|light|deep]

  none  - reuse downloaded source if present; rebuild Emacs
  light - remove extracted Emacs source and rebuild
  deep  - remove workroot and rebuild

Environment overrides:
  EMACS_VER EMACS_TAR_SHA256 JOBS WORKROOT STOW_DIR EMACS_STOW_NAME EMACS_PREFIX
EOF
}

for arg in "$@"; do
	case "$arg" in
	--clean=none | --clean=light | --clean=deep)
		CLEAN_MODE="${arg#--clean=}"
		;;
	-h | --help)
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
	log "1) Installing native Trixie build prerequisites"

	sudo apt-get update
	sudo apt-get install -y \
		build-essential \
		pkg-config \
		curl \
		ca-certificates \
		xz-utils \
		file \
		stow \
		gettext \
		texinfo \
		libgtk-3-dev \
		libwebkit2gtk-4.1-dev \
		libsoup-3.0-dev \
		libjansson-dev \
		libtree-sitter-dev \
		libgccjit-14-dev \
		libsystemd-dev \
		libdbus-1-dev \
		librsvg2-dev \
		libmailutils-dev \
		libmagickwand-7.q16-dev \
		liblcms2-dev \
		libseccomp-dev \
		libgmp-dev \
		libselinux1-dev \
		libxpm-dev \
		libncurses-dev \
		libacl1-dev \
		libgpm-dev \
		libgnutls28-dev \
		libasound2-dev \
		libasound2-plugins \
		alsa-utils \
		libsqlite3-dev \
		libxml2-dev \
		libharfbuzz-dev \
		libjpeg-dev \
		libpng-dev \
		libtiff-dev \
		libgif-dev \
		libwebp-dev \
		libotf-dev \
		libm17n-dev \
		glib-networking \
		bubblewrap \
		xdg-dbus-proxy \
		gstreamer1.0-plugins-good \
		gstreamer1.0-plugins-bad \
		gstreamer1.0-plugins-ugly \
		gstreamer1.0-libav \
		gstreamer1.0-alsa \
		imagemagick
}

prepare_workspace() {
	log "2) Preparing workspace at ${WORKROOT} (clean=${CLEAN_MODE})"

	case "$CLEAN_MODE" in
	deep)
		rm -rf "$WORKROOT"
		;;
	light)
		rm -rf "$WORKROOT/emacs-${EMACS_VER}"
		;;
	none)
		:
		;;
	esac

	mkdir -p "$WORKROOT"
}

fetch_emacs() {
	log "3) Ensuring Emacs ${EMACS_VER} source"

	local emacs_tar="emacs-${EMACS_VER}.tar.xz"
	local emacs_url="https://ftp.gnu.org/gnu/emacs/${emacs_tar}"
	local emacs_src_dir="${WORKROOT}/emacs-${EMACS_VER}"

	if [[ -d "$emacs_src_dir" ]]; then
		return
	fi

	if [[ ! -f "${WORKROOT}/${emacs_tar}" ]]; then
		curl -L "$emacs_url" -o "${WORKROOT}/${emacs_tar}"
	fi

	if [[ -n "$EMACS_TAR_SHA256" ]]; then
		(
			cd "$WORKROOT"
			printf '%s  %s\n' "$EMACS_TAR_SHA256" "$emacs_tar" | sha256sum -c -
		)
	fi

	tar -xf "${WORKROOT}/${emacs_tar}" -C "$WORKROOT"

	[[ -d "$emacs_src_dir" ]] || die "Expected Emacs source dir ${emacs_src_dir} after extraction"
}

prepare_emacs_prefix() {
	log "4) Preparing Emacs stow prefix at ${EMACS_PREFIX}"

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

	log "5) Cleaning previous Emacs build"
	make distclean >/dev/null 2>&1 || true

	log "6) Verifying native WebKitGTK discovery"
	pkg-config --modversion webkit2gtk-4.1
	pkg-config --modversion libsoup-3.0

	unset PKG_CONFIG_PATH CPPFLAGS LDFLAGS

	log "7) Configuring Emacs for GTK3/X11 with native WebKitGTK"
	./configure \
		--prefix="$EMACS_PREFIX" \
		--with-x-toolkit=gtk3 \
		--with-xwidgets \
		--with-cairo \
		--with-native-compilation \
		--with-tree-sitter \
		--with-libsystemd \
		--with-dbus \
		--with-gsettings \
		--with-mailutils \
		--with-imagemagick \
		--with-lcms2 \
		--with-selinux \
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
		--with-xml2 \
		--with-sound=alsa

	log "7a) Verifying Emacs configure result"

	grep -q '^#define HAVE_ALSA 1$' src/config.h ||
		die "Emacs configure did not enable ALSA (expected HAVE_ALSA=1 in src/config.h)"

	grep -q '^#define HAVE_XWIDGETS 1$' src/config.h ||
		{
			grep -n 'webkit2gtk\|xwidget\|HAVE_XWIDGETS\|WEBKIT_' config.log || true
			die "Emacs configure did not enable xwidgets"
		}

	grep -q '^#define HAVE_X_WINDOWS 1$' src/config.h ||
		{
			grep -n 'HAVE_X_WINDOWS\|x-toolkit\|gtk' config.log src/config.h || true
			die "Emacs configure did not enable X11"
		}

	grep -q '^#define HAVE_GTK3 1$' src/config.h ||
		{
			grep -n 'HAVE_GTK3\|gtk+-3.0' config.log src/config.h || true
			die "Emacs configure did not enable GTK3"
		}

	grep -q '^#define HAVE_DBUS 1$' src/config.h ||
		die "Emacs configure did not enable D-Bus"

	grep -q '^#define HAVE_GSETTINGS 1$' src/config.h ||
		die "Emacs configure did not enable GSettings"

	grep -q '^#define HAVE_LIBSYSTEMD 1$' src/config.h ||
		die "Emacs configure did not enable libsystemd"

	grep -q '^#define HAVE_LCMS2 1$' src/config.h ||
		die "Emacs configure did not enable LCMS2"

	grep -q '^#define HAVE_LIBSELINUX 1$' src/config.h ||
		die "Emacs configure did not enable SELinux support"

	log "8) Building Emacs (jobs=${JOBS})"
	make -j"$JOBS"

	log "9) Installing Emacs into stow tree"
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

unstow_other_emacs_packages() {
	local pkg
	local pkg_path

	shopt -s nullglob
	for pkg_path in "$STOW_DIR"/emacs-*; do
		[[ -d "$pkg_path" ]] || continue

		pkg="${pkg_path##*/}"
		[[ "$pkg" == "$EMACS_STOW_NAME" ]] && continue

		log "Unstowing existing Emacs package ${pkg} from /usr/local"
		sudo stow -D -d "$STOW_DIR" -t /usr/local "$pkg"
	done
	shopt -u nullglob
}

stow_emacs() {
	log "10) Stowing ${EMACS_STOW_NAME} into /usr/local"

	[[ -d "${STOW_DIR}/${EMACS_STOW_NAME}" ]] ||
		die "Expected stow directory missing: ${STOW_DIR}/${EMACS_STOW_NAME}"

	sudo mkdir -p "$STOW_DIR"
	unstow_other_emacs_packages
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

install_wrappers() {
	log "11) Installing native-WebKit X11 launchers"

	sudo tee /usr/local/bin/emacs-x11-nativewk >/dev/null <<'EOF'
#!/usr/bin/env bash
set -euo pipefail
export GDK_BACKEND=x11
exec /usr/local/bin/emacs "$@"
EOF
	sudo chmod 755 /usr/local/bin/emacs-x11-nativewk

	sudo tee /usr/local/bin/emacs-x11-nativewk-safe >/dev/null <<'EOF'
#!/usr/bin/env bash
set -euo pipefail
exec env \
  WEBKIT_DISABLE_COMPOSITING_MODE=1 \
  LIBGL_ALWAYS_SOFTWARE=1 \
  GSK_RENDERER=cairo \
  /usr/local/bin/emacs-x11-nativewk "$@"
EOF
	sudo chmod 755 /usr/local/bin/emacs-x11-nativewk-safe
}

run_tests() {
	log "12) Running verification tests"

	local emacs_bin="/usr/local/bin/emacs"

	[[ -x "$emacs_bin" ]] || die "Installed emacs binary not found at $emacs_bin"

	"$emacs_bin" --version | head -n 1
	ldd "$emacs_bin" | grep -E 'webkit|soup|xwidget' || true

	"$emacs_bin" -Q --batch \
		--eval "(progn
              (princ (format \"system-configuration=%s\n\" system-configuration))
              (princ (format \"feature-xwidgets=%s\n\" (featurep 'xwidget-internal)))
              (princ (format \"module-file-suffix=%s\n\" module-file-suffix))
              (princ \"ok\n\"))"

	"$emacs_bin" -Q --batch \
		--eval "(unless (featurep 'xwidget-internal) (kill-emacs 11))"

	log "13) Done"
	cat <<EOF
Installed:
  Emacs prefix: ${EMACS_PREFIX}

Native WebKitGTK:
  $(pkg-config --modversion webkit2gtk-4.1)

Launchers:
  /usr/local/bin/emacs
  /usr/local/bin/emacs-x11-nativewk
  /usr/local/bin/emacs-x11-nativewk-safe

Manual xwidget test:
  /usr/local/bin/emacs-x11-nativewk -Q
  M-x xwidget-webkit-browse-url RET https://www.bbc.com RET
EOF
}

main() {
	need_cmd sudo
	need_cmd apt-get

	sudo_keepalive
	install_build_prereqs

	need_cmd curl
	need_cmd tar
	need_cmd make
	need_cmd stow
	need_cmd grep
	need_cmd pkg-config
	need_cmd sha256sum

	prepare_workspace
	fetch_emacs
	build_emacs
	stow_emacs
	install_wrappers
	run_tests
}

main "$@"
