#!/usr/bin/env bash
set -euo pipefail

info() { printf "\n[INFO] %s\n" "$*"; }
warn() { printf "\n[WARN] %s\n" "$*" >&2; }
die()  { printf "\n[ERR ] %s\n" "$*" >&2; exit 1; }

VER="7.5.4"

ARCH="$(dpkg --print-architecture)"
case "$ARCH" in
  amd64) PS_ARCH="x64" ;;
  arm64) PS_ARCH="arm64" ;;
  *) die "Unsupported arch: $ARCH" ;;
esac

cd ~/installs

BASE="https://github.com/PowerShell/PowerShell/releases/download/v${VER}"
TARBALL="powershell-${VER}-linux-${PS_ARCH}.tar.gz"
HASHFILE="hashes.sha256"

info "Target PowerShell version: ${VER}"
info "Detected Debian arch: ${ARCH} -> ${PS_ARCH}"
info "Tarball asset: ${TARBALL}"
info "Downloading tarball + hashes..."

wget -nv --show-progress "${BASE}/${TARBALL}" -O "${TARBALL}"
wget -nv --show-progress "${BASE}/${HASHFILE}" -O "${HASHFILE}"

info "Downloaded files:"
ls -lh "${TARBALL}" "${HASHFILE}"

# Normalize hashes file (strip CR/NUL and any leading junk)
NORM="hashes.norm"
tr -d '\r\000' < "${HASHFILE}" | sed 's/^[^0-9A-Fa-f]*//' > "${NORM}"

if ! grep -qE '^[0-9A-Fa-f]{64}[[:space:]]+\*?' "${NORM}"; then
  warn "${HASHFILE} doesn't look like a SHA256 list even after normalization."
  nl -ba "${NORM}" | head -n 10 >&2
  die "Cannot verify download."
fi

EXPECTED="$(awk -v f="${TARBALL}" '$2==f || $2==("*" f) {print $1; exit}' "${NORM}")"
[[ -n "${EXPECTED}" ]] || die "Could not find ${TARBALL} in ${HASHFILE}"

ACTUAL="$(sha256sum "${TARBALL}" | awk '{print $1}')"

info "SHA256 expected: ${EXPECTED}"
info "SHA256 actual:   ${ACTUAL}"
[[ "${ACTUAL}" == "${EXPECTED}" ]] || die "SHA256 mismatch — refusing to install."

info "SHA256 OK."

info "Ensuring ICU runtime exists (Debian 13 ships libicu76)..."
sudo apt-get update -y
sudo apt-get install -y libicu76

PREFIX="/opt/microsoft/powershell/${VER}"
info "Installing into: ${PREFIX}"
sudo mkdir -p "${PREFIX}"
sudo tar -xzf "${TARBALL}" -C "${PREFIX}"

# Critical: ensure pwsh is executable (your earlier 'Permission denied' was this)
sudo chmod 0755 "${PREFIX}/pwsh"

# Convenience symlink
info "Linking /usr/local/bin/pwsh -> ${PREFIX}/pwsh"
sudo ln -sf "${PREFIX}/pwsh" /usr/local/bin/pwsh

info "Verifying..."
command -v pwsh
pwsh -NoLogo -NoProfile -Command '$PSVersionTable.PSVersion'
