#!/usr/bin/env bash
# Watch Wayland clipboard in WSLg by polling (wlroots data-control not available)
# - Prints MIME types when they change
# - Tries to pull common image types and text/html/plain payloads

set -u
LOGDIR="${LOGDIR:-/tmp/wslg-cliptest}"
INTERVAL="${INTERVAL:-0.4}"   # seconds

require() { command -v "$1" >/dev/null 2>&1 || { echo "Missing dependency: $1"; exit 1; }; }
require wl-paste
require stat
require date

mkdir -p "$LOGDIR"

prev_types=""

ext_for_type() {
  case "$1" in
    image/png) echo "png" ;;
    image/jpeg) echo "jpg" ;;
    image/bmp) echo "bmp" ;;
    image/tiff) echo "tiff" ;;
    text/html) echo "html" ;;
    text/plain*|STRING) echo "txt" ;;
    *) echo "" ;;
  esac
}

echo "[WAYLAND] Polling wl-paste every ${INTERVAL}s; saving to $LOGDIR"
while true; do
  types="$(wl-paste --list-types 2>/dev/null | tr -d '\r')"
  # Normalize order to reduce noisy diffs (sort + uniq)
  types_sorted="$(printf '%s\n' "$types" | awk 'NF' | sort -u)"
  if [ "$types_sorted" != "$prev_types" ]; then
    echo
    echo "$(date +%T) [WAYLAND] Clipboard types changed"
    if [ -n "$types_sorted" ]; then
      echo "$types_sorted" | sed 's/^/    - /'
    else
      echo "    (no types)"
    fi

    # Try to pull a few useful types if offered
    for t in image/png image/jpeg image/bmp image/tiff text/html "text/plain;charset=utf-8" STRING; do
      if printf '%s\n' "$types_sorted" | grep -qx "$t"; then
        ext="$(ext_for_type "$t")"
        [ -n "$ext" ] || continue
        out="$LOGDIR/wayland-clipboard.$ext"
        if wl-paste --type "$t" >"$out" 2>/dev/null; then
          if [[ "$t" == text/* || "$t" == STRING ]]; then
            bytes=$(wc -c <"$out" | tr -d ' ')
            echo "  Pulled $t -> $out (len: $bytes)"
          else
            bytes=$(stat -c%s "$out")
            echo "  Pulled $t -> $out (size: $bytes bytes)"
          fi
        else
          echo "  $t was advertised but could not be retrieved."
        fi
      fi
    done

    prev_types="$types_sorted"
  fi
  sleep "$INTERVAL"
done
