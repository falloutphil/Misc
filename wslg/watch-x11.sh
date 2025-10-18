#!/usr/bin/env bash
# Watch X11 clipboard in WSLg (via xclip) by polling
# - Lists TARGETS when they change
# - Forces a pull of text (UTF8_STRING/TEXT) to trigger Wayland->X11 conversion
# - Attempts to pull images if they are ever advertised (rare in WSLg)

set -u
LOGDIR="${LOGDIR:-/tmp/wslg-cliptest}"
INTERVAL="${INTERVAL:-0.5}"   # seconds
SELECTION="${SELECTION:-clipboard}"  # set to 'primary' to test PRIMARY

require() { command -v "$1" >/dev/null 2>&1 || { echo "Missing dependency: $1"; exit 1; }; }
require xclip
require sha1sum
require date
require stat

mkdir -p "$LOGDIR"

prev_targets=""
prev_txt_hash=""

echo "[X11] Polling xclip ($SELECTION) every ${INTERVAL}s; saving to $LOGDIR"
while true; do
  targets="$(xclip -selection "$SELECTION" -t TARGETS -o 2>/dev/null | tr -d '\r')"
  targets_sorted="$(printf '%s\n' "$targets" | awk 'NF' | sort -u)"

  # Try to pull text proactively (conversion happens on demand)
  txt="$(xclip -selection "$SELECTION" -o -target UTF8_STRING 2>/dev/null || true)"
  if [ -z "$txt" ]; then
    txt="$(xclip -selection "$SELECTION" -o -target TEXT 2>/dev/null || true)"
  fi
  txt_hash="$(printf '%s' "$txt" | sha1sum 2>/dev/null | awk '{print $1}')"

  if [ "$targets_sorted" != "$prev_targets" ] || [ "$txt_hash" != "$prev_txt_hash" ]; then
    echo
    echo "$(date +%T) [X11] Clipboard check ($SELECTION)"
    if [ -n "$targets_sorted" ]; then
      echo "$targets_sorted" | sed 's/^/    - /'
    else
      echo "    (no targets)"
    fi

    if [ -n "$txt" ]; then
      out="$LOGDIR/x11-text-${SELECTION}.txt"
      printf '%s' "$txt" > "$out"
      echo "  Pulled text via X11 -> $out (len: ${#txt})"
    else
      echo "  No text available via X11 right now."
    fi

    # Try images only if advertised
    for t in image/png image/jpeg image/bmp image/tiff; do
      if printf '%s\n' "$targets_sorted" | grep -qx "$t"; then
        ext="${t##*/}"
        out="$LOGDIR/x11-clipboard-${SELECTION}.${ext}"
        if xclip -selection "$SELECTION" -t "$t" -o >"$out" 2>/dev/null; then
          echo "  Pulled $t via X11, size: $(stat -c%s "$out") bytes -> $out"
        else
          echo "  $t was advertised but could not be retrieved."
        fi
      fi
    done

    prev_targets="$targets_sorted"
    prev_txt_hash="$txt_hash"
  fi

  sleep "$INTERVAL"
done
