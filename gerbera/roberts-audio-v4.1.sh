#!/bin/sh
# =============================================================================
# Roberts Revival iStream 3 - fast adaptive FLAC / DSF compatibility transcoder
# Version 4.1 - 2026-08-17
#
# PURPOSE
# -------
# Gerbera calls this wrapper as:
#
#     roberts-audio.sh flac INPUT OUTPUT_FIFO
#     roberts-audio.sh dsf  INPUT OUTPUT_FIFO
#
# The NAS originals are NEVER modified.  The music bind mount is read-only.
# This wrapper either streams a safe FLAC byte-for-byte or asks FFmpeg to create
# a live compatibility FLAC for the Roberts.
#
# THE IMPORTANT V4 DESIGN CHANGE: INTELLIGENCE MUST NOT MAKE PLAYBACK FEEL SLOW
# -----------------------------------------------------------------------------
# V3 proved that sustained-load metrics are much better than a single FLAC-frame
# peak, but its first-play path could scan every packet in an entire file.  That
# is unacceptable for a live renderer, especially for multi-hour mixes.
#
# V4 therefore has a strict fast-path architecture:
#
#   1. CACHE FIRST
#      A previous decision for an unchanged file is used before ffprobe is even
#      started.  Cache entries live in /var/run/gerbera/roberts-cache-v4, which
#      is inside the Gerbera host bind mount and therefore survives container
#      recreation.  The cache key is derived from the full path; an entry is
#      accepted only when its policy ID and file stat signature still match.
#      Writes use temp-file + mv so readers never see a half-written decision.
#
#   2. CONSTANT-TIME METADATA / MEAN BITRATE
#      On a cache miss, one ffprobe metadata read obtains sample rate, channels,
#      source precision, duration, size and (when present) container bitrate.
#      Mean bitrate is obtained from metadata or size/duration.  No audio decode
#      and no whole-file packet walk is needed.
#
#   3. OBVIOUS FILES SHORT-CIRCUIT
#      Supported stereo <=48-kHz FLAC with mean <=1.30 Mbit/s passes immediately.
#      A supported file with mean >=1.50 Mbit/s is known to need intervention.
#      Files outside the Roberts envelope (>48 kHz, >24 bit or multichannel) also
#      go directly to compatibility selection.  Only the grey middle band needs
#      more analysis.
#
#   4. GREY-BAND ANALYSIS IS SAMPLED, NOT WHOLE-FILE
#      ffprobe -read_intervals seeks to nine short five-second windows spread
#      across the track.  At most about 45 seconds of compressed packets are
#      inspected, irrespective of whether the source is 4 minutes or 4 hours.
#      ffprobe is native C code and reads packet headers/sizes without decoding.
#
#   5. AWK DOES THE SMALL NUMERIC JOB
#      Packet sizes are reduced to one-second buckets in one AWK pass.  P95/P99
#      use 10-kbit/s histogram bins rather than an expensive full sort.  Shell is
#      used only for orchestration; no Python, Perl, jq or giant Bash arrays sit
#      on the track-start path.
#
#   6. ONLY DIFFICULT FILES ARE CALIBRATED
#      For a FLAC that genuinely needs conversion, V4 encodes a 12-second sample
#      around the hardest location found by the sampled probe.  It starts near a
#      calculated effective-bit estimate, then tries adjacent bit depths until it
#      finds the highest candidate meeting the sustained-load target.  There is a
#      hard five-candidate limit.  This work happens only on the first play of an
#      edge/problem file; the result is then cached.
#
#   7. FFmpeg THREADING
#      We deliberately do not pin a manual thread count.  FFmpeg's codec/filter
#      defaults are allowed to choose their supported threading automatically.
#      Our NAS benchmark already encoded a 269.8-second DSF track at FLAC level 8
#      in ~13 seconds (>20x real time), so forcing thread knobs is more likely to
#      add complexity/overhead than improve track-start latency on this workload.
#
# WHY WE MEASURE SUSTAINED LOAD, NOT ONE TALL SPIKE
# -------------------------------------------------
# The Roberts behaves as if ~1.5 Mbit/s is a sustained-load guide, not an exact
# per-frame brick wall.  A brief 1.6-1.8 Mbit/s burst can be absorbed by buffering;
# living near/above the limit for seconds or minutes is what causes Loading... .
#
# The sampled analysis records:
#
#   mean/average  - long-term pressure.  If the mean itself is too high, a finite
#                   buffer eventually loses.
#   1 s P95/P99   - the normal high end without letting one freak packet dominate.
#   max 1 s       - useful diagnostic, but intentionally not a lone veto.
#   max 3 s avg   - short sustained stress, much more meaningful than frame peak.
#   time >1.5M    - distinguishes a rare excursion from a track living too hot.
#   longest run   - consecutive one-second buckets above 1.5M.
#   buffer debt   - comparative model of bytes accumulated above a hypothetical
#                   constant 1.5-Mbit/s consumer.  It is NOT claimed to equal the
#                   radio's actual RAM; sampled gaps reset this model.
#
# MEASURED FINDINGS BEHIND THE POLICY
# -----------------------------------
# A) SACD/DSF: Alice In Chains - Man In The Box, 5.1 DSD64-family source
#
#    24-bit / 44.1-kHz stereo FLAC:
#        ~1518.6 kbit/s mean, ~1719.1 kbit/s packet peak -> too hot.
#
#    Genuine 20-effective-bit / 44.1-kHz stereo, stored as nominal 24-bit FLAC:
#        FLAC level 8 ~1292.9 kbit/s mean / ~1485.6 peak -> played cleanly.
#
#    19 effective bits gave more margin but needlessly discarded another bit.
#    DSF is therefore fixed at 20 effective bits / 44.1 kHz.  44.1 kHz is the
#    natural family for DSD64 (2.8224 MHz = 44.1 kHz x 64).
#
# B) Difficult 24/48 FLAC: Kora Burning Man mix (YouTube-derived)
#
#    Original whole-track measurements during development:
#        mean 1572.9, P95 1777.1, P99 1869.8, max1 1962.5,
#        max3 1870.7 kbit/s, 77.21% of seconds >1.5M,
#        longest run 525 seconds, comparative debt ~104023 KiB.
#
#    This was NOT a brief-peak problem.  On its worst 525-second region, while
#    KEEPING the original 48-kHz sampling rate:
#
#        22 effective bits: mean 1548.7, P99 1697.7, max3 1637.0,
#                           60.4% >1.5M, longest run 19 s -> too hot
#        21 effective bits: mean 1453.4, P99 1596.4, max3 1538.7,
#                           35.2% >1.5M, longest run 1 s  -> marginal
#        20 effective bits: mean 1358.1, P99 1498.5, max3 1441.7,
#                            1.1% >1.5M, longest run 1 s  -> excellent balance
#
#    The lesson is NOT "everything should be 20-bit".  The DSF and Kora tests
#    happened to converge on 20 effective bits for different reasons.  A calmer
#    24-bit FLAC may pass untouched, or need only 23/22/21 bits.  V4 chooses per
#    file and caches that decision.  Crucially, 48/96/192-family PCM targets
#    48 kHz; 44.1/88.2/176.4-family PCM targets 44.1 kHz.
#
#    V4.1 live-calibration refinement:
#    A deliberately-hot 12-second Kora sample made 20-bit look harsher than the
#    long 525-second validation region: 20-bit measured ~1447 mean / ~1481 max3
#    with three isolated >1.5M seconds out of 12.  V4's first thresholds therefore
#    rejected 20 and chose 19.  That was safe but too conservative.  V4.1 treats
#    this short calibration for what it is -- a worst-case hotspot -- and gives
#    more weight to 3-second load, run length and debt.  The thresholds still
#    reject the measured 21-bit hotspot but accept the measured 20-bit hotspot.
#
# C) FLAC compression level
#
#    Same DSF test at 20-effective-bit/44.1 kHz:
#        level 5 : 13 s encode, ~1296.2 mean / 1488.7 peak kbit/s
#        level 8 : 13 s encode, ~1292.9 mean / 1485.6 peak kbit/s
#        level 12: 17 s encode, ~1290.2 mean / 1484.0 peak kbit/s
#
#    Level 8 therefore gives essentially level-12 size at level-5 speed here.
#    FLAC remains lossless at every compression level; level 8 is the live-stream
#    choice for responsiveness and CPU headroom.
#
# CACHE SAFETY
# ------------
# Cache decisions are advisory only; they never alter source media.  A cache hit
# requires the same policy ID and a stat signature derived from size, inode,
# numeric mtime/ctime and their high-resolution textual timestamps when supported.  Changing this script's policy ID invalidates every old decision.
# Cache files are mode 0600 under a mode-0700 directory and are atomically replaced.
# If stat/hash/cache operations fail, caching is simply disabled for that request.
# =============================================================================

set -eu

MODE=${1:-}

# -----------------------------------------------------------------------------
# Lightweight diagnostics.  This mode takes no media arguments and is useful
# immediately after installation.  IMPORTANT: run it as the same gerbera user
# that Gerbera uses for transcoding, not as docker-exec root:
#
#     docker exec gerbera sh -c \
#       'su-exec gerbera /var/run/gerbera/roberts-audio.sh check'
#
# V4.1 deliberately refuses to CREATE a missing cache directory when this check
# is accidentally run as UID 0.  V4's root-run check could create a root-owned
# directory which later blocked the real UID-1042 transcoder from writing cache
# entries.  Normal media execution still creates the directory as gerbera.
# -----------------------------------------------------------------------------
if [ "$MODE" = "check" ]; then
    echo "Roberts transcoder v4.1 tool check"
    for tool in ffmpeg ffprobe awk stat cat mv mkdir cksum head id; do
        if command -v "$tool" >/dev/null 2>&1; then
            printf '%-12s %s\n' "$tool" "$(command -v "$tool")"
        else
            printf '%-12s MISSING\n' "$tool"
        fi
    done
    if command -v sha256sum >/dev/null 2>&1; then
        printf '%-12s %s\n' sha256sum "$(command -v sha256sum)"
    else
        printf '%-12s %s\n' sha256sum "not present; cksum fallback will be used"
    fi
    ffmpeg -version 2>/dev/null | head -n 1 || true
    ffprobe -version 2>/dev/null | head -n 1 || true
    printf 'stat-signature %s\n' "$(stat -c '%s|%Y|%Z|%i|%y|%z' "$0" 2>/dev/null || echo FAILED)"

    CHECK_CACHE=/var/run/gerbera/roberts-cache-v4
    CHECK_UID=$(id -u 2>/dev/null || echo unknown)

    if [ ! -d "$CHECK_CACHE" ]; then
        if [ "$CHECK_UID" = "0" ]; then
            echo "cache-dir    MISSING: $CHECK_CACHE"
            echo "cache-note   not creating it as root; rerun check via su-exec gerbera"
            exit 0
        fi
        mkdir -p "$CHECK_CACHE" 2>/dev/null || true
    fi

    if (umask 077; : > "$CHECK_CACHE/.write-test") 2>/dev/null; then
        rm -f "$CHECK_CACHE/.write-test" 2>/dev/null || true
        echo "cache-dir    writable: $CHECK_CACHE (uid=$CHECK_UID)"
    else
        echo "cache-dir    NOT WRITABLE: $CHECK_CACHE (uid=$CHECK_UID)"
    fi
    exit 0
fi

IN=${2:?input required}
OUT=${3:?output required}

# -------------------------- policy / performance knobs -----------------------
LIMIT_BPS=1500000
HIST_BIN_BPS=10000
FLAC_LEVEL=8

# Constant-time mean-rate fast paths for supported native FLAC.
FAST_PASS_AVG_BPS=1300000
FAST_TRANSCODE_AVG_BPS=1500000

# Grey-band sampled native acceptance.  These are combined engineering criteria,
# not claimed Roberts hardware specifications.
NATIVE_AVG_MAX=1475000
NATIVE_P99_MAX=1850000
NATIVE_MAX3_MAX=1700000
NATIVE_OVER_PERMILLE_MAX=250      # <=25% of sampled one-second buckets >1.5M
NATIVE_LONGEST_MAX=4              # sampled consecutive seconds >1.5M
NATIVE_DEBT_KIB_MAX=512           # comparative sampled debt only

# Once transcoding is necessary, aim for roughly 1.3-1.4 Mbit/s in real material,
# but remember that calibration is deliberately taken from a WORST sampled hotspot.
# A 12-second hotspot must therefore not be judged as if it were a whole-track
# random sample.  Brief one-second excursions above 1.5M are acceptable when the
# 3-second load, longest run and buffer-debt model remain well behaved.
#
# Live Kora calibration taught us why this distinction matters:
#   21-bit/48k hotspot: avg 1539, p99~1650, max3 1575, 50% >1.5M, run 2s -> reject
#   20-bit/48k hotspot: avg 1447, p99~1560, max3 1481, 25% >1.5M, run 1s, debt 7K
#                      -> accept; the much longer 525-second torture region had
#                         avg 1358, p99 1498.5, max3 1441.7 and only 1.1% >1.5M.
#   19-bit/48k hotspot: avg 1354, max3 1388 -> safe but unnecessarily conservative.
#
# These thresholds are intentionally shaped so the measured 20-bit case passes
# while 21-bit still fails on several independent sustained-load signals.  They
# are engineering policy, not claimed hardware specifications.
CAND_TARGET_AVG=1350000
CAND_AVG_MAX=1475000
CAND_P99_MAX=1650000
CAND_MAX3_MAX=1525000
CAND_OVER_PERMILLE_MAX=350        # <=35% in the deliberately-hot 12s calibration
CAND_LONGEST_MAX=2
CAND_DEBT_KIB_MAX=128

# Nine 5-second packet windows = ~45 seconds inspected regardless of track length.
SAMPLE_WINDOWS=9
SAMPLE_SECONDS=5

# Difficult-file calibration: short decode/encode near sampled worst 3-second point.
CAL_SECONDS=12
CAL_LEAD_SECONDS=4
MAX_CANDIDATE_ATTEMPTS=5

# Change this whenever decision logic/thresholds change; old cache is then ignored.
CACHE_POLICY=roberts-v4.1-20260817b
CACHE_DIR=/var/run/gerbera/roberts-cache-v4

log() {
    printf 'roberts-audio: %s\n' "$*" >&2
}

is_uint() {
    case "$1" in
        ''|*[!0-9]*) return 1 ;;
        *) return 0 ;;
    esac
}

# ------------------------------- cache layer ---------------------------------
CACHE_ENABLED=0
CACHE_FILE=
FILE_SIG=

hash_text() {
    if command -v sha256sum >/dev/null 2>&1; then
        sha256sum | awk '{print $1; exit}'
    else
        cksum | awk '{print $1 "-" $2; exit}'
    fi
}

init_cache() {
    CACHE_ENABLED=0

    # Include inode, size, mtime/ctime and high-resolution timestamp forms.  Media
    # files are normally immutable; this is a strong invalidation signature without
    # reading file payload.  BusyBox/GNU stat both support these format fields.
    STAT_RAW=$(stat -c '%s|%Y|%Z|%i|%y|%z' "$IN" 2>/dev/null || true)
    [ -n "$STAT_RAW" ] || return 0

    FILE_SIG=$(printf '%s' "$STAT_RAW" | hash_text 2>/dev/null || true)
    PATH_KEY=$(printf '%s' "$IN" | hash_text 2>/dev/null || true)
    [ -n "$FILE_SIG" ] && [ -n "$PATH_KEY" ] || return 0

    if ! mkdir -p "$CACHE_DIR" 2>/dev/null; then
        log "CACHE disabled: cannot create/access $CACHE_DIR"
        return 0
    fi
    chmod 700 "$CACHE_DIR" 2>/dev/null || true

    # Do not merely assume that mkdir -p success means the current Gerbera user
    # can write here: an existing root-owned directory also makes mkdir -p return
    # success.  Probe actual write permission before enabling cache operations.
    CACHE_PROBE="$CACHE_DIR/.write-probe.$$"
    if ! (umask 077; : > "$CACHE_PROBE") 2>/dev/null; then
        log "CACHE disabled: $CACHE_DIR is not writable by the transcoder user"
        return 0
    fi
    rm -f "$CACHE_PROBE" 2>/dev/null || true

    CACHE_FILE="$CACHE_DIR/$PATH_KEY.cache"
    CACHE_ENABLED=1
}

run_cached_action() {
    action=$1
    bits=${2:-0}
    rate=${3:-0}

    case "$action" in
        pass)
            log "CACHE HIT -> native FLAC"
            exec cat "$IN" > "$OUT"
            ;;
        effective)
            is_uint "$bits" && is_uint "$rate" || return 1
            log "CACHE HIT -> FLAC ${bits}-bit-effective/${rate}Hz/stereo, level ${FLAC_LEVEL}"
            stream_effective "$bits" "$rate" "cached decision"
            ;;
        reencode24)
            log "CACHE HIT -> lossless 24-bit FLAC re-encode, level ${FLAC_LEVEL}"
            stream_reencode24 "cached decision"
            ;;
        compat24)
            is_uint "$rate" || return 1
            log "CACHE HIT -> 24-bit/${rate}Hz/stereo compatibility FLAC, level ${FLAC_LEVEL}"
            stream_compat24 "$rate" "cached decision"
            ;;
        s16)
            is_uint "$rate" || return 1
            log "CACHE HIT -> 16-bit/${rate}Hz/stereo fallback, level ${FLAC_LEVEL}"
            stream_16bit "$rate" "cached decision"
            ;;
        *) return 1 ;;
    esac
}

try_cache() {
    [ "$CACHE_ENABLED" -eq 1 ] && [ -r "$CACHE_FILE" ] || return 1

    C_POLICY= C_SIG= C_ACTION= C_BITS=0 C_RATE=0
    while IFS='=' read -r ck cv; do
        case "$ck" in
            policy) C_POLICY=$cv ;;
            sig)    C_SIG=$cv ;;
            action) C_ACTION=$cv ;;
            bits)   C_BITS=$cv ;;
            rate)   C_RATE=$cv ;;
        esac
    done < "$CACHE_FILE"

    [ "$C_POLICY" = "$CACHE_POLICY" ] || return 1
    [ "$C_SIG" = "$FILE_SIG" ] || return 1

    run_cached_action "$C_ACTION" "$C_BITS" "$C_RATE"
}

write_cache() {
    [ "$CACHE_ENABLED" -eq 1 ] || return 0

    wc_action=$1
    wc_bits=${2:-0}
    wc_rate=${3:-0}
    wc_tmp="$CACHE_FILE.tmp.$$"

    umask 077
    if {
        printf 'policy=%s\n' "$CACHE_POLICY"
        printf 'sig=%s\n' "$FILE_SIG"
        printf 'action=%s\n' "$wc_action"
        printf 'bits=%s\n' "$wc_bits"
        printf 'rate=%s\n' "$wc_rate"
    } > "$wc_tmp" 2>/dev/null; then
        chmod 600 "$wc_tmp" 2>/dev/null || true
        mv -f "$wc_tmp" "$CACHE_FILE" 2>/dev/null || rm -f "$wc_tmp"
    else
        rm -f "$wc_tmp" 2>/dev/null || true
    fi
}

# ------------------------------- quick probe ---------------------------------
probe_metadata() {
    ffprobe -v error -select_streams a:0 \
        -show_entries stream=sample_rate,channels,bits_per_raw_sample:format=duration,size,bit_rate \
        -of default=noprint_wrappers=1 "$IN" 2>/dev/null |
    awk -F= '
        $1=="sample_rate"         { sr=$2 }
        $1=="channels"            { ch=$2 }
        $1=="bits_per_raw_sample" { bits=$2 }
        $1=="duration"            { dur=$2 }
        $1=="size"                { size=$2 }
        $1=="bit_rate"            { br=$2 }
        END {
            if (sr=="") sr=0
            if (ch=="") ch=0
            if (bits=="" || bits=="N/A") bits=0
            if (dur=="" || dur=="N/A") dur=0
            if (size=="" || size=="N/A") size=0
            if (br=="" || br=="N/A") br=0
            printf "P_SR=%s P_CH=%s P_BITS=%s P_DUR=%s P_SIZE=%s P_BR=%s\n", sr,ch,bits,dur,size,br
        }
    '
}

load_metadata() {
    # Values above are numeric ffprobe fields only.
    eval "$(probe_metadata)"

    is_uint "$P_SR" || P_SR=0
    is_uint "$P_CH" || P_CH=0
    is_uint "$P_BITS" || P_BITS=0
    is_uint "$P_SIZE" || P_SIZE=0
    is_uint "$P_BR" || P_BR=0

    # Some FLACs omit format bit_rate; size/duration is just as useful and does
    # not read the whole media file because size is metadata/stat information.
    AVG_BPS=$P_BR
    if [ "$AVG_BPS" -le 0 ]; then
        AVG_BPS=$(awk -v s="$P_SIZE" -v d="$P_DUR" 'BEGIN {
            if ((s+0)>0 && (d+0)>0) printf "%.0f\n", (s*8)/d; else print 0
        }')
    fi
    is_uint "$AVG_BPS" || AVG_BPS=0

    DUR_INT=$(awk -v d="$P_DUR" 'BEGIN { if ((d+0)>0) printf "%d\n", d; else print 0 }')
}

choose_rate_family() {
    cr_sr=$1
    if [ "$cr_sr" -le 48000 ]; then
        printf '%s\n' "$cr_sr"
        return
    fi

    case "$cr_sr" in
        88200|176400|352800|705600) printf '44100\n' ;;
        96000|192000|384000|768000) printf '48000\n' ;;
        *)
            if [ $((cr_sr % 44100)) -eq 0 ]; then printf '44100\n'; else printf '48000\n'; fi
            ;;
    esac
}

# -------------------------- sampled packet analysis ---------------------------
build_intervals() {
    bi_d=$1
    awk -v d="$bi_d" -v n="$SAMPLE_WINDOWS" -v w="$SAMPLE_SECONDS" 'BEGIN {
        if (d <= 0) exit 1
        if (d <= w*n) {
            printf "0%%+%.3f\n", d
            exit
        }
        usable=d-w
        first=1
        last=-1
        for (i=0; i<n; i++) {
            s=int((usable*i)/(n-1))
            if (s==last) continue
            if (!first) printf ","
            printf "%d%%+%d", s,w
            first=0
            last=s
        }
        printf "\n"
    }'
}

# Read ffprobe packet CSV on stdin and emit numeric shell assignments.  Missing
# seconds between seek windows are gaps, not zero-bitrate audio; run/debt/3-second
# calculations therefore reset across gaps.
packet_metrics_awk() {
    awk -F'|' -v LIMIT="$LIMIT_BPS" -v BIN="$HIST_BIN_BPS" '
        {
            t=0; d=0; s=0
            for (f=1; f<=NF; f++) {
                split($f,kv,"=")
                if (kv[1]=="pts_time") t=kv[2]+0
                else if (kv[1]=="duration_time") d=kv[2]+0
                else if (kv[1]=="size") s=kv[2]+0
            }
            if (d<=0 || s<=0) next
            sec=int(t)
            bytes[sec]+=s
            present[sec]=1
            totalbytes+=s
            totaldur+=d
            if (!seen) { minsec=sec; maxsec=sec; seen=1 }
            if (sec<minsec) minsec=sec
            if (sec>maxsec) maxsec=sec
        }
        END {
            if (!seen || totaldur<=0) { print "M_OK=0"; exit }

            n=0; over=0; run=0; maxrun=0; debt=0; maxdebt=0
            max1=0; maxbin=0; prev=-999999

            for (i=minsec; i<=maxsec; i++) {
                if (!present[i]) continue
                rate=bytes[i]*8
                r[i]=rate
                n++

                bin=int(rate/BIN)
                hist[bin]++
                if (bin>maxbin) maxbin=bin
                if (rate>max1) { max1=rate; worst1=i }

                if (i != prev+1) { run=0; debt=0 }

                if (rate>LIMIT) {
                    over++; run++
                    if (run>maxrun) maxrun=run
                } else run=0

                debt += (rate-LIMIT)/8
                if (debt<0) debt=0
                if (debt>maxdebt) maxdebt=debt
                prev=i
            }

            max3=0; worst3=worst1
            for (i=minsec; i<=maxsec; i++) {
                if (present[i] && present[i+1] && present[i+2]) {
                    v=(r[i]+r[i+1]+r[i+2])/3
                    if (v>max3) { max3=v; worst3=i }
                }
            }
            if (max3==0) max3=max1

            t95=int(n*0.95+0.999999)
            t99=int(n*0.99+0.999999)
            cum=0; p95=0; p99=0
            for (b=0; b<=maxbin; b++) {
                cum += hist[b]+0
                if (p95==0 && cum>=t95) p95=(b+1)*BIN
                if (p99==0 && cum>=t99) p99=(b+1)*BIN
            }

            avg=(totalbytes*8)/totaldur
            permille=int((over*1000/n)+0.5)
            printf "M_OK=1 M_AVG=%.0f M_P95=%.0f M_P99=%.0f ",avg,p95,p99
            printf "M_MAX1=%.0f M_MAX3=%.0f M_OVER=%d M_TOTAL=%d ",max1,max3,over,n
            printf "M_OVER_PERMILLE=%d M_LONGEST=%d M_DEBT_KIB=%.0f M_WORST3=%d\n", \
                   permille,maxrun,maxdebt/1024,worst3
        }
    '
}

load_sample_metrics() {
    lsm_file=$1
    lsm_intervals=$2
    eval "$(ffprobe -v error -select_streams a:0 -read_intervals "$lsm_intervals" \
        -show_packets -show_entries packet=pts_time,duration_time,size -of compact=p=0:nk=0 \
        "$lsm_file" 2>/dev/null | packet_metrics_awk)"
}

load_full_metrics() {
    lfm_file=$1
    eval "$(ffprobe -v error -select_streams a:0 -show_packets \
        -show_entries packet=pts_time,duration_time,size -of compact=p=0:nk=0 \
        "$lfm_file" 2>/dev/null | packet_metrics_awk)"
}

log_metrics() {
    lm_label=$1
    if [ "${M_OK:-0}" -ne 1 ]; then
        log "$lm_label metrics unavailable"
        return
    fi
    log "$lm_label avg=$((M_AVG/1000))kbps p95~$((M_P95/1000)) p99~$((M_P99/1000)) max1=$((M_MAX1/1000)) max3=$((M_MAX3/1000))kbps over1.5M=${M_OVER}/${M_TOTAL} (${M_OVER_PERMILLE}permille) longest=${M_LONGEST}s debt=${M_DEBT_KIB}KiB worst3=${M_WORST3}s"
}

native_sample_safe() {
    [ "$AVG_BPS" -le "$NATIVE_AVG_MAX" ] &&
    [ "$M_P99" -le "$NATIVE_P99_MAX" ] &&
    [ "$M_MAX3" -le "$NATIVE_MAX3_MAX" ] &&
    [ "$M_OVER_PERMILLE" -le "$NATIVE_OVER_PERMILLE_MAX" ] &&
    [ "$M_LONGEST" -le "$NATIVE_LONGEST_MAX" ] &&
    [ "$M_DEBT_KIB" -le "$NATIVE_DEBT_KIB_MAX" ]
}

candidate_safe() {
    [ "$M_AVG" -le "$CAND_AVG_MAX" ] &&
    [ "$M_P99" -le "$CAND_P99_MAX" ] &&
    [ "$M_MAX3" -le "$CAND_MAX3_MAX" ] &&
    [ "$M_OVER_PERMILLE" -le "$CAND_OVER_PERMILLE_MAX" ] &&
    [ "$M_LONGEST" -le "$CAND_LONGEST_MAX" ] &&
    [ "$M_DEBT_KIB" -le "$CAND_DEBT_KIB_MAX" ]
}

# ---------------------------- encoding primitives -----------------------------
scale_for_bits() {
    case "$1" in
        23) printf '4194304\n' ;;
        22) printf '2097152\n' ;;
        21) printf '1048576\n' ;;
        20) printf '524288\n' ;;
        19) printf '262144\n' ;;
        18) printf '131072\n' ;;
        17) printf '65536\n' ;;
        *) return 1 ;;
    esac
}

stream_effective() {
    se_bits=$1
    se_rate=$2
    shift 2
    se_scale=$(scale_for_bits "$se_bits")
    log "TRANSCODE -> FLAC ${se_bits}-bit-effective/${se_rate}Hz/stereo (24-bit container), level ${FLAC_LEVEL}: $*"
    exec ffmpeg -nostdin -hide_banner -loglevel error \
        -i "$IN" -map 0:a:0 -vn \
        -af "aresample=${se_rate}:ochl=stereo,aeval=round(val(ch)*${se_scale})/${se_scale}:c=same" \
        -sample_fmt s32 -c:a flac -compression_level "$FLAC_LEVEL" \
        -f flac -y "$OUT"
}

stream_reencode24() {
    shift 0
    log "TRANSCODE -> lossless PCM-preserving 24-bit FLAC re-encode, level ${FLAC_LEVEL}: $*"
    exec ffmpeg -nostdin -hide_banner -loglevel error \
        -i "$IN" -map 0:a:0 -vn \
        -sample_fmt s32 -c:a flac -compression_level "$FLAC_LEVEL" \
        -f flac -y "$OUT"
}

stream_compat24() {
    sc_rate=$1
    shift
    log "TRANSCODE -> FLAC 24-bit/${sc_rate}Hz/stereo compatibility stream, level ${FLAC_LEVEL}: $*"
    exec ffmpeg -nostdin -hide_banner -loglevel error \
        -i "$IN" -map 0:a:0 -vn \
        -af "aresample=${sc_rate}:ochl=stereo" \
        -sample_fmt s32 -c:a flac -compression_level "$FLAC_LEVEL" \
        -f flac -y "$OUT"
}

stream_16bit() {
    s16_rate=$1
    shift
    log "FALLBACK -> FLAC 16-bit/${s16_rate}Hz/stereo with HP triangular dither, level ${FLAC_LEVEL}: $*"
    exec ffmpeg -nostdin -hide_banner -loglevel error \
        -i "$IN" -map 0:a:0 -vn \
        -af "aresample=${s16_rate}:ochl=stereo:osf=s16:dither_method=triangular_hp" \
        -sample_fmt s16 -c:a flac -compression_level "$FLAC_LEVEL" \
        -f flac -y "$OUT"
}

pass_flac() {
    log "PASS native FLAC: $*"
    write_cache pass 0 0
    exec cat "$IN" > "$OUT"
}

encode_candidate() {
    ec_bits=$1
    ec_rate=$2
    ec_start=$3
    ec_out=$4

    if [ "$ec_bits" -eq 24 ]; then
        if [ "$P_CH" -eq 2 ] && [ "$P_SR" -eq "$ec_rate" ]; then
            ffmpeg -nostdin -hide_banner -loglevel error -y \
                -ss "$ec_start" -t "$CAL_SECONDS" -i "$IN" -map 0:a:0 -vn \
                -sample_fmt s32 -c:a flac -compression_level "$FLAC_LEVEL" \
                -f flac "$ec_out"
        else
            ffmpeg -nostdin -hide_banner -loglevel error -y \
                -ss "$ec_start" -t "$CAL_SECONDS" -i "$IN" -map 0:a:0 -vn \
                -af "aresample=${ec_rate}:ochl=stereo" \
                -sample_fmt s32 -c:a flac -compression_level "$FLAC_LEVEL" \
                -f flac "$ec_out"
        fi
    else
        ec_scale=$(scale_for_bits "$ec_bits")
        ffmpeg -nostdin -hide_banner -loglevel error -y \
            -ss "$ec_start" -t "$CAL_SECONDS" -i "$IN" -map 0:a:0 -vn \
            -af "aresample=${ec_rate}:ochl=stereo,aeval=round(val(ch)*${ec_scale})/${ec_scale}:c=same" \
            -sample_fmt s32 -c:a flac -compression_level "$FLAC_LEVEL" \
            -f flac "$ec_out"
    fi
}

# Estimate a sensible first candidate.  This is deliberately only a starting
# point; the short encoded calibration is the authority.  Account approximately
# for sample-rate reduction and multichannel->stereo before mapping mean bitrate
# to an effective precision around the 1.35-Mbit/s target.
estimate_start_bits() {
    es_bits=$1
    es_avg=$2
    es_sr=$3
    es_ch=$4
    es_target_rate=$5

    awk -v b="$es_bits" -v a="$es_avg" -v sr="$es_sr" -v ch="$es_ch" \
        -v tr="$es_target_rate" -v target="$CAND_TARGET_AVG" 'BEGIN {
        if (b>24) b=24
        if (b<17) b=17
        if (a<=0 || sr<=0 || ch<=0) { print (b<20 ? b : 20); exit }
        norm=a*(tr/sr)*(2/ch)
        if (norm<=target) g=b
        else g=int((b*target/norm)+0.5)
        if (g>b) g=b
        if (g>24) g=24
        if (g<17) g=17
        print g
    }'
}

cache_and_stream_selected() {
    cs_bits=$1
    cs_rate=$2
    cs_reason=$3

    if [ "$cs_bits" -eq 24 ]; then
        if [ "$P_CH" -eq 2 ] && [ "$P_SR" -eq "$cs_rate" ]; then
            write_cache reencode24 24 "$cs_rate"
            stream_reencode24 "$cs_reason"
        else
            write_cache compat24 24 "$cs_rate"
            stream_compat24 "$cs_rate" "$cs_reason"
        fi
    else
        write_cache effective "$cs_bits" "$cs_rate"
        stream_effective "$cs_bits" "$cs_rate" "$cs_reason"
    fi
}

# -------------------------------- DSF policy ----------------------------------
dsf20_44100() {
    # Fixed, zero-analysis path: proved on the Roberts.  No cache is needed because
    # there is no decision to rediscover.
    log "TRANSCODE -> FLAC 20-bit-effective/44.1kHz/stereo (24-bit container), level ${FLAC_LEVEL}: fixed DSF/SACD policy"
    exec ffmpeg -nostdin -hide_banner -loglevel error \
        -i "$IN" -map 0:a:0 -vn \
        -af "aresample=44100:ochl=stereo,aeval=round(val(ch)*524288)/524288:c=same" \
        -sample_fmt s32 -c:a flac -compression_level "$FLAC_LEVEL" \
        -f flac -y "$OUT"
}

# ------------------------------- FLAC policy ----------------------------------
adaptive_flac() {
    init_cache
    if try_cache; then
        : # A valid cache action execs and never returns.
    fi

    load_metadata

    if [ "$P_SR" -le 0 ] || [ "$P_CH" -le 0 ]; then
        log "Metadata probe incomplete (sr=${P_SR}, ch=${P_CH}, bits=${P_BITS}); conservative 20-bit/44.1kHz"
        write_cache effective 20 44100
        stream_effective 20 44100 "metadata probe failure"
    fi

    if [ "$P_BITS" -le 0 ]; then P_BITS=24; fi
    TARGET_RATE=$(choose_rate_family "$P_SR")

    IN_ENVELOPE=1
    if [ "$P_CH" -ne 2 ] || [ "$P_SR" -gt 48000 ] || [ "$P_BITS" -gt 24 ]; then
        IN_ENVELOPE=0
    fi

    log "SOURCE ${P_BITS}-bit/${P_SR}Hz/${P_CH}ch mean=$((AVG_BPS/1000))kbps duration=${DUR_INT}s cache=miss"

    # The large majority of the library should finish here: one metadata probe,
    # then byte-for-byte streaming.  No packet sampling and no decode.
    if [ "$IN_ENVELOPE" -eq 1 ] && [ "$AVG_BPS" -gt 0 ] && [ "$AVG_BPS" -le "$FAST_PASS_AVG_BPS" ]; then
        pass_flac "obvious fast-pass mean=$((AVG_BPS/1000))kbps"
    fi

    NEED_TRANSCODE=0
    HAVE_SAMPLE=0

    if [ "$IN_ENVELOPE" -eq 0 ]; then
        NEED_TRANSCODE=1
        log "FAST DECISION -> transcode: outside Roberts native envelope"
    elif [ "$AVG_BPS" -ge "$FAST_TRANSCODE_AVG_BPS" ]; then
        NEED_TRANSCODE=1
        log "FAST DECISION -> transcode: mean $((AVG_BPS/1000))kbps >= $((FAST_TRANSCODE_AVG_BPS/1000))kbps"
    else
        # Grey band (or missing mean): spend a little effort, but only on sampled
        # windows.  ffprobe seeks; it does not walk the whole multi-hour source.
        INTERVALS=$(build_intervals "$P_DUR" 2>/dev/null || true)
        if [ -n "$INTERVALS" ]; then
            load_sample_metrics "$IN" "$INTERVALS"
            HAVE_SAMPLE=1
            log_metrics "SAMPLED NATIVE"

            if [ "${M_OK:-0}" -eq 1 ] && native_sample_safe; then
                pass_flac "grey-band sampled sustained load acceptable"
            fi
            NEED_TRANSCODE=1
            log "SAMPLED DECISION -> transcode: grey-band sustained-load criteria not met"
        else
            NEED_TRANSCODE=1
            log "SAMPLED DECISION -> transcode: interval construction failed"
        fi
    fi

    [ "$NEED_TRANSCODE" -eq 1 ] || pass_flac "no compatibility trigger"

    # A true <=16-bit source cannot gain precision.  Preserve 16 bits and only fix
    # channel/rate compatibility.  This is already a rare edge case.
    if [ "$P_BITS" -le 16 ]; then
        write_cache s16 16 "$TARGET_RATE"
        stream_16bit "$TARGET_RATE" "16-bit source requires compatibility conversion"
    fi

    # We need one sampled view to locate a difficult region for calibration.  For
    # obvious-hot/out-of-envelope sources this is the first sampled work we do.
    if [ "$HAVE_SAMPLE" -eq 0 ]; then
        INTERVALS=$(build_intervals "$P_DUR" 2>/dev/null || true)
        if [ -n "$INTERVALS" ]; then
            load_sample_metrics "$IN" "$INTERVALS"
            HAVE_SAMPLE=1
            log_metrics "SAMPLED SOURCE"
        fi
    fi

    if [ "$HAVE_SAMPLE" -eq 1 ] && [ "${M_OK:-0}" -eq 1 ]; then
        SRC_SAMPLE_AVG=$M_AVG
        SRC_WORST3=$M_WORST3
    else
        SRC_SAMPLE_AVG=$AVG_BPS
        SRC_WORST3=0
    fi

    # Use the more stressful of whole-file mean and sampled-window mean as the
    # starting estimate.  Calibration below still decides the final bit depth.
    EST_AVG=$AVG_BPS
    if [ "$SRC_SAMPLE_AVG" -gt "$EST_AVG" ]; then EST_AVG=$SRC_SAMPLE_AVG; fi

    START_BITS=$P_BITS
    if [ "$START_BITS" -gt 24 ]; then START_BITS=24; fi
    if [ "$START_BITS" -lt 17 ]; then START_BITS=17; fi

    GUESS=$(estimate_start_bits "$START_BITS" "$EST_AVG" "$P_SR" "$P_CH" "$TARGET_RATE")
    is_uint "$GUESS" || GUESS=20
    if [ "$GUESS" -gt "$START_BITS" ]; then GUESS=$START_BITS; fi
    if [ "$GUESS" -lt 17 ]; then GUESS=17; fi

    CAL_START=$((SRC_WORST3 - CAL_LEAD_SECONDS))
    if [ "$CAL_START" -lt 0 ]; then CAL_START=0; fi

    TMPDIR=/tmp/roberts-audio.$$
    rm -rf "$TMPDIR"
    mkdir -p "$TMPDIR"
    trap 'rm -rf "$TMPDIR"' EXIT HUP INT TERM

    log "CALIBRATE first-play-only target=${TARGET_RATE}Hz sample=${CAL_START}s..$((CAL_START+CAL_SECONDS))s guess=${GUESS}bit max_attempts=${MAX_CANDIDATE_ATTEMPTS}"

    ATTEMPTS=0
    SELECTED=

    # First test the estimate.
    CFILE="$TMPDIR/candidate-${GUESS}.flac"
    ATTEMPTS=$((ATTEMPTS+1))
    if encode_candidate "$GUESS" "$TARGET_RATE" "$CAL_START" "$CFILE"; then
        load_full_metrics "$CFILE"
        log_metrics "CANDIDATE ${GUESS}-effective/${TARGET_RATE}Hz"
        if [ "${M_OK:-0}" -eq 1 ] && candidate_safe; then
            SELECTED=$GUESS
            DIRECTION=up
        else
            DIRECTION=down
        fi
    else
        DIRECTION=down
    fi

    if [ "$DIRECTION" = up ]; then
        # Preserve as much precision as possible.  Walk upward only until the first
        # failure or the hard attempt limit; typical Kora-like cases use two tries.
        NEXT=$((GUESS+1))
        while [ "$NEXT" -le "$START_BITS" ] && [ "$ATTEMPTS" -lt "$MAX_CANDIDATE_ATTEMPTS" ]; do
            CFILE="$TMPDIR/candidate-${NEXT}.flac"
            ATTEMPTS=$((ATTEMPTS+1))
            if ! encode_candidate "$NEXT" "$TARGET_RATE" "$CAL_START" "$CFILE"; then
                break
            fi
            load_full_metrics "$CFILE"
            log_metrics "CANDIDATE ${NEXT}-effective/${TARGET_RATE}Hz"
            if [ "${M_OK:-0}" -eq 1 ] && candidate_safe; then
                SELECTED=$NEXT
                NEXT=$((NEXT+1))
            else
                break
            fi
        done
    else
        # Estimate was still too hot.  Walk down only as far as needed.
        NEXT=$((GUESS-1))
        while [ "$NEXT" -ge 17 ] && [ "$ATTEMPTS" -lt "$MAX_CANDIDATE_ATTEMPTS" ]; do
            CFILE="$TMPDIR/candidate-${NEXT}.flac"
            ATTEMPTS=$((ATTEMPTS+1))
            if encode_candidate "$NEXT" "$TARGET_RATE" "$CAL_START" "$CFILE"; then
                load_full_metrics "$CFILE"
                log_metrics "CANDIDATE ${NEXT}-effective/${TARGET_RATE}Hz"
                if [ "${M_OK:-0}" -eq 1 ] && candidate_safe; then
                    SELECTED=$NEXT
                    break
                fi
            fi
            NEXT=$((NEXT-1))
        done
    fi

    rm -rf "$TMPDIR"
    trap - EXIT HUP INT TERM

    if [ -n "$SELECTED" ]; then
        log "CALIBRATE selected ${SELECTED} effective bits after ${ATTEMPTS} candidate(s); decision will be cached"
        cache_and_stream_selected "$SELECTED" "$TARGET_RATE" "fast sampled calibration"
    fi

    # Rare pathological endpoint.  Do not spend another calibration pass delaying
    # playback: use the known-compatible 16-bit family-rate stream and cache it.
    log "CALIBRATE no 17-24-bit candidate passed within ${ATTEMPTS} attempts -> 16-bit fallback"
    write_cache s16 16 "$TARGET_RATE"
    stream_16bit "$TARGET_RATE" "calibration exhausted"
}

case "$MODE" in
    dsf)  dsf20_44100 ;;
    flac) adaptive_flac ;;
    *)
        log "ERROR unknown mode '$MODE'"
        exit 2
        ;;
esac
