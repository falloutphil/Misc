# Emacs 30.2 + PGTK/Wayland + xwidgets on Debian 13 WSLg

This directory is the Debian 13 / Trixie version of the older Debian 12
Wayland recipe. It targets Windows WSL with WSLg, so the assumptions are:

- Debian 13 userspace
- WSLg provides the Wayland socket at `/mnt/wslg/runtime-dir/wayland-0`
- Emacs is built as a PGTK/Wayland application, not an X11/GTK3 application
- xwidgets uses a private pinned WebKitGTK stack
- native Emacs sound uses ALSA, with WSLg audio reached through PulseAudio

The closest existing recipe is `../debian_12_bookworm_wayland_pgtk/`. The
Trixie X11 recipe is useful for the newer ICU/WebKit packaging structure, but
this directory deliberately keeps the Wayland/PGTK shape.

## What Carries Over From Debian 12

The Debian 12 WSLg setup and this Debian 13 setup are very similar at runtime.
The important inherited pieces are:

- use `--with-pgtk` when configuring Emacs
- force GTK onto the WSLg Wayland backend with `GDK_BACKEND=wayland`
- point WSL sessions at `/mnt/wslg/runtime-dir/wayland-0` when needed
- use private WebKitGTK under `/opt/wk240`
- set `WEBKIT_EXEC_PATH` so WebKit helper processes match the private WebKit
- do not override `GIO_MODULE_DIR`
- use system GIO/TLS modules through `GIO_EXTRA_MODULES`
- disable WebKit DMA-BUF rendering by default on WSLg
- install `libasound2-plugins`, `alsa-utils`, and a Pulse-backed `~/.asoundrc`

That last point matters because Emacs sound support is compiled against ALSA.
WSLg normally gives you PulseAudio as the useful audio path, not a normal ALSA
hardware device.

## What Changes On Trixie

Trixie has a newer system library stack, so this recipe follows the Debian 13
X11 script for the private support libraries:

- private ICU 75.1 under `/opt/icu75`
- private WebKitGTK 2.40.5 under `/opt/wk240`
- WebKit built against the private ICU
- a marker file under `/opt/wk240` so the script does not silently reuse the
  X11-only WebKit build from the other Trixie recipe
- Emacs installed into `/usr/local/stow/emacs-30.2-pgtkwk`
- portable `.txz` artifacts for client installs
- separate builder and client installer scripts

The display-stack choices are different from the X11 recipe:

- WebKitGTK: `ENABLE_WAYLAND_TARGET=ON`
- WebKitGTK: `ENABLE_X11_TARGET=OFF`
- Emacs: `--with-pgtk`
- no `--with-x-toolkit=gtk3`

The client installer uses Trixie's GTK runtime package name, `libgtk-3-0t64`.

## Files

- `emacs-wayland-pgtk-webgtk-build-and-package-trixie.sh`
  builds ICU, WebKitGTK, and Emacs, then creates portable archives.
- `emacs-wayland-pgtk-webgtk-client-install-trixie.sh`
  installs those archives on another Debian 13 WSL instance.
- `emacs_prompt.zsh`
  optional zsh `emacs()` launcher for interactive use.

## Build On The Main WSL Machine

Run this from Debian 13 under WSL:

```bash
cd emacs/debian_13_trixie_wayland_pgtk
./emacs-wayland-pgtk-webgtk-build-and-package-trixie.sh
```

Default output:

```text
/opt/icu75
/opt/wk240
/usr/local/stow/emacs-30.2-pgtkwk
~/dist/emacs-30.2-pgtkwk-trixie-wslg/
```

The build is intentionally rerunnable:

```bash
./emacs-wayland-pgtk-webgtk-build-and-package-trixie.sh --clean=none
./emacs-wayland-pgtk-webgtk-build-and-package-trixie.sh --clean=light
./emacs-wayland-pgtk-webgtk-build-and-package-trixie.sh --clean=deep
```

Use `--clean=none` for normal reruns. It reuses installed ICU/WebKit if they
are already present and only rebuilds Emacs.

Use `--clean=deep` only when you want to remove the private `/opt` prefixes and
rebuild the expensive WebKitGTK layer from scratch.

If `/opt/wk240` already contains a WebKit build from the X11 Trixie recipe, the
script stops instead of reusing it. That is intentional: the X11 recipe built
WebKit with `ENABLE_WAYLAND_TARGET=OFF`. Use `--clean=deep` to replace it, or
set `WK_PREFIX` to a different empty prefix if you need to keep both variants
inside the same WSL distribution.

The default is conservative even with `--clean=deep`: it will not remove an
existing unmarked or X11-marked WebKit install at `/opt/wk240`. If replacing
that prefix is intentional, run:

```bash
ALLOW_REPLACE_MISMATCHED_WEBKIT=yes \
  ./emacs-wayland-pgtk-webgtk-build-and-package-trixie.sh --clean=deep
```

The client installer has the same guard before it removes `/opt/wk240`.

## Install On Another Debian 13 WSL Instance

Copy the files from the dist directory to the target WSL instance:

```text
icu75.txz
wk240.txz
emacs-30.2-pgtkwk.txz
SHA256SUMS
emacs-wayland-pgtk-webgtk-client-install-trixie.sh
```

Then run:

```bash
./emacs-wayland-pgtk-webgtk-client-install-trixie.sh
```

The installer extracts ICU and WebKit into `/opt`, extracts Emacs into Stow,
activates it under `/usr/local`, and installs these launchers:

```text
/usr/local/bin/emacs
/usr/local/bin/emacs-pgtkwk
/usr/local/bin/emacs-pgtkwk-safe
```

Use `emacs-pgtkwk` for normal WSLg Wayland use. Use `emacs-pgtkwk-safe` if an
xwidget buffer opens but renders blank or incorrectly.

## Runtime Environment

For WSLg, the launcher needs to do more than just run `/usr/local/bin/emacs`.

It sets the Wayland side:

```bash
XDG_RUNTIME_DIR=/mnt/wslg/runtime-dir
WAYLAND_DISPLAY=wayland-0
GDK_BACKEND=wayland
WEBKIT_DISABLE_DMABUF_RENDERER=1
```

It also keeps WebKit aligned:

```bash
LD_LIBRARY_PATH=/opt/wk240/lib:/opt/icu75/lib
WEBKIT_EXEC_PATH=/opt/wk240/libexec/webkit2gtk-4.0
GIO_EXTRA_MODULES=/usr/lib/x86_64-linux-gnu/gio/modules
XDG_DATA_DIRS=/opt/wk240/share:/usr/local/share:/usr/share
```

Do not set `GIO_MODULE_DIR` for this. Overriding it can hide the system TLS
modules from `glib-networking`, which breaks HTTPS inside xwidgets.

## Optional Shell Function

If you want the plain `emacs` shell command to apply the WSLg wrapper behavior,
source the zsh helper:

```zsh
source /path/to/emacs/debian_13_trixie_wayland_pgtk/emacs_prompt.zsh
```

Useful switches:

```bash
EMACS_NO_WK240=1 emacs
EMACS_ENABLE_DMABUF=1 emacs
EMACS_FORCE_X11=1 emacs
EMACS_WEBKIT_SAFE_RENDER=1 emacs
```

`EMACS_WEBKIT_SAFE_RENDER=1` applies the same conservative rendering settings
as `/usr/local/bin/emacs-pgtkwk-safe`:

```bash
WEBKIT_DISABLE_COMPOSITING_MODE=1
LIBGL_ALWAYS_SOFTWARE=1
GSK_RENDERER=cairo
```

Those settings are app-specific compatibility settings, not global WSL defaults.

## Verification

Basic checks:

```bash
/usr/local/bin/emacs --version
/usr/local/bin/emacs -Q --batch --eval "(princ (featurep 'xwidget-internal))"
/usr/local/bin/emacs-pgtkwk -Q
```

Inside Emacs:

```elisp
(featurep 'xwidget-internal)
(play-sound-file "/usr/share/sounds/purple/receive.wav")
```

For a browser test:

```text
M-x xwidget-webkit-browse-url RET https://www.bbc.com RET
```

If the page opens but draws blank, retry with:

```bash
/usr/local/bin/emacs-pgtkwk-safe -Q
```

## Troubleshooting

Blank xwidget content usually means a rendering path problem. Use
`emacs-pgtkwk-safe` first. That disables WebKit compositing mode, forces
software GL, and asks GTK to use Cairo rendering.

HTTPS failures inside xwidgets usually mean GIO/TLS modules are not visible.
Install `glib-networking` and `ca-certificates`, make sure `GIO_MODULE_DIR` is
unset, and make sure `GIO_EXTRA_MODULES` points at the system GIO module
directory.

Errors mentioning invalid WebKit messages usually mean Emacs loaded one WebKit
library stack while WebKit helper processes came from another. Check:

```bash
echo "$WEBKIT_EXEC_PATH"
```

It should be:

```text
/opt/wk240/libexec/webkit2gtk-4.0
```

No sound in WSLg usually means the ALSA-to-Pulse bridge is missing. The client
installer writes this if no conflicting default already exists:

```conf
pcm.!default {
  type pulse
}
ctl.!default {
  type pulse
}
```

## Summary

Use this directory for Debian 13 on Windows WSL when you want the same broad
behavior as the old Debian 12 Wayland setup, but with the newer Trixie private
ICU/WebKit packaging model.
