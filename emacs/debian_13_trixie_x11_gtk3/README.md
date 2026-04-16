# Emacs 30.2 + GTK3/X11 + xwidgets on Debian 13 (Trixie)

This directory contains a working, rerunnable recipe for building and installing:

- **Emacs 30.2**
- with **GTK3**
- on **X11**
- with **xwidgets enabled**
- against a **private pinned WebKitGTK 2.40.5**
- against a **private pinned ICU 75.1**
- installed cleanly with **GNU Stow**

This was debugged on **Debian 13 / Trixie**, and the final result is:

- Emacs builds successfully
- `featurep 'xwidget-internal` is true
- `M-x xwidget-webkit-browse-url` opens an xwidget browser buffer
- WebKit content renders successfully
- but on this machine, correct rendering required these runtime environment variables:

```bash
WEBKIT_DISABLE_COMPOSITING_MODE=1 LIBGL_ALWAYS_SOFTWARE=1 GSK_RENDERER=cairo /usr/local/bin/emacs -Q
```

That point matters.

The build itself was correct before this. The remaining problem was a **runtime rendering/compositing issue**, not a build failure, and not a reason to disable the WebKit sandbox.

---

## Final working shape

The final layout is:

- **private ICU** under `/opt/icu75`
- **private WebKitGTK** under `/opt/wk240`
- **Emacs** under `/usr/local/stow/emacs-30.2-x11wk`
- symlinked into `/usr/local` with **GNU Stow**

That split is intentional:

- `/opt` is for the **private pinned library stack**
- Stow is for the **user-facing application**

That is the cleanest arrangement.

---

## What this setup was trying to achieve

This is not just “build Emacs from source”.

It is a specific build target:

- **Emacs 30.2**
- **GTK3/X11**
- **xwidgets**
- **private pinned WebKitGTK**
- **private pinned ICU**
- **clean install and upgrade story**
- a layout that is easy to inspect, re-run, and remove

The hard part here is not ordinary Emacs compilation. The hard part is getting:

- WebKitGTK
- ICU
- pkg-config discovery
- compiler selection
- Stow layout
- runtime rendering behavior

all aligned.

---

## The final architecture

There are three layers.

### 1. Debian-managed packages

These come from `apt` and provide:

- compilers and build tools
- GTK3 and media headers
- GStreamer stack pieces
- graphics stack libraries
- native compilation dependencies
- general Emacs feature dependencies

### 2. Private pinned libraries under `/opt`

These are built from source and intentionally kept out of the normal distro library namespace:

- `/opt/icu75`
- `/opt/wk240`

### 3. Emacs installed under Stow

Emacs installs into:

- `/usr/local/stow/emacs-30.2-x11wk`

Then Stow exposes it through:

- `/usr/local/bin`
- `/usr/local/share`
- `/usr/local/lib`
- and related paths

That gives you a clean install tree and easy rollback/removal.

---

## Why `/opt` for ICU and WebKitGTK

Because they are **private support libraries for this Emacs build**, not generic libraries you want to treat like first-class Debian-managed system components.

Using `/opt` makes that explicit:

- they are clearly local/private
- they do not pretend to be distro packages
- they are easy to remove or replace
- they are easy to package separately if you want to move the stack to another similar machine

---

## Why Stow for Emacs

Emacs is the thing you actually want to “install”.

Stow is the right tool here because it lets you keep the actual files in one contained package tree while presenting them under `/usr/local` via symlinks.

That gives you:

- clean upgrades
- clean removal
- easy inspection of what belongs to this build
- less `/usr/local` clutter

The important fix here was that Emacs must install into the **package root itself**, not into a nested `usr/local` below it.

Correct:

```bash
--prefix=/usr/local/stow/emacs-30.2-x11wk
```

Wrong:

```bash
--prefix=/usr/local/stow/emacs-30.2-x11wk/usr/local
```

That wrong nested prefix is exactly how you end up with files in the tree but nothing appearing under `/usr/local/bin` after Stow runs.

---

## What went wrong during debugging, and what fixed it

This is the part that actually matters if you ever have to repeat this.

### 1. ICU source extraction path had to be right

The ICU tarball expands to `icu/`, with the build happening under:

```bash
$WORKROOT/icu/source
```

So the script had to look for:

- source root: `$WORKROOT/icu`
- build dir: `$WORKROOT/icu/source`

not some guessed variant.

### 2. WebKitGTK needed to be built against the private ICU

That required:

- `CMAKE_PREFIX_PATH`
- `PKG_CONFIG_PATH`
- explicit ICU CMake variables
- runtime rpath to `/opt/icu75/lib`

Without that, the build may pick up the system ICU instead of the private one.

### 3. Compiler verification originally used the wrong check

CMake stored:

- `/usr/bin/gcc`
- `/usr/bin/g++`

but those resolve on Debian to versioned compiler backends like:

- `/usr/bin/x86_64-linux-gnu-gcc-14`
- `/usr/bin/x86_64-linux-gnu-g++-14`

That is normal.

So the check had to compare **resolved paths** rather than insist the cache literally contained only `/usr/bin/gcc` and `/usr/bin/g++` in the simplistic sense.

### 4. `ccache` was a red herring, but the script still neutralises launcher issues

The logs showed WebKit’s build system probing for `ccache`. At one point the script also had over-strict launcher checks.

In the successful path:

- no active compiler launcher remained in the CMake cache
- `ccache` was not the real blocker
- the script explicitly forces the compiler and clears launcher-related environment variables during WebKit configure

So `ccache` was not the root cause, but cleaning up the compiler-selection logic was still necessary.

### 5. WebKitGTK did build and install correctly

The script eventually produced a correct private install under `/opt/wk240`, with pkg-config files available there.

That allowed Emacs configure to find WebKitGTK and enable xwidgets.

### 6. Emacs configure verification was checking the wrong thing

The correct post-configure check was:

```c
#define HAVE_XWIDGETS 1
```

in `src/config.h`.

Not:

```bash
HAVE_XWIDGETS=yes
```

That string is not the right success test.

### 7. `--with-json` had to be removed

Emacs 30.2 on this system warned that `--with-json` was an unrecognised option.

That is because JSON support is not controlled by that configure switch in this build context. So the flag had to go.

### 8. Reuse logic had to be made real

A rerunnable script is only useful if it actually skips the expensive parts when the installed results are already present.

The successful logic was:

- reuse ICU if `/opt/icu75/bin/icuinfo` and `/opt/icu75/lib/libicuuc.so` exist
- reuse WebKitGTK if pkg-config file and library exist under `/opt/wk240`
- still rebuild Emacs, because that part is comparatively cheap and often what you are iterating on

That is why rerunning with no switches did **not** rebuild WebKitGTK again once it was installed successfully.

### 9. The final runtime problem was rendering, not building

This was the last and most important lesson.

You had:

- successful WebKit build
- successful Emacs build
- xwidgets enabled
- `xwidget-webkit-browse-url` opening a buffer

but one test page showed a blank main content area.

That turned out not to be a missing xwidget feature at all. It was a runtime rendering path problem on this machine.

The successful run was:

```bash
WEBKIT_DISABLE_COMPOSITING_MODE=1 LIBGL_ALWAYS_SOFTWARE=1 GSK_RENDERER=cairo /usr/local/bin/emacs -Q
```

and **that** is the real fix to document.

---

## What those runtime variables do

### `WEBKIT_DISABLE_COMPOSITING_MODE=1`

This tells WebKitGTK to disable compositing mode.

In practice, this pushes WebKit away from an accelerated compositing path that can be problematic on some Linux graphics stacks, remote/X11 setups, virtualised environments, WSL-adjacent stacks, and certain driver combinations.

For this build, it was the single most important runtime workaround.

### `LIBGL_ALWAYS_SOFTWARE=1`

This tells Mesa to force software rendering.

So instead of trying to use the system’s hardware GL stack, it falls back to software rendering.

That is slower, but it is often much more reliable for diagnosis and for awkward GPU/driver situations.

### `GSK_RENDERER=cairo`

This tells GTK’s scene kit to use the Cairo renderer.

That again avoids a more complex GL-oriented renderer path and keeps the drawing pipeline conservative and predictable.

---

## Important: this is **not** the same as disabling the WebKit sandbox

Do **not** confuse the successful workaround with turning off WebKit sandboxing.

The successful command was:

```bash
WEBKIT_DISABLE_COMPOSITING_MODE=1 LIBGL_ALWAYS_SOFTWARE=1 GSK_RENDERER=cairo /usr/local/bin/emacs -Q
```

It was **not** a sandbox-disabling command.

That distinction matters because:

- the problem observed was rendering/compositing
- the fix was rendering/compositing-related
- there is no reason from this successful run to claim that the WebKit sandbox itself was the problem

So the correct explanation is:

> xwidgets were built correctly; runtime rendering on this machine required disabling WebKit compositing mode and forcing a software/Cairo rendering path.

That is the accurate diagnosis.

---

## The build choices that proved successful

### WebKitGTK build shape

The successful WebKitGTK configuration was intentionally narrow:

- GTK port
- X11 target ON
- Wayland target OFF
- GTK4 OFF
- Soup2 ON
- JPEG XL OFF
- introspection OFF
- documentation OFF
- MiniBrowser OFF

That reduces the number of moving parts and keeps the build aligned to the actual Emacs target.

### Why introspection and docs were disabled

They were not needed for Emacs xwidgets.

Turning them off removed unnecessary build surface and avoided extra dependency churn.

This did **not** reduce Emacs xwidget capability for the target outcome.

### Why ImageMagick stayed enabled

Because you wanted ImageMagick support in Emacs, and it was not the source of the xwidget problem.

So it made sense to keep it enabled.

### Why GTK3/X11

Because this target was explicitly about a GTK3/X11 Emacs build, not a Wayland/PGTK build.

That was a deliberate constraint, not an accident.

---

## Rerun behaviour of the final script

With the final successful logic, running the script with **no switches** should be safe in the normal case.

### `--clean=none`

This means:

- reuse installed ICU if present
- reuse installed WebKitGTK if present
- fetch sources only if missing
- rebuild Emacs
- restow Emacs
- rerun verification

So yes: with no switches, it should **not** redo the long WebKitGTK build if `/opt/wk240` is already complete and valid.

### `--clean=light`

This is for when you want to throw away build directories and source trees that are cheap to recreate, while keeping installed prefixes if they are still valid.

### `--clean=deep`

This wipes the workroot and installed private prefixes and rebuilds everything from scratch.

Use that only when you really mean it, because WebKitGTK is the expensive part.

---

## Final successful verification state

The successful final checks showed:

- `GNU Emacs 30.2`
- `libwebkit2gtk-4.0.so` loading from `/opt/wk240/lib`
- ICU 75 loading from `/opt/icu75/lib`
- `feature-xwidgets=t`
- `module-file-suffix=.so`
- batch sanity checks succeeding

That means the core build and linkage were correct.

The only remaining nuance was the need for the runtime rendering environment variables on this machine.

---

## Recommended way to launch this build on this machine

For the known-good runtime path, use:

```bash
WEBKIT_DISABLE_COMPOSITING_MODE=1 LIBGL_ALWAYS_SOFTWARE=1 GSK_RENDERER=cairo /usr/local/bin/emacs -Q
```

Once that is confirmed stable, you can decide how you want to make it convenient.

Typical options are:

- a shell alias
- a small wrapper script
- a desktop launcher
- setting the environment only for this Emacs build, not globally

The key point is that these settings should be treated as **app-specific runtime compatibility settings**, not as machine-wide defaults unless you actually want that.

---

## Bottom line

The successful path was:

1. build private ICU 75.1 under `/opt/icu75`
2. build private WebKitGTK 2.40.5 under `/opt/wk240`
3. build Emacs 30.2 with GTK3/X11 and xwidgets
4. install Emacs into `/usr/local/stow/emacs-30.2-x11wk`
5. activate it with Stow into `/usr/local`
6. run Emacs with:

```bash
WEBKIT_DISABLE_COMPOSITING_MODE=1 LIBGL_ALWAYS_SOFTWARE=1 GSK_RENDERER=cairo /usr/local/bin/emacs -Q
```

That is the working Debian 13 success path.
