# Emacs 30.2 + GTK3/X11 + xwidgets + ImageMagick on Debian 13.4 (Trixie)

This repository of scripts is a **self-contained build, packaging, and install workflow** for a custom Emacs build on **Debian 13.x (Trixie)**.

It is aimed at the situation where you want:

- **Emacs 30.2**
- **GTK3/X11** rather than Wayland/PGTK
- **xwidgets** enabled, so Emacs can host native GTK widgets such as the WebKit widget used by `xwidget-webkit-browse-url`
- **ImageMagick support** in Emacs
- a **portable delivery bundle** that can be installed on another Debian 13.x machine **without rebuilding**

The deliverables are:

- `emacs-x11-webgtk-build-and-package-trixie.sh`
- `emacs-x11-webgtk-client-install-trixie.sh`
- a build output directory containing `*.txz` payloads and a combined bundle

---

## Why this build exists at all

Emacs xwidgets on GNU/Linux are awkward because they sit on top of a stack that is not especially small:

- Emacs must be built with xwidget support.
- The relevant GTK toolkit pieces must be present.
- WebKitGTK must be available at build time and usable at runtime.
- WebKitGTK itself is a large, opinionated component with many optional features and a history of being more fragile to build than most ordinary libraries.

GNU Emacs documents xwidgets as **embedded native widgets** available when Emacs is built with the necessary support libraries, and the usual feature test is whether `xwidget-internal` is present. citeturn385550search3

That sounds simple until you try to stabilize the exact WebKitGTK version and dependency behavior you want. In practice, many people end up pinning or locally building a known-good WebKitGTK stack rather than trusting whatever happens to be current in the distro at the time.

That is what these scripts do.

---

## High-level architecture

The build script intentionally splits the world into **three layers**:

1. **System packages from Debian**
   - compilers, headers, GTK, GStreamer, sandboxing helpers, and runtime support packages

2. **Private locally built dependencies under `/opt`**
   - `ICU` under `/opt/icu75`
   - `WebKitGTK 2.40.5` under `/opt/wk240`

3. **Emacs itself under GNU Stow**
   - package contents live under `/usr/local/stow/<package>`
   - visible symlinks appear under `/usr/local/bin`, `/usr/local/share`, and so on

This split is deliberate.

### Why `/opt` for ICU and WebKitGTK?

These are **not** intended to become normal shared libraries for the rest of the machine. They are bundled private dependencies for this Emacs build.

Keeping them under `/opt` makes that explicit:

- they are clearly **non-Debian-managed**
- they do not masquerade as first-class system libraries
- they are easy to package and extract as a distinct private stack
- you can remove or replace them without pretending they belong in the main system library namespace

### Why Stow for Emacs?

GNU Stow describes itself as a **symlink farm manager** that takes separate package trees and makes them appear to be installed in a single place. citeturn590599search0turn590599search8

That is exactly the right fit for hand-built user-facing software under `/usr/local`.

Instead of spraying files directly into:

- `/usr/local/bin`
- `/usr/local/share`
- `/usr/local/lib`

we install Emacs into:

- `/usr/local/stow/emacs-30.2-x11wk`

and then let Stow create symlinks into `/usr/local`.

That gives you:

- cleaner upgrades
- cleaner removal
- easier inspection of what belongs to this build
- less risk of slowly cluttering `/usr/local`

### Why not Stow ICU and WebKitGTK as well?

You *could* do that, but it is the wrong abstraction here.

Stow is best when you want software to **present itself as installed into one normal prefix** like `/usr/local`. ICU and WebKitGTK here are not really being offered to the system as generic libraries; they are being carried as a **private pinned stack** for one application. `/opt` is therefore the clearer and safer home.

So the short version is:

- **Stow for the application**
- **`/opt` for the private bundled dependency stack**

That is the cleanest mental model.

---

## Why WebKitGTK is such a pain

WebKitGTK is large, complex, and highly configurable. It has a long list of optional and semi-optional build-time features. It is also the part of the stack most likely to cause build trouble.

The WebKitGTK build system exposes options such as:

- `ENABLE_INTROSPECTION`
- `ENABLE_DOCUMENTATION`
- `ENABLE_X11_TARGET`
- `ENABLE_WAYLAND_TARGET`
- `USE_GTK4`

and more. The GTK options file in the upstream WebKit source shows that documentation depends on introspection, and that many of these features are individually configurable. citeturn590599search10

For this build, we intentionally choose the smallest practical shape that still serves Emacs xwidgets well:

- **GTK port**
- **X11 target ON**
- **Wayland target OFF**
- **GTK4 OFF**
- **Soup2 ON**
- **Introspection OFF**
- **Documentation OFF**
- **MiniBrowser OFF**

That is not because those other options are “bad” in general. It is because they are not required for the desired Emacs runtime and they widen the build surface unnecessarily.

---

## Why GObject Introspection is turned off

GObject Introspection is a system that scans C libraries and generates metadata so that non-C language bindings and tools can understand those APIs automatically. citeturn590599search1turn590599search5

That machinery is useful when you need generated typelib/GIR metadata for bindings or tooling.

But **Emacs xwidgets do not need WebKitGTK introspection metadata in order to use the WebKit widget**.

So this workflow turns it off deliberately:

- `-DENABLE_INTROSPECTION=OFF`
- `-DENABLE_DOCUMENTATION=OFF`

This has two benefits:

1. it removes packages and moving parts that are not needed for the target result
2. it avoids a class of build failures that often cluster around introspection/doc generation

You may notice commented references to:

- `gobject-introspection`
- `libgirepository1.0-dev`
- `gi-docgen`

Those remain in the builder as commented reference only. They are there so future-you can see what was consciously *not* included.

---

## Why ImageMagick is included

This setup enables ImageMagick for **two related but separate reasons**.

### 1. Emacs image support

GNU Emacs can be built with ImageMagick support so that image handling goes through the ImageMagick libraries where appropriate. The Emacs Lisp manual documents ImageMagick-backed images and related image handling behavior. citeturn590599search3

The sanity test used by the scripts is:

```elisp
(image-type-available-p 'imagemagick)
```

### 2. External image tooling

Separately, many Emacs-based graphics or sprite workflows use the external ImageMagick CLI (`magick` or `convert`) to crop or compose images ahead of display.

Those are **not the same thing**.

Building Emacs with ImageMagick support does **not** automatically replace an external CLI-based asset pipeline. It simply means Emacs itself can use ImageMagick-backed image support where relevant.

So the scripts install:

- the **runtime tool**: `imagemagick`
- the **development headers**: `libmagickwand-7.q16-dev`

On current Debian Trixie, the versioned `libmagickwand-7.q16-dev` package is the real development package for MagickWand. citeturn385550search0turn385550search4

---

## Why native compilation needs `libgccjit0`

Emacs native compilation relies on libgccjit. The build machine needs the development package in order to compile Emacs with native compilation enabled, and the client machine needs the runtime shared library so Emacs can use that support normally.

That is why the scripts use:

- build machine: `libgccjit-14-dev`
- client/runtime: `libgccjit0`

Debian Trixie ships `libgccjit0` as the shared runtime library package. citeturn385550search1

---

## Why `ninja` is used

`ninja` is a small, fast build tool designed to execute build graphs efficiently. In this workflow it is not replacing `make` everywhere; it is specifically used because the WebKitGTK build system is driven through **CMake + Ninja**.

So the division is:

- `configure` + `make` for ICU and Emacs
- `cmake -GNinja` + `ninja` for WebKitGTK

That is normal and appropriate for those upstream projects.

---

## About `umask` and why the scripts set it

The builder and installer both set:

```bash
umask 022
```

This matters more than people often realize.

When you create packaged artifacts without a predictable umask, you can end up with tarballs that preserve overly private permissions. That can turn into confusing client-side install failures or unreadable files after extraction.

Using `022` means:

- owner gets write permission
- group and world get read permission by default

For build artifacts intended for handoff, that is generally the sensible default.

So yes, this is one of those boring details that is worth getting right up front.

---

## Files in this workflow

### `emacs-x11-webgtk-build-and-package-trixie.sh`

This script runs on the **build machine**.

It does all of the following:

- installs build dependencies
- installs runtime dependencies needed on the build machine itself
- builds ICU privately into `/opt/icu75`
- builds WebKitGTK 2.40.5 privately into `/opt/wk240`
- builds Emacs 30.2 into `/usr/local/stow/<package>`
- activates Emacs through Stow on the build machine
- runs sanity checks
- produces portable `.txz` payloads
- optionally copies the client installer and README into the artifact directory
- emits a final combined bundle tarball

### `emacs-x11-webgtk-client-install-trixie.sh`

This script runs on a **client machine**.

It does all of the following:

- installs runtime packages only
- verifies checksums if `SHA256SUMS` is present
- extracts the private ICU and WebKitGTK payloads under `/opt`
- extracts the Emacs payload under `/usr/local/stow`
- activates Emacs via Stow
- checks xwidgets and ImageMagick support

---

# Builder script walkthrough

Below is the practical logic of the build script, section by section.

## 1. Tunables

The script exposes variables such as:

- `WK_VER`
- `ICU_VER`
- `WK_PREFIX`
- `ICU_PREFIX`
- `EMACS_VER`
- `EMACS_STOW_NAME`
- `BUILDROOT`
- `DISTDIR`
- `JOBS`
- `CLEAN`

These let you keep the script stable while changing versions or paths without rewriting the file.

The defaults are chosen so that the resulting build tree is easy to recognize:

- `/opt/icu75`
- `/opt/wk240`
- `/usr/local/stow/emacs-30.2-x11wk`

## 2. Argument parsing

The script accepts:

- `--clean=none`
- `--clean=light`
- `--clean=deep`

This is there because WebKitGTK builds are expensive and you do not always want to throw away everything.

- **none**: keep existing build trees where possible
- **light**: clean in-place build products and rebuild
- **deep**: stash or remove source/build trees and start properly fresh

## 3. Helper functions

The script defines helpers such as:

- `log`
- `die`
- `stash_dir`
- `need_cmd`

These are not fancy. They just make the script easier to read and fail in a controlled way.

That matters because long source builds are miserable to debug if every failure looks the same.

## 4. `umask 022`

Set early, so everything that follows inherits sensible default permissions.

## 5. Build prerequisites

The builder installs:

- core tooling: compilers, `cmake`, `ninja`, `pkg-config`, `ccache`, `stow`
- graphics and GTK development headers
- media and rendering stack dependencies
- Emacs-specific development headers
- ImageMagick development headers and CLI tooling

The commented-out introspection packages are intentionally left commented.

This is one of the key design choices of the workflow.

## 6. Runtime prerequisites on the build machine

The build machine also needs to *run* the built Emacs. So the builder installs the runtime package set too.

This matters because you want to test the exact thing you just built, not just compile it.

## 7. Workspace preparation

The build root is created and optionally cleaned.

`ccache` is enabled by exporting:

- `CC="ccache gcc"`
- `CXX="ccache g++"`

That is a practical quality-of-life improvement, especially when you are iterating after a failed WebKit build or reconfiguring Emacs.

## 8. ICU build

ICU is built first into `/opt/icu75`.

Why?

Because the whole point is to pin a private ICU for the WebKitGTK build rather than relying on whatever system ICU happens to be current.

The build uses a conventional `configure` + `make` flow and disables samples/tests to keep the build leaner.

## 9. WebKitGTK source fetch

The script downloads the `webkitgtk-2.40.5.tar.xz` release tarball directly from the WebKitGTK release site. WebKitGTK 2.40.5 is indeed an upstream release in the stable 2.40 series. citeturn385550search2turn385550search6

## 10. WebKitGTK configure step

This is the most important part of the whole workflow.

### The key environment forcing

The script exports:

- `CMAKE_PREFIX_PATH`
- `PKG_CONFIG_PATH`
- `CFLAGS`
- `CXXFLAGS`
- `LDFLAGS`

pointing at the private ICU.

That is what makes the WebKitGTK build prefer the pinned ICU instead of the system one.

### The chosen CMake options

The script uses:

- `-DPORT=GTK`
- `-DENABLE_X11_TARGET=ON`
- `-DENABLE_WAYLAND_TARGET=OFF`
- `-DUSE_GTK4=OFF`
- `-DUSE_SOUP2=ON`
- `-DUSE_JPEGXL=OFF`
- `-DENABLE_INTROSPECTION=OFF`
- `-DENABLE_DOCUMENTATION=OFF`
- `-DENABLE_MINIBROWSER=OFF`

#### Why each matters

- **GTK port**: needed because xwidgets on Linux are GTK/WebKitGTK-based.
- **X11 ON**: matches the intended Emacs target.
- **Wayland OFF**: narrows the build and avoids pulling in another display target you do not need.
- **GTK4 OFF**: keeps the build on GTK3 where the Emacs/xwidgets target lives here.
- **Soup2 ON**: matches the intended pinned stack.
- **JPEGXL OFF**: removes another optional dependency edge.
- **Introspection OFF**: removes unneeded GIR/typelib generation.
- **Documentation OFF**: avoids doc-generation requirements and because documentation depends on introspection in WebKit’s build logic. citeturn590599search10
- **MiniBrowser OFF**: we are not building WebKitGTK as a general desktop browser toolkit demo package; we only need the library stack for Emacs.

## 11. WebKitGTK build and install

This uses `ninja`, because that is the selected CMake generator.

Then it installs under `/opt/wk240`.

## 12. WebKitGTK verification

The script checks:

```bash
pkg-config --modversion webkit2gtk-4.0
```

through the private prefix.

That is a cheap but useful sanity check: it verifies that the install tree advertises the expected WebKitGTK version.

## 13. Emacs configure step

Emacs is configured with:

- `--with-x=yes`
- `--with-x-toolkit=gtk3`
- `--with-xwidgets`
- `--with-native-compilation`
- `--with-tree-sitter`
- `--with-modules`
- `--with-cairo`
- `--with-harfbuzz`
- `--with-mailutils`
- `--with-rsvg`
- `--with-webp`
- `--with-imagemagick`
- `--with-sqlite3`
- `--with-gnutls`
- `--with-xml2`
- `--with-gpm`

### Why these flags are used

#### `--with-x=yes`
You want a graphical X11 build.

#### `--with-x-toolkit=gtk3`
This selects GTK3 rather than Lucid or other toolkit variants.

#### `--with-xwidgets`
This is the whole reason we are doing the WebKitGTK work at all.

#### `--with-native-compilation`
Turns on native compilation support via libgccjit.

#### `--with-tree-sitter`
Enables Tree-sitter integration.

#### `--with-modules`
Allows dynamic module support.

#### `--with-cairo` and `--with-harfbuzz`
These are the modern rendering/text shaping choices you normally want.

#### `--with-rsvg`, `--with-webp`, `--with-imagemagick`
Enable broader image handling support.

#### `--with-sqlite3`
Useful for newer Emacs features that depend on SQLite.

#### `--with-gnutls`
TLS support.

#### `--with-xml2`
Useful for XML and HTML parsing features in Emacs.

#### `--with-gpm`
Console mouse support; harmless if you want the feature available.

## 14. Installing Emacs into Stow

The `--prefix` is set to:

- `/usr/local/stow/<package>`

That means `make install` writes a complete standalone package tree there.

After that, `stow -R -d /usr/local/stow -t /usr/local <package>` activates it.

That creates the visible `/usr/local/bin/emacs` symlink and corresponding shared data links.

## 15. Sanity checks

The builder checks:

- `featurep 'xwidget-internal`
- `image-type-available-p 'imagemagick`
- that `/usr/local/bin/emacs` resolves through Stow
- that `magick` or `convert` is available

These checks are intentionally simple and fast.

They do not prove every xwidget action will work perfectly, but they do prove the build has the expected core features wired in.

## 16. Packaging

The builder creates three payload archives:

- `icu75.txz`
- `wk240.txz`
- `<emacs-stow-name>.txz`

Why three instead of one enormous blind blob?

Because this is easier to inspect, easier to re-use, and easier to debug.

It also generates:

- `BUILD-METADATA.txt`
- `SHA256SUMS`
- a final combined bundle tarball

So you get both:

- **modular payloads**
- and **one hand-off bundle**

That is the right compromise.

---

# Client installer walkthrough

The client-side script is deliberately smaller.

Its job is not to build. Its job is to:

1. optionally verify checksums
2. install runtime packages only
3. extract the three payloads
4. activate Emacs through Stow
5. run checks

## Why runtime packages are still needed

The portable bundle contains your private ICU and WebKitGTK stack and the built Emacs package.

It does **not** contain every generic runtime library that Debian would normally provide, nor should it.

The client machine still needs Debian runtime packages such as:

- GL stack pieces
- GStreamer plugin packages
- sandbox helpers
- `libgccjit0`
- `imagemagick`
- `stow`

That is normal. The client script is meant to avoid the source build, not to replace all system package management.

## Why the client script verifies checksums if present

This is a cheap integrity check and is worth doing when you are moving bundles between machines.

If `SHA256SUMS` is present alongside the payloads, the script checks it. If not, it carries on.

That keeps the script convenient without throwing away a useful safeguard.

---

# Typical usage

## On the build machine

Put these files together in one directory:

- `emacs-x11-webgtk-build-and-package-trixie.sh`
- `emacs-x11-webgtk-client-install-trixie.sh`
- `README.md`

Run:

```bash
chmod +x emacs-x11-webgtk-build-and-package-trixie.sh
./emacs-x11-webgtk-build-and-package-trixie.sh --clean=none
```

For a fresh rebuild:

```bash
./emacs-x11-webgtk-build-and-package-trixie.sh --clean=deep
```

At the end, the artifact directory will contain:

- `icu75.txz`
- `wk240.txz`
- `emacs-30.2-x11wk.txz`
- `SHA256SUMS`
- `BUILD-METADATA.txt`
- client installer script
- README
- combined bundle tarball

## On the client machine

Copy the artifact directory or the relevant files into one directory and run:

```bash
chmod +x emacs-x11-webgtk-client-install-trixie.sh
./emacs-x11-webgtk-client-install-trixie.sh
```

That installs runtime packages, extracts the payloads, activates Emacs via Stow, and runs checks.

---

# Practical cautions

## 1. Debian major version

This workflow is designed for **Debian 13.x** machines.

Using a bundle built on 13.2 and installed on 13.4 is a reasonable expectation on the same architecture, because the private ICU and WebKitGTK stack travel with the build and the client script reinstalls the relevant Debian runtime packages.

Using the same bundle across different Debian majors is a different question and should not be assumed safe without testing.

## 2. Architecture

Do not assume a bundle built on one architecture will work on another.

## 3. X11 target

This workflow is intentionally X11/GTK3 oriented. It is not trying to be a generic Wayland build recipe.

## 4. `convert` vs `magick`

Different ImageMagick packaging or local habits may expose either `magick`, `convert`, or both. The sanity checks accept either.

---

# Final design summary

This build strategy is opinionated, but the opinions are sensible:

- keep the **private pinned dependency stack** under `/opt`
- keep the **application install** under Stow
- **disable GObject introspection and docs** because they are not needed here and they enlarge the failure surface
- use **Ninja** because that is what the WebKitGTK CMake build wants
- use **ImageMagick** both for Emacs support and for external image tooling convenience
- set **umask 022** because portable build artifacts should unpack sanely

That is the core of the system.

If you later decide to revise the pinned versions, the layout remains valid. You would just update the version variables and, if needed, the package names or feature flags that belong to those versions.
