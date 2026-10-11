# Emacs Build Recipes

This directory contains versioned, platform-specific Emacs build workflows.
The version directory is the first compatibility boundary; the Debian release
and display stack are the second.

## Current Default

For Debian 13 / Trixie, the current default is Emacs 31.1 with Debian's native
WebKitGTK 4.1 packages:

- `31.1/debian_13_trixie_wayland_pgtk/`
- `31.1/debian_13_trixie_x11_gtk3/`

The Wayland/PGTK build has been validated on this machine and is the active
default. The X11/GTK3 script is the matching 31.1 native-WebKit recipe, but it
still needs runtime testing on an actual X11 session.

Start with:

```text
31.1/README.md
```

## Preserved 30.2 Builds

The Emacs 30.2 recipes are preserved under:

```text
30.2/
```

These include the older Bookworm and Trixie recipes that used pinned private
WebKitGTK/ICU builds under `/opt`. Keep them for rollback and historical
reproducibility, but do not use them as the starting point for new 31.1 Trixie
builds.

## Layout

```text
30.2/
  debian_12_bookworm_wayland_pgtk/
  debian_13_trixie_wayland_pgtk/
  debian_13_trixie_x11_gtk3/

31.1/
  README.md
  debian_13_trixie_wayland_pgtk/
  debian_13_trixie_x11_gtk3/
```

## Which One Should I Use?

Use `31.1/debian_13_trixie_wayland_pgtk/` for the current Debian 13 Wayland
laptop build.

Use `31.1/debian_13_trixie_x11_gtk3/` if you specifically need a GTK3/X11
build on Trixie.

Use the `30.2/` tree only when you need the old pinned-WebKit builds or a
rollback target.

## Notes

The recipes are intentionally separate. Emacs with xwidgets is sensitive to the
Debian release, display backend, WebKitGTK version, and runtime environment, so
each known-good combination keeps its own script and local notes.

GNU Stow is used for installed Emacs packages under `/usr/local/stow`, allowing
multiple builds to exist while only one is active in `/usr/local/bin/emacs`.
