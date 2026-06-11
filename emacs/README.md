# Emacs Build Recipes

This directory contains **versioned, platform-specific Emacs build and packaging workflows**.

The recipes here are not interchangeable. They target different Debian releases, different display stacks, and different runtime assumptions.

## Directory Layout

### `debian_12_bookworm_wayland_pgtk/`

Older **Debian 12 (bookworm)** workflow focused on:

- **Wayland / PGTK**
- **xwidgets**
- wrapper-driven runtime setup
- environments such as Wayland desktops and WSLg-style setups

Use this if you specifically want the older Bookworm-era approach and you are working in a Wayland-oriented environment.

This path now also assumes `libasound2-dev` is present before `./configure`, so
native Emacs sound support is not silently omitted.

### `debian_13_trixie_x11_gtk3/`

Newer **Debian 13 (trixie)** workflow focused on:

- **X11 + GTK3**
- **xwidgets**
- private pinned WebKitGTK and ICU under `/opt`
- GNU Stow-managed Emacs install under `/usr/local/stow`
- a proper **builder/packager** flow
- a separate **client installer** for deploying the built artifacts to another machine

Use this if you want the newer, more structured Debian 13 workflow with packaging and client-side installation.

This path also expects `libasound2-dev` before `./configure` so the resulting
Emacs has ALSA-backed native sound support.

## Why there are two separate trees

Emacs with `xwidgets` is awkward because it depends on a compatible WebKitGTK stack, and the practical solution differs depending on:

- Debian release
- X11 vs Wayland/PGTK
- whether the target machine is the build machine
- how much runtime environment wrapping is needed

Rather than trying to force all cases into one script, this repository keeps the workflows separate and explicit.

That is intentional.

## Which one should I use?

Use:

- `debian_12_bookworm_wayland_pgtk/` for the older **Bookworm + Wayland/PGTK** path
- `debian_13_trixie_x11_gtk3/` for the newer **Trixie + X11/GTK3** path

If you are unsure, start with the README in each subdirectory and check:

1. your Debian version
2. whether you are targeting X11 or Wayland
3. whether you need to build locally or deploy a packaged result to another machine

## Notes on naming

The subdirectories are named to preserve the important compatibility boundary:

- Debian release
- display/backend model

That makes it easier to keep multiple known-good build recipes without confusing them.

## Recommended usage

Enter the directory that matches your target platform and follow its local `README.md`.

Each subtree should be treated as a self-contained build reference for that platform.

## Future additions

If more variants are added later, they should follow the same pattern:

- one directory per distro/display-stack combination
- one local README per variant
- scripts and helper files kept inside that variant's directory

Examples:

- `debian_13_wayland/`
- `debian_14_x11/`
- `ubuntu_24_04_wayland/`

## Summary

This directory is a small collection of **separate, known-good Emacs build recipes**, not one universal installer.

Pick the subtree that matches your platform and use that as the source of truth.
