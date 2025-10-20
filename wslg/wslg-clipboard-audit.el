;;; wslg-clipboard-audit.el --- Audit WSLg clipboard behavior (Wayland vs X11) -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Philip Beadling
;; Keywords: tools, unix
;; Version: 1.0
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; This script audits clipboard behavior under WSLg (Windows Subsystem for Linux GUI),
;; comparing Wayland (pgtk) vs X11 (Xwayland) clipboard integration.
;;
;; It captures:
;; - Environment info (window-system, WAYLAND_DISPLAY, DISPLAY)
;; - Available clipboard TARGETS
;; - Image data (trying image/png, image/bmp, image/jpeg, image/tiff)
;; - Text data (UTF8_STRING, TEXT, STRING)
;;
;; All artifacts are saved to /tmp/wslg-cliptest/ with a timestamped report.
;;
;; Usage:
;;   M-x ph/clipboard-audit-run
;;
;; Or from command line:
;;   # Wayland/pgtk
;;   emacs -Q -l ./wslg-clipboard-audit.el -f ph/clipboard-audit-run
;;
;;   # X11 (force X11 backend)
;;   env GDK_BACKEND=x11 WAYLAND_DISPLAY= DISPLAY=:0 \
;;     emacs -Q -l ./wslg-clipboard-audit.el -f ph/clipboard-audit-run
;;
;; Compatibility:
;; - Tested with Emacs 27.1+ (both pgtk and X11 builds)
;; - Requires GUI Emacs (not TTY)
;; - For BMP rendering, external image converter (ImageMagick) may be needed

;;; Code:

(defun ph/ts () (format-time-string "%Y%m%d-%H%M%S"))

(defun ph/ext-for-type (ty)
  "Map MIME type symbol TY to file extension for saving."
  (pcase ty
    ('image/png  "png") ('image/jpeg "jpg") ('image/bmp "bmp") ('image/tiff "tiff")
    ('text/html  "html") (_ "txt")))

(defun ph/insert-image-bytes (bytes &optional mime)
  "Insert BYTES as an image, letting Emacs auto-detect; hint type when known.
Handles the X11+Emacs28 multibyte clipboard case and non-native formats (BMP).

BYTES: Raw image data from clipboard.
MIME: Optional MIME type symbol (e.g., 'image/png, 'image/bmp) as a hint.

Strategy:
1. Ensure data is unibyte (X11/Emacs 28 may give multibyte strings).
2. Try auto-detection via create-image.
3. For non-native formats (BMP), enable external converter with :format hint.
4. Fall back to using MIME symbol as DATA-P hint to create-image."
  (let* ((raw (if (multibyte-string-p bytes)
                  (encode-coding-string bytes 'binary)  ; ensure UNIBYTE
                bytes))
         ;; 1) Normal autodetect (works for PNG/JPEG/TIFF/etc. when data is unibyte)
         (img (ignore-errors (create-image raw nil t))))
    ;; 2) BMP and other non-native formats: enable external converter (ImageMagick/ffmpeg).
    ;;    BMP is NOT natively supported in Emacs, so we need image-use-external-converter.
    ;;    The :format hint tells the converter what the source format is.
    (unless img
      (when (eq mime 'image/bmp)
        (let ((image-use-external-converter t))
          (setq img (ignore-errors
                     (create-image raw nil t :format 'image/bmp))))))
    ;; 3) Last resort: pass the MIME symbol as the DATA-P hint to create-image.
    ;;    This can help Emacs guess the type when auto-detection fails.
    (unless img
      (when mime
        (setq img (ignore-errors (create-image raw nil mime)))))
    ;; Render or report failure.
    (if img
        (progn (insert-image img) (insert "\n[Inline render OK]\n"))
      (insert "[Inline render failed]\n"))))

(defun ph/targets (selection)
  "Return TARGETS as a list of symbols (or nil). Handles list/vector/atom."
  (let ((ts (ignore-errors (gui-get-selection selection 'TARGETS))))
    (cond ((null ts) nil)
          ((vectorp ts) (append ts nil))
          ((listp ts) ts)
          (t (list ts)))))

(defun ph/try-get (selection target)
  "Return BYTES/STRING from clipboard for TARGET or nil."
  (let ((selection-coding-system 'binary))        ;; <-- crucial on X11/Emacs 28
    (ignore-errors (gui-get-selection selection target))))

(defun ph/first-common (candidates haystack)
  "Return first symbol in CANDIDATES that is `memq' in HAYSTACK, else nil."
  (catch 'found
    (dolist (c candidates)
      (when (memq c haystack) (throw 'found c)))
    nil))

(defun ph/clipboard-audit-run ()
  "Snapshot clipboard: env + targets; try image/* (even if TARGETS empty on pgtk); then text."
  (interactive)
  (unless (display-graphic-p)
    (user-error "Run in GUI Emacs (not TTY)."))
  (let* ((logdir "/tmp/wslg-cliptest/")
         (selection 'CLIPBOARD)
         (ts (ph/ts))
         (backend (cond ((eq window-system 'pgtk) "pgtk-wayland")
                        ((eq window-system 'x) "x11")
                        ((display-graphic-p) "gui")
                        (t "tty")))
         (base (format "emacs-%s-%s" backend ts))
         (report (get-buffer-create "*clipboard-audit*"))
         (targets (ph/targets selection)) ;; NOTE: This is where we actually query the clipboard for TARGETS!
         ;; IMPORTANT: do NOT call this `image-types` (that shadows Emacs's global)
         (clipboard-image-targets '(image/png image/bmp image/jpeg image/tiff))
         (report-path (expand-file-name (concat base ".log") logdir)))
    (unless (file-directory-p logdir) (make-directory logdir t))

    ;; report header
    (with-current-buffer report
      (erase-buffer)
      (insert (format "Emacs: %s\n" (emacs-version)))
      (insert (format "window-system: %S\n" window-system))
      (insert (format "backend: %s\n" backend))
      (insert (format "WAYLAND_DISPLAY=%S\n" (getenv "WAYLAND_DISPLAY")))
      (insert (format "DISPLAY=%S\n" (getenv "DISPLAY")))
      (insert (format "Selection: %S\n" selection))
      (insert (format "Timestamp: %s\n\n" ts))
      (insert "TARGETS:\n")
      (if targets ; anything on the clipboard?
          (dolist (sym (sort (mapcar #'symbol-name targets) #'string<))
            (insert (format " - %s\n" sym))) ; print each target symbol
        (insert " (none reported)\n"))
      (insert "\n--- IMAGE ATTEMPT ---\n"))

    ;; image attempt
    (let* ((try-direct (eq window-system 'pgtk))
           ;; First, try to find image type advertised in TARGETS
           (chosen-target (ph/first-common clipboard-image-targets targets))
           bytes)
      ;; If no image target found in TARGETS but we're on pgtk/Wayland,
      ;; try probing all image formats directly (WSLg/Wayland sometimes has empty TARGETS).
      (when (and (not chosen-target) try-direct)
        (catch 'found
          (dolist (candidate clipboard-image-targets)
            (let ((data (ph/try-get selection candidate)))
              (when (and (stringp data) (> (length data) 0))
                (setq chosen-target candidate
                      bytes data)
                (throw 'found t))))))
      ;; If we found a target in TARGETS, fetch its data
      (unless bytes
        (when chosen-target
          (setq bytes (ph/try-get selection chosen-target))))

      (with-current-buffer report
        (cond
         ((and (stringp bytes) (> (length bytes) 0))
          (let* ((ext (ph/ext-for-type chosen-target))
                 (img-path (expand-file-name (format "%s.%s" base ext) logdir))
                 (itype (ignore-errors (image-type-from-data bytes)))
                 (is-mb (multibyte-string-p bytes))
                 (emacs-image-types (and (boundp 'image-types) image-types)))
            (insert (format "Found %s (%d bytes)\n" chosen-target (length bytes)))
            (insert (format "Debug: image-type-from-data=%S multibyte=%s emacs-image-types=%S\n"
                            itype (if is-mb "t" "nil") emacs-image-types))
            (condition-case err
                (let ((coding-system-for-write 'binary))
                  (write-region bytes nil img-path nil 'silent)
                  (insert (format "Saved image -> %s\n" img-path)))
              (error
               (insert (format "Failed to save image: %s\n" (error-message-string err)))))
            (ph/insert-image-bytes bytes chosen-target)
            (insert "\n")))
         (t
          (insert "No image available.\n\n")))))

    ;; text attempt
    (with-current-buffer report
      (insert "\n--- TEXT ATTEMPT ---\n"))
    (let ((txt (or (ph/try-get selection 'UTF8_STRING)
                   (ph/try-get selection 'TEXT)
                   (ph/try-get selection 'STRING))))
      (with-current-buffer report
        (if (and (stringp txt) (> (length txt) 0))
            (let ((txt-path (expand-file-name (format "%s.txt" base) logdir)))
              (insert (format "Pulled text (%d chars)\n" (length txt)))
              (condition-case err
                  (let ((coding-system-for-write 'utf-8-unix))
                    (write-region txt nil txt-path nil 'silent)
                    (insert (format "Saved text -> %s\n" txt-path)))
                (error
                 (insert (format "Failed to save text: %s\n" (error-message-string err))))))
          (insert "No text available.\n"))))

    ;; finalize
    (with-current-buffer report
      (goto-char (point-min))
      (condition-case err
          (progn
            (write-region (point-min) (point-max) report-path)
            (message "Clipboard audit → %s" report-path))
        (error
         (message "Clipboard audit complete (failed to save log: %s)" (error-message-string err)))))
    (display-buffer report)))

(provide 'wslg-clipboard-audit)
;;; wslg-clipboard-audit.el ends here
