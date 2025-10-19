;; Minimal WSLg clipboard audit (Wayland vs X11), no headers / no customs.

(defun ph/ts () (format-time-string "%Y%m%d-%H%M%S"))

(defun ph/ext-for-type (ty)
  (pcase ty
    ('image/png  "png") ('image/jpeg "jpg") ('image/bmp "bmp") ('image/tiff "tiff")
    ('text/html  "html") (_ "txt")))

(defun ph/insert-image-bytes (bytes)
  "Insert BYTES as an image, letting Emacs auto-detect the format."
  ;; EXACT SAME semantics as the proven 3 lines:
  ;;   (setq img (create-image bytes nil t))
  ;;   (insert-image img)
  (let* ((img (create-image bytes nil t)))
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
  (ignore-errors (gui-get-selection selection target)))

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
                        ((display-graphic-p) "gui") (t "tty")))
         (base (format "emacs-%s-%s" backend ts))
         (report (get-buffer-create "*clipboard-audit*"))
         (targets (ph/targets selection))
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
      (if targets
          (dolist (sym (sort (mapcar #'symbol-name targets) #'string<))
            (insert (format " - %s\n" sym)))
        (insert " (none reported)\n"))
      (insert "\n--- IMAGE ATTEMPT ---\n"))

    ;; Try images:
    ;; - if TARGETS listed -> prefer first image/* it advertises
    ;; - if pgtk and TARGETS empty -> probe image/png directly
    (let* ((try-direct (eq window-system 'pgtk))
           (chosen-target
            (or (ph/first-common clipboard-image-targets targets)
                (and try-direct 'image/png)))
           (bytes (and chosen-target (ph/try-get selection chosen-target))))
      (with-current-buffer report
        (cond
         ((and (stringp bytes) (> (length bytes) 0))
          (let* ((ext (ph/ext-for-type chosen-target))
                 (img-path (expand-file-name (format "%s.%s" base ext) logdir))
                 (itype (ignore-errors (image-type-from-data bytes)))
                 (is-mb (multibyte-string-p bytes))
                 ;; Pull the *real* Emacs-supported image-types (png/jpeg/…)
                 (emacs-image-types (and (boundp 'image-types) image-types)))
            (insert (format "Found %s (%d bytes)\n" chosen-target (length bytes)))
            (insert (format "Debug: image-type-from-data=%S multibyte=%s emacs-image-types=%S\n"
                            itype (if is-mb "t" "nil") emacs-image-types))
            (message "[clipboard-audit] chosen=%S bytes=%d itype=%S mb=%s emacs-image-types=%S"
                     chosen-target (length bytes) itype (if is-mb "t" "nil") emacs-image-types)
            (let ((coding-system-for-write 'binary))
              (write-region bytes nil img-path nil 'silent))
            (insert (format "Saved image -> %s\n" img-path))
            ;; Exact proven path: let Emacs auto-detect type from BYTES.
            (condition-case err
                (let ((img (create-image bytes nil t)))
                  (if img
                      (progn (insert-image img) (insert "\n[Inline render OK]\n\n"))
                    (insert "[Inline render failed]\n\n")))
              (error
               (insert (format "Render error: %S\n\n" err))
               (message "[clipboard-audit] render error: %S" err)))))
         (t
          (insert "No image available.\n\n")))))

    ;; Text attempt
    (with-current-buffer report
      (insert "\n--- TEXT ATTEMPT ---\n"))
    (let ((txt (or (ph/try-get selection 'UTF8_STRING)
                   (ph/try-get selection 'TEXT)
                   (ph/try-get selection 'STRING))))
      (with-current-buffer report
        (if (and (stringp txt) (> (length txt) 0))
            (let ((txt-path (expand-file-name (format "%s.txt" base) logdir)))
              (insert (format "Pulled text (%d chars)\n" (length txt)))
              (let ((coding-system-for-write 'utf-8-unix))
                (write-region txt nil txt-path nil 'silent))
              (insert (format "Saved text -> %s\n" txt-path)))
          (insert "No text available.\n"))))

    ;; finalize
    (with-current-buffer report
      (goto-char (point-min))
      (write-region (point-min) (point-max) report-path))
    (message "Clipboard audit → %s" report-path)
    (display-buffer report)))
