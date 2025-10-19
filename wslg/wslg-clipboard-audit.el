;; Minimal WSLg clipboard audit (Wayland vs X11), no headers / no customs.

(defun ph/ts () (format-time-string "%Y%m%d-%H%M%S"))

(defun ph/ext-for-type (ty)
  (pcase ty
    ('image/png  "png") ('image/jpeg "jpg") ('image/bmp "bmp") ('image/tiff "tiff")
    ('text/html  "html") (_ "txt")))

(defun ph/insert-image-bytes (bytes ty)
  (let* ((fmt (intern (substring (symbol-name ty) 6))) ; 'png, 'jpeg, etc.
         (img (create-image bytes fmt t)))
    (insert (propertize " " 'display img))
    (insert (format "\n[Inserted %s]\n" ty))))

(defun ph/targets (selection)
  "Return TARGETS as a list of symbols (or nil). Handles list/vector/atom."
  (let ((ts (ignore-errors (gui-get-selection selection 'TARGETS))))
    (cond
     ((null ts) nil)
     ((vectorp ts) (append ts nil))   ;; vector -> list
     ((listp ts) ts)                  ;; list -> as-is
     (t (list ts)))))   

(defun ph/try-get (selection target)
  "Return BYTES/STRING from clipboard for TARGET or nil."
  (ignore-errors (gui-get-selection selection target)))

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
         (image-types '(image/png image/bmp image/jpeg image/tiff))
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
    ;; - if TARGETS listed -> try those types
    ;; - if pgtk and TARGETS empty -> still try common image types directly
    (let* ((try-direct (eq window-system 'pgtk))
           (image-saved nil))
      (dolist (ty image-types)
        (when (and (not image-saved)
                   (or (member ty targets) try-direct))
          (let ((bytes (ph/try-get selection ty)))
            (with-current-buffer report
              (when (and (stringp bytes) (> (length bytes) 0))
                (let* ((ext (ph/ext-for-type ty))
                       (img-path (expand-file-name (format "%s.%s" base ext) logdir)))
                  (insert (format "Found %s (%d bytes)\n" ty (length bytes)))
                  (let ((coding-system-for-write 'binary))
                    (write-region bytes nil img-path nil 'silent))
                  (insert (format "Saved image -> %s\n" img-path))
                  (condition-case err
                      (progn (ph/insert-image-bytes bytes ty) (insert "\n"))
                    (error (insert (format "Render error: %S\n" err))))
                  (setq image-saved t))))))))

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
