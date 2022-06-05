;; -*- lexical-binding: t; -*-

;;
;; Buffer indentation and format utility
;;

(defconst format-buffer-output-name "*Format Buffer Output*")

(defun indent-region-delete-whitespace (&optional beg end)
  "Delete trailing whitespace and indent for selected region. If
no region is activated, this will operate on the entire buffer."
  (interactive
   (progn
     (barf-if-buffer-read-only)
     (if (use-region-p)
         (list (region-beginning) (region-end))
       (list (point-min) (point-max)))))
  (save-excursion
    (unless (eq beg end)
      (delete-trailing-whitespace beg end)
      (indent-region beg end))))

(defun format-buffer-exec (exec &rest opts)
  "Format current buffer using external EXEC along with its
OPTS. It returns numeric status code of the command execution
result."
  (let ((old-buf (current-buffer))
        (fmt-buf format-buffer-output-name)
        (coding-system-for-read 'utf-8)
        (coding-system-for-write 'utf-8)
        status)
    (save-restriction
      (widen)
      (with-current-buffer (get-buffer-create fmt-buf)
        (let ((inhibit-read-only t))
          (erase-buffer)
          (message "Formatting buffer using %s..." exec)
          (with-current-buffer old-buf
            (setq status
                  (apply #'call-process-region
                         (append
                          (list nil nil exec nil fmt-buf nil) opts))))))
      (if (zerop status)
          (unwind-protect
              (if (zerop (compare-buffer-substrings
                          old-buf nil nil fmt-buf nil nil))
                  (message "Buffer already formatted")
                (let ((p (point)))
                  (erase-buffer)
                  (insert-buffer-substring fmt-buf)
                  (goto-char p))
                (message "Formatting buffer using %s...done" exec))
            (let ((fmt-win (get-buffer-window fmt-buf)))
              (if fmt-win (delete-window fmt-win))
              (kill-buffer fmt-buf)))
        (with-current-buffer fmt-buf
          (let ((inhibit-read-only t))
            (goto-char (point-min))
            (insert (format "Command '%s %s'\n\n" exec (mapconcat 'identity opts " "))))
          (special-mode)
          (display-buffer fmt-buf)
          (message "Failed to format buffer using %s" exec))))
    status))

(defun format-buffer-cmd ()
  "Return command line for external tool used to format code if
configured, or nil if not."
  (cond
   ((memq major-mode (list 'c-mode 'c++-mode))
    (list "clang-format" "-style=file"))
   ((equal major-mode 'nxml-mode)
    (list "tidy" "-indent" "-wrap" "0" "-omit" "-quiet" "-utf8"))
   ((equal major-mode 'python-mode)
    (list "autopep8" "--max-line-length=100" "--aggressive" "-"))
   ((equal major-mode 'json-mode)
    (list "jq" "--monochrome-output" "--indent" "2" "."))
   ((equal major-mode 'jsonnet-mode)
    (list "jsonnetfmt" "-"))
   ((equal major-mode 'terraform-mode)
    (list "terraform" "fmt" "-no-color" "-"))
   (t nil)))

;;;###autoload
(defun format-buffer (&optional beg end)
  "Format current buffer."
  (interactive
   (progn
     (barf-if-buffer-read-only)
     (if (use-region-p)
         (list (region-beginning) (region-end))
       (list (point-min) (point-max)))))
  (let ((cmd (format-buffer-cmd)))
    (if (and (not (use-region-p)) cmd)
        (if (zerop (apply #'format-buffer-exec cmd))
            (delete-trailing-whitespace))
      (if indent-tabs-mode
          (tabify beg end)
        (untabify beg end))
      (indent-region-delete-whitespace beg end))))

(add-to-list 'display-buffer-alist
             `(,(format "\\`%s\\'" (regexp-quote format-buffer-output-name))
               (display-buffer-at-bottom)
               (inhibit-same-window . t)
               (window-height lambda
                              (w)
                              (fit-window-to-buffer w
                                                    (/
                                                     (frame-height)
                                                     2)
                                                    10))))

(provide 'format-buffer)
