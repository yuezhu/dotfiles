;; -*- lexical-binding: t; -*-

(require 'package)

;;
;; Emacs package management
;;
(defconst package-must-elpa-packages '(org)
  "Packages in this list must be from ELPA if present when checking
installation status.")

(defconst package-upgrade-check-interval 7200
  "Interval to perform ELPA packages upgrade check.")

(defconst package-upgrade-check-stamp
  (expand-file-name "package-upgrade-check-stamp"
                    user-emacs-directory)
  "Filename that store the timestamp when ELPA packages upgrade
check is performed.")

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(advice-add 'package-installed-p :around
            (lambda (func &rest args)
              (let ((pkg (car args)))
                (if (memq pkg package-must-elpa-packages)
                    (assq pkg package-alist)
                  (apply func args)))))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(defun package-directory (name)
  "Return the directory location that package NAME will be
installed with the current version in ELPA
`package-archive-contents'."
  (let* ((pkg-desc (cadr (assq name package-archive-contents)))
         (pkg-full-name (and pkg-desc
                             (package-desc-full-name pkg-desc))))
    (if pkg-full-name
        (file-name-as-directory
         (concat (file-name-as-directory package-user-dir)
                 pkg-full-name)))))

(defun package-update ()
  "Return a list of packages that have new versions available."
  (let (result)
    (cl-flet ((get-version
               (name where)
               (let ((pkg (cadr (assq name where))))
                 (when pkg
                   (package-desc-version pkg)))))
      (dolist (package (mapcar #'car package-alist))
        (let ((in-archive (get-version package package-archive-contents)))
          (when (and in-archive
                     (version-list-< (get-version package package-alist)
                                     in-archive))
            (push (cadr (assq package package-archive-contents))
                  result)))))
    result))

(defun package-do-upgrade ()
  "Upgrade all ELPA packages to their latest versions."
  (remove-hook 'package--post-download-archives-hook #'package-do-upgrade)
  (let* ((packages (package-update))
         (msg (mapconcat #'package-desc-full-name packages ", "))
         (num (length packages))
         (sfx (if (<= num 1) "" "s")))
    (if (not packages)
        (message "All packages are up to date")
      (message "%d package%s available for upgrade: %s" num sfx msg)
      (save-window-excursion
        (dolist (package-desc packages)
          (let ((new-package-full-name
                 (package-desc-full-name package-desc))
                (old-package-desc
                 (cadr (assq (package-desc-name package-desc)
                             package-alist))))
            (message "Installing package ‘%s’..." new-package-full-name)
            (package-install package-desc)
            (message "Installing package ‘%s’...done" new-package-full-name)
            (package-delete old-package-desc))))
      (message "%d package%s upgraded: %s" num sfx msg))))

(defun package-upgrade (&optional async)
  "Refresh and upgrade all installed ELPA packages.
Optional argument ASYNC specifies whether to perform the
downloads in the background."
  (interactive)
  (message "Package refresh started at %s" (current-time-string))
  (add-hook 'package--post-download-archives-hook  #'package-do-upgrade)
  (package-refresh-contents async))

(defun dump-into-file (data filename)
  "Dump DATA to FILENAME."
  (when (file-writable-p filename)
    (with-temp-file filename
      (insert (let (print-length) (prin1-to-string data))))))

(defun load-from-file (filename)
  "Load data from FILENAME."
  (with-demoted-errors
      "Error during file read: %S"
    (when (file-exists-p filename)
      (with-temp-buffer
        (insert-file-contents filename)
        ;; this will blow up if the contents of the file aren't
        ;; lisp data structures
        (read (buffer-string))))))

(add-hook 'after-init-hook
          (lambda ()
            "Auto upgrade ELPA packages when idle for a while."
            (run-with-idle-timer
             600 t
             (lambda ()
               (let* ((now (round (float-time)))
                      (last (or (load-from-file package-upgrade-check-stamp) 0))
                      (elapsed (- now last)))
                 (when (>= elapsed package-upgrade-check-interval)
                   (package-upgrade t)
                   (dump-into-file now package-upgrade-check-stamp)))))
            (message "Check package upgrade every %ds"
                     package-upgrade-check-interval)) t)

(provide 'init-elpa)
