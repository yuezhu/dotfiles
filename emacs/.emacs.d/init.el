;; -*- lexical-binding: t; -*-

(defconst emacs-start-time (current-time))

(unless noninteractive
  (message "Loading %s..." load-file-name))

;; No special file handling during Emacs startup (affecting startup time).
(defvar file-name-handler-alist-tmp file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'after-init-hook
          `(lambda ()
             (setq file-name-handler-alist file-name-handler-alist-tmp)
             (makunbound 'file-name-handler-alist-tmp)
             (garbage-collect)) t)


;;
;; Use customizations
;;
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

(let ((elapsed (float-time (time-subtract (current-time)
                                          emacs-start-time))))
  (message "Loading %s (source)...done (%.3fs) (GC: %d)"
           custom-file elapsed gcs-done))

;; Initialize package
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'init-elpa)


;;
;; Initialize use-package
;; https://github.com/jwiegley/use-package
;;
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))


;;
;; Packages configurations
;;
(use-package diminish
  :ensure t
  :commands diminish)


(use-package isearch
  :no-require t
  :bind (:map isearch-mode-map
              ;; DEL during isearch should edit the search string, not jump
              ;; back to the previous result
              ;; https://github.com/purcell/emacs.d/blob/b484cada4356803d0ecb063d33546835f996fefe/lisp/init-isearch.el#L14
              ([remap isearch-delete-char] . isearch-del-char)))


(use-package uniquify
  :defer t)


(use-package hl-line
  :hook ((dired-mode
          magit-mode
          occur-mode
          ibuffer-mode
          compilation-mode
          gnus-mode)
         . hl-line-mode)
  :defer t)


(use-package goto-addr
  :hook
  (prog-mode . goto-address-prog-mode)
  ((text-mode magit-process-mode) . goto-address-mode)
  :defer t)


(use-package display-fill-column-indicator
  :hook
  (auto-fill-mode
   . (lambda ()
       (display-fill-column-indicator-mode (if auto-fill-function 1 -1))))
  :defer t)


(use-package simple
  :diminish auto-fill-function
  :hook ((org-mode
          markdown-mode
          prog-mode
          protobuf-mode)
         . turn-on-auto-fill)
  :hook (((occur-mode
           dired-mode
           speedbar-mode
           compilation-mode)
          . (lambda () (visual-line-mode -1))))
  :defer t)


(use-package xref
  :commands (xref-pulse-momentarily
             xref-push-marker-stack))


(use-package subword
  :diminish
  :defer t)


(use-package eldoc
  :diminish
  :defer t)


(use-package ialign
  :ensure t
  :bind ("C-c |" . ialign)
  :bind (:map ialign-minibuffer-keymap
              ("C-c =" . ialign-increment-spacing)))


(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :disabled ;; 06/28/19 disabled since it significantly slows down Emacs startup
  :ensure t
  :demand t
  :config
  (dolist (var '("LANG"
                 "LC_CTYPE"
                 "JAVA_HOME"
                 "GOROOT"))
    (add-to-list #'exec-path-from-shell-variables var))
  (exec-path-from-shell-initialize))


(use-package winner
  :unless noninteractive
  :preface
  (defun transient-winner-undo ()
    "Transient version of `winner-undo'."
    (interactive)
    (let ((echo-keystrokes nil))
      (winner-undo)
      (message "Winner: [u]ndo [r]edo")
      (set-transient-map
       (let ((map (make-sparse-keymap)))
         (define-key map [?u] #'winner-undo)
         (define-key map [?r] #'winner-redo)
         map)
       t)))

  :bind ("C-c u" . transient-winner-undo)

  :hook
  ;;  (after-init . winner-mode)
  (ediff-before-setup . winner-mode)
  (ediff-quit . winner-undo)

  :defer 2

  :config
  (winner-mode))


(use-package midnight
  :defer 2
  :config
  (midnight-mode))


(use-package recentf
  :preface
  ;; https://github.com/jwiegley/dot-emacs/blob/master/init.el
  (defun recentf-add-dired-directory ()
    (if (and dired-directory
             (file-directory-p dired-directory)
             (not (string= "/" dired-directory)))
        (let ((last-idx (1- (length dired-directory))))
          (recentf-add-file
           (if (= ?/ (aref dired-directory last-idx))
               (substring dired-directory 0 last-idx)
             dired-directory)))))

  (defun recentf-maybe-save-list ()
    (defvar recentf-list-old nil)
    (unless (equal recentf-list-old
                   recentf-list)
      (let ((inhibit-message t))
        (recentf-save-list))
      (setq recentf-list-old
            (copy-sequence recentf-list))))

  (defun recentf-remove-filename (filename)
    "Remove FILENAME from the recent list."
    (let ((m (recentf-string-member
              (recentf-expand-file-name filename) recentf-list)))
      (and m (setq recentf-list (delq (car m) recentf-list)))))

  :hook
  (dired-mode . recentf-add-dired-directory)

  :commands (recentf-mode
             recentf-add-file
             recentf-save-list
             recentf-string-member)

  ;; Loaded by `consult'
  :defer 2

  :config
  (recentf-mode)

  (advice-add 'recentf-cleanup :around
              (lambda (func &rest args)
                "Do not echo the message onto minibuffer when
cleaning up `recentf-list'."
                (let ((inhibit-message t))
                  (apply func args))))

  ;; 05/30/21 not needed
  ;; (run-at-time t 300 #'recentf-maybe-save-list)
  )


(use-package whitespace
  :bind (("C-c w m" . whitespace-mode)
         ("C-c w r" . whitespace-report)
         ("C-c w c" . whitespace-cleanup))
  :diminish (global-whitespace-mode
             whitespace-mode
             whitespace-newline-mode)
  :hook ((makefile-mode
          ssh-config-mode
          conf-mode
          json-mode
          yaml-mode)
         . whitespace-mode))


(use-package ispell
  :bind ("C-c i w" . ispell-word))


(use-package flyspell
  :bind ("C-c i b" . flyspell-buffer)
  ;; :hook
  ;; (text-mode . flyspell-mode)
  :config
  ;; https://github.com/abo-abo/oremacs/blob/github/modes/ora-flyspell.el
  (defun flyspell-ignore-http-and-https ()
    "Function used for `flyspell-generic-check-word-predicate' to
    ignore stuff starting with \"http\" or \"https\"."
    (save-excursion
      (forward-whitespace -1)
      (not (looking-at "[\t ]+https?\\b"))))
  (dolist (mode '(text-mode org-mode)) ;; need to specify all derived modes
    (put mode 'flyspell-mode-predicate #'flyspell-ignore-http-and-https)))


(use-package flyspell-correct
  :ensure t
  :after flyspell
  :bind (:map flyspell-mode-map
              ("C-c i c" . flyspell-correct-wrapper)))


(use-package info
  ;; :init
  ;; (remove-hook 'menu-bar-update-hook 'mac-setup-help-topics)
  :defer t ;; Does not work. It is loaded somewhere else.
  :config
  (advice-add 'Info-exit :after
              (lambda ()
                "When quitting `info', remove its window."
                (if (> (length (window-list)) 1)
                    (delete-window)))))


;; Loaded by `projectile'
(use-package ibuffer
  :bind ("C-x C-b" . ibuffer)
  :hook
  (ibuffer-mode . ibuffer-auto-mode)
  (ibuffer
   . (lambda ()
       (ibuffer-vc-set-filter-groups-by-vc-root)
       (unless (eq ibuffer-sorting-mode 'filename/process)
         (ibuffer-do-sort-by-filename/process))))

  :config
  (define-ibuffer-column size-h
    (:name "Size" :inline t)
    (cond
     ((> (buffer-size) 1000) (format "%7.3fK" (/ (buffer-size) 1024.0)))
     ((> (buffer-size) 1000000) (format "%7.3fM" (/ (buffer-size) 1048576.0)))
     (t (format "%8d" (buffer-size))))))


(use-package ibuffer-vc
  :ensure t
  :after ibuffer)


(use-package hideshow
  :diminish hs-minor-mode
  :hook (prog-mode . hs-minor-mode)
  :defer t)


(use-package diff-hl
  :ensure t
  :hook ((prog-mode
          conf-mode
          text-mode
          protobuf-mode
          ssh-config-mode)
         . diff-hl-mode)
  (magit-pre-refresh . diff-hl-magit-pre-refresh)
  (magit-post-refresh . diff-hl-magit-post-refresh)
  :defer t)


(use-package tramp-sh
  :init
  ;; https://www.gnu.org/software/emacs/manual/html_node/tramp/Frequently-Asked-Questions.html
  (setq vc-ignore-dir-regexp
        (format "\\(%s\\)\\|\\(%s\\)"
                vc-ignore-dir-regexp
                tramp-file-name-regexp))
  :defer t)


(use-package rg
  :ensure t
  :bind ("C-c s" . rg-menu)
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*rg\\*\\'"
                 (display-buffer-at-bottom)
                 (inhibit-same-window . t)
                 (window-height . 0.5))))


(use-package dumb-jump
  :ensure t
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  :defer t)


(use-package hippie-exp
  :bind ("M-/" . hippie-expand)
  :config
  (advice-add 'hippie-expand :around
              (lambda (func &rest args)
                "Make `hippie-expand' do case-sensitive
expanding. Though not all, this is effective with some expand
functions, eg., `try-expand-all-abbrevs'"
                (let ((case-fold-search nil))
                  (apply func args)))))


(use-package dired
  :preface
  (defun dired-find-directory (dir)
    (interactive "DFind directory: ")
    (let ((orig (current-buffer)))
      (dired dir)
      (kill-buffer orig)))

  ;; https://github.com/jwiegley/dot-emacs/blob/master/init.el
  (defun dired-next-window ()
    (interactive)
    (let ((next (car (cl-remove-if-not (lambda (wind)
                                         (with-current-buffer (window-buffer wind)
                                           (eq major-mode 'dired-mode)))
                                       (cdr (window-list))))))
      (when next
        (select-window next))))

  (defun dired-find-file-reuse-buffer ()
    "Replace current buffer if file is a directory."
    (interactive)
    (let ((orig (current-buffer))
          (filename (dired-get-file-for-visit)))
      (dired-find-file)
      (when (and (file-directory-p filename)
                 (not (eq (current-buffer) orig)))
        (kill-buffer orig))))

  (defun dired-up-directory-reuse-buffer ()
    "Replace current buffer if file is a directory."
    (interactive)
    (let ((orig (current-buffer)))
      (dired-up-directory)
      (kill-buffer orig)))

  :commands (dired-get-file-for-visit
             dired-find-file
             dired-up-directory
             dired-hide-details-mode)

  :bind (:map dired-mode-map
              ("/"     . dired-find-directory)
              ("<tab>" . dired-next-window)
              ("M-p"   . dired-up-directory-reuse-buffer)
              ("M-n"   . dired-find-file-reuse-buffer)
              ("!"     . crux-open-with))

  :hook
  (dired-mode
   . (lambda ()
       ;; (dired-hide-details-mode)
       (setq-local auto-revert-verbose nil)))

  :init
  (when (and (equal system-type 'darwin)
             (executable-find "gls"))
    (setq insert-directory-program "gls"))

  (setq dired-listing-switches
        (cond
         ((equal system-type 'gnu/linux)
          "-Ahl --group-directories-first")
         ((equal system-type 'darwin)
          (if (string-suffix-p "gls" insert-directory-program)
              "-Alh --group-directories-first"
            "-Ahl")))))


(use-package diredfl
  :ensure t
  :hook
  (dired-mode . diredfl-mode)
  :defer t)


(use-package dired-x
  :disabled
  :hook
  (dired-mode . dired-omit-mode)
  :defer t)


(use-package flycheck
  :ensure t
  :defer 2
  :config
  (global-flycheck-mode 1))


(use-package company
  :ensure t
  ;; 2022-11-06 enable lighter because it shows the backend used for
  ;; the current completion.
  ;; :bind ("<tab>" . company-indent-or-complete-common)
  :bind (:map company-active-map
              ("C-p" . company-select-previous-or-abort)
              ("C-n" . company-select-next-or-abort)
              (" "   . (lambda ()
                         "When completion is active, entering a
space aborts the completion, and inserts whatever we have
followed by a space."
                         (interactive)
                         (company-abort)
                         (self-insert-command 1))))
  :bind (:map company-search-map
              ("C-p" . company-select-previous)
              ("C-n" . company-select-next)
              ("C-f" . company-search-toggle-filtering))

  :hook
  (after-init . global-company-mode))


(use-package company-quickhelp
  :disabled ;; 2022-11-07 `company' has quick help messages in the echo area.
  :ensure t
  :after company
  :defer 2
  :config
  (company-quickhelp-mode 1))


(use-package eglot
  :disabled
  :ensure t
  :init
  (setq eglot-workspace-configuration
        '((:gopls . (:hoverKind "NoDocumentation"))))
  :hook
  ((go-mode
    c-mode
    c++-mode
    python-mode)
   . eglot-ensure)

  (eglot-managed-mode . (lambda () (flymake-mode -1)))

  :defer t

  :bind (:map eglot-mode-map
              ("C-c l d" . eglot-find-declaration)
              ("C-c l t" . eglot-find-typeDefinition)
              ("C-c l i" . eglot-find-implementation)
              ("C-c l r" . eglot-rename))

  :config
  (add-to-list 'eglot-stay-out-of 'flymake-diagnostic-functions))


;; https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/
(use-package lsp-mode
  :disabled ;; 2022-11-05 temporarily not used.
  :ensure t
  ;; :init
  ;; `lsp-mode' provides no option to complete disable the diagnostics. The
  ;; `:disabled' is not a supported option, but can disable the diagnostics.
  ;; 06/01/21 start using LSP diagnostics
  ;; (setq lsp-diagnostics-provider :disabled)
  :hook
  (go-mode
   . (lambda ()
       (unless (string-suffix-p "integration_test.go"
                                (or buffer-file-name ""))
         (lsp-deferred))))
  ((
    c-mode
    ;; c++-mode
    python-mode)
   . lsp-deferred)

  (lsp-mode . lsp-enable-which-key-integration)

  :defer t)


(use-package ccls
  :disabled ;; 04/25/21 slows down emacs when indexing ceph
  :ensure t
  :after (lsp-mode c-mode c++-mode)
  :defer t)


(use-package projectile
  :ensure t
  :preface
  (defun projectile-set-env ()
    "Set environment based on the current project."
    (unless (file-remote-p default-directory)
      (let ((proj-name (projectile-project-name))
            (proj-root (projectile-project-root)))

        (cond
         ((string-prefix-p (expand-file-name "~/go") proj-root)
          (setenv "GO111MODULE" "on")
          (setenv "GOPATH" (expand-file-name "~/go"))
          (setq-local projectile-project-type 'go)
          (setq-local projectile-project-compilation-dir
                      (file-name-directory (string-trim-left
                                            (or buffer-file-name
                                                default-directory)
                                            (regexp-quote proj-root))))))

        (message "Project: %s %s" proj-name proj-root))))

  (defun projectile-promote-mode-line ()
    "Show the project name right next to the major mode name in the
mode line."
    (let ((entry (assq 'projectile-mode minor-mode-alist)))
      (if entry
          (setq minor-mode-alist
                (assq-delete-all (car entry) minor-mode-alist)))
      (add-to-list 'minor-mode-alist entry)))

  :bind-keymap ("C-c p" . projectile-command-map)

  :commands (projectile-project-name
             projectile-project-root
             projectile-update-mode-line)

  :defer 2

  ;; If not calling `projectile-set-env' in the `find-file-hook', the
  ;; `lsp-go-directory-filters' set in that function won't be picked up by
  ;; `lsp-mode' for some reason.
  :hook
  (find-file . projectile-set-env)

  :config
  (let ((project (alist-get 'go projectile-project-types)))
    (when project
      (plist-put project 'test-command "go test -count=1 -race ./...")))

  (advice-add 'projectile-update-mode-line :after
              #'projectile-promote-mode-line)

  (projectile-mode)

  ;; Foribly update mode-line when loading. Without this, if `projectile'
  ;; loading is deferred as specified by the `:defer', the projectile lighter
  ;; on the mode-line cannot properly show the project name.
  (projectile-update-mode-line))


(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :mode ("/\\.emacs\\.d/snippets/" . snippet-mode)
  :hook ((text-mode
          prog-mode
          conf-mode
          ssh-config-mode
          protobuf-mode)
         . yas-minor-mode))


(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)


(use-package abbrev
  :diminish
  :hook
  ;; ((text-mode prog-mode) . abbrev-mode)
  (expand-load
   . (lambda ()
       (add-hook 'expand-expand-hook #'indent-according-to-mode)
       (add-hook 'expand-jump-hook #'indent-according-to-mode)))
  :commands abbrev-mode ;; `cc-mode' turns on `abbrev-mode'.

  ;; Defer does not work. It is loaded somewhere else.
  ;; 09/11/20 `enh-ruby-mode' loads `abbrev'.
  :defer t

  :config
  (if (file-exists-p abbrev-file-name)
      (quietly-read-abbrev-file)))


(use-package imenu
  :preface
  (defun imenu-rescan ()
    "Rescan the buffer to refresh the imenu index"
    (interactive)
    (imenu--menubar-select imenu--rescan-item))

  :commands (imenu-add-menubar-index
             imenu--menubar-select)

  :hook
  ((emacs-lisp-mode
    go-mode
    markdown-mode)
   . imenu-add-menubar-index))


(use-package crux
  :ensure t
  :bind (("C-c c t" . crux-transpose-windows)
         ("C-c c i" . crux-find-user-init-file)
         ("C-c c r" . crux-rename-file-and-buffer)
         ("C-c c d" . crux-delete-file-and-buffer)))


;; Do not allow the cursor to move onto the minibuffer prompt
(use-package cursor-sensor
  :init
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  :hook
  (minibuffer-setup . cursor-intangible-mode))


(use-package vertico
  :ensure t
  :hook (after-init . vertico-mode)
  :custom
  (vertico-count 20)
  (vertico-multiform-categories
   '((buffer flat (vertico-cycle . t))))
  (vertico-multiform-commands
   '((consult-imenu (completion-ignore-case . t))
     (consult-recent-file (completion-ignore-case . t))))
  :config
  (vertico-multiform-mode))


(use-package orderless
  :ensure t
  :after vertico
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))


(use-package marginalia
  :ensure t
  :after vertico
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))


(use-package consult
  :ensure t
  :after vertico
  :bind (("C-c TAB"                  . consult-imenu)
         ("C-x C-r"                  . consult-recent-file)
         ("C-M-s"                    . consult-line)
         ([remap yank-pop]           . consult-yank-replace)
         ([remap goto-line]          . consult-goto-line)
         ([remap projectile-ripgrep] . consult-ripgrep)
         )
  :bind (:map isearch-mode-map
              ("M-o" . consult-line))
  :init
  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Use `consult-completion-in-region' if Vertico is enabled.
  ;; Otherwise use the default `completion--in-region' function.
  (setq completion-in-region-function
        (lambda (&rest args)
          (apply (if vertico-mode
                     #'consult-completion-in-region
                   #'completion--in-region)
                 args)))

  (with-eval-after-load 'projectile
    (setq consult-project-root-function #'projectile-project-root))

  :config
  (consult-customize consult-ripgrep
                     consult-git-grep
                     consult-grep
                     consult-bookmark
                     consult-recent-file
                     consult-xref
                     consult--source-recent-file
                     consult--source-project-recent-file
                     consult--source-bookmark
                     :preview-key (kbd "M-.")))


(use-package embark
  :disabled ;; 2022-11-07 not used
  :ensure t
  :after vertico
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim)
   ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))


(use-package embark-consult
  :disabled ;; 2022-11-07 not used
  :ensure t
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))


(use-package ace-window
  :ensure t
  :bind ("M-o" . ace-window))


(use-package ace-link
  :ensure t
  :bind ("C-c j a" . ace-link-addr))


(use-package avy
  :ensure t
  :bind (("C-c j c" . avy-goto-char)
         ("C-c j w" . avy-goto-word-1)
         ("C-c j l" . avy-goto-line)
         ("C-c j j" . avy-resume))
  :bind (:map isearch-mode-map
              ("C-," . avy-isearch)))


(use-package compile
  :defer t
  :preface
  (defun delete-compile-windows-if-success (buffer string)
    "Delete compilation windows if succeeded without warnings."
    (when (and (buffer-live-p buffer)
               (string-match "compilation" (buffer-name buffer))
               (string-match "finished" string)
               (not (with-current-buffer buffer
                      (goto-char (point-min))
                      (search-forward "warning" nil t))))
      (run-with-timer 1 nil
                      (lambda (buf)
                        (bury-buffer buf)
                        (delete-windows-on buf))
                      buffer)))

  :hook
  (shell-mode . compilation-shell-minor-mode)

  :init
  (add-hook 'compilation-finish-functions
            #'delete-compile-windows-if-success))


(use-package comint
  :preface
  (defun comint-output-read-only (&optional _string)
    "Add to comint-output-filter-functions to make comint output read only."
    (let ((inhibit-read-only t)
          (comint-last-output-end
           (process-mark (get-buffer-process (current-buffer)))))
      (put-text-property
       comint-last-output-start comint-last-output-end 'read-only t)))

  :init
  (add-hook 'comint-output-filter-functions #'comint-truncate-buffer)
  (add-hook 'comint-output-filter-functions #'comint-output-read-only)
  (add-hook 'comint-output-filter-functions #'ansi-color-process-output)
  (add-hook 'comint-mode-hook #'ansi-color-for-comint-mode-on)

  :defer t)


(use-package term
  :hook (term-mode
         . (lambda ()
             (setq term-prompt-regexp "^[^#$]+[#$] ")
             (setq-local transient-mark-mode nil)
             (auto-fill-mode -1)))
  :defer t)


(use-package terminal-here
  :ensure t
  :if window-system
  :defer t)


(use-package cwarn
  :commands cwarn-mode
  :diminish)


(use-package ps-print
  :preface
  (defun ps-print-to-file (file-name)
    "Prints the buffer to PostScript file"
    (interactive
     (list (read-string "PostScript File: "
                        (expand-file-name
                         (format "%s.ps"
                                 (file-name-nondirectory (buffer-name)))
                         default-directory))))
    (ps-spool-buffer-with-faces)
    (with-current-buffer ps-spool-buffer-name
      (write-file file-name t)
      (kill-buffer)))

  :commands (ps-print-buffer
             ps-print-buffer-with-faces
             ps-print-region
             ps-print-region-with-faces
             ps-spool-buffer
             ps-spool-buffer-with-faces
             ps-spool-region
             ps-spool-region-with-faces))


(use-package org
  :ensure t
  :pin gnu
  :hook
  (org-mode
   . (lambda ()
       (org-indent-mode 1)
       (setq-local fill-column 120)))
  :defer t)


(use-package org-make-toc
  :ensure t
  :after org
  :hook (org-mode . org-make-toc-mode))


(use-package ox-gfm
  :ensure t
  :after org)


(use-package htmlize
  :ensure t
  :defer t)


(use-package markdown-mode
  :ensure t
  :mode (("\\.md\\'"       . gfm-mode)
         ("\\.markdown\\'" . markdown-mode))
  :hook (markdown-mode . markdown-toc-mode))


(use-package markdown-toc
  :ensure t
  :diminish
  :hook
  (markdown-mode
   . (lambda ()
       (add-hook 'before-save-hook #'markdown-toc-refresh-toc t t)))
  :defer t)


(use-package paredit
  :ensure t
  :diminish
  :bind (:map lisp-mode-map       ("<return>" . paredit-newline))
  :bind (:map emacs-lisp-mode-map ("<return>" . paredit-newline))
  :hook ((emacs-lisp-mode lisp-interaction lisp-mode) . enable-paredit-mode))


(use-package aggressive-indent
  :ensure t
  :diminish
  :hook (emacs-lisp-mode . aggressive-indent-mode))


(use-package rainbow-delimiters
  :ensure t
  :hook ((prog-mode ielm-mode) . rainbow-delimiters-mode))


(use-package highlight-escape-sequences
  :ensure t
  :hook (prog-mode . hes-mode)
  :defer t
  :config
  (put 'hes-escape-backslash-face 'face-alias 'font-lock-builtin-face)
  (put 'hes-escape-sequence-face 'face-alias 'font-lock-builtin-face)
  (push `(json-mode . ,hes-js-escape-sequence-re) hes-mode-alist)
  (push `(enh-ruby-mode . ,hes-ruby-escape-sequence-keywords) hes-mode-alist))


(use-package highlight-indent-guides
  :diminish
  :ensure t
  :init
  (setq highlight-indent-guides-method 'character)
  :hook ((yaml-mode json-mode) . highlight-indent-guides-mode))


(use-package git-link
  :ensure t
  :bind (("C-c g l" . git-link)
         ("C-c g c" . git-link-commit)
         ("C-c g h" . git-link-homepage)))


(use-package magit
  :ensure t
  :defer t
  :config
  (add-to-list 'display-buffer-alist
               '("\\`magit:"
                 (display-buffer-at-bottom)
                 (inhibit-same-window . t)
                 (window-height . 0.5)))
  (add-to-list 'display-buffer-alist
               '("\\`magit-process:"
                 (display-buffer-in-direction)
                 (direction . right))))


(use-package which-key
  :ensure t
  :diminish
  :defer 2
  :config
  (which-key-mode))


(use-package persistent-scratch
  :disabled ;; 12/18/19 it slows down Emacs startup
  :ensure t
  :demand t
  :commands persistent-scratch-setup-default
  :config
  (persistent-scratch-autosave-mode)
  (with-demoted-errors "Error: %S"
    (persistent-scratch-setup-default)))


(use-package unfill
  :ensure t
  :bind ([remap fill-paragraph] . unfill-toggle))


(use-package reformatter
  :ensure t
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*.*-format errors\\*\\'"
                 (display-buffer-at-bottom)
                 (inhibit-same-window . t)
                 (window-height lambda
                                (w)
                                (fit-window-to-buffer w
                                                      (/
                                                       (frame-height)
                                                       2)
                                                      10))))

  ;; Using `reformatter-define' will cause `:config' to run even though it
  ;; should be deferred due to the presence of `:hook'.
  (reformatter-define json-format
    :program "jq"
    :args '("--monochrome-output" "--indent" "2"))
  (reformatter-define nxml-format
    :program "tidy"
    :args '("-indent" "-wrap" "0" "-omit" "-quiet" "-utf8" "-xml"))
  (reformatter-define python-format
    :program "black"
    :args '("-"))
  (reformatter-define jsonnet-format
    :program "jsonnetfmt"
    :args '("-"))
  (reformatter-define terraform-format
    :program "terraform"
    :args '("fmt" "-no-color" "-"))

  :hook
  (json-mode
   . (lambda ()
       (bind-key "<f12>" #'json-format-buffer json-mode-map)))
  (nxml-mode
   . (lambda ()
       (bind-key "<f12>" #'nxml-format-buffer nxml-mode-map)))
  (python-mode
   . (lambda ()
       (bind-key "<f12>" #'python-format-buffer python-mode-map)))
  (jsonnet-mode
   . (lambda ()
       (bind-key "<f12>" #'jsonnet-format-buffer jsonnet-mode-map)))
  (terraform-mode
   . (lambda ()
       (bind-key "<f12>" #'terraform-format-buffer terraform-mode-map))))


(use-package prog-mode
  :preface
  (defun indent-delete-trailing-whitespace (&optional beg end)
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

  :hook
  (prog-mode
   . (lambda ()
       (setq-local comment-auto-fill-only-comments t)
       (font-lock-add-keywords
        nil '(("\\<\\(FIXME\\|DEBUG\\|TODO\\):"
               1 font-lock-warning-face prepend)))))

  :bind (:map prog-mode-map
              ("<f12>" . indent-delete-trailing-whitespace))
  :defer t)


(use-package conf-mode
  :hook
  (conf-mode
   . (lambda ()
       (electric-indent-local-mode -1)
       ;; Just change the value of `indent-line-function' to the
       ;; `insert-tab' function and configure tab insertion as 4 spaces.
       ;; https://stackoverflow.com/a/1819405
       (setq-local indent-line-function #'insert-tab)))

  :mode
  (("/\\.htaccess\\'"        . conf-unix-mode)
   ("/\\.tmux\\.conf\\'"     . conf-unix-mode)
   ("/\\.aws/credentials\\'" . conf-unix-mode)
   ("/\\.properties\\'"      . conf-javaprop-mode)))


(use-package lisp-mode
  :defer t
  :hook
  (emacs-lisp-mode
   . (lambda ()
       (add-hook 'after-save-hook #'check-parens nil t))))


(use-package make-mode
  :mode ("/Makefile\\..*" . makefile-gmake-mode)
  :hook
  (makefile-mode . (lambda () (setq-local indent-tabs-mode t)))
  :defer t)


(use-package sh-script
  :mode ("\\.zsh_custom\\'" . sh-mode)
  :defer t)


(use-package cc-mode
  :bind
  (:map c-mode-base-map
        ("M-q" . c-fill-paragraph))

  :mode (("\\.h\\(h?\\|xx\\|pp\\)\\'" . c++-mode)
         ("\\.m\\'"                   . c-mode)
         ("\\.mm\\'"                  . c++-mode))
  :hook
  (c-mode-common . (lambda () (cwarn-mode 1)))
  (c++-mode . (lambda () (c-set-style "clang")))

  :defer t

  :config
  (unbind-key "C-c C-c" c++-mode-map)

  (add-to-list
   'c-style-alist
   '("clang"
     (indent-tabs-mode . nil)
     (c-basic-offset . 2)
     (c-comment-only-line-offset . (0 . 0))
     (c-hanging-braces-alist
      . ((substatement-open before after)
         (arglist-cont-nonempty)))
     (c-offsets-alist
      . ((statement-block-intro . +)
         (knr-argdecl-intro . 5)
         (substatement-open . 0)
         (substatement-label . 0)
         (label . 0)
         (case-label . 0)
         (statement-case-open . 0)
         (statement-cont . +)
         (arglist-intro . +)
         (arglist-close . +)
         (inline-open . 0)
         (brace-list-open . 0)
         (innamespace . 0)
         (topmost-intro-cont
          . (first c-lineup-topmost-intro-cont
                   c-lineup-gnu-DEFUN-intro-cont))))
     (c-special-indent-hook . c-gnu-impose-minimum)
     (c-block-comment-prefix . ""))))


(use-package python
  :interpreter ("python" . python-mode)
  :mode ("\\.pyx?\\'" . python-mode)
  :defer t)


(use-package rpm-spec-mode
  :ensure t
  :defer t)


(use-package nxml-mode
  :hook
  (nxml-mode
   . (lambda ()
       (setq-local indent-line-function #'nxml-indent-line)
       (when (and
              buffer-file-name
              (file-exists-p buffer-file-name)
              (string= (file-name-extension
                        (file-name-nondirectory buffer-file-name))
                       "plist"))
         (setq-local indent-tabs-mode t)
         (setq-local nxml-child-indent 4)
         (setq-local nxml-outline-child-indent 4))))

  :defer t

  :config
  (require 'sgml-mode))


(use-package applescript-mode
  :ensure t
  :bind (:map as-mode-map
              ("<tab>" . tab-to-tab-stop))
  :defer t)


(use-package js
  :if (>= emacs-major-version 27)
  :mode ("\\.pac\\'" . js-mode)
  :mode ("\\.jsx?\\'" . js-jsx-mode)
  :defer t)


(use-package ruby-mode
  :disabled
  :interpreter "ruby"
  :defer t)


(use-package enh-ruby-mode
  :ensure t
  :mode "\\(?:\\.rb\\|ru\\|rake\\|thor\\|jbuilder\\|gemspec\\|podspec\\|/\\(?:Gem\\|Rake\\|Cap\\|Thor\\|Vagrant\\|Guard\\|Pod\\)file\\)\\'"
  :interpreter "ruby"
  :hook
  (enh-ruby-mode . (lambda () (setq-local tab-width 2)))
  :defer t)


(use-package rspec-mode
  :ensure t
  :diminish
  :defer t)


(use-package lua-mode
  :ensure t
  :interpreter ("lua" . lua-mode)
  :mode "\\.lua\\.erb\\'"
  :defer t)


(use-package go-mode
  :ensure t
  :bind
  (:map go-mode-map ("<f12>" . gofmt))
  :hook
  (go-mode
   . (lambda ()
       (setq-local indent-tabs-mode t)
       (setq-local tab-width 4)
       (add-hook 'before-save-hook #'gofmt t t)))
  :defer t

  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*\\(Gofmt Errors\\|go-rename\\)\\*\\'"
                 (display-buffer-at-bottom)
                 (inhibit-same-window . t)
                 (window-height lambda
                                (w)
                                (fit-window-to-buffer w
                                                      (/
                                                       (frame-height)
                                                       2)
                                                      10)))))


(use-package go-rename
  :ensure t
  :after go-mode)


(use-package vcl-mode
  :ensure t
  :mode "\\.vcl\\.erb\\'"
  :defer t)


(use-package vimrc-mode
  :ensure t
  :defer t)


(use-package systemd
  :ensure t
  :magic ("\\[Unit\\]" . systemd-mode)
  :defer t)


(use-package upstart-mode
  :load-path "lisp/upstart-mode"
  :pin manual
  :commands upstart-mode
  :mode "\\.upstart\\'")


(use-package yaml-mode
  :ensure t
  :mode "/\\.config/yamllint/config\\'"
  :mode "/\\(group\\|host\\)_vars/"
  :mode "/\\.flyrc\\'"
  :mode "/\\.clang-format\\'"
  :defer t)


(use-package terraform-mode
  :ensure t
  :hook (terraform-mode . turn-off-auto-fill)
  :defer t)


(use-package protobuf-mode
  :ensure t
  :hook
  (protobuf-mode
   . (lambda ()
       (c-add-style "my-protobuf-style"
                    '((c-basic-offset   . 2)
                      (indent-tabs-mode . nil))
                    t)))

  :defer t)


(use-package json-mode
  :ensure t
  :hook
  (json-mode . (lambda () (setq-local tab-width 2)))
  :defer t)


(use-package ovpn-mode
  :ensure t
  :defer t)


(use-package jsonnet-mode
  :ensure t
  :bind (:map jsonnet-mode-map
              ("M-." . jsonnet-jump))
  :mode "\\.\\(jsonnet\\|libsonnet\\)\\'"
  :defer t
  :config
  ;; Unbind non-standard keybindings for `jsonnet-jump' and
  ;; `jsonnet-reformat-buffer'
  (unbind-key "C-c C-f" jsonnet-mode-map)
  (unbind-key "C-c C-r" jsonnet-mode-map))


(use-package jinja2-mode
  :ensure t
  :defer t)


(use-package dockerfile-mode
  :ensure t
  :defer t)


(use-package syslog-mode
  :ensure t
  :mode "\\`/var/log/\\(syslog\\|messages\\|system\\.log\\)\\'")


(use-package ssh-config-mode
  :ensure t
  :defer t
  :hook
  (ssh-config-mode
   . (lambda ()
       (setq-local indent-line-function #'indent-relative))))


(use-package cmake-mode
  :ensure t
  :defer t)


(use-package jenkinsfile-mode
  :ensure t
  :defer t)


(use-package zenburn-theme
  :disabled
  :ensure t
  :init
  (unless (display-graphic-p)
    (setq zenburn-override-colors-alist
          '(("zenburn-bg" . "unspecified-bg")
            ("zenburn-fg" . "unspecified-fg"))))
  :config
  (load-theme 'zenburn t)
  ;; (zenburn-with-color-variables
  ;;  (custom-set-faces
  ;;   '(eglot-mode-line ((t (:inherit font-lock-constant-face :weight normal))))))
  )


(use-package color-theme-sanityinc-tomorrow
  :ensure t
  :config
  (load-theme 'sanityinc-tomorrow-night t)
  ;; (set-face-attribute 'font-lock-comment-delimiter-face nil :slant 'normal)
  ;; (set-face-attribute 'font-lock-comment-face nil :slant 'normal)
  ;; (unless (display-graphic-p)
  ;;   (set-face-attribute 'default nil :background "unspecified-bg"))
  )


(when (display-graphic-p)
  (defsubst display-name (&optional frame)
    (pcase (sort (frame-monitor-attribute 'mm-size frame) '>)
      ('(599 340) 'lg-ultrafine-27)
      ('(596 335) 'dell-up2716d)
      ('(330 206) 'macbook-pro-15)
      ('(344 214) 'macbook-pro-16)))

  (defsubst font-Menlo (size)
    (format "-*-Menlo-normal-normal-normal-*-%d-*-*-*-m-0-iso10646-1"
            size))

  (defsubst font-SFMono (size)
    (format "-*-SF Mono-normal-normal-normal-*-%d-*-*-*-m-0-iso10646-1"
            size))

  (defsubst font-UbuntuMono (size)
    (format "-*-Ubuntu Mono-normal-normal-normal-*-%d-*-*-*-m-0-iso10646-1"
            size))

  (defsubst font-Hack (size)
    (format "-*-Hack-normal-normal-normal-*-%d-*-*-*-m-0-iso10646-1"
            size))

  (defconst STHeiTi-size-map
    `((,(font-Menlo 12)  . 14.0)
      (,(font-Menlo 13)  . 16.0)
      (,(font-Menlo 14)  . 16.0)
      (,(font-Menlo 15)  . 18.0)
      (,(font-Menlo 16)  . 20.0)
      (,(font-Menlo 17)  . 20.0)
      (,(font-Menlo 18)  . 22.0)
      (,(font-Menlo 19)  . 22.0)
      (,(font-Menlo 20)  . 24.0)
      (,(font-Menlo 21)  . 26.0)
      (,(font-Menlo 22)  . 26.0)
      (,(font-SFMono 12) . 12.0)
      (,(font-SFMono 13) . 12.0)
      (,(font-SFMono 14) . 14.0)
      (,(font-SFMono 15) . 16.0)
      (,(font-SFMono 16) . 16.0)
      (,(font-SFMono 17) . 18.0)
      (,(font-SFMono 18) . 18.0)
      (,(font-SFMono 19) . 18.0)
      (,(font-SFMono 20) . 20.0)
      (,(font-SFMono 21) . 20.0)
      (,(font-SFMono 22) . 20.0)
      (,(font-Hack 12)   . 12.0)
      (,(font-Hack 13)   . 12.0)
      (,(font-Hack 14)   . 14.0)
      (,(font-Hack 15)   . 16.0)
      (,(font-Hack 16)   . 16.0)
      (,(font-Hack 17)   . 18.0)
      (,(font-Hack 18)   . 18.0)
      (,(font-Hack 19)   . 18.0)
      (,(font-Hack 20)   . 20.0)
      (,(font-Hack 21)   . 20.0)
      (,(font-Hack 22)   . 20.0))
    "Font mapping to corresponding STHeiTi size.")

  (defconst default-font-size 13
    "Default font size")

  (defun set-emacs-frame (&optional size)
    (interactive "nFont Size: ")
    (setq size (or size default-font-size))
    (if (or (< size 12) (> size 22))
        (user-error "Font size must be in the range [12, 22]"))

    (set-face-attribute 'default nil :font (font-Hack size))

    (when (equal system-type 'darwin)
      (dolist (charset '(han cjk-misc bopomofo))
        (set-fontset-font t charset (font-spec :family "STHeiTi")))
      (setq face-font-rescale-alist
            `(("STHeiTi" . ,(/ (cdr (assoc (frame-parameter nil 'font)
                                           STHeiTi-size-map))
                               size))))
      (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji"))
      ;; `variable-pitch' face uses the same height as the `default'.
      ;; https://protesilaos.com/codelog/2020-09-05-emacs-note-mixed-font-heights/
      (set-face-attribute 'variable-pitch nil :family "Palatino" :height 1.0))

    (set-frame-parameter nil 'fullscreen 'maximized))

  (add-hook 'after-init-hook #'set-emacs-frame t)
  (bind-key* "s-`" #'set-emacs-frame))


;;
;; Configure system trash
;;
(when (equal system-type 'darwin)
  (if (executable-find "trash")
      (defun system-move-file-to-trash (file)
        "Use \"trash\" to move FILE to the MacOS Trash."
        (call-process "trash" nil 0 nil
                      file))
    (setq trash-directory "~/.Trash")))


;;
;; Configure additional keybindings
;;
(bind-key "<C-M-backspace>" #'backward-kill-sexp)
(when (display-graphic-p) (unbind-key "C-z"))


;; Display elapsed time
;; https://github.com/jwiegley/dot-emacs/blob/master/init.el
(let ((elapsed (float-time (time-subtract (current-time)
                                          emacs-start-time))))
  (message "Loading %s...done (%.3fs) (GC: %d)"
           load-file-name elapsed gcs-done))

(add-hook 'after-init-hook
          `(lambda ()
             (let ((elapsed
                    (float-time
                     (time-subtract (current-time) emacs-start-time))))
               (message "Loading %s...done (%.3fs) [after-init] (GC: %d)"
                        ,load-file-name elapsed gcs-done))) t)
