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
;; Initialize ELPA
;;
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'init-elpa)


;;
;; Initialize use-package
;; https://github.com/jwiegley/use-package
;;
(setq use-package-enable-imenu-support t
      use-package-verbose t)

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


(use-package emacs
  :custom
  (ns-alternate-modifier 'super)
  (ns-command-modifier 'meta)
  (confirm-kill-emacs 'yes-or-no-p)
  (delete-by-moving-to-trash t)
  (directory-free-space-args "-Pkh")
  (enable-recursive-minibuffers t)
  (fill-column 78)
  (frame-resize-pixelwise t)
  (history-delete-duplicates t)
  (indent-tabs-mode nil)
  (inhibit-startup-screen t)
  (initial-major-mode 'fundamental-mode)
  (initial-scratch-message nil)
  (large-file-warning-threshold 50000000)
  (make-backup-files nil)
  (mark-ring-max 100)
  (max-lisp-eval-depth 2000)
  (max-specpdl-size 16384)
  (message-log-max 16384)
  (mode-require-final-newline t)
  (require-final-newline t)
  (ring-bell-function 'ignore)
  (tab-always-indent 'complete)
  (tab-width 4)
  (undo-limit 1000000)
  (use-dialog-box nil)
  (use-file-dialog nil)
  (use-short-answers t)
  (x-stretch-cursor t)
  :config
  (add-hook 'after-save-hook
            #'executable-make-buffer-file-executable-if-script-p)
  :custom-face
  (aw-leading-char-face
   ((t (:inherit aw-leading-char-face :weight bold :height 3.0)))))


(use-package mouse
  :defer t
  :custom
  (mouse-drag-copy-region t)
  (mouse-yank-at-point t))


(use-package mwheel
  :defer t
  :custom
  (mouse-wheel-progressive-speed nil))


(use-package window
  :defer t
  :custom
  (scroll-error-top-bottom t))


(use-package delsel
  :defer t
  :custom
  (delete-selection-mode t))


(use-package cus-edit
  :disabled
  :defer t
  :init
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (setq custom-buffer-done-kill t)
  (load custom-file)
  :config
  (let ((elapsed (float-time (time-subtract (current-time)
                                            emacs-start-time))))
    (message "Loading %s (source)...done (%.3fs) (GC: %d)"
             custom-file elapsed gcs-done)))


(use-package image-file
  :defer t
  :custom
  (auto-image-file-mode t))


(use-package font-core
  :defer t
  :custom
  (global-font-lock-mode t))


(use-package frame
  :defer t
  :custom
  (blink-cursor-mode nil))


(use-package isearch
  :no-require t
  :bind (:map isearch-mode-map
              ;; DEL during isearch should edit the search string, not jump
              ;; back to the previous result
              ;; https://github.com/purcell/emacs.d/blob/b484cada4356803d0ecb063d33546835f996fefe/lisp/init-isearch.el#L14
              ([remap isearch-delete-char] . isearch-del-char))
  :custom
  (isearch-allow-scroll t)
  (search-highlight t))


(use-package replace
  :defer t
  :custom
  (list-matching-lines-default-context-lines 3)
  (query-replace-highlight t))


(use-package select
  :defer t
  :custom
  (select-enable-clipboard t))


(use-package uniquify
  :defer t
  :custom
  (uniquify-after-kill-buffer-p t)
  (uniquify-buffer-name-style
   'post-forward-angle-brackets nil (uniquify)))


(use-package hl-line
  :defer t
  :hook ((dired-mode
          magit-mode
          occur-mode
          ibuffer-mode
          compilation-mode
          gnus-mode) . hl-line-mode))


(use-package goto-addr
  :defer t
  :hook
  (prog-mode . goto-address-prog-mode)
  ((text-mode magit-process-mode) . goto-address-mode))


(use-package display-fill-column-indicator
  :defer t
  :hook
  (auto-fill-mode
   . (lambda ()
       (display-fill-column-indicator-mode
        (if auto-fill-function 1 -1)))))


(use-package simple
  :defer t
  :diminish auto-fill-function
  :hook ((org-mode
          markdown-mode
          prog-mode
          protobuf-mode) . turn-on-auto-fill)
  :hook (((occur-mode
           dired-mode
           speedbar-mode
           compilation-mode) . (lambda () (visual-line-mode -1))))
  :custom
  (global-mark-ring-max 500)
  (kill-do-not-save-duplicates t)
  (kill-ring-max 1000000)
  (kill-whole-line nil)
  (next-line-add-newlines nil)
  (size-indication-mode t)
  (line-number-mode t)
  (transient-mark-mode t)
  (column-number-mode t))


(use-package ns-win
  :defer t
  :custom
  (ns-pop-up-frames nil))


(use-package subword
  :defer t
  :diminish
  :custom
  (global-subword-mode t))


(use-package xref
  :defer t
  :commands (xref-pulse-momentarily
             xref-push-marker-stack)
  :custom
  (xref-prompt-for-identifier nil))


(use-package autorevert
  :defer 2
  :custom
  (auto-revert-verbose t)
  (global-auto-revert-non-file-buffers t)
  :config
  (global-auto-revert-mode t))


(use-package time
  :defer 2
  :custom
  (display-time-day-and-date nil)
  :config
  (display-time-mode t))


(use-package so-long
  :defer 2
  :config
  (global-so-long-mode t))


(use-package eldoc
  ;; Loaded by something else; defer 2 doesn't work.
  :defer 2
  :diminish
  :hook
  (prog-mode . global-eldoc-mode)
  :config
  (global-eldoc-mode t))


(use-package which-func
  :defer 2
  :custom
  (which-func-unknown "n/a")
  :hook
  (prog-mode . which-function-mode)
  :config
  (which-function-mode t))


(use-package electric
  ;; Loaded by something else; defer 2 doesn't work.
  :defer 2
  :hook
  (prog-mode . electric-indent-mode)
  :config
  (electric-indent-mode t))


(use-package elec-pair
  :defer 2
  :custom
  (electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
  :hook
  (prog-mode . electric-pair-mode)
  :config
  (electric-pair-mode t))


(use-package ediff-wind
  :defer t
  :custom
  (ediff-split-window-function 'split-window-horizontally)
  (ediff-window-setup-function 'ediff-setup-windows-plain))


(use-package gnutls
  :defer t
  :custom
  (gnutls-algorithm-priority "SECURE192:+SECURE128:-VERS-ALL:+VERS-TLS1.2")
  (gnutls-min-prime-bits 4096)
  (gnutls-verify-error t))


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
  :defer 2
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

  :config
  (winner-mode))


(use-package midnight
  :defer 2
  :config
  (midnight-mode))


(use-package repeat
  :defer t
  :custom
  (repeat-exit-key [return])
  (repeat-mode t))


(use-package paren
  :defer t
  :custom
  (show-paren-delay 0)
  (show-paren-mode t)
  (show-paren-style 'parentheses))


(use-package recentf
  :defer 2
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

  :custom
  (recentf-auto-cleanup 60)
  (recentf-exclude
   '("\\`out\\'"
     "\\.log\\'"
     "\\.el\\.gz\\'"
     "/\\.emacs\\.d/elpa/.*-\\(autoloads\\|pkg\\)\\.el\\'"
     "/\\.emacs\\.d/\\(auto-save-list\\|elfeed\\|package-upgrade-check-epoch\\|projects\\|recentf\\|snippets\\|tramp\\|var\\)"
     "/\\.git/COMMIT_EDITMSG\\'"))
  (recentf-filename-handlers '(abbreviate-file-name))
  (recentf-max-saved-items 2000)

  :hook
  (dired-mode . recentf-add-dired-directory)

  :commands (recentf-mode
             recentf-add-file
             recentf-save-list
             recentf-string-member)

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
  :defer t
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
         . whitespace-mode)
  :custom
  (whitespace-line-column 100)
  (whitespace-style '(face trailing tabs)))


(use-package ispell
  :defer t
  :bind ("C-c i w" . ispell-word)
  :custom
  (ispell-extra-args
   '("--sug-mode=normal" "--ignore=2" "--run-together" "--camel-case"))
  (ispell-local-dictionary
   "en_US")
  (ispell-personal-dictionary
   "~/.emacs.d/ispell-personal-dictionary")
  (ispell-silently-savep t))


(use-package flyspell
  :bind ("C-c i b" . flyspell-buffer)
  ;; :hook
  ;; (text-mode . flyspell-mode)
  :custom
  (flyspell-sort-corrections t)
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

  :custom
  (ibuffer-filter-group-name-face 'font-lock-doc-face)
  (ibuffer-formats
   '((mark modified read-only vc-status-mini " "
           (name 32 32 :left :elide)
           " "
           (size-h 9 -1 :right)
           " "
           (mode 16 16 :left :elide)
           " "
           (vc-status 16 -1 :left)
           " " filename-and-process)))

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
  :defer t
  :diminish hs-minor-mode
  :hook (prog-mode . hs-minor-mode))


(use-package diff-hl
  :ensure t
  :defer t
  :hook ((prog-mode
          conf-mode
          text-mode
          protobuf-mode
          ssh-config-mode)
         . diff-hl-mode)
  (magit-pre-refresh . diff-hl-magit-pre-refresh)
  (magit-post-refresh . diff-hl-magit-post-refresh)
  :custom
  (diff-hl-draw-borders nil))


(use-package tramp-sh
  :defer t
  :init
  ;; https://www.gnu.org/software/emacs/manual/html_node/tramp/Frequently-Asked-Questions.html
  (setq
   vc-ignore-dir-regexp
   (format "\\(%s\\)\\|\\(%s\\)"
           vc-ignore-dir-regexp
           tramp-file-name-regexp)
   tramp-connection-timeout 15
   tramp-default-method "ssh"
   tramp-use-ssh-controlmaster-options nil))


(use-package rg
  :ensure t
  :defer t
  :bind ("C-c s" . rg-menu)
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*rg\\*\\'"
                 (display-buffer-at-bottom)
                 (inhibit-same-window . t)
                 (window-height . 0.5))))


(use-package dumb-jump
  :ensure t
  :defer t
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  :custom
  (dumb-jump-force-searcher 'rg))


(use-package hippie-exp
  :bind ("M-/" . hippie-expand)
  :defer t
  :config
  (advice-add 'hippie-expand :around
              (lambda (func &rest args)
                "Make `hippie-expand' do case-sensitive
expanding. Though not all, this is effective with some expand
functions, eg., `try-expand-all-abbrevs'"
                (let ((case-fold-search nil))
                  (apply func args)))))


(use-package dired
  :defer t
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

  (setq
   dired-listing-switches
   (cond
    ((equal system-type 'gnu/linux)
     "-Ahl --group-directories-first")
    ((equal system-type 'darwin)
     (if (string-suffix-p "gls" insert-directory-program)
         "-Alh --group-directories-first"
       "-Ahl")))
   dired-dwim-target t
   dired-isearch-filenames 'dwim
   dired-omit-files "^\\.?#\\|^\\.$\\|^\\.\\.$\\|^\\..+$"
   dired-omit-size-limit nil
   dired-omit-verbose nil
   dired-recursive-copies 'always
   dired-recursive-deletes 'always))


(use-package diredfl
  :ensure t
  :defer t
  :hook
  (dired-mode . diredfl-mode))


(use-package dired-x
  :disabled
  :defer t
  :hook
  (dired-mode . dired-omit-mode))


(use-package flycheck
  :ensure t
  :defer 2
  :custom
  (flycheck-disabled-checkers
   '(c/c++-cppcheck
     c/c++-gcc
     go-build
     go-errcheck
     go-gofmt
     go-golint
     go-staticcheck
     go-test
     go-unconvert
     go-vet
     json-jsonlint
     json-python-json
     python-mypy
     python-pycompile
     python-pyright
     ruby-rubocop
     sh-bash
     sh-posix-bash
     sh-posix-dash
     sh-zsh
     yaml-jsyaml
     yaml-ruby
     emacs-lisp-checkdoc))
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
  (after-init . global-company-mode)

  :custom
  (company-backends
   '((company-capf company-dabbrev-code :separate)
     company-files company-keywords company-dabbrev))
  (company-dabbrev-downcase nil)
  (company-dabbrev-minimum-length 2)
  (company-minimum-prefix-length 2)
  (company-selection-wrap-around t)
  (company-transformers '(delete-dups)))


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

  :custom
  (eglot-autoshutdown t)
  (eglot-events-buffer-size 0)
  (eglot-ignored-server-capabilites
   '(:documentHighlightProvider
     :codeActionProvider
     :codeLensProvider
     :documentFormattingProvider
     :documentRangeFormattingProvider
     :documentOnTypeFormattingProvider
     :documentLinkProvider))

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
  :defer t
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

  :custom
  (lsp-enable-dap-auto-configure nil)
  (lsp-enable-file-watchers nil)
  (lsp-enable-folding nil)
  (lsp-enable-indentation nil)
  (lsp-enable-links nil)
  (lsp-enable-on-type-formatting nil)
  (lsp-enable-symbol-highlighting nil)
  (lsp-enable-text-document-color nil)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-keep-workspace-alive nil)
  (lsp-lens-enable nil)
  (lsp-modeline-code-actions-enable nil)
  (lsp-modeline-diagnostics-enable nil)
  (lsp-progress-function 'lsp-on-progress-legacy)
  (lsp-signature-auto-activate nil)
  (lsp-signature-render-documentation nil))


(use-package ccls
  :disabled ;; 04/25/21 slows down emacs when indexing ceph
  :ensure t
  :after (lsp-mode c-mode c++-mode)
  :defer t)


(use-package projectile
  :ensure t
  :defer 2
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

  ;; If not calling `projectile-set-env' in the `find-file-hook', the
  ;; `lsp-go-directory-filters' set in that function won't be picked up by
  ;; `lsp-mode' for some reason.
  :hook
  (find-file . projectile-set-env)

  :custom
  (projectile-enable-caching t)
  (projectile-file-exists-local-cache-expire 300)
  (projectile-files-cache-expire 259200)
  (projectile-find-dir-includes-top-level t)
  (projectile-mode-line-prefix " ")
  (projectile-sort-order 'recentf)
  (projectile-switch-project-action 'projectile-dired)

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
  :defer t
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
  ;; Defer does not work. It is loaded somewhere else.
  ;; 09/11/20 `enh-ruby-mode' loads `abbrev'.
  :defer t
  :diminish
  :hook
  ;; ((text-mode prog-mode) . abbrev-mode)
  (expand-load
   . (lambda ()
       (add-hook 'expand-expand-hook #'indent-according-to-mode)
       (add-hook 'expand-jump-hook #'indent-according-to-mode)))
  :commands abbrev-mode ;; `cc-mode' turns on `abbrev-mode'.

  :custom
  (save-abbrevs 'silently)

  :config
  (if (file-exists-p abbrev-file-name)
      (quietly-read-abbrev-file)))


(use-package imenu
  :defer t
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
   . imenu-add-menubar-index)

  :custom
  (imenu-auto-rescan t)
  (imenu-auto-rescan-maxout 600000)
  (imenu-max-item-length "Unlimited"))


(use-package crux
  :ensure t
  :defer t
  :bind (("C-c c t" . crux-transpose-windows)
         ("C-c c i" . crux-find-user-init-file)
         ("C-c c r" . crux-rename-file-and-buffer)
         ("C-c c d" . crux-delete-file-and-buffer)))


;; Prevent cursor from moving onto the minibuffer prompt
(use-package cursor-sensor
  :defer t
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
  (vertico-multiform-commands
   '((consult-yank-pop indexed)
     (consult-yank-replace indexed)
     (consult-imenu (completion-ignore-case . t))
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
  (require 'recentf)
  (consult-customize consult-ripgrep
                     consult-git-grep
                     consult-grep
                     consult-bookmark
                     consult-recent-file
                     consult-xref
                     consult--source-recent-file
                     consult--source-project-recent-file
                     consult--source-bookmark
                     :preview-key "M-."))


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
  :defer t
  :bind ("M-o" . ace-window)
  :custom
  (aw-scope 'frame))


(use-package ace-link
  :ensure t
  :defer t
  :bind ("C-c j a" . ace-link-addr))


(use-package avy
  :ensure t
  :defer t
  :bind (("C-c j c" . avy-goto-char)
         ("C-c j w" . avy-goto-word-1)
         ("C-c j l" . avy-goto-line)
         ("C-c j j" . avy-resume))
  :bind (:map isearch-mode-map
              ("C-," . avy-isearch))
  :custom
  (avy-case-fold-search t))


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
            #'delete-compile-windows-if-success)

  :custom
  (compilation-always-kill t)
  (compilation-context-lines 10)
  (compilation-scroll-output t))


(use-package comint
  :defer t
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

  :custom
  (comint-buffer-maximum-size 16384)
  (comint-completion-addsuffix t)
  (comint-input-ignoredups t)
  (comint-input-ring-size 16384)
  (comint-move-point-for-output nil)
  (comint-prompt-read-only t)
  (comint-scroll-show-maximum-output t)
  (comint-scroll-to-bottom-on-input t))


(use-package term
  :defer t
  :hook (term-mode
         . (lambda ()
             (setq term-prompt-regexp "^[^#$]+[#$] ")
             (setq-local transient-mark-mode nil)
             (auto-fill-mode -1))))


(use-package terminal-here
  :ensure t
  :if window-system
  :defer t
  :custom
  (terminal-here-mac-terminal-command 'iterm2))


(use-package cwarn
  :commands cwarn-mode
  :diminish)


(use-package ps-print
  :defer t
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
             ps-spool-region-with-faces)

  :custom
  (ps-bottom-margin 19.84251968503937)
  (ps-font-family 'Courier)
  (ps-font-size 6.0)
  (ps-header-font-family 'Courier)
  (ps-header-font-size 8.0)
  (ps-header-line-pad 0.25)
  (ps-header-lines 1)
  (ps-header-offset 14.173228346456693)
  (ps-header-title-font-size 8.0)
  (ps-inter-column 19.84251968503937)
  (ps-landscape-mode t)
  (ps-left-margin 19.84251968503937)
  (ps-lpr-switches '("-o sides=two-sided-short-edge"))
  (ps-number-of-columns 2)
  (ps-paper-type 'letter)
  (ps-print-color-p 'black-white)
  (ps-print-footer nil)
  (ps-print-header t)
  (ps-print-header-frame t)
  (ps-printer-name "HP LaserJet Professional P 1102w")
  (ps-printer-name-option "-P")
  (ps-right-margin 19.84251968503937)
  (ps-show-n-of-n t)
  (ps-spool-duplex t)
  (ps-top-margin 19.84251968503937)
  (ps-warn-paper-type t))


(use-package org
  :ensure t
  :defer t
  :pin gnu
  :hook
  (org-mode
   . (lambda ()
       (org-indent-mode 1)
       (setq-local fill-column 120)))
  :custom
  (org-catch-invisible-edits t)
  (org-export-default-language "en")
  (org-export-with-creator t)
  (org-export-with-section-numbers nil)
  (org-export-with-sub-superscripts '{})
  (org-export-with-toc nil)
  (org-hide-leading-stars t)
  (org-html-extension "html")
  (org-html-htmlize-output-type 'inline-css)
  (org-image-actual-width nil)
  (org-log-done 'time)
  (org-src-fontify-natively t)
  (org-startup-folded "nofold"))


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
  :hook (markdown-mode . markdown-toc-mode)
  :custom
  (markdown-command "multimarkdown")
  (markdown-nested-imenu-heading-index nil)
  (markdown-toc-header-toc-title "# Table of Contents"))


(use-package markdown-toc
  :ensure t
  :defer t
  :diminish
  :hook
  (markdown-mode
   . (lambda ()
       (add-hook 'before-save-hook #'markdown-toc-refresh-toc t t)))
  :defer t)


(use-package paredit
  :disabled
  :ensure t
  :diminish
  :bind (:map lisp-mode-map       ("<return>" . paredit-newline))
  :bind (:map emacs-lisp-mode-map ("<return>" . paredit-newline))
  :hook ((emacs-lisp-mode lisp-interaction lisp-mode) . enable-paredit-mode))


(use-package aggressive-indent
  :ensure t
  :defer t
  :diminish
  :hook (emacs-lisp-mode . aggressive-indent-mode)
  :custom
  (aggressive-indent-dont-indent-if
   '((string-match "^\\s-+$" (thing-at-point 'line)))))


(use-package rainbow-delimiters
  :ensure t
  :defer t
  :hook ((prog-mode ielm-mode) . rainbow-delimiters-mode))


(use-package highlight-escape-sequences
  :ensure t
  :defer t
  :hook (prog-mode . hes-mode)
  :config
  (put 'hes-escape-backslash-face 'face-alias 'font-lock-builtin-face)
  (put 'hes-escape-sequence-face 'face-alias 'font-lock-builtin-face)
  (push `(json-mode . ,hes-js-escape-sequence-re) hes-mode-alist)
  (push `(enh-ruby-mode . ,hes-ruby-escape-sequence-keywords) hes-mode-alist))


(use-package highlight-indent-guides
  :ensure t
  :defer t
  :diminish
  :init
  (setq highlight-indent-guides-method 'character)
  :hook ((yaml-mode json-mode) . highlight-indent-guides-mode))


(use-package git-link
  :ensure t
  :defer t
  :bind (("C-c g l" . git-link)
         ("C-c g c" . git-link-commit)
         ("C-c g h" . git-link-homepage))
  :custom
  (git-link-use-commit t))


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
                 (direction . right)))
  :custom
  (magit-commit-show-diff nil))


(use-package which-key
  :ensure t
  :defer 2
  :diminish
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
  :defer t
  :bind ([remap fill-paragraph] . unfill-toggle))


(use-package reformatter
  :ensure t
  :defer t
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
    :args '("." "--monochrome-output" "--indent" "2"))
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
  :defer t
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
              ("<f12>" . indent-delete-trailing-whitespace)))


(use-package conf-mode
  :defer t
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
  :defer t
  :mode ("/Makefile\\..*" . makefile-gmake-mode)
  :hook
  (makefile-mode . (lambda () (setq-local indent-tabs-mode t))))


(use-package sh-script
  :mode ("\\.zsh_custom\\'" . sh-mode)
  :defer t
  :custom
  (sh-basic-offset 2))


(use-package cc-mode
  :defer t
  :bind
  (:map c-mode-base-map
        ("M-q" . c-fill-paragraph))

  :mode (("\\.h\\(h?\\|xx\\|pp\\)\\'" . c++-mode)
         ("\\.m\\'"                   . c-mode)
         ("\\.mm\\'"                  . c++-mode))
  :hook
  (c-mode-common . (lambda () (cwarn-mode 1)))
  (c++-mode . (lambda () (c-set-style "clang")))

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
  :defer t
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

  :custom
  (nxml-child-indent 2)
  (nxml-outline-child-indent 2)
  (nxml-slash-auto-complete-flag t)

  :config
  (require 'sgml-mode))


(use-package applescript-mode
  :ensure t
  :defer t
  :bind (:map as-mode-map
              ("<tab>" . tab-to-tab-stop)))


(use-package js
  :if (>= emacs-major-version 27)
  :defer t
  :mode ("\\.pac\\'" . js-mode)
  :mode ("\\.jsx?\\'" . js-jsx-mode)
  :custom
  (js-indent-level 2))


(use-package ruby-mode
  :disabled
  :defer t
  :interpreter "ruby")


(use-package enh-ruby-mode
  :ensure t
  :defer t
  :mode "\\(?:\\.rb\\|ru\\|rake\\|thor\\|jbuilder\\|gemspec\\|podspec\\|/\\(?:Gem\\|Rake\\|Cap\\|Thor\\|Vagrant\\|Guard\\|Pod\\)file\\)\\'"
  :interpreter "ruby"
  :hook
  (enh-ruby-mode . (lambda () (setq-local tab-width 2)))
  :custom
  (enh-ruby-check-syntax nil))


(use-package rspec-mode
  :ensure t
  :defer t
  :diminish)


(use-package lua-mode
  :ensure t
  :defer t
  :interpreter ("lua" . lua-mode)
  :mode "\\.lua\\.erb\\'"
  :custom
  (lua-indent-level 2)
  (lua-indent-nested-block-content-align nil))


(use-package go-mode
  :ensure t
  :defer t
  :bind
  (:map go-mode-map ("<f12>" . gofmt))
  :hook
  (go-mode
   . (lambda ()
       (setq-local indent-tabs-mode t)
       (setq-local tab-width 4)
       (add-hook 'before-save-hook #'gofmt t t)))
  :custom
  (gofmt-command "goimports")
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
  :defer t
  :mode "\\.upstart\\'")


(use-package yaml-mode
  :ensure t
  :defer t
  :mode "/\\.config/yamllint/config\\'"
  :mode "/\\(group\\|host\\)_vars/"
  :mode "/\\.flyrc\\'"
  :mode "/\\.clang-format\\'")


(use-package terraform-mode
  :ensure t
  :defer t
  :hook (terraform-mode . turn-off-auto-fill))


(use-package protobuf-mode
  :ensure t
  :defer t
  :hook
  (protobuf-mode
   . (lambda ()
       (c-add-style "my-protobuf-style"
                    '((c-basic-offset   . 2)
                      (indent-tabs-mode . nil))
                    t))))


(use-package json-mode
  :ensure t
  :defer t
  :hook
  (json-mode . (lambda () (setq-local tab-width 2))))


(use-package ovpn-mode
  :ensure t
  :defer t)


(use-package jsonnet-mode
  :ensure t
  :bind (:map jsonnet-mode-map
              ("M-." . jsonnet-jump))
  :defer t
  :mode "\\.\\(jsonnet\\|libsonnet\\)\\'"
  :config
  ;; Unbind non-standard keybindings for `jsonnet-jump' and
  ;; `jsonnet-reformat-buffer'
  (unbind-key "C-c C-f" jsonnet-mode-map)
  (unbind-key "C-c C-r" jsonnet-mode-map)
  :custom
  (jsonnet-indent-level 2)
  (jsonnet-use-smie t))


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
  (load-theme 'sanityinc-tomorrow-bright t)
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

  (defun set-frame-font ()
    (let ((font-size 14))
      (cond
       ((equal system-type 'gnu/linux)
        (set-face-attribute 'default nil :font (font-Hack font-size)))
       ((equal system-type 'darwin)
        (set-face-attribute 'default nil :font (font-Menlo font-size))
        (dolist (charset '(han cjk-misc bopomofo))
          (set-fontset-font t charset (font-spec :family "STHeiTi")))
        (setq face-font-rescale-alist
              `(("STHeiTi" . ,(/ (cdr (assoc (frame-parameter nil 'font)
                                             STHeiTi-size-map))
                                 font-size))))
        (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji"))
        ;; `variable-pitch' face uses the same height as the `default'.
        ;; https://protesilaos.com/codelog/2020-09-05-emacs-note-mixed-font-heights/
        (set-face-attribute 'variable-pitch nil :family "Palatino" :height 1.0)))))

  (add-hook 'after-init-hook
            #'(lambda ()
                (set-frame-font)
                (set-frame-parameter nil 'fullscreen 'maximized))
            t))


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
(bind-key "C-x k"           #'kill-this-buffer)
(bind-key "<C-M-backspace>" #'backward-kill-sexp)
(when (display-graphic-p) (unbind-key "C-z"))

;;
;; Display elapsed time
;; https://github.com/jwiegley/dot-emacs/blob/master/init.el
;;
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
