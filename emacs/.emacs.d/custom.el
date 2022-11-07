(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(after-save-hook '(executable-make-buffer-file-executable-if-script-p))
 '(aggressive-indent-dont-indent-if '((string-match "^\\s-+$" (thing-at-point 'line))))
 '(auto-image-file-mode t)
 '(auto-revert-verbose t)
 '(avy-case-fold-search t)
 '(aw-scope 'frame)
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(comint-buffer-maximum-size 16384)
 '(comint-completion-addsuffix t)
 '(comint-input-ignoredups t)
 '(comint-input-ring-size 16384)
 '(comint-move-point-for-output nil)
 '(comint-prompt-read-only t)
 '(comint-scroll-show-maximum-output t)
 '(comint-scroll-to-bottom-on-input t)
 '(company-backends
   '((company-capf company-dabbrev-code :separate)
     company-files company-keywords company-dabbrev))
 '(company-dabbrev-downcase nil)
 '(company-dabbrev-minimum-length 2)
 '(company-minimum-prefix-length 2)
 '(company-selection-wrap-around t)
 '(company-transformers '(delete-dups))
 '(compilation-always-kill t)
 '(compilation-context-lines 10)
 '(compilation-scroll-output t)
 '(confirm-kill-emacs 'yes-or-no-p)
 '(custom-buffer-done-kill t)
 '(delete-by-moving-to-trash t)
 '(delete-selection-mode t)
 '(diff-hl-draw-borders nil)
 '(directory-free-space-args "-Pkh")
 '(dired-dwim-target t)
 '(dired-isearch-filenames 'dwim)
 '(dired-omit-files "^\\.?#\\|^\\.$\\|^\\.\\.$\\|^\\..+$")
 '(dired-omit-size-limit nil)
 '(dired-omit-verbose nil)
 '(dired-recursive-copies 'always)
 '(dired-recursive-deletes 'always)
 '(display-time-12hr-format t)
 '(display-time-day-and-date nil)
 '(display-time-mode t)
 '(dumb-jump-force-searcher 'rg)
 '(ediff-split-window-function 'split-window-horizontally)
 '(ediff-window-setup-function 'ediff-setup-windows-plain)
 '(eglot-autoshutdown t)
 '(eglot-events-buffer-size 0)
 '(eglot-ignored-server-capabilites
   '(:documentHighlightProvider :codeActionProvider :codeLensProvider :documentFormattingProvider :documentRangeFormattingProvider :documentOnTypeFormattingProvider :documentLinkProvider))
 '(electric-indent-mode t)
 '(electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
 '(electric-pair-mode t)
 '(enable-recursive-minibuffers t)
 '(enh-ruby-check-syntax nil)
 '(epg-pinentry-mode 'loopback)
 '(fill-column 78)
 '(find-ls-option '("-print0 | xargs -0 ls -ld" . "-ld"))
 '(flycheck-disabled-checkers
   '(c/c++-cppcheck c/c++-gcc go-build go-errcheck go-gofmt go-golint go-staticcheck go-test go-unconvert go-vet json-jsonlint json-python-json python-mypy python-pycompile python-pyright ruby-rubocop sh-bash sh-posix-bash sh-posix-dash sh-zsh yaml-jsyaml yaml-ruby emacs-lisp-checkdoc))
 '(flyspell-sort-corrections t)
 '(frame-resize-pixelwise t)
 '(git-link-use-commit t)
 '(global-auto-revert-mode t)
 '(global-auto-revert-non-file-buffers t)
 '(global-display-line-numbers-mode nil)
 '(global-eldoc-mode t)
 '(global-font-lock-mode t)
 '(global-mark-ring-max 500)
 '(global-so-long-mode t)
 '(global-subword-mode t)
 '(gnutls-algorithm-priority "SECURE192:+SECURE128:-VERS-ALL:+VERS-TLS1.2")
 '(gnutls-min-prime-bits 4096)
 '(gnutls-verify-error t)
 '(gofmt-command "goimports")
 '(history-delete-duplicates t)
 '(ibuffer-filter-group-name-face 'font-lock-doc-face)
 '(ibuffer-formats
   '((mark modified read-only vc-status-mini " "
           (name 32 32 :left :elide)
           " "
           (size-h 9 -1 :right)
           " "
           (mode 16 16 :left :elide)
           " "
           (vc-status 16 -1 :left)
           " " filename-and-process)))
 '(imenu-auto-rescan t)
 '(imenu-auto-rescan-maxout 600000)
 '(imenu-max-item-length "Unlimited")
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(initial-major-mode 'fundamental-mode)
 '(initial-scratch-message nil)
 '(isearch-allow-scroll t)
 '(ispell-extra-args
   '("--sug-mode=normal" "--ignore=2" "--run-together" "--camel-case"))
 '(ispell-local-dictionary "en_US")
 '(ispell-personal-dictionary "~/.emacs.d/ispell-personal-dictionary")
 '(ispell-silently-savep t)
 '(js-indent-level 2)
 '(jsonnet-indent-level 2)
 '(jsonnet-use-smie t)
 '(kill-do-not-save-duplicates t)
 '(kill-ring-max 1000000)
 '(kill-whole-line nil)
 '(large-file-warning-threshold 50000000)
 '(line-number-mode t)
 '(list-matching-lines-default-context-lines 3)
 '(lsp-enable-dap-auto-configure nil)
 '(lsp-enable-file-watchers nil)
 '(lsp-enable-folding nil)
 '(lsp-enable-indentation nil)
 '(lsp-enable-links nil)
 '(lsp-enable-on-type-formatting nil)
 '(lsp-enable-symbol-highlighting nil)
 '(lsp-enable-text-document-color nil)
 '(lsp-headerline-breadcrumb-enable nil)
 '(lsp-keep-workspace-alive nil)
 '(lsp-lens-enable nil)
 '(lsp-modeline-code-actions-enable nil)
 '(lsp-modeline-diagnostics-enable nil)
 '(lsp-progress-function 'lsp-on-progress-legacy)
 '(lsp-signature-auto-activate nil)
 '(lsp-signature-render-documentation nil)
 '(lua-indent-level 2)
 '(lua-indent-nested-block-content-align nil)
 '(magit-commit-show-diff nil)
 '(make-backup-files nil)
 '(mark-ring-max 100)
 '(markdown-command "multimarkdown")
 '(markdown-nested-imenu-heading-index nil)
 '(markdown-toc-header-toc-title "# Table of Contents")
 '(max-lisp-eval-depth 2000)
 '(max-specpdl-size 16384)
 '(message-log-max 16384)
 '(mode-require-final-newline t)
 '(mouse-drag-copy-region t)
 '(mouse-wheel-progressive-speed nil)
 '(mouse-yank-at-point t)
 '(native-comp-async-report-warnings-errors 'silent)
 '(next-line-add-newlines nil)
 '(ns-alternate-modifier 'super)
 '(ns-command-modifier 'meta)
 '(ns-pop-up-frames nil)
 '(nxml-child-indent 2)
 '(nxml-outline-child-indent 2)
 '(nxml-slash-auto-complete-flag t)
 '(org-catch-invisible-edits t)
 '(org-export-default-language "en")
 '(org-export-with-creator t)
 '(org-export-with-section-numbers nil)
 '(org-export-with-sub-superscripts '{})
 '(org-export-with-toc nil)
 '(org-hide-leading-stars t)
 '(org-html-extension "html")
 '(org-html-htmlize-output-type 'inline-css)
 '(org-image-actual-width nil)
 '(org-log-done 'time)
 '(org-src-fontify-natively t)
 '(org-startup-folded "nofold")
 '(package-enable-at-startup nil)
 '(package-selected-packages
   '(color-theme-sanityinc-tomorrow embark-consult ht bind-key pos-tip jenkinsfile-mode marginalia reformatter embark compat avy org with-editor highlight-indent-guides magit-section markdown-mode transient spinner git-commit f popup s orderless dash cmake-mode ssh-config-mode syslog-mode gitattributes-mode gitignore-mode gitconfig-mode dockerfile-mode jinja2-mode jsonnet-mode ovpn-mode json-mode protobuf-mode terraform-mode yaml-mode systemd vimrc-mode vcl-mode go-rename go-mode lua-mode rspec-mode enh-ruby-mode applescript-mode rpm-spec-mode unfill magit git-link highlight-escape-sequences rainbow-delimiters aggressive-indent paredit markdown-toc edit-indirect htmlize ox-gfm terminal-here ace-link ace-window consult vertico crux yasnippet-snippets yasnippet projectile company flycheck diredfl dumb-jump rg diff-hl ibuffer-vc flyspell-correct ialign diminish use-package))
 '(projectile-enable-caching t)
 '(projectile-file-exists-local-cache-expire 300)
 '(projectile-files-cache-expire 259200)
 '(projectile-find-dir-includes-top-level t)
 '(projectile-mode-line-prefix " ")
 '(projectile-sort-order 'recentf)
 '(projectile-switch-project-action 'projectile-dired)
 '(ps-bottom-margin 19.84251968503937)
 '(ps-font-family 'Courier)
 '(ps-font-size 6.0)
 '(ps-header-font-family 'Courier)
 '(ps-header-font-size 8.0)
 '(ps-header-line-pad 0.25)
 '(ps-header-lines 1)
 '(ps-header-offset 14.173228346456693)
 '(ps-header-title-font-size 8.0)
 '(ps-inter-column 19.84251968503937)
 '(ps-landscape-mode t)
 '(ps-left-margin 19.84251968503937)
 '(ps-lpr-switches '("-o sides=two-sided-short-edge"))
 '(ps-number-of-columns 2)
 '(ps-paper-type 'letter)
 '(ps-print-color-p 'black-white)
 '(ps-print-footer nil)
 '(ps-print-header t)
 '(ps-print-header-frame t)
 '(ps-printer-name "HP LaserJet Professional P 1102w")
 '(ps-printer-name-option "-P")
 '(ps-right-margin 19.84251968503937)
 '(ps-show-n-of-n t)
 '(ps-spool-duplex t)
 '(ps-top-margin 19.84251968503937)
 '(ps-warn-paper-type t)
 '(query-replace-highlight t)
 '(recentf-auto-cleanup 60)
 '(recentf-exclude
   '("\\`out\\'" "\\.log\\'" "\\.el\\.gz\\'" "/\\.emacs\\.d/elpa/.*-\\(autoloads\\|pkg\\)\\.el\\'" "/\\.emacs\\.d/\\(auto-save-list\\|elfeed\\|package-upgrade-check-epoch\\|projects\\|recentf\\|snippets\\|tramp\\|var\\)" "/\\.git/COMMIT_EDITMSG\\'"))
 '(recentf-filename-handlers '(abbreviate-file-name))
 '(recentf-max-saved-items 2000)
 '(repeat-exit-key [return])
 '(repeat-mode t)
 '(require-final-newline t)
 '(ring-bell-function 'ignore)
 '(save-abbrevs 'silently)
 '(scroll-bar-mode nil)
 '(scroll-error-top-bottom t)
 '(search-highlight t)
 '(select-enable-clipboard t)
 '(sh-basic-offset 2)
 '(show-paren-delay 0)
 '(show-paren-mode t)
 '(show-paren-style 'parentheses)
 '(size-indication-mode t)
 '(tab-always-indent 'complete)
 '(tab-width 4)
 '(terminal-here-mac-terminal-command 'iterm2)
 '(tls-checktrust t)
 '(tool-bar-mode nil)
 '(tramp-connection-timeout 15)
 '(tramp-default-method "ssh")
 '(tramp-use-ssh-controlmaster-options nil)
 '(transient-mark-mode t)
 '(undo-limit 1000000)
 '(uniquify-after-kill-buffer-p t)
 '(uniquify-buffer-name-style 'post-forward-angle-brackets nil (uniquify))
 '(use-dialog-box nil)
 '(use-file-dialog nil)
 '(use-package-enable-imenu-support t)
 '(use-package-verbose t)
 '(use-short-answers t)
 '(vertico-count 20)
 '(which-func-unknown "n/a" t)
 '(which-function-mode t)
 '(whitespace-line-column 100)
 '(whitespace-style '(face trailing tabs))
 '(x-stretch-cursor t)
 '(xref-prompt-for-identifier nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:inherit aw-leading-char-face :weight bold :height 3.0)))))
