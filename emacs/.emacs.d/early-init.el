;;   -*- lexical-binding: t; -*-

(setq gc-cons-threshold 50000000)
(setq read-process-output-max 4194304)
(setq load-prefer-newer t)

(setq tool-bar-mode nil)
(setq scroll-bar-mode nil)
(setq menu-bar-mode 1)
(setq frame-title-format
      '((:eval (or buffer-file-name (buffer-name)))
        (:eval (if (buffer-modified-p) " * " " - "))
        "GNU Emacs " emacs-version " - " system-name))
