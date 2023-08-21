;; -*- lexical-binding: t; -*-

;; Set the garbage collector threshold, to avoid collections
;; To avoid collections while loading the `init.el', they must be set using
;; the `early-init.el'.
(setq gc-cons-percentage 0.5
      gc-cons-threshold (* 128 1024 1024))
