;;; module-macos.el --- module -*- lexical-binding: t -*-

(customize-set-variable mac-right-option-modifier nil)
(customize-set-variable mac-command-modifier 'super)
(customize-set-variable ns-function-modifier 'hyper)

;; Visit files opened outside of Emacs in existing frame, not a new one
(setq ns-pop-up-frames nil)

;; sane trackpad/mouse scroll settings
(setq mac-redisplay-dont-reset-vscroll t
      mac-mouse-wheel-smooth-scroll nil)

;; Sets `ns-transparent-titlebar' and `ns-appearance' frame parameters so window
;; borders will match the enabled theme.
(and (or (daemonp)
         (display-graphic-p))
     (require 'ns-auto-titlebar nil t)
     (ns-auto-titlebar-mode +1))

(use-package osx-trash
  :init (osx-trash-setup))

(provide 'module-macos)
