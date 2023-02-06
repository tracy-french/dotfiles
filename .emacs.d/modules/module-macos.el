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

(let ((gls (executable-find "gls")))
  (when gls
    (setq insert-dictionary-program gls)))

(use-package launchctl
  :defer t
  :general
  (:keymaps 'launch-ctl-mode
	    "q" 'quit-window
            "s" 'tabulated-list-sort
            "g" 'launchctl-refresh
            "n" 'launchctl-new
            "e" 'launchctl-edit
            "v" 'launchctl-view
            "l" 'launchctl-load
            "u" 'launchctl-unload
            "r" 'launchctl-reload
            "S" 'launchctl-start
            "K" 'launchctl-stop
            "R" 'launchctl-restart
            "D" 'launchctl-remove
            "d" 'launchctl-disable
            "E" 'launchctl-enable
            "i" 'launchctl-info
            "f" 'launchctl-filter
            "=" 'launchctl-setenv
            "#" 'launchctl-unsetenv
            "h" 'launchctl-help)
  :init (add-to-list 'auto-mode-alist '("\\.plist\\'" . nxml-mode)))

(use-package osx-dictionary
  :general
  (:states 'normal
	   "SPC xwd" 'osx-dictionary-search-pointer)

  (:keymaps 'osx-dictionary-mode
	    "q" 'osx-dictionary-quit
            "r" 'osx-dictionary-read-word
            "s" 'osx-dictionary-search-input
            "o" 'osx-dictionary-open-dictionary.app)

  :commands (osx-dictionary-search-pointer
	     osx-dictionary-search-input
	     osx-dictionary-cli-find-or-recompile))

(use-package osx-trash
  :init (osx-trash-setup))

(use-package reveal-in-osx-finder
  :general
  (:states 'normal
	   "SPC bf" 'reveal-in-osx-finder))

(when (fboundp 'set-fontset-font)
  (set-fontset-font "fontset-default"
		    '(#x1F600 . #x1F64F)
		    (font-spec :name "Apple Color Emoji") nil 'prepend))

(provide 'module-macos)
