;;; module-macos.el --- module -*- lexical-binding: t -*-

(customize-set-variable mac-right-option-modifier nil)
(customize-set-variable mac-command-modifier 'super)
(customize-set-variable ns-function-modifier 'hyper)

;;
;;; Reasonable defaults for macOS

;; Use spotlight search backend as a default for M-x locate (and helm/ivy
;; variants thereof), since it requires no additional setup.
(setq locate-command "mdfind")


;;
;;; Compatibilty fixes

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

;;; Packages

(use-package osx-trash
  :commands osx-trash-move-file-to-trash
  :init
  (progn
    ;; Delete files to trash on macOS, as an extra layer of precaution against
    ;; accidentally deleting wanted files.
    (setq delete-by-moving-to-trash t)

    ;; Lazy load `osx-trash'
    (when (not (fboundp 'system-move-file-to-trash))
      (defun system-move-file-to-trash (file)
	"Move FILE to trash."
	(when (and (not IS-LINUX)
		   (not (file-remote-p default-directory)))
	  (osx-trash-move-file-to-trash file))))))

(provide 'module-macos)
