;;; module-modeline.el --- module -*- lexical-binding: t -*-

;; modeline
(use-package doom-modeline
    :ensure t
    :custom
    (doom-modeline-icon t)
    (doom-modeline-major-mode-color-icon t)
    (doom-modeline-height 15)
    (doom-modeline-bar-width 6)
    (doom-modeline-buffer-file-name-style 'relative-to-project)
    (doom-modeline-buffer-encoding 'nondefault)
    :config
    (doom-modeline-mode))

;; display time in modeline
(display-time-mode 1)

;; display battery in modeline
(display-battery-mode 1)

(provide 'module-modeline)
