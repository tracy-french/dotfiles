;;; core-editor.el --- core -*- lexical-binding: t -*-

(setq create-lockfiles nil
      make-backup-files nil
      version-control t
      backup-by-copying t
      delete-old-versions t
      kept-old-versions 5
      kept-new-versions 5)

(setq-default indent-tabs-mode nil
              tab-width 2)

(setq-default tab-always-indent nil)

(setq-default fill-column 80)

(setq-default word-wrap t)

(setq-default truncate-lines t)

(setq truncate-partial-width-windows nil)

(setq sentence-end-double-space nil)

(setq require-final-newline t)

(add-hook 'text-mode-hook #'visual-line-mode)

(show-paren-mode t)

(electric-pair-mode t)

(use-package helpful
  :commands helpful--read-symbol
  :hook (helpful-mode . visual-line-mode)
  :init
  (setq apropos-do-all t)

  (global-set-key [remap describe-function] #'helpful-callable)
  (global-set-key [remap describe-command]  #'helpful-command)
  (global-set-key [remap describe-variable] #'helpful-variable)
  (global-set-key [remap describe-key]      #'helpful-key)
  (global-set-key [remap describe-symbol]   #'helpful-symbol))

(provide 'core-editor)
