;;; module-visual.el --- module -*- lexical-binding: t -*-

(use-package all-the-icons
  :defer t)

(use-package desktop
  :defer t
  :init
  (setq desktop-dirname tf-emacs-cache-directory)
  :config
  (add-to-list 'desktop-path tf-emacs-cache-directory))

(global-display-fill-column-indicator-mode)

(use-package fill-column-indicator
  :defer t
  :init
  (progn
    (setq fci-rule-width 1)
    (add-to-list 'minor-mode-alist '(fci-mode ""))
    (fci-mode)))

(use-package hl-todo
  :defer t
  :init
  (global-hl-todo-mode 1))

(use-package popwin
  :config
  (progn
    (popwin-mode 1)))

(provide 'module-visual)
