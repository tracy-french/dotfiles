;;; module-visual.el --- module -*- lexical-binding: t -*-

(use-package all-the-icons
  :defer t)

(use-package desktop
  :defer t
  :init
  (setq desktop-dirname tf-emacs-cache-directory)
  :config
  (add-to-list 'desktop-path tf-emacs-cache-directory))

(use-package hl-todo
  :defer t
  :init
  (global-hl-todo-mode 1))

(provide 'module-visual)
