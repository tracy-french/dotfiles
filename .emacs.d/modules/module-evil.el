;;; module-evil.el --- module -*- lexical-binding: t -*-

(use-package evil
  :demand t
  :general
  :custom (evil-want-Y-yank-to-eol t)
  :init
  (setq evil-want-keybinding nil
        evil-want-integration t)

  (setq-default evil-shift-width 2)
  :config
  (evil-mode 1))

;; evil everywhere
(use-package evil-collection
  :after evil
  :custom
  (evil-collection-setup-minibuffer t)
  :config
  (evil-collection-init))

(provide 'module-evil)
