;;; module-explorer.el --- module -*- lexical-binding: t; -*-

;; https://github.com/Alexander-Miller/treemacs
(use-package treemacs
  :defer t
  :general
  (:states 'normal
           "SPC f e" 'treemacs-select-window)
  :init
  (setq treemacs-follow-after-init t
        treemacs-is-never-other-window t
        treemacs-sorting 'alphabetic-case-insensitive-asc)
  :config
  (treemacs-follow-mode -1))

(use-package treemacs-evil
  :after (treemacs evil)
  :ensure t)

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t)

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

(use-package lsp-treemacs
  :after (treemacs eglot)
  :ensure t)

(provide 'module-explorer)
