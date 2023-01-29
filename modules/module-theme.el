;;; module-theme.el --- module -*- lexical-binding: t -*-

(use-package all-the-icons)

(use-package doom-themes
    :config
  (doom-themes-visual-bell-config) ; flash modeline on error
  (doom-themes-org-config) ; org-mode integration
  (load-theme 'doom-challenger-deep t))

(provide 'module-theme)
