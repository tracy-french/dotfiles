;;; module-theme.el --- module -*- lexical-binding: t -*-

(use-package doom-themes
  :config
  (progn
    (setq doom-themes-treemacs-enable-variable-pitch nil
	  doom-themes-treemacs-theme "doom-atom")
    (load-theme 'doom-solarized-light t)
    (doom-themes-visual-bell-config)
    (doom-themes-treemacs-config)
    (doom-themes-org-config)))

(provide 'module-theme)
