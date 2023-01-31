;;; module-org.el --- module -*- lexical-binding: t -*-

(use-package org
  :commands (org-clock-out org-occur-in-agenda-files org-agenda-files)
  :defer t
  :init
  (progn
    (setq org-startup-with-inline-images t
	  org-src-fontify-natively t
	  org-imenu-depth 8))
  :config
  (progn
    (font-lock-add-keywords
     'org-mode '(("\\(@@html:<kbd>@@\\) \\.(.*\\) \\(.*\\) \\(@@html:</kbd>@@\\)"
		  (1 font-lock-comment-face prepend)
		  (2 font-lock-function-name-face)
		  (3 font-lock-comment-face prepend))))
    (evil-define-key 'norm org-mode-map (kbd "RET") 'org-open-at-point)))

(use-package org-superstar
  :defer t
  :init (add-hook 'org-mode-hook 'org-superstar-mode))

(use-package toc-org
  :defer t
  :init
  (progn
    (setq toc-org-max-depth 10)
    (add-hook 'org-mode-hook 'toc-org-enable)))

(use-package evil-org
  :defer t
  :init
  (progn
    (add-hook 'org-mode-hook 'evil-org-mode)
    (setq evil-org-use-additional-insert t)))

(provide 'module-org)
