;;; module-misc.el --- module -*- lexical-binding: t -*-

(setq request-storage-directory
      (concat tf-emacs-cache-directory "request/"))

(use-package dumb-jump
  :defer t
  :hook (xref-backend-functions . dumb-jump-xref-activate))

(use-package devdocs
  :general
  (:keymaps 'normal
	    "SPC bhd" 'devdocs-search))

(provide 'module-misc)
