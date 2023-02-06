;;; module-imenu.el --- module -*- lexical-binding: t -*-

(use-package imenu-list
  :general
  (:keymaps 'imenu-list-major-mode-map
	    "d" 'imenu-list-display-entry
	    "r" 'imenu-list-refresh)
  (:states 'normal
	   :prefix "SPC"
	   "bi" 'tf/imenu-list-smart-focus
	   "Ti" 'imenu-list-smart-toggle)
  :init
  (progn
    (defun tf/imenu-list-smart-focus ()
      (interactive)
      (if (get-buffer-window imenu-list-buffer-name t)
	  (imenu-list-show)
	(imenu-list-smart-toggle)))

    (setq imenu-list-focus-after-activation t
	  imenu-list-auto-resize t)))

(provide 'module-imenu)
