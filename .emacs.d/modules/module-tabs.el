;;; module-tabs.el --- module -*- lexical-binding: t -*-

(use-package centaur-tabs
  :demand
  :hook
  (dashboard-mode . centaur-tabs-local-mode)
  (term-mode . centaur-tabs-local-mode)
  (calendar-mode . centaur-tabs-local-mode)
  (org-agenda-mode . centaur-tabs-local-mode)
  :init (setq centaur-tabs-enable-key-bindings t)
  :config
  (progn
    (setq centaur-tabs-style "bar"
	  centaur-tabs-height 32
	  centaur-tabs-set-icons t
	  centaur-tabs-gray-out-icons 'buffer
          centaur-tabs-show-new-tab-button t
          centaur-tabs-set-modified-marker t
	  centaur-tabs-close-button "✕"
	  centaur-tabs-modified-marker "•"
	  centaur-tabs-cycle-scope 'tabs
	  centaur-tabs-show-navigation-buttons t
	  centaur-tabs-set-bar 'under
	  centaur-tabs-show-count nil
	  x-underline-at-descent-line t
	  centaur-tabs-left-edge-margin nil)
    (centaur-tabs-change-fonts (face-attribute 'default :font) 110)
    (centaur-tabs-headline-match)
    (centaur-tabs-mode t)
    (setq uniquify-separator "/"
	  uniquify-buffer-name-style 'forward)

    (let ((project-name (cdr (project-current))))
      (when (listp project-name)
        (setq project-name (cadr project-name)))
      (if project-name
          (format "Project: %s" (expand-file-name project-name))
        centaur-tabs-common-group-name)))
  :general
  (:states 'normal
	   "g t" 'centaur-tabs-forward
	   "g T" 'centaur-tabs-backward
	   "g C-t" 'centaur-tabs-move-current-tab-to-right
	   "g C-S-t" 'centaur-tabs-move-current-tab-to-left
	   "C-c t p" 'centaur-tabs-group-by-projectile-project
	   "C-c t g" 'centaur-tabs-group-buffer-groups))

(provide 'module-tabs)
