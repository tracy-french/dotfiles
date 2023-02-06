;;; module-treemacs.el --- module -*- lexical-binding: t -*-

(use-package treemacs
  :commands (treemacs-select-window
	     treemacs-select-scope-type
	     treemacs--window-number-ten
	     treemacs-current-visibility)
  :defer t
  :hook (treemacs-mode . (lambda () (setq-local display-line-numbers-mode nil)))
  :general
  (:states 'normal
	   :prefix "SPC"
	   "f" '(:ignore t :wk "files")
	   "f t"   'treemacs
	   "f B"   'treemacs-bookmark
	   "f T"   'treemacs-find-file

	   "p"   '(:ignore t :wk "project")
	   "p t" '(tf/treemacs-project-toggle :wk "open project in file tree"))

  (:keymaps 'treemacs-mode
	    :states 'normal
	    "c"         '(:wk "treemacs-create")
	    "o"         '(:wk "treemacs-visit-node")
	    "oa"        '(:wk "treemacs-visit-node-ace")
	    "t"         '(:wk "treemacs-toggles")
	    "y"         '(:wk "treemacs-copy")
	    "C-c C-p"   '(:wk "treemacs-projects")
	    "C-C C-p c" '(:wk "treemacs-projects-collapse"))
  :init
  (progn
    (defun tf/treemacs-project-toggle ()
      "toggle and add the current project to treemacs if not already added"
      (interactive)
      (if (eq (treemacs-current-visibility) 'visible)
	  (delete-window (treemacs-get-local-window))
	(let ((path (projectile-ensure-project (projectile-project-root)))
	      (name (projectile-project-name)))
	  (unless (treemacs-current-workspace)
	    (treemacs--find-workspace))
	  (treemacs-do-add-project-to-workspace path name)
	  (treemacs-select-window)))))
  :config
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-git-mode 'simple))

(use-package treemacs-evil
  :defer t
  :after treemacs
  :general
  (:keymaps 'evil-treemacs-state-map
	    [return] 'treemacs-RET-action
	    [tab] 'treemacs-TAB-action
	    "TAB" 'treemacs-TAB-action
	    "o v" 'treemacs-visit-node-horizontal-split
	    "o s" 'treemacs-visit-node-vertical-split))

(use-package treemacs-projectile
  :after treemacs
  :defer t
  :init (require 'treemacs-projectile))

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-mode))

(use-package treemacs-all-the-icons
  :hook ((treemacs-mode dired-mode) . (lambda ()
					(treemacs-load-theme 'all-the-icons))))

(use-package treemacs-magit
  :after treemacs magit
  :defer t)

(provide 'module-treemacs)
