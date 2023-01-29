;;; module-treemacs.el --- module -*- lexical-binding: t -*-

(defun tf-emacs/treemacs-project-toggle ()
  "toggle and add the current project to treemacs if not already added"
  (interactive)
  (if (eq (treemacs-current-visibility) 'visible)
      (delete-window (treemacs-get-local-window))
    (let ((path (projectile-ensure-project (projectile-project-root)))
	  (name (projectile-project-name)))
      (unless (treemacs-current-workspace)
	(treemacs--find-workspace))
      (treemacs-do-add-project-to-workspace path name)
      (treemacs-select-window))))

(defun tf-emacs/treemacs-setup-width-lock ()
  "setup the width lock of treemacs buffer base on `treemacs-lock-width'"
  (interactive)
  (unless (eq (not treemacs--width-is-locked)
	      (not treemacs-lock-width))
    (treemacs-without-messages
     (treemacs-toggle-fixed-width))))

(use-package treemacs
  :commands (treemacs-select-window
	     treemacs-select-scope-type
	     treemacs--window-number-ten
	     treemacs-current-visibility)
  :defer t
  :general
  (:states 'normal
	   "SPC f" '(:ignore t :wk "files")
	   "SPC f t" 'treemacs
	   "SPC f B" 'treemacs-bookmark
	   "SPC f T" 'treemacs-find-file
	   "SPC f M-t" 'treemacs-find-tag

	   "SPC p" '(:ignore t :wk "project")
	   "SPC p t" '(:wk "open project in file tree"))
  :init
  (progn
    (setq treemacs-follow-after-init t)
    (add-hook 'treemacs-mode-hook
	      #'tf-emacs/treemacs-setup-width-lock)))

(use-package treemacs-evil
  :after treemacs)

(use-package treemacs-projectile
  :after treemacs
  :defer t
  :init (require 'treemacs-projectile))

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-mode))

(use-package treemacs-all-the-icons
  :hook ((treemacs-mode dired-mode) . (lambda () (treemacs-load-theme 'all-the-icons))))

(use-package treemacs-magit
  :after treemacs magit
  :defer t)

(provide 'module-treemacs)
