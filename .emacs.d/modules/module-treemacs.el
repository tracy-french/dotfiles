;;; module-treemacs.el --- module -*- lexical-binding: t -*-

(defvar treemacs-use-follow-mode t
  "When non-nil use `treemacs-follow-mode'.")

(defvar treemacs-use-filewatch-mode t
  "When non-nil use `treemacs-filewatch-mode'.")

(defvar treemacs-use-scope-type 'Frames
  "Determines the scope of treemacs buffers and workspaces.
Possible values are:
 - `Frames' - to scope treemacs to the current frame
 - `Perspectives' - to scope treemacs in conjunction with `persp-mode'.")

(defvar treemacs-use-git-mode nil
  "Type of git integration for `treemacs-git-mode'.
There are 3 possible values:
1) simple, which highlights only files based on their git status, and is
   slightly faster
2) extended, which highlights both files and directories, but requires python
3) deferred, which is the same is extended, but delays highlighting for improved
   performance")

(defvar treemacs-lock-width nil
  "When non-nil the treemacs window will not be manually resizable by default.")

(defvar treemacs-use-icons-dired t
  "When non-nil use `treemacs-icons-dired'")

(defvar treemacs-use-all-the-icons-theme nil
  "Enable the treemacs supported `all-the-icons' theme")

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
      (treemacs-select-window))))

(defun tf/treemacs-setup-width-lock ()
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
  :hook (treemacs-mode . (lambda () (setq-local display-line-numbers-mode nil)))
  :general
  (:states 'normal
	   :prefix "SPC"
	   "f" '(:ignore t :wk "files")
	   "f t"   'treemacs
	   "f B"   'treemacs-bookmark
	   "f T"   'treemacs-find-file
	   "f M-t" 'treemacs-find-tag

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
    (setq treemacs-follow-after-init t
	  treemacs-is-never-other-window t
	  treemacs-sorting 'alphabetic-case-insensitive-asc
	  treemacs-persist-file (concat tf-emacs-cache-directory "treemacs-persist")
	  treemacs-last-error-persist-file (concat tf-emacs-cache-directory "treemacs-last-error-persist"))
    (add-hook 'treemacs-mode-hook
	      #'tf/treemacs-setup-width-lock))
  :config
  (treemacs-follow-mode -1))

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
  :defer t)

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-mode))

(use-package treemacs-all-the-icons
  :hook ((treemacs-mode dired-mode) . (lambda () (treemacs-load-theme 'all-the-icons))))

(use-package treemacs-magit
  :after treemacs magit
  :defer t)

(provide 'module-treemacs)
