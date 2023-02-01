;;; module-git.el --- module -*- lexical-binding: t -*-

(use-package golden-ratio
  :config
  (add-to-list 'golden-ratio-exclude-buffer-names " *transient*"))

(use-package git-commit
  :defer t)

(use-package git-link
  :defer t
  :general
  (:states 'normal
	   "SPC gl" '(:ignore t :wk "Links")
	   "SPC glc" 'git-link-commit
           "SPC gll" 'git-link)
  :init (setq git-link-open-in-browser t))

(use-package git-messenger
  :defer t
  :general
  (:states 'normal
	   "gM" 'git-messenger:popup-message)
  (:keymaps 'git-messenger-map
	    [escape] 'git-messenger:popup-close))

(use-package git-timemachine
  :defer t)

(use-package git-modes
  :defer t)

(use-package gitignore-templates
  :defer t
  :general
  (:keymaps 'gitignore-mode
	    "SPC mi" 'gitignore-templates-insert)
  (:states 'normal
	   "SPC gfi" 'gitignore-templates-new-file))

;;; git
(use-package magit
  :defer t
  :custom (magit-bury-buffer-function #'magit-restore-window-configuration)
  :general
  (:states 'normal
	   "SPC gb" '(magit-blame :wk "Blame")
	   "SPC gc" '(magit-clone :wk "Clone")
           "SPC gi" '(magit-init :wk "Init")
           "SPC gL" 'magit-list-repositories
           "SPC gm" 'magit-dispatch
           "SPC gs" '(magit-status :wk "Status")
           "SPC gS" '(magit-stage-file :wk "Stage file")
           "SPC gU" '(magit-unstage-file :wk "Unstage file")

	   "SPC gf" '(:ignore t :wk "File")
	   "SPC gfF" 'magit-find-file
           "SPC gfl" 'magit-log-buffer-file
           "SPC gfd" 'magit-diff
           "SPC gfm" 'magit-file-dispatch)

  (:keymaps 'magit-repolist-mode-map
	    "SPC gr" 'magit-list-repositories
	    "SPC RET" 'magit-repolist-status)

  (:states '(normal motion)
	   :keymaps 'with-editor-mode-map
	   "SPC mm" 'with-editor-finish
	   "SPC ma" 'with-editor-cancel
	   "SPC mc" 'with-editor-finish
	   "SPC mk" 'with-editor-cancel
	   :keymaps 'magit-log-select-mode-map
	   "SPC mm" 'magit-log-select-pick
	   "SPC ma" 'magit-log-select-quit
	   "SPC mc" 'magit-log-select-pick
	   "SPC mk" 'magit-log-select-quit)

  (:keymaps 'magit-status-mode-map
	    "gf" '(:wk "jump-to-unpulled")
	    "gp" '(:wk "jump-to-unpushed"))

  (:states 'normal
	   :keymaps 'magit-blame-read-only-mode-map
	   "RET" 'magit-show-commit)

  :init
  (progn
    (setq magit-revision-show-gravatars '("^Author:     " . "^Commit:     ")
	  magit-display-buffer-function
	  'magit-display-buffer-fullframe-status-v1)))

(use-package magit-delta
  :hook (magit-mode . magit-delta-mode))

(use-package magit-gitflow
  :hook (magit-mode . magit-gitflow-mode)
  :general
  (:keymaps 'magit-mode-map
	    "%" 'magit-gitflow-popup)
  :init (setq magit-gitflow-popup-key "%"))

(use-package magit-section
  :defer t)

(use-package magit-todos
  :hook (magit-mode . magit-todos-mode))

(use-package orgit
  :defer t)

(use-package orgit-forge
  :after forge
  :defer t)

(use-package smeargle
  :defer t
  :general
  (:states 'normal
	   "SPC gH" '(:ignore t :wk "Highlight")
	   "SPC gHc" '(smeargle-clear :wk "Clear highlights")
	   "SPC gHh" '(smeargle-commits :wk "Highlight by age of changes")
	   "SPC gHt" '(smeargle :wk "Highlight by last update time")))

(use-package transient
  :defer t
  :init
  (progn
    (setq transient-history-file (expand-file-name "transient/history.el"
						   tf-emacs-cache-directory)
	  transient-levels-file (expand-file-name "transient/levels.el"
						  tf-emacs-cache-directory)
	  transient-values-file (expand-file-name "transient/values.el"
						  tf-emacs-cache-directory))))

(use-package forge
  :after magit
  :general
  (:keymaps 'forge-topic-mode
	    "SPC ma" 'forge-edit-topic-assignees
	    "SPC mc" 'forge-create-post
            "SPC mC" 'forge-checkout-pullreq
            "SPC mb" 'forge-browse-topic
            "SPC md" 'forge-delete-comment
            "SPC me" 'forge-edit-post
            "SPC mm" 'forge-edit-topic-marks
            "SPC mM" 'forge-create-mark
            "SPC mn" 'forge-edit-topic-note
            "SPC mr" 'forge-edit-topic-review-requests
            "SPC ms" 'forge-edit-topic-state
            "SPC mt" 'forge-edit-topic-title
            "SPC mu" 'forge-copy-url-at-point-as-kill)
  (:keymaps 'forge-post-mode
	    "SPC mm" 'forge-post-submit
	    "SPC mc" 'forge-post-submit
	    "SPC mk" 'forge-post-cancel
	    "SPC ma" 'forge-post-cancel)
  :init
  (progn
    (setq forge-database-file (concat tf-emacs-cache-directory
				      "forge-database.sqlite")
	  forge-add-default-bindings nil)))

(provide 'module-git)
