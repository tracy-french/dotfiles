;;; module-files.el --- module -*- lexical-binding: t -*-

(use-package recentf
  :commands (recentf-save-list)
  :init
  (progn
    (add-hook 'find-file-hook (lambda ()
				(unless recentf-mode
				  (recentf-mode)
				  (recentf-track-opened-file))))
    (setq recentf-save-file "/.emacs.d/cache/recentf"
	  recentf-max-saved-items 1000
	  recentf-auto-cleanup 'never
	  recentf-auto-save-timer (run-with-idle-timer 600 t
						       'recentf-save-list)))
  :config
  (progn
    (add-to-list 'recentf-exclude "~/.emacs.d/cache/recentf")
    (add-to-list 'recentf-exclude (recentf-expand-file-name package-user-dir))
    (add-to-list 'recentf-exclude "COMMIT_EDITMSG\\")
    (when custom-file
      (add-to-list 'recentf-exclude (recentf-expand-file-name custom-file)))))

(use-package savehist
  :init
  (progn
    (setq savehist-file "~/.emacs.d/cache/savehist"
	  enable-recursive-minibuffers t
	  history-length 1000
	  savehist-additional-variables '(mark-ring
					  global-mark-ring
					  search-ring
					  regexp-search-ring
					  extended-command-history
					  kill-ring)
	  savehist-autosave-interval 60)
    (savehist-mode t)))

(use-package saveplace
  :init
  (save-place-mode))

;; keep directories clean of litter
(use-package no-littering
  :config
  ;; handle auto-save litter
  (setq auto-save-file-name-transforms
	`((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

  ;; prevent customization litter
  (setq custom-file (no-littering-expand-etc-file-name "custom.el")))

(provide 'module-files)
