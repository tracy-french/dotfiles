;;; module-defaults.el --- module -*- lexical-binding: t -*-

(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode)

(use-package bookmark
  :general
  (:states 'normal
	   :prefix "SPC"
	   "fb" 'bookmark-jump)
  :init
  (progn
    (setq bookmark-default-file (concat tf-emacs-cache-directory "bookmarks")
	  bookmark-save-flag 1)))

(use-package dired
  :straight nil
  :general
  (:states 'normal
	   :prefix "SPC"
	   "ad" 'dired
	   "fj" 'dired-jump
	   "jd" 'dired-jump
	   "jD" 'dired-jump-other-window)

  (:keymaps 'dired-mode-map
	    :states 'normal
	    "n" 'evil-ex-search-next
	    "N" 'evil-ex-search-previous)
  :init
  (progn
    (setq dired-dwim-target t  ; suggest a target for moving/copying intelligently
          dired-hide-details-hide-symlink-targets nil
          ;; don't prompt to revert, just do it
          dired-auto-revert-buffer #'dired-buffer-stale-p
          ;; Always copy/delete recursively
          dired-recursive-copies  'always
          dired-recursive-deletes 'top
          ;; Ask whether destination dirs should get created when copying/removing files.
          dired-create-destination-dirs 'ask
          ;; Where to store image caches
          image-dired-dir (concat tf-emacs-cache-directory "image-dired/")
          image-dired-db-file (concat image-dired-dir "db.el")
          image-dired-gallery-dir (concat image-dired-dir "gallery/")
          image-dired-temp-image-file (concat image-dired-dir "temp-image")
          image-dired-temp-rotate-image-file (concat image-dired-dir "temp-rotate-image")
          ;; Screens are larger nowadays, we can afford slightly larger thumbnails
          image-dired-thumb-size 150))
  :config
  (setq insert-directory-program "gls" dired-use-ls-dired t)
  (setq dired-listing-switches "-ahl -v --group-directories-first"))

  (use-package dired-x
    :straight nil
    :commands (dired-jump
	       dired-jump-other-window
	       dired-omit-mode))

(use-package image-dired
  :defer t
  :general
  (:keymaps 'image-dired-thumbnail-mode-map
	    :states 'normal
	    "j" 'image-dired-next-line
	    "k" 'image-dired-previous-line
	    "l" 'image-dired-forward-image
	    "h" 'image-dired-backward-image))

(use-package dired-rsync
  :general (dired-mode-map "C-c C-r" #'dired-rsync))


(use-package diredfl
  :hook (dired-mode . diredfl-mode))

(use-package ediff
  :defer t
  :init
  (progn
    ;; sane defaults
    (setq-default
     ediff-window-setup-function 'ediff-setup-windows-plain
     ediff-split-window-function 'split-window-horizontally
     ediff-merge-split-window-function 'split-window-horizontally)
    ;; show org ediffs unfolded
    (require 'outline)
    (add-hook 'ediff-prepare-buffer-hook #'show-all)
    ;; restore window layout when done
    (add-hook 'ediff-quit-hook #'winner-undo)))

(use-package eldoc
  :defer t
  :config
  (progn
    ;; enable eldoc in `eval-expression'
    (add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode)
    ;; enable eldoc in IELM
    (add-hook 'ielm-mode-hook #'eldoc-mode)

    ;; eldoc-message-commands
    (eldoc-add-command #'evil-insert)
    (eldoc-add-command #'evil-insert-line)
    (eldoc-add-command #'evil-append)
    (eldoc-add-command #'evil-append-line)
    (eldoc-add-command #'evil-force-normal-state)))

(use-package help-fns+
  :commands (describe-keymap)
  :general
  (:states 'normal
	   "SPC hdK" 'describe-keymap)
  :init
  (progn
    (advice-add 'help-do-xref :after (lambda () (setq-local tab-width 8)))))

(use-package image-mode
  :defer t
  :general
  (:keymaps 'image-mode
	    :states 'normal
	    "SPC ma" '(:ignore t :wk "animate")
	    "SPC maaa" 'image-toggle-animation
	    "SPC maa+" 'image-increase-speed
	    "SPC maa-" 'image-decrease-speed
	    "SPC maar" 'image-reset-speed

	    "SPC mg" '(:ignore t :wk "goto file")
	    "SPC mgn" 'image-next-file
	    "SPC mgN" 'image-previous-file

	    "SPC mt" '(:ignore t :wk "transform/resize")
	    "SPC mt+" 'image-increase-size
	    "SPC mt-" 'image-decrease-size
	    "SPC mtf" 'image-mode-fit-frame
	    "SPC mtr" 'image-transform-rotation
	    "SPC mth" 'image-transform-fit-to-height
	    "SPC mtw" 'image-transform-fit-to-width
	    "SPC mts" 'image-transform-set-scale
	    "SPC mtR" 'image-transform-reset

	    "h" 'image-backward-hscroll
	    "j" 'image-next-line
	    "k" 'image-previous-line
	    "l" 'image-forward-hscroll)

  :init
  (progn
    (setq image-animate-loop t)))

(use-package imenu
  :defer t
  :general
  (:states 'normal
	   "SPC ji" 'imenu))

(provide 'module-defaults)
