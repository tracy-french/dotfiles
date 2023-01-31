;;; module-editing.el --- module -*- lexical-binding: t -*-

;; pair handling
(electric-pair-mode 1)

(use-package aggressive-indent
  :defer t
  :init
  (progn
    (global-aggressive-indent-mode)))

(use-package clean-aindent-mode
  :config
  (progn
    (clean-aindent-mode)))

(use-package editorconfig
  :config
  (editorconfig-mode t))

(use-package eval-sexp-fu
  :commands eval-sexp-fu-flash-mode)

(use-package expand-region
  :defer t
  :config
  (setq expand-region-contract-fast-key "V"
	expand-region-reset-fast-key "r"))

(use-package origami
  :defer t
  :general
  (:states 'normal
	   "za" 'origami-forward-toggle-node
	   "zc" 'origami-close-node
	   "zC" 'origami-close-node-recursively
	   "zO" 'origami-open-node-recursively
	   "zo" 'origami-open-node
	   "zr" 'origami-open-all-nodes
	   "zm" 'origami-close-all-nodes
	   "zn" 'origami-next-fold
	   "zR" 'origami-reset
	   "z TAB" 'origami-recursively-toggle-node)
  :init
  (global-origami-mode))

(use-package vimish-fold
  :after evil)

(use-package evil-vimish-fold
  :after vimish-fold
  :init
  (setq evil-vimish-fold-target-modes '(prog-mode conf-mode text-mode))
  :config (global-evil-vimish-fold-mode))

(use-package undo-tree
  :defer t
  :custom (undo-tree-history-directory-alist `(("." . ,(concat tf-emacs-cache-directory "undo-tree-hist/"))))
  :general
  (:keymaps 'undo-tree-visualizer-mode-map
	    "j" 'undo-tree-visualize-redo
	    "k" 'undo-tree-visualize-undo
	    "h" 'undo-tree-visualize-switch-branch-left
	    "l" 'undo-tree-visualize-switch-branch-right)
  :init (global-undo-tree-mode)
  :config
  (progn
    (setq
     undo-tree-visualizer-diff t
     undo-tree-visualizer-timestamps t
     undo-tree-enable-undo-in-region t
     undo-limit 800000
     undo-strong-limit 12000000
     undo-outer-limit 120000000)

    (global-undo-tree-mode)))

(use-package persistent-scratch
  :defer t
  :init
  (progn
    (setq persistent-scratch-save-file (concat tf-emacs-cache-directory ".persistent-scratch")
	  persistent-scratch-autosave-interval 60
	  persistent-scratch-what-to-save '(point narrowing))
    (persistent-scratch-autosave-mode t)))

(use-package unkillable-scratch
  :defer t
  :init
  (progn
    (setq unkillable-scratch-do-not-reset-scratch-buffer t)
    (unkillable-scratch t)))

(provide 'module-editing)
