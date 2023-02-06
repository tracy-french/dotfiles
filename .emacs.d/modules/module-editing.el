;;; module-editing.el --- module -*- lexical-binding: t -*-

(use-package aggressive-indent
  :hook (diff-auto-refine-mode . (lambda () (aggressive-indent-mode -1)))
  :general
  (:states 'normal
	   :prefix "SPC t"
	   "I" 'aggressive-indent-mode
	   "C-I" 'global-aggressive-indent-mode))

(use-package avy
  :general
  (:states 'normal
	   :prefix "SPC j"
	   "b" 'avy-pop-mark
	   "j" 'evil-avy-goto-char-timer
	   "l" 'evil-avy-goto-line
	   "u" 'tf/avy-goto-url
	   "U" 'tf/avy-open-url
	   "w" 'evil-avy-goto-word-or-subword-1
	   :prefix "SPC x"
	   "o" 'tf/avy-open-url)
  :config
  (progn
    (defun tf/avy-goto-url ()
      (interactive)
      (avy-jump "https?://"))

    (defun tf/avy-open-url ()
      (interactive)
      (save-excursion
	(tf/avy-goto-url)
	(browse-url-at-point)))))

(use-package clean-aindent-mode
  :config
  (progn
    (defun tf//put-clean-aindent-last ()
      (when clean-aindent-mode
	(remove-hook 'post-command-hook 'clean-aindent--check-last-point)
	(add-hook 'post-command-hook 'clean-aindent--check-last-point t)))

    (clean-aindent-mode)
    (add-hook 'prog-mode-hook 'tf//put-clean-aindent-last t)))

(use-package drag-stuff
  :defer t
  :init (drag-stuff-mode t))

(use-package editorconfig
  :config
  (editorconfig-mode t))

(use-package eval-sexp-fu
  :commands eval-sexp-fu-flash-mode)

(use-package expand-region
  :general
  (:states 'normal
	   "SPC v" 'er/expand-region)
  :config
  (setq expand-region-contract-fast-key "V"
	expand-region-reset-fast-key "r"))

(use-package hungry-delete
  :general
  (:states 'normal
	   "SPC td" 'hungry-delete-mode)
  :config
  (progn
    (nconc hungry-delete-except-modes '(term-mode v-term-mode))
    (setq-default hungry-delete-chars-to-skil " \t\f\v")
    (define-key hungry-delete-mode-map (kbd "DEL") 'hungry-delete-backward)
    (define-key hungry-delete-mode-map (kbd "S-DEL") 'delete-backward-char)))

(use-package link-hint
  :general
  (:states 'normal
	   :prefix "SPC x"
	   "A" 'link-hint-open-all-links
	   "m" 'link-hint-open-multiple-links
	   "o" 'link-hint-open-link-at-point
	   "O" 'link-hint-open-link
	   "y" 'link-hint-copy-link-at-point
	   "Y" 'link-hint-copy-link))

(use-package lorem-ipsum
  :general
  (:states 'normal
	   :prefix "SPC il"
	   "" '(:ignore t :wk "Lorem Ipsum")
	   "l" 'lorem-ipsum-insert-list
	   "p" 'lorem-ipsum-insert-paragraphs
	   "s" 'lorem-ipsum-insert-sentences))

(use-package origami
  :general
  (:states 'normal
	   :prefix "z"
	   :override t
	   "a" 'origami-forward-toggle-node
	   "c" 'origami-close-node
	   "C" 'origami-close-node-recursively
	   "O" 'origami-open-node-recursively
	   "o" 'origami-open-node
	   "r" 'origami-open-all-nodes
	   "m" 'origami-close-all-nodes
	   "n" 'origami-next-fold
	   "R" 'origami-reset
	   "TAB" 'origami-recursively-toggle-node)
  :init
  (global-origami-mode))

(use-package vimish-fold
  :after evil)

(use-package evil-vimish-fold
  :after vimish-fold
  :init (setq evil-vimish-fold-target-modes '(prog-mode conf-mode text-mode))
  :config (global-evil-vimish-fold-mode))

(use-package evil-easymotion
  :defer t
  :after evil
  :init
  (defun tf/buffer-evil-avy-goto-char-timer ()
    (interactive)
    (let ((current-prefix-arg t))
      (evil-avy-goto-char-timer)))

  (evilem-default-keybindings "gs")
  (define-key evilem-map "a" (evilem-create #'evil-forward-arg))
  (define-key evilem-map "A" (evilem-create #'evil-backward-arg))
  (define-key evilem-map "o" (evilem-create #'evil-jump-out-args))
  (define-key evilem-map "s" #'evil-avy-goto-char-2)
  (define-key evilem-map "/" #'evil-avy-goto-char-timer)
  (define-key evilem-map (kbd "SPC") #'tf/buffer-evil-avy-goto-char-timer)

  ;; Provide proper prefixes for which key
  (which-key-add-keymap-based-replacements evil-motion-state-map
    "gs"  "evil-easymotion")
  (which-key-add-keymap-based-replacements evilem-map
    "g" "misc"
    "[" "section backward"
    "]" "section forward")

  ;; Use evil-search backend, instead of isearch
  (evilem-make-motion evilem-motion-search-next #'evil-ex-search-next
                      :bind ((evil-ex-search-highlight-all nil)))
  (evilem-make-motion evilem-motion-search-previous #'evil-ex-search-previous
                      :bind ((evil-ex-search-highlight-all nil)))
  (evilem-make-motion evilem-motion-search-word-forward #'evil-ex-search-word-forward
                      :bind ((evil-ex-search-highlight-all nil)))
  (evilem-make-motion evilem-motion-search-word-backward #'evil-ex-search-word-backward
                      :bind ((evil-ex-search-highlight-all nil))))

(use-package password-generator
  :general
  (:states 'normal
	   :prefix "SPC ip"
	   "" '(:ignore t :wk "Passwords")
	   "1" 'password-generator-simple
	   "2" 'password-generator-strong
	   "3" 'password-generator-paranoid
	   "p" 'password-generator-phonetic
	   "n" 'password-generator-numeric))

(use-package smartparens
  :hook ((prog-mode . smartparens-mode)
	  (comint-mode . smartparens-mode)
	  (minibuffer-setup . tf//conditionally-enable-smartparens-mode))
  :general
  (:states 'normal
	   :prefix "SPC j"
	   "s" 'sp-split-sexp
	   "n" 'sp-newline)
  :init
  (progn
    (defun tf//conditionally-enable-smartparens-mode ()
      (if (or (eq this-command 'eval-expression)
	      (eq this-command 'eldoc-eval-expression))
	  (smartparens-mode 1)))

    (defun tf/smartparens-pair-newline (id action context)
      (save-excursion
	(newline)
	(indent-according-to-mode)))

    (defun tf/smartparens-pair-newline-and-indent (id action context)
      (tf/smartparens-pair-newline id action context)
      (indent-according-to-mode))

    (defun tf/smart-closing-parenthesis ()
      (interactive)
      (let* ((sp-navigate-close-if-unbalanced t)
	     (current-pos (point))
	     (current-line (line-number-at-pos current-pos))
	     next-pos
	     next-line)
	(save-excursion
	  (let ((buffer-undo-list)
		(modified (buffer-modified-p)))
	    (unwind-protect
		(progn
		  (sp-up-sexp)
		  (setq next-pos (point)
			next-line (line-number-at-pos)))
	      (primitive-undo (length buffer-undo-list)
			      buffer-undo-list)
	      (set-buffer-modified-p modified))))
	(cond
	 ((and (= current-line next-line)
	       (not (= current-pos next-pos)))
	  (sp-up-sexp))
	 (t
	  (insert-char ?\))))))

    (setq sp-show-pair-delay 0.2
	  sp-show-pair-from-inside t
	  sp-cancel-autoskip-on-backward-movement nil
	  sp-highlight-pair-overlay nil
	  sp-highlight-wrap-overlay nil
	  sp-highlight-wrap-tag-overlay nil))

  :config
  (progn
    (require 'smartparens-config)
    (show-smartparens-global-mode +1)
    (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)
    (sp-pair "{" nil :post-handlers
	     '(:add (tf/smartparens-pair-newline-and-indent "RET")))
    (sp-pair "[" nil :post-handlers
	     '(:add (tf/smartparens-pair-newline-and-indent "RET")))

    (define-key evil-insert-state-map ")"
		'tf/smart-closing-parenthesis)))

(use-package string-inflection
  :general
  (:states 'normal
	   :prefix "SPC xi"
	   "" '(:ignore t :wk "Inflection")
	   "c" 'string-inflection-lower-camelcase
           "C" 'string-inflection-camelcase
           "-" 'string-inflection-kebab-case
           "k" 'string-inflection-kebab-case
           "_" 'string-inflection-underscore
           "u" 'string-inflection-underscore
           "U" 'string-inflection-upcase))

(use-package string-edit-at-point
  :general
  (:states 'normal
	   "SPC xe" 'string-edit-at-point)

  (:keymaps 'string-edit-at-point-mode
	    :prefix "SPC m"
	    "," 'string-edit-conclude
	    "c" 'string-edit-conclude
	    "a" 'string-edit-abort
	    "k" 'string-edit-abort))

(use-package multi-line
  :defer t)

(use-package undo-tree
  :general
  (:keymaps 'undo-tree-visualizer-mode-map
	    "j" 'undo-tree-visualize-redo
	    "k" 'undo-tree-visualize-undo
	    "h" 'undo-tree-visualize-switch-branch-left
	    "l" 'undo-tree-visualize-switch-branch-right)
  :init (global-undo-tree-mode)
  :config
  (progn
    (setq undo-tree-visualizer-timestamps t
	  undo-tree-visualizer-diff t
	  undo-tree-enable-undo-in-region t
	  undo-limit 800000
	  undo-strong-limit 12000000
	  undo-outer-limit 120000000
	  undo-tree-history-directory-alist
	  `(("." . ,(let ((dir (expand-file-name "undo-tree-history" tf-emacs-cache-directory)))
		      (if (file-exists-p dir)
			  (unless (file-accessible-directory-p dir)
			    (warn "Cannot access directory `%s'.
Perhaps you don't have required permissions, or it's not a directory.
See variable `undo-tree-history-directory-alist'." dir))
			(make-directory dir))
		      dir))))
    (global-undo-tree-mode))
  :config
  (defun tf/undo-tree-restore-default ()
    (setq undo-tree-visualizer-diff t))

  (advice-add 'undo-tree-visualizer-quit :after #'tf/undo-tree-restore-default))

(use-package uuidgen
  :general
  (:states 'normal
	   :prefix "SPC iU"
	   "" '(:ignore t :wk "UUID")
	   "1" '(tf/uuidgen-1 :wk "v1")
	   "4" '(tf/uuidgen-4 :wk "v4")
	   "U" '(tf/uuidgen-4 :wk "v4"))
  :init
  (defun tf/uuidgen-1 ()
    (interactive)
    (insert (uuidgen-1)))

  (defun tf/uuidgen-4 ()
    (interactive)
    (insert (uuidgen-4))))

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
