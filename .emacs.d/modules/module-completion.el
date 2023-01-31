;;; module-completion.el --- module -*- lexical-binding: t -*-
(add-to-list 'load-path
             (expand-file-name "straight/build/vertico/extensions"
                               straight-base-dir))

;;; vertical minibuffer completion
(use-package vertico
  :general
  (:keymaps 'vertico-map
	    "C-j" 'vertico-next
	    "C-k" 'vertico-previous
	    "M-h" 'vertico-directory-up)
  :hook (minibuffer-setup-hook . vertico-repeat-save)
  :init
  (progn
    (setq vertico-resize nil
	  vertico-count 10
	  vertico-cycle t)
    (vertico-mode 1)))

;; directory completion extension
(use-package vertico-directory
  :straight nil
  :after vertico
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)
  :general
  (:keymaps 'vertico-map :states 'normal
	    "RET" 'vertico-directory-enter
	    "DEL" 'vertico-directory-delete-char
	    "M-DEL" 'vertico-directory-delete-word))

;; completion annotations
(use-package marginalia
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-list nil))
  :init (marginalia-mode 1))

;; fuzzy completion
(use-package orderless
  :init
  (progn
    (setq completion-styles '(orderless basic)
	  completion-category-overrides '((file (styles . (partial-completion)))))))

(use-package dabbrev
  :general
  (:states 'normal
	   "M-/" 'dabbrev-completion
	   "C-M-/" 'dabbrev-expand)
  :custom
  (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))

;;; Corfu
(add-to-list 'load-path
	     (expand-file-name "straight/build/corfu/extensions"
			       straight-base-dir))

;; completion ui
(use-package corfu
  :general
  (:keymaps 'corfu-map
	    "M-p" 'corfu-popupinfo-scroll-down
	    "M-n" 'corfu-popupinfo-scroll-up
	    "M-d" 'corfu-popupinfo-toggle)
  :init
  (progn
    ;; M-SPC inserts orderless separator
    (setq corfu-auto t
	  corfu-auto-delay 0.0
	  corfu-auto-prefix 2
	  corfu-cycle t
	  corfu-echo-documentation 0.25)
    (global-corfu-mode 1)

    (require 'corfu-popupinfo)
    (corfu-popupinfo-mode 1))
  :config
  (eldoc-add-command #'corfu-insert))

;; completion at point
(use-package cape
  :init
  (progn
    (add-to-list 'completion-at-point-functions #'cape-file)
    (add-to-list 'completion-at-point-functions #'cape-dabbrev)))

;; fuzzy lists for everything
(use-package consult
  :defer t
  :general
  ("C-s" 'consult-line)
  (:keymaps 'minibuffer-local-map
	    :override t
	    "C-r" 'consult-history)
  :init (setq completion-in-region-function #'consult-completion-in-region))

;; act at point
(use-package embark
  :defer t
  :general
  ("C-." 'embark-act)
  ("C-h B" 'embark-bindings)
  :init
  (progn
    (setq prefix-help-command #'embark-prefix-help-command)))

;; integrate embark and consult
(use-package embark-consult
  :after (embark consult)
  :demand t
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(provide 'module-completion)
