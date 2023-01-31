;;; module-code.el --- module -*- lexical-binding: t -*-

(require 'eglot)
(require 'flymake)
(require 'treesit)

;; -----------------------------------------------------------------------------
;; Tree Sitter
;; -----------------------------------------------------------------------------

(push '(bash-mode . bash-ts-mode) major-mode-remap-alist)
(push '(css-mode . css-ts-mode) major-mode-remap-alist)
(push '(html-mode . html-ts-mode) major-mode-remap-alist)
(push '(javascript-mode . js-ts-mode) major-mode-remap-alist)
(push '(js-mode . js-ts-mode) major-mode-remap-alist)
(push '(json-mode . json-ts-mode) major-mode-remap-alist)
(push '(js-json-mode . json-ts-mode) major-mode-remap-alist)
(push '(typescript-mode . typescript-ts-mode) major-mode-remap-alist)
(push '(tsx-mode . tsx-ts-mode) major-mode-remap-alist)

;; -----------------------------------------------------------------------------
;; Formatting
;; -----------------------------------------------------------------------------

(use-package apheleia
  :defer t
  :general
  (:states 'normal
	   "C-c C-f" 'apheleia-format-buffer)
  :init
  (apheleia-global-mode +1))


;; -----------------------------------------------------------------------------
;; Web
;; -----------------------------------------------------------------------------

(use-package import-js
  :defer t)

(use-package add-node-modules-path
  :straight
  (add-node-modules-path
   :type git
   :host github
   :repo "codesuki/add-node-modules-path")
  :defer t
  :hook ((css-ts-mode . add-node-modules-path)
	 (html-mode . add-node-modules-path)
	 (js-ts-mode . add-node-modules-path)
	 (typescript-ts-mode . add-node-modules-path)
	 (tsx-ts-mode . add-node-modules-path)))

(use-package npm-mode
  :defer t
  :general
  (:states 'normal
	   :prefix "SPC m"
	   "n" '(:ignore t :wk "npm")
	   "i" 'npm-mode-npm-install
	   "r" 'npm-mode-npm-run
	   "s" 'npm-mode-npm-install-save
	   "d" 'npm-mode-npm-install-save-dev
	   "n" 'npm-mode-npm-init
	   "u" 'npm-mode-npm-uninstall
	   "l" 'npm-mode-npm-list
	   "p" 'npm-mode-visit-project-file)
  :init (add-hook 'js2-mode #'npm-mode))

(use-package js-doc
  :defer t)

(use-package emmet-mode
  :defer t
  :hook ((html-mode . emmet-mode)
	 (css-mode . emmet-mode)
	 (js-jsx-mode . emmet-mode)
	 (tsx-ts-mode . emmet-mode)
	 (web-mode . web-mode)))


;; -----------------------------------------------------------------------------
;; Eglot
;; -----------------------------------------------------------------------------

(use-package eglot
  :defer t
  :hook ((html-mode . eglot-ensure)
	 (css-ts-mode . eglot-ensure)
	 (js-jsx-mode . eglot-ensure)
	 (js-ts-mode . eglot-ensure)
	 (typescript-ts-mode . eglot-ensure)
	 (tsx-ts-mode . eglot-ensure))
  :custom (eglot-autoshutdown t)
  :init
  (setq eglot-events-buffer-size 0))

(provide 'module-code)
