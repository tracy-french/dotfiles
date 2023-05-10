;;; module-code.el --- module -*- lexical-binding: t -*-

(require 'eglot)
(require 'flymake)
(require 'treesit)

;; -----------------------------------------------------------------------------
;; Formatting
;; -----------------------------------------------------------------------------

(use-package apheleia
  :general
  (:states 'normal
	   "C-c C-f" 'apheleia-format-buffer)
  :init
  (apheleia-global-mode +1))

;; -----------------------------------------------------------------------------
;; Syntax
;; -----------------------------------------------------------------------------

(use-package treesit
  :straight nil
  :init
  (push '(css-mode . css-ts-mode) major-mode-remap-alist)
  (push '(javascript-mode . js-ts-mode) major-mode-remap-alist)
  (push '(js-mode . js-ts-mode) major-mode-remap-alist)
  (push '(js-json-mode . json-ts-mode) major-mode-remap-alist)
  (push '(python-mode . python-ts-mode) major-mode-remap-alist))

(use-package bash-mode
  :straight (:type built-in)
  :defer t
  :mode ("\\.sh\\'" . bash-ts-mode))

(use-package js-mode
  :straight (:type built-in)
  :defer t
  :mode (("\\.js\\'" . js-ts-mode)
         ("\\.jsx\\'" . js-ts-mode))
  :init
  (progn
    (setq js-indent-level 2
          js-jsx-indent-level 2)))

(use-package typescript-mode
  :straight (:type built-in)
  :defer t
  :mode (("\\.ts\\'" . typescript-ts-mode)
         ("\\.tsx\\'" . tsx-ts-mode)))

(use-package json-mode
  :straight (:type built-in)
  :defer t
  :mode ("\\.json\\'" . json-ts-mode))

(use-package ccs-mode
  :straight (:type built-in)
  :defer t
  :mode ("\\.css\\'" . css-ts-mode))

(use-package yaml-mode
  :straight (:type built-in)
  :defer t
  :mode (("\\.yml\\'" . yaml-ts-mode)
         ("\\.yaml\\'" . yaml-ts-mode)))


;; -----------------------------------------------------------------------------
;; linting
;; -----------------------------------------------------------------------------

(use-package flymake-eslint :defer t)

(defun tf/flymake-eslint-enable-maybe ()
  "enable `flymake-eslint' based on the project configuration."
  (flymake-eslint-enable)
  (setq-local flymake-eslint-project-root
              (locate-dominating-file buffer-file-name ".eslintrc.js")))


;; -----------------------------------------------------------------------------
;; IDE
;; -----------------------------------------------------------------------------

(use-package eglot
  :straight (:type built-in)
  :defer t
  :general
  (:states 'normal
           "SPC c" '(:ignore t :which-key "code")
           "SPC c s" '(eglot :which-key "server")
           "SPC c r" '(eglot-rename :which-key "rename"))
  :custom
  (eglot-autoshutdown t)
  (eglot-events-buffer-size 0)
  :hook ((eglot-managed-mode . tf/flymake-eslint-enable-maybe)
         (html-mode . eglot-ensure)
         (css-ts-mode . eglot-ensure)
         (js-ts-mode . eglot-ensure)
         (typescript-ts-base-mode . eglot-ensure))
  :init
  (put 'eglot-server-programs 'safe-local-variable 'listp)
  :config
  (add-to-list 'eglot-stay-out-of 'eldoc-documentation-strategy)
  (put 'eglot-error 'flymake-overlay-control nil)
  (put 'eglot-warning 'flymake-overlay-control nil))

(use-package consult-eglot
  :general
  (:keymaps 'eglot-mode-map
            [remap xref-find-apropos] #'consult-eglot-symbols))

;; -----------------------------------------------------------------------------
;; Copilot 
;; -----------------------------------------------------------------------------

;; https://github.com/zerolfx/copilot.el
(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :ensure t
  :hook (prog-mode . copilot-mode)
  :general
  (:keymaps 'copilot-completion-map
            "<tab>" 'copilot-accept-completion
            "TAB" 'copilot-accept-completion))

(provide 'module-code)
