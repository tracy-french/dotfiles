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
;; Eglot
;; -----------------------------------------------------------------------------

;; I'm not sure why this is needed, but it throws an error if I remove it
(cl-defmethod project-root ((project (head eglot-project)))
  (cdr project))

(defun my-project-try-tsconfig-json (dir)
  (when-let* ((found (locate-dominating-file dir "tsconfig.json")))
    (cons 'eglot-project found)))

(add-hook 'project-find-functions
          'my-project-try-tsconfig-json nil nil)

(add-to-list 'eglot-server-programs
             '((typescript-ts-mode) "typescript-language-server" "--stdio"))

(use-package eglot
  :straight (:type built-in)
  :defer t
  :hook ((html-mode . eglot-ensure)
         (css-ts-mode . eglot-ensure)
         (js-ts-mode . eglot-ensure)
         (typescript-ts-mode . eglot-ensure)
         (tsx-ts-mode . eglot-ensure))
  :init
  (setq eglot-sync-connect 1
        eglot-connect-timeout 10
        eglot-autoshutdown t
        eglot-send-changes-idle-time 0.5
        eglot-events-buffer-size 0))

(use-package consult-eglot
  :general
  (:keymaps 'eglot-mode-map
            [remap xref-find-apropos] #'consult-eglot-symbols))

(provide 'module-code)
