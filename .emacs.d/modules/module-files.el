;;; module-files.el --- module -*- lexical-binding: t -*-

;; keep directories clean of litter
(use-package no-littering
  :config
  ;; handle auto-save litter
  (setq auto-save-file-name-transforms
	`((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

  ;; prevent customization litter
  (setq custom-file (no-littering-expand-etc-file-name "custom.el")))

(use-package general
  :general
  (:states 'normal
           "SPC f" '(:ignore t :which-key "files")
           "SPC f f" 'find-file))

(provide 'module-files)
