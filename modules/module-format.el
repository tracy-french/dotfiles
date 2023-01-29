;;; module-format.el --- module -*- lexical-binding: t -*-

(use-package apheleia
  :general
  (:states 'normal
           "C-c C-f" 'apheleia-format-buffer)
  :init
  (apheleia-global-mode +1))

(provide 'module-format)
