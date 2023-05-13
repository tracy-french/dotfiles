;;; module-buffers.el --- module -*- lexical-binding: t; -*-

(use-package ibuffer
  :straight (:type built-in)
  :defer t
  :general
  (:states 'normal
           "SPC b" '(:ignore t :which-key "buffers")
           "SPC b i" 'ibuffer))

(provide 'module-buffers)
