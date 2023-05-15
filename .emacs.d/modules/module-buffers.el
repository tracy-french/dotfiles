;;; module-buffers.el --- module -*- lexical-binding: t; -*-

(use-package ibuffer
  :straight (:type built-in)
  :defer t
  :general
  (:states 'normal
           "SPC b i" '(ibuffer :wk "ibuffer")))

(use-package ibuffer-vc
  :defer t
  :hook (ibuffer . ibuffer-vc-set-filter-groups-by-vc-root))

(use-package all-the-icons-ibuffer
  :ensure t
  :hook (ibuffer-mode . all-the-icons-ibuffer-mode))

(use-package consult
  :general
  (:states 'normal
           "SPC b b" 'consult-buffer))

(use-package general
  :general
  (:states 'normal
           "SPC b" '(:ignore t :which-key "buffers")
           "SPC b k" '(kill-current-buffer :wk "kill current buffer")
           "SPC b K" '(kill-buffer :wk "kill buffer")
           "SPC b p" '(previous-buffer :wk "previous buffer")
           "SPC b n" '(next-buffer :wk "next buffer")
           "SPC b s" '(save-buffer :wk "save buffer")
           "SPC b S" '(save-some-buffers :wk "save some buffers")))

(provide 'module-buffers)
