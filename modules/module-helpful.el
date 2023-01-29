;;; module-helpful.el --- module -*- lexical-binding: t -*-

(use-package helpful
  :defer t
  :general
  (:states 'normal
	   "SPC h d k" 'helpful-key
	   "SPC h d f" 'helpful-callable
	   "SPC h d v" 'helpful-variable)
  (:keymaps 'helpful-mode
	    "q" 'helpful-kill-buffers)

  (:keymaps 'helpful-mode
	    :states 'normal
	    "g r" 'helpful-update
	    "q" 'quit-window)
  :init
  (progn
    (evil-set-initial-state 'helpful-mode 'normal)))

(provide 'module-helpful)
