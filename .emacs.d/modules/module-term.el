;;; module-term.el --- module -*- lexical-binding: t -*-

(use-package vterm
  :defer t
  :commands (vterm vterm-other-window)
  :general
  (:keymaps 'vterm-mode-map
	    :states 'normal
	    "M-n" 'vterm-send-down
	    "M-p" 'vterm-send-up
	    "M-y" 'vterm-yank-pop
	    "M-/" 'vterm-send-tab
	    [escape] 'vterm-send-escape
	    [return] 'vterm-send-return
	    "p" 'vterm-yank
	    "u" 'vterm-undo)
  (:keymaps 'vterm-mode-map
	    :states 'insert
	    "C-y" 'vterm-yank
	    "C-o" 'evil-execute-in-normal-state)
  :hook (vterm-mode . (lambda () (setq-local global-hl-line-mode nil))))
  :init
  ;; move point to the end of buffer on new output
  (setq comint-move-point-for-output t)
  ;; allow moving around the buffer in normal mode
  (setq term-char-mode-point-at-process-mark nil)
  :config
  (progn
    (setq vterm-shell shell-file-name))

(provide 'module-term)
