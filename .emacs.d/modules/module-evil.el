;;; module-evil.el --- module -*- lexical-binding: t -*-

(require 'undo-tree)

(use-package evil
  :demand t
  :general
  (:states 'motion
	   [C-i] 'evil-jump-foward)
  (:keymaps 'help-mode-map
	    :states 'motion
	    "<escape>" 'quit-window
	    "<tab>" 'forward-button
	    "S-<tab>" 'backward-button
	    "]" 'help-go-forward
	    "gf" 'help-go-forward
	    "[" 'help-go-back
	    "gb" 'help-go-back
	    "gh" 'help-follow-symbol)
  (:states 'normal
	   "zf" 'reposition-window)
  (:states 'normal
	   :prefix "SPC"
	   "re" 'evil-show-registers)
  (:states 'visual
	   "<" "<gv"
	   ">" ">gv"
	   ;; "J" 'drag-stuff-up
	   ;; "K" 'drag-stuff-down
	   "u" 'undo
	   )
  (:keymaps 'comint-mode-map
	    :states '(normal insert)
	    "C-k" 'comint-previous-input
	    "C-j" 'comint-next-input)
  :custom (evil-want-Y-yank-to-eol t)
  :init
  (setq evil-normal-state-cursor '("DarkGoldenrod2" box)
	evil-insert-state-cursor '("chartreuse3" (bar . 2))
	evil-emacs-state-cursor '("SkyBlue2" box)
	evil-replace-state-cursor '("chocolate" (hbar . 2))
	evil-visual-state-cursor '("gray" (hbar . 2))
	evil-motion-state-cursor '("plum3" box)
	evil-undo-system 'undo-tree
	evil-want-keybinding nil
	evil-want-integration t)

  (setq-default evil-shift-width 2)
  :config
  (evil-mode 1))

;; evil everywhere
(use-package evil-collection
  :after evil
  :custom
  (evil-collection-setup-minibuffer t)
  :config
  (evil-collection-init))

(use-package evil-escape
  :commands evil-escape
  :init
  (setq evil-escape-excluded-states '(normal visual multiedit emacs motion)
	evil-escape-excluded-major-modes '(vterm-mode)
	evil-escape-key-sequence "jk"
	evil-escape-delay 0.15)
  (evil-escape-mode))

(use-package evil-nerd-commenter)

(use-package evil-exchange
  :commands evil-exchange
  :config
  (evil-exchange-install))

(provide 'module-evil)
