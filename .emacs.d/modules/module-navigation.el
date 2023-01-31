;;; module-navigation.el --- module -*- lexical-binding: t -*-

(use-package auto-highlight-symbol
  :defer t
  :commands (ahs-highlight-p)
  :init
  (progn
    (setq ahs-case-fold-search nil
	  ahs-default-range 'ahs-range-whole-buffer
	  ahs-idle-interval 0.24
	  ahs-inhibit-face-list nil)))

(use-package centered-cursor-mode
  :commands (centered-cursor-mode
	     global-centered-cursor-mode)
  :config
  (progn
    (setq ccm-recenter-at-end-of-file t
	  ccm-ignored-commands '(mouse-drag-region
				 mouse-set-point
				 mouse-set-region
				 widget-button-click
				 scroll-bar-toolkit-scroll
				 evil-mouse-drag-region))
    (global-centered-cursor-mode)))

(use-package golden-ratio
  :defer t
  :general
  (:states 'normal
	   "SPC tg" 'toggle-golden-ratio-mode)
  :init
  (progn
    (defun toggle-golden-ratio-mode ()
      (interactive)
      (if golden-ratio-mode
	  (progn
	    (golden-ratio-mode -1)
	    (balance-windows))
	(progn
	  (golden-ratio-mode)
	  (golden-ratio)))))
  :config
  (progn
    (dolist (m '("calc-mode"
		 "ediff-mode"
		 "dired-mode"))
      (add-to-list 'golden-ratio-exclude-modes m))

    (dolist (f '(evil-window-delete
		 evil-window-split
		 evil-window-vsplit
		 evil-window-left
		 evil-window-right
		 evil-window-up
		 evil-window-down
		 evil-window-bottom-right
		 evil-window-top-left
		 evil-window-mru
		 evil-window-next
		 evil-window-prev
		 evil-window-new
		 evil-window-vnew
		 evil-window-rotate-upwards
		 evil-window-rotate-downwards
		 evil-window-move-very-top
		 evil-window-move-far-left
		 evil-window-move-far-right
		 evil-window-move-very-bottom
		 next-multiframe-window
		 previous-multiframe-window
		 quit-window
		 winum-select-window-0-or-10
		 winum-select-window-1
		 winum-select-window-2
		 winum-select-window-3
		 winum-select-window-4
		 winum-select-window-5
		 winum-select-window-6
		 winum-select-window-7
		 winum-select-window-8
		 winum-select-window-9
		 windmove-left
		 windmove-right
		 windmove-up
		 windmove-down))
      (add-to-list 'golden-ratio-extra-commands f))

    (add-to-list 'golden-ratio-exclude-buffer-names " *which-key*")
    (add-to-list 'golden-ratio-exclude-buffer-regexp (rx "*Treemacs" (0+ any)))))

					; restart-emacs

(use-package restart-emacs
  :after files
  :general
  (:states 'normal
	   "SPC qR" 'restart-emacs))

(setq scroll-preserve-screen-position t
      scroll-margin 0
      scroll-conservatively 101)

(provide 'module-navigation)
