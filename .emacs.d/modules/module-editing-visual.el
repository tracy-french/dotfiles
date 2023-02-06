;;; module-editing-visual.el --- module -*- lexical-binding: t -*-

(use-package column-enforce-mode
  :general
  (:states 'normal
	   :prefix "SPC t"
	   "8" 'column-enforce-mode
	   "C-8" 'global-column-enforce-mode)
  :config
  (global-column-enforce-mode))

(use-package hide-comnt
  :general
  (:states 'normal
	   "SPC ch" 'hide/show-comments-toggle)
  :init
  (progn
    (advice-add 'hide/show-comments
		:after (lambda (&optional hide/show start end)
			 (pcase hide/show
			   ('hide (message "Hide comments enabled."))
			   ('show (message "Hide commends disabled")))))))

(use-package highlight-indentation
  :general
  (:states 'normal
	   :prefix "SPC th"
	   "i" 'highlight-indentation-mode
	   "c" 'highlight-indentation-current-column-mode))

(use-package highlight-parentheses
  :hook ((prog-mode . highlight-parentheses-mode)
	 (minibuffer-setup . highlight-parentheses-minibuffer-setup))
  :general
  (:states 'normal
	   :prefix "SPC th"
	   "p" 'highlight-parentheses-mode
	   "P" 'global-highlight-parentheses-mode)
  :custom
  (highlight-parentheses-delay 0.2)
  (highlight-parentheses-colors '("Springgreen3"
                                  "IndianRed1"
                                  "IndianRed3"
                                  "IndianRed4"))
  :custom-face (highlight-parentheses-highlight ((nil (:weight ultra-bold))))
  :commands highlight-parentheses-minibuffer-setup)

(use-package indent-guide
  :general
  (:states 'normal
	   :prefix "SPC t"
	   "i" 'indent-guide-mode
	   "TAB" 'indent-guide-global-mode)
  :custom
  (indent-guide-delay 0.3))

(use-package rainbow-delimiters
  :general
  (:states 'normal
	   "SPC tCd" 'rainbow-delimeters-mode)
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package volatile-highlights
  :general
  (:states 'normal
	   "SPC thv" 'volatile-highlights-mode)
  :init
  (volatile-highlights-mode t)
  :config
  (progn
    (with-eval-after-load 'evil
      (vhl/define-extension 'evil
			    'evil-move
			    'evil-paste-after
			    'evil-paste-before
			    'evil-paste-pop)
      (vhl/install-extension 'evil)
      (vhl/load-extension 'evil))

    (with-eval-after-load 'undo-tree
      (vhl/define-extension 'undo-tree
			    'undo-tree-mode
			    'undo-tree-yank)
      (vhl/install-extension 'undo-tree)
      (vhl/load-extension 'undo-tree))))

(use-package writeroom-mode
  :general
  (:states 'normal
	   :prefix "SPC wc"
	   "c" '(tf/toggle-centered-buffer :wk "Center buffer")
	   "C" '(tf/toggle-distraction-free :wk "Center and maximize buffer"))
  :custom (writeroom-mode-line-toggle-position 'mode-line-format)
  :init
  (progn
    (defun tf/toggle-centered-buffer ()
      (interactive)
      (if writeroom-mode
	  (writeroom-mode -1)
	(progn
	  (let ((writeroom-maximize-window nil)
		(writeroom-mode-line t))
	    (writeroom-mode 1)))))

    (defun tf/toggle-distraction-free ()
      (interactive)
      (if writeroom-mode
	  (writeroom-mode -1)
	(progn
	  (let ((writeroom-maximize-window t)
		(writeroom-mode-line nil))
	    (writeroom-mode 1)))))))

(provide 'module-editing-visual)
