;;; module-keys.el --- module -*- lexical-binding: t -*-

;; instantly display current keystrokes in mini buffer
(setq echo-keystrokes 0.02)
;; auto-indent on RET
(define-key global-map (kbd "RET") 'newline-and-indent)

(use-package which-key
  :hook (which-key-init-buffer . (lambda ()
				   (setq-local line-spacing 3)))
  :init
  (progn
    (setq which-key-add-column-padding 1
	  which-key-max-display-columns nil
	  which-key-min-display-lines 6
	  which-key-prevent-C-h-from-cycling t
	  which-key-sort-order 'which-key-key-order-alpha
	  which-key-sort-uppercase-first nil)
  :config
  (which-key-setup-side-window-bottom)
  (which-key-mode)))

(use-package general
  :config
  (general-define-key
   "RET" 'newline-and-indent
   "M-SPC" 'cycle-spacing)

  (general-define-key
   :keymaps 'isearch-mode-map
   ;; alternate binding to search next occurrence with isearch without
   ;; exiting isearch
   "S-<return>" 'isearch-repeat-forward
   "M-S-<return>" 'isearch-repeat-backward
   ;; Escape from isearch-mode("/" and "?" in evil-mode) like vim
   "<escape>" 'isearch-cancel)

  (general-define-key
   :states 'normal
   :prefix "SPC"
   :non-normal-prefix "M-SPC"
   "u" 'universal-argument
   "!" 'shell-command)

  (general-define-key
   :states 'normal
   ;; easy line movement
   "H" 'evil-first-non-blank
   "L" 'evil-end-of-line

   ;; easy paragraph movement
   "J" 'evil-forward-paragraph
   "K" 'evil-backward-paragraph)

  ;; move in insert mode
  (general-define-key
   :states 'insert
   "C-h" "<left>"
   "C-j" "<down>"
   "C-k" "<up>"
   "C-l" "<right>"))

(provide 'module-keys)
