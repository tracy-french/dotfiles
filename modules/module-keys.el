;;; module-keys.el --- module -*- lexical-binding: t -*-

(use-package which-key
  :config
  (setq which-key-idle-delay 0.5)
  (which-key-mode))

(use-package general
  :config
  (general-define-key
   :states 'normal
   ;; easy line movement
   "H" 'evil-first-non-blank
   "L" 'evil-end-of-line

   ;; easy paragraph movement
   "J" 'evil-forward-paragraph
   "K" 'evil-backward-paragraph

  ;; move in insert mode
  (general-define-key
   :states 'insert
   "C-h" "<left>"
   "C-j" "<down>"
   "C-k" "<up>"
   "C-l" "<right>")))

(provide 'module-keys)
