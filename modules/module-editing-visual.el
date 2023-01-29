;;; module-editing-visual.el --- module -*- lexical-binding: t -*-

;; line numbers
(setq-default display-line-numbers 'visual
              display-line-numbers-widen t
              display-line-numbers-current-absolute t)

(defun my/relative-line-numbers ()
  "show relative line numbers"
  (setq-local display-line-numbers 'visual))

(defun my/absolute-line-numbers ()
  "show absolute line numbers"
  (setq-local display-line-numbers t))

;; use absolute line numbers in insert mode
(add-hook 'evil-insert-state-entry-hook #'my/absolute-line-numbers)
(add-hook 'evil-insert-state-exit-hook #'my/relative-line-numbers)

;; add some color to the current line number
(custom-set-faces '(line-number-current-line ((t :weight bold
                                               :foreground "#ffffff"
                                               :background "#483d8a"))))

(use-package highlight-indentation
  :defer t
  :init
  (progn
    (highlight-indentation-mode)))

(use-package highlight-numbers
  :defer t
  :init
  (progn
    (add-hook 'prog-mode-hook 'highlight-numbers-mode)))

(use-package highlight-parentheses
  :defer t
  :custom
  (highlight-parentheses-delay 0.2)
  (highlight-parentheses-colors '("Springgreen3"
				  "IndianRed1"
				  "IndianRed3"
				  "IndianRed4"))
  :custom-face (highlight-parentheses-highlight ((nil (:weight ultra-bold))))
  :commands highlight-parentheses-minibuffer-setup
  :init
  (progn
    (add-hook 'prog-mode-hook #'highlight-parentheses-mode)
    (add-hook 'minibuffer-setup-hook #'highlight-parentheses-minibuffer-setup)))

(use-package indent-guide
  :defer t
  :custom
  (indent-guide-delay 0.3)
  :init
  (indent-guide-global-mode))

(use-package rainbow-delimiters
  :defer t
  :init
  (progn
    (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)))

(provide 'module-editing-visual)
