;;; module-editing-visual.el --- module -*- lexical-binding: t -*-

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
  :commands highlight-parentheses-minibuffer-setup
  :init
  (progn
    (add-hook 'prog-mode-hook #'highlight-parentheses-mode)
    (add-hook 'minibuffer-setup-hook #'highlight-parentheses-minibuffer-setup)))

(use-package rainbow-delimiters
  :defer t
  :init
  (progn
    (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)))

(provide 'module-editing-visual)
