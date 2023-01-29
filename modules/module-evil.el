;;; module-evil.el --- module -*- lexical-binding: t -*-

;; pre-load evil config
(setq evil-want-integration t)
(setq evil-want-keybinding nil)

(defvar evil-want-C-g-bindings t)
(defvar evil-want-C-g-bindings t)
(defvar evil-want-C-i-jump nil)
(defvar evil-want-C-u-scroll t)
(defvar evil-want-C-u-delete t)
(defvar evil-want-C-w-delete t)
(defvar evil-want-Y-yank-to-eol t)
(defvar evil-want-abbrev-expand-on-insert-exit nil)
(defvar evil-respect-visual-line-mode t)

(use-package evil
  :demand t
  :preface
  (setq evil-ex-search-vim-style-regexp t
        evil-ex-visual-char-range t
        evil-mode-line-format 'nil
        evil-symbol-word-search t
        evil-emacs-state-cursor '("#b52c2c" box)
        evil-normal-state-cursor '("#b52c2c" box)
        evil-visual-state-cursor '("#b52c2c" hollow)
        evil-insert-state-cursor '("#b52c2c" bar)
        evil-replace-state-cursor '("#b52c2c" hbar)
        evil-operator-state-cursor '("#b52c2c" box)
        evil-ex-interactive-search-highlight 'selected-window
        evil-kbd-macro-suppress-motion-error t)
    :config
    (setq evil-normal-state-modes
          (append evil-emacs-state-modes
                  evil-insert-state-modes
                  evil-normal-state-modes
                  evil-motion-state-modes))
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
