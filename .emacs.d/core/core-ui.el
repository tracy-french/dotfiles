;;; core-ui.el --- core -*- lexical-binding: t -*-

;; -----------------------------------------------------------------------------
;; ux
;; -----------------------------------------------------------------------------

;; confirm before killing emacs for safety
(setq confirm-kill-emacs 'yes-or-no-p)

;; no prompting when creating new files and buffers
(setq confirm-nonexistent-file-or-buffer nil)

(setq uniquify-buffer-name-style 'forward
      ring-bell-function #'ignore
      visible-bell nil)


;; -----------------------------------------------------------------------------
;; scrolling
;; -----------------------------------------------------------------------------

(setq hscroll-margin 2
      hscroll-step 1
      scroll-conservatively 101
      scroll-margin 0
      scroll-preserve-screen-position t
      auto-window-vscroll nil)


;; -----------------------------------------------------------------------------
;; cursor
;; -----------------------------------------------------------------------------

(blink-cursor-mode -1)

;; don't blink matching paren at point
(setq blink-matching-paren nil)

(setq x-stretch-cursor nil)


;; -----------------------------------------------------------------------------
;; fringes
;; -----------------------------------------------------------------------------

;; less visual noise
(setq indicate-buffer-boundaries nil
      indicate-empty-lines nil)


;; -----------------------------------------------------------------------------
;; frames and windows
;; -----------------------------------------------------------------------------

(setq frame-title-format '("%b - Emacs")
      icon-title-format frame-title-format)

(setq frame-resize-pixelwise t
      window-resize-pixelwise nil)

(setq use-dialog-box nil)
(when (bound-and-true-p tooltip-mode)
  (tooltip-mode -1))

(setq window-divider-default-places t
      window-divider-default-bottom-width 1
      window-divider-default-right-width 1)

;; favor vertical splits
(setq split-width-threshold 160
      split-height-threshold nil)


;; -----------------------------------------------------------------------------
;; minibuffer
;; -----------------------------------------------------------------------------

(setq enable-recursive-minibuffers t)

(setq echo-keystrokes 0.02)

(setq resize-mini-windows 'grow-only)

(setq use-short-answers t)

;; keep cursor out of readonly portions of the minibuffer
(setq minibuffer-prompt-properties '(read-only t intangible t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)


;; -----------------------------------------------------------------------------
;; packages
;; -----------------------------------------------------------------------------

(use-package hl-line
  :defer t
  :config (global-hl-line-mode))

;; undo/redo changes to Emacs' window layout
(use-package winner
  :preface (defvar winner-dont-bind-my-keys t) 
  :init
  (winner-mode +1))

(use-package all-the-icons
   :commands (all-the-icons-octicon
             all-the-icons-faicon
             all-the-icons-fileicon
             all-the-icons-wicon
             all-the-icons-material
             all-the-icons-alltheicon))

(use-package highlight-numbers
  :hook ((prog-mode conf-mode) . highlight-numbers-mode)
  :config (setq highlight-numbers-generic-regexp "\\_<[[:digit:]]+\\(?:\\.[0-9]*\\)?\\_>"))

;; -----------------------------------------------------------------------------
;; line numbers
;; -----------------------------------------------------------------------------

(setq-default display-line-numbers-type 'relative
              display-line-numbers-width 3
              display-line-numbers-widen t)

(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'text-mode-hook #'display-line-numbers-mode)
(add-hook 'conf-mode-hook #'display-line-numbers-mode)

(column-number-mode t)

(provide 'core-ui)
