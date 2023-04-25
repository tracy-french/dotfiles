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

(setq blink-matching-paren nil)

(setq x-stretch-cursor nil)


;; -----------------------------------------------------------------------------
;; fringes
;; -----------------------------------------------------------------------------

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
(add-hook 'tf/init-ui-hook #'window-divider-mode)

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

(use-package hl-line
  :defer t
  :config (global-hl-line-mode))

(use-package all-the-icons
  :defer t)

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
