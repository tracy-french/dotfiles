;;; core-ui.el --- core -*- lexical-binding: t -*-


;; -----------------------------------------------------------------------------
;; hooks
;; -----------------------------------------------------------------------------

(defvar tf/init-ui-hook nil
  "list of hooks ran when ui has been intialized")

(defvar tf/switch-buffer-hook nil
  "list of hooks ran after changing the current buffer")

(defvar tf/switch-window-hook nil
  "list of hooks ran after changing the focused window")

(defvar tf/switch-frame-hook nil
  "list of hooks ran after changing the focused frame")

(defun tf/run-switch-buffer-hook-h (&optional _)
  (let ((gc-cons-threshold most-positive-fixnum)
        (inhibit-redisplay t))
    (run-hooks 'tf/switch-buffer-hook)))

(defun tf/run-switch-window-or-frame-hooks-h (&optional _)
  (let ((gc-cons-threshold most-positive-fixnum)
        (inhibit-redisplay t))
    (unless (equal (old-selected-frame) (selected-frame))
      (run-hooks 'tf/switch-frame-hook))
    (unless (or (minibufferp)
                (equal (old-selected-window) (minibuffer-window)))
      (run-hooks 'tf/switch-window-hook))))


;; -----------------------------------------------------------------------------
;; functions
;; -----------------------------------------------------------------------------

(defun tf/visible-buffers (&optional buffer-list)
  "return list of visible buffers"
  (let ((buffers (delete-dups (mapcar #'window-buffer (window-list)))))
    (if buffer-list
        (cl-delete-if (lambda (b) (memq b buffer-list))
                      buffers)
      (delete-dups buffers))))


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

;; keep the cursor out of readonly minibuffer sections
(setq minibuffer-prompt-properties '(read-only t
                                     intangible t
                                     cursor-intangible t
                                     face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)


;; -----------------------------------------------------------------------------
;; built-in package config
;; -----------------------------------------------------------------------------

(setq ansi-color-for-comint-mode t)

(with-eval-after-load 'comint
  (progn
    (setq comint-prompt-read-only t
          comint-buffer-maximum-size 2048)))

(with-eval-after-load 'compile
  (progn
    (setq compilation-always-kill t
          compilation-ask-about-save nil
          compilation-scroll-output 'first-error)

    ;; truncate compilation buffers to prevent build up
    (autoload 'comint-truncate-buffer "comint" nil t)
    (add-hook 'compilation-filter-hook #'comint-truncate-buffer)))

(with-eval-after-load 'ediff
  (setq ediff-diff-options "-w"
        ediff-split-window-function #'split-window-horizontally
        ediff-window-setup-function #'ediff-setup-windows-plain)

  ;; restore window config when quitting ediff
  (defvar tf--ediff-saved-wconf nil)

  (defun tf/ediff-save-wconf-h ()
    (setq tf--ediff-saved-wconf (current-window-configuration)))

  (defun tf/ediff-restore-wconf-h ()
    (when (window-configuration-p tf--ediff-saved-wconf)
      (set-window-configuration tf--ediff-saved-wconf)))

  (add-hook 'ediff-before-setup-hook #'tf/ediff-save-wconf-h)
  (add-hook 'ediff-quit-hook :append #'tf/ediff-restore-wconf-h)
  (add-hook 'ediff-suspend-hook :append #'tf/ediff-restore-wconf-h))

(use-package hl-line
  :defer t
  :config (global-hl-line-mode))

(use-package winner
  :defer t
  :preface (defvar winner-dont-bind-my-keys t)
  :config (winner-mode))

(use-package paren
  :defer t
  :config
  (setq show-paren-delay 0.1
        show-paren-highlight-openparen t
        show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t)

  (show-paren-mode))

(use-package whitespace
  :config
  (setq whitespace-line-column nil
        whitespace-style
        '(face indentation tabs tab-mark spaces space-mark newline newline-mark
               trailing lines-tail)
        whitespace-display-mappings
        '((tab-mark ?\t [?> ?\t])
          (newline-mark ?\n [?¬ ?\n])
          (space-mark ?\  [?·] [?.]))))

(use-package all-the-icons
  :defer t)

;; hide modeline in some places it's not needed
(add-hook 'completion-list-mode-hook #'hide-mode-line-mode)
(add-hook 'Man-mode-hook #'hide-mode-line-hook)

(use-package highlight-numbers
  :defer t
  :hook ((prog-mode conf-mode) . highlight-numbers-mode)
  :config
  (setq highlight-numbers-generic-regexp
        "\\_<[[:digit:]]+\\(?:\\.[0-9]*\\)?\\_>"))

(setq image-animate-loop t)

(setq rainbow-delimiters-max-face-count 4)

(add-hook 'text-mode-hook #'auto-fill-mode)
(add-hook 'prog-mode-hook #'auto-fill-mode)

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

;; -----------------------------------------------------------------------------
;; bootstrap
;; -----------------------------------------------------------------------------

(defun tf/init-ui-h (&optional _)
  (run-hooks 'tf/init-ui-hook)

  (add-hook 'window-selection-change-functions
            #'tf/run-switch-window-or-frame-hooks-h)
  (add-hook 'window-buffer-change-functions
            #'tf/run-switch-buffer-hook-h)
  (add-hook 'server-visit-hook
            #'tf/run-switch-buffer-hook-h))

(add-hook 'window-setup-hook #'tf/init-ui-h)

(provide 'core-ui)
