;;; init.el --- personal configuration -*- lexical-binding: t; -*-

;; ---------------------------------------------------------------------------
;; navigation
;; ---------------------------------------------------------------------------

(global-auto-revert-mode 1)
;; revert dired
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)
(add-to-list 'global-auto-revert-ignore-modes 'Buffer-menu-mode)

;; Make dired "guess" target directory for some operations, like copy to
;; directory visited in other split buffer.
(setq dired-dwim-target t)

(setq ring-bell-function 'ignore
      visible-bell nil)

;; Highlight and allow to open http link at point in programming buffers
;; goto-address-prog-mode only highlights links in strings and comments
(add-hook 'prog-mode-hook 'goto-address-prog-mode)

;; Highlight and follow bug references in comments and strings
(add-hook 'prog-mode-hook 'bug-reference-prog-mode)

;; Keep focus while navigating help buffers
(setq help-window-select 't)

;; Scroll compilation to first error or end
(setq compilation-scroll-output 'first-error)

;; Don't try to ping things that look like domain names
(setq ffap-machine-p-known 'reject)

;; ---------------------------------------------------------------------------
;; mouse
;; ---------------------------------------------------------------------------

;; Mouse cursor in terminal mode
(xterm-mouse-mode 1)

(when (boundp 'mouse-wheel-scroll-amount)
  ;; scroll two line at a time (less "jumpy" than defaults)
  (setq mouse-wheel-scroll-amount '(2)
        ;; don't accelerate scrolling
        mouse-wheel-progressive-speed nil))

;; ---------------------------------------------------------------------------
;; edit
;; ---------------------------------------------------------------------------

;; Start with the *scratch* buffer in text mode (speeds up Emacs load time,
;; because it avoids autoloads of elisp modes)
(setq initial-major-mode 'text-mode)

;; use only spaces and no tabs
(setq indent-tabs-mode nil
      tab-width 2)

;; text
(setq longlines-show-hard-newlines t)

;; Use system trash for file deletion.
;; This should work on Windows and Linux distros.
;; For macOS, see the osx layer.
(setq delete-by-moving-to-trash t)

;; auto fill breaks line beyond buffer's fill-column
(setq-default fill-column 80)

;; persistent abbreviation file
(setq abbrev-file-name "~/.emacs.d/cache/abbrev_defs")

;; Save clipboard contents into kill-ring before replace them
(setq save-interprogram-paste-before-kill t)

;; Single space between sentences is more widespread than double
(setq-default sentence-end-double-space nil)

;; The C-d rebinding that most shell-like buffers inherit from
;; comint-mode assumes non-evil configuration with its
;; `comint-delchar-or-maybe-eof' function, so we disable it
(with-eval-after-load 'comint
  (define-key comint-mode-map (kbd "C-d") nil))

;; ---------------------------------------------------------------------------
;; ui
;; ---------------------------------------------------------------------------

;; important for golden-ratio to better work
(setq window-combination-resize t)

;; Show column number in mode line
(setq column-number-mode t)

;; highlight current line
(global-hl-line-mode t)

;; no blinking cursor
(blink-cursor-mode 0)

;; When emacs asks for "yes" or "no", let "y" or "n" suffice
(fset 'yes-or-no-p 'y-or-n-p)

;; draw underline lower
(setq x-underline-at-descent-line t)

;; don't let the cursor go into minibuffer prompt
(setq minibuffer-prompt-properties
      '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt))

(setq ns-use-native-fullscreen t)

;; ---------------------------------------------------------------------------
;; session
;; ---------------------------------------------------------------------------

;; scratch buffer empty
(setq initial-scratch-message nil)

;; don't create backups
(setq make-backup-files nil)

;; auto save files
(setq auto-save-default t)

;; remove annoying ellipsis when printing sexp in message buffer
(setq eval-expression-print-length nil
      eval-expression-print-level nil)

;; cache files
(setq tramp-persistency-file-name "~/.emacs.d/tramp")

;; unlock disabled commands
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

;; ---------------------------------------------------------------------------
;; compilation
;; ---------------------------------------------------------------------------

;; Don't load outdated compiled files.
(setq load-prefer-newer t)

;; Suppress the *Warnings* buffer when native compilation shows warnings.
(setq native-comp-async-report-warnings-errors 'silent)

;; ---------------------------------------------------------------------------
;; packages
;; ---------------------------------------------------------------------------

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(use-package straight
  :custom (straight-use-package-by-default t))

;; ---------------------------------------------------------------------------
;; load
;; ---------------------------------------------------------------------------

(setenv "PATH" (concat (getenv "PATH") ":/bin"))
(setq exec-path (append exec-path '("/bin")))

;; setup $PATH correctly
(use-package exec-path-from-shell
  :init
  (exec-path-from-shell-initialize))

;; set shell
(setenv "SHELL" (expand-file-name "~/bin/zsh"))

(add-to-list 'load-path "~/.emacs.d/core/")
(add-to-list 'load-path "~/.emacs.d/modules/")

(load-file "~/.emacs.d/core/core-emacs.el")
(load-file "~/.emacs.d/modules/modules.el")
