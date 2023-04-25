;;; init.el --- personal configuration -*- lexical-binding: t; -*-

;; ---------------------------------------------------------------------------
;; navigation
;; ---------------------------------------------------------------------------

;; revert dired
(global-auto-revert-mode)
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)
(add-to-list 'global-auto-revert-ignore-modes 'Buffer-menu-mode)

;; ---------------------------------------------------------------------------
;; edit
;; ---------------------------------------------------------------------------

;; Start with the *scratch* buffer in text mode (speeds up Emacs load time,
;; because it avoids autoloads of elisp modes)
(setq initial-major-mode 'text-mode)

;; ---------------------------------------------------------------------------
;; session
;; ---------------------------------------------------------------------------

;; scratch buffer empty
(setq initial-scratch-message nil)

;; ---------------------------------------------------------------------------
;; compilation
;; ---------------------------------------------------------------------------

;; Don't load outdated compiled files.
(setq load-prefer-newer t)

;; Suppress the *Warnings* buffer when native compilation shows warnings.
(setq native-comp-async-report-warnings-errors 'silent)

;; suppress warnings of using `cl'
;; TODO: change usage of `cl' to `cl-lib'
(setq byte-compile-warnings '(cl-functions))

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

;; setup $PATH correctly
(use-package exec-path-from-shell
  :hook (after-init . exec-path-from-shell-initialize))

(add-to-list 'load-path "~/.emacs.d/core/")
(add-to-list 'load-path "~/.emacs.d/modules/")

(load-file "~/.emacs.d/core/core-emacs.el")
(load-file "~/.emacs.d/modules/modules.el")
