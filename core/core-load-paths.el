;;; core-load-paths.el --- core -*- no-byte-compile: t; lexical-binding: t -*-

;;;; PATH variables and constants

;; ~/
(defconst tf-emacs-home-directory
  (expand-file-name "~/")
  "home directory")

;; ~/.emacs.d
(defconst tf-emacs-start-directory
  (concat tf-emacs-home-directory ".emacs.d/")
  "start directory")

;; ~/.emacs.d/core
(defconst tf-emacs-core-directory
  (concat tf-emacs-start-directory "core/"))

;; ~/.emacs.d/.cache
(defconst tf-emacs-cache-directory
  (concat tf-emacs-start-directory ".cache/")
  "storage area for persistent files")

;; ~/.emacs.d/.cache/auto-save
(defconst tf-emacs-auto-save-directory
  (concat tf-emacs-cache-directory "auto-save/")
  "auto-save-directory")


;;;; setup cache directories

(unless (file-exists-p tf-emacs-cache-directory)
  (make-directory tf-emacs-cache-directory))

(setq pcache-directory (concat tf-emacs-cache-directory "pcache/"))

(provide 'core-load-paths)
