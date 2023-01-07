;;; -*- lexical-binding: t; -*-

;;; garbage collection
(setq gc-cons-threshold (* 50 1000 1000))

;;; compilation
;; load newest compiled .el file
(customize-set-variable 'load-prefer-newer t)

;; ignore compilation errors
(setq native-comp-async-report-warnings-errors nil)

;;; user interface
(setq inhibit-startup-message t)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;;; start in fundamental-mode
(customize-set-variable 'initial-major-mode 'fundamental-mode)
