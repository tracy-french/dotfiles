;;; early-init.el -*- lexical-binding: t -*-

(add-to-list 'default-frame-alist '(undecorated-round . t))

(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(tooltip-mode -1)

(setq load-prefer-newer t)
(setq gc-cons-threshold (* 16 1024 1024))

(setq package-enable-at-startup nil)
