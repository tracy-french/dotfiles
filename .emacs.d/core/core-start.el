;;; core-start.el --- core -*- lexical-binding: t -*-

;;; disable ui early
(push '(menu-bar-lines . 0)   default-frame-alist)
(push '(tool-bar-lines . 0)   default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(setq menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil)

;;; encodings
(set-language-environment "UTF-8")
(setq default-input-method nil)

(provide 'core-start)
