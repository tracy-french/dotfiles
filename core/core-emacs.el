;;; core-emacs.el --- core -*- lexical-binding: t -*-

(setq message-log-max 10000)

(defgroup tf-emacs nil
  "tf emacs customizations"
  :group 'emacs
  :prefix 'tf-emacs-)

(require 'core-load-paths)
(require 'core-hooks)

(provide 'tf-emacs-core)
