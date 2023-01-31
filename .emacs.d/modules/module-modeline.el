;;; module-modeline.el --- module -*- lexical-binding: t -*-

;; modeline
(use-package doom-modeline
  :defer t
  :init
  (progn
    (setq display-time-default-load-average nil
	  doom-modeline-bar-width 6
	  doom-modeline-buffer-encoding nil
	  doom-modeline-buffer-file-name-style 'file-name
	  doom-modeline-height 15
	  doom-modeline-modal nil
	  doom-modeline-modal-icon nil)
    (display-battery-mode 1)
    (display-time-mode 1)
    (doom-modeline-mode)))

(use-package fancy-battery
  :defer t
  :init
  (progn
    (setq fancy-batter-show-percentage t)
    (fancy-battery-mode)))

(provide 'module-modeline)
