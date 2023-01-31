;;; module-helpful.el --- module -*- lexical-binding: t -*-

(use-package helpful
  :defer t
  :general
  (:states 'normal
	   :prefix "C-h"
	   "k" 'helpful-key
	   "f" 'helpful-callable
	   "v" 'helpful-variable)
  (:states 'normal
	   :prefix "SPC h"
	   "dk" 'helpful-key
	   "df" 'helpful-callable
	   "dv" 'helpful-variable)
  (:keymaps 'helpful-mode
	    "q" 'helpful-kill-buffers)

  (:keymaps 'helpful-mode
	    :states 'normal
	    "g r" 'helpful-update
	    "q" 'quit-window)
  :init
  (progn
    (evil-set-initial-state 'helpful-mode 'normal)
    (defalias 'describe-function 'helpful-callable)
    (defalias 'describe-variable 'helpful-variable)
    (defalias 'describe-key 'helpful-key)
    (add-hook 'helpful-mode-hook (lambda () (setq-local tab-width 8)))
    (advice-add 'helpful--navigate :after (lambda (_) (setq-local tab-width 8)))))

(with-eval-after-load 'helpful
  (evil-define-key 'normal helpful-mode-map (kbd "o") 'link-hint-open-link))

(provide 'module-helpful)
