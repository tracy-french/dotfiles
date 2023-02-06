;;; core-dashboard.el --- core -*- lexical-binding: t -*-

(use-package dashboard
  :hook (dashboard-mode . (lambda () (setq-local global-hl-line-mode nil)))
  :init
  (progn
    (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*"))
          dashboard-center-content t
          dashboard-items '((projects . 5))
          dashboard-set-file-icons t
          dashboard-set-heading-icons t
          dashboard-set-init-info nil
          dashboard-set-footer nil))
  :config
  (dashboard-setup-startup-hook))

(provide 'core-dashboard)
