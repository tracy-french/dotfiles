;;; module-git.el --- module -*- lexical-binding: t -*-

(use-package magit :defer t)
(use-package forge :after magit)

(use-package magit-todos
  :after magit
  :hook (magit-mode . magit-todos-mode))

(use-package git-gutter
  :hook (prog-mode . git-gutter-mode)
  :config
  (setq git-gutter:update-interval 0.02))

(use-package git-gutter-fringe
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))

(use-package general
  :general
  (:states 'normal
           "SPC g" '(:ignore t :which-key "git")
           "SPC g f" '(forge-dispatch :which-key "forge")
           "SPC g m" '(magit-dispatch :which-key "magit")
           "SPC g s" '(magit-status :which-key "status")))

(provide 'module-git)
