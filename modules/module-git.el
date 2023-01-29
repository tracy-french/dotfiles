;;; module-git.el --- module -*- lexical-binding: t -*-

;;; git
(use-package magit
    :if (executable-find "git")
    :general
    ("C-x g" 'magit-status)
    (:keymaps 'magit-status-mode-map
              "M-RET" 'magit-diff-visit-file-other-window)
    :config
    (put 'magit-edit-line-commit 'disabled nil))

(provide 'module-git)
