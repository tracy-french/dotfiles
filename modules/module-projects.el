;;; module-projects.el --- module -*- lexical-binding: t -*-

(use-package projectile
  :commands (projectile-project-root
             projectile-project-name
             projectile-project-p
             projectile-locate-dominating-file
             projectile-relevant-known-projects)
  :init
  (setq projectile-cache-file "~/.emacs.d/cache/projectile.cache"
        projectile-enable-caching (not noninteractive)
        projectile-globally-ignored-files '(".DS_Store")
        projectile-globally-ignored-file-suffixes '(".elc")
        projectile-kill-buffers-filter 'kill-only-files
        projectile-known-projects-file "~/.emacs.d/cache/projectile.projects"
        projectile-ignored-projects '("~/"))
  :config
  ;; per-project compilation buffers
  (setq compilation-buffer-name-function #'projectile-compilation-buffer-name
        compilation-save-buffers-predicate #'projectile-current-project-buffer-p)
    :general
  ("C-c p" 'projectile-command-map)
  :config
  (projectile-mode 1)
  (add-to-list 'projectile-globally-ignored-directories "node_modules"))

(provide 'module-projects)
