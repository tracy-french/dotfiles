;;; module-tabs.el --- module -*- lexical-binding: t -*-

(use-package centaur-tabs
  :general
  (:states 'normal
  "gt" 'centaur-tabs-forward
  "gT" 'centaur-tabs-backward
           )
  :config
  (setq centaur-tabs-set-icons t
        centaur-tabs-gray-out-icons 'buffer
        centaur-tabs-set-bar 'left
        centaur-tabs-set-modified-marker t
        centaur-tabs-close-button "×"
        centaur-tabs-modified-marker "•"
        centaur-tabs-cycle-scope 'tabs)
  (centaur-tabs-mode 1))

(provide 'module-tabs)
