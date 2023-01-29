;;; module-org.el --- module -*- lexical-binding: t -*-

(setq org-agenda-files '("~/src/org/gtd/")) ; pre-load files

(use-package org
  :commands (org-clock-out org-occur-in-agenda-files org-agenda-files)
  :defer t
  :init
  (progn
    (setq org-startup-with-inline-images t
	  org-src-fontify-natively t
	  org-imenu-depth 8))
  :config
  (progn
    (font-lock-add-keywords
     'org-mode '(("\\(@@html:<kbd>@@\\) \\.(.*\\) \\(.*\\) \\(@@html:</kbd>@@\\)"
		  (1 font-lock-comment-face prepend)
		  (2 font-lock-function-name-face)
		  (3 font-lock-comment-face prepend))))
    (evil-define-key 'norm org-mode-map (kbd "RET") 'org-open-at-point)))

(use-package org-superstar
  :defer t
  :init (add-hook 'org-mode-hook 'org-superstar-mode))

(use-package toc-org
  :defer t
  :init
  (progn
    (setq toc-org-max-depth 10)
    (add-hook 'org-mode-hook 'toc-org-enable)))

(use-package evil-org
  :defer t
  :init
  (progn
    (add-hook 'org-mode-hook 'evil-org-mode)
    (setq evil-org-use-additional-insert t)))

(use-package org
  :config
  (setq org-startup-folded t)
  (setq org-startup-indented t)
  (setq org-cycle-separator-lines 0)
  (setq org-special-ctrl-a/e t)
  (setq org-special-ctrl-k t)
  (setq org-yank-adjusted-subtrees t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-return-follows-link t)
  (setq org-tags-match-list-sublevels t)
  (setq org-src-fontify-natively t)
  (setq org-src-preserve-indentation nil)
  (setq org-edit-src-content-indentation 0)
  (setq org-catch-invisible-edits 'error)

  (setq org-todo-keywords
        '((sequence "TODO(t)" "|" "DONE(d)")
          (sequence "WAIT(w@/!)" "|" "DONE(d)")))

  (setq org-capture-templates
        '(("i" "inbox" entry (file "~/src/org/gtd/inbox.org") "* TODO %?")))

  ;;; custom agendas
  (setq org-agenda-custom-commands
        '(("P" "prioritized next actions"
           ((agenda ""
                    ((org-agenda-overriding-header "deadlines today")
                     (org-agenda-span 1)
                     (org-deadline-warning-days 1)
                     (org-agenda-entry-types '(:deadline))))
            (agenda ""
                    ((org-agenda-overriding-header "scheduled today")
                     (org-agenda-span 1)
                     (org-agenda-entry-types '(:scheduled))))
            (tags-todo "+PRIORITY=\"1\""
                       ((org-agenda-overriding-header "important/urgent")))
            (tags-todo "+PRIORITY=\"2\""
                       ((org-agenda-overriding-header "important/not urgent")))
            (tags-todo "+Priority=\"3\""
                       ((org-agenda-overriding-header "not important/urgent")))
            (tags-todo "+PRIORITY=\"4\""
                       ((org-agenda-overriding-header "not important/not urgent")))))

          ("D" "daily review"
           ((agenda ""
                    ((org-agenda-overriding-header "deadlines today")
                     (org-agenda-time-grid nil)
                     (org-agenda-span 1)
                     (org-deadline-warning-days 1)
                     (org-agenda-entry-types '(:deadline))))
            (agenda ""
                    ((org-agenda-overriding-header "scheduled today")
                     (org-agenda-time-grid nil)
                     (org-agenda-span 1)
                     (org-agenda-entry-types '(:scheduled))))
            (tags-todo "+CATEGORY=\"inbox\""
                       ((org-agenda-overriding-header "need to be processed")))
            (tags-todo "+CATEGORY=\"next\""
                       ((org-agenda-overriding-header "available actions")))
            (tags-todo "+CATEGORY=\"waiting\""
                       ((org-agenda-overriding-header "waiting on")))))

          ("W" "weekly review"
           ((agenda ""
                    ((org-agenda-overriding-header "deadlines this week")
                     (org-agenda-span 7)
                     (org-agenda-start-on-weekday 1) ;; start on Monday
                     (org-deadline-warning-days 7)
                     (org-agenda-entry-types '(:deadline))))
            (agenda ""
                    ((org-agenda-overriding-header "scheduled this week")
                     (org-agenda-span 7)
                     (org-agenda-start-on-weekday 1) ;; start on Monday
                     (org-agenda-entry-types '(:scheduled))))
            (tags-todo "+CATEGORY=\"inbox\""
                       ((org-agenda-overriding-header "need to be processed")))
            (tags-todo "+CATEGORY=\"next\""
                       ((org-agenda-overriding-header "available actions")))
            (tags-todo "+CATEGORY=\"waiting\""
                       ((org-agenda-overriding-header "waiting on"))))))))

(provide 'module-org)
