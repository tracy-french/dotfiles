;;; -*- lexical-binding: t; -*-

;;; packages
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

;;; defaults
(set-default-coding-systems 'utf-8)
(setq indent-tabs-mode nil) 
(setq use-short-answers t) ; use y/n instead of yes/no
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode)
(global-set-key (kbd "C-M-u") 'universal-argument) ; C-u is used by vi to scroll

;; remember recent files
(recentf-mode 1)

;; keep directories clean
(use-package no-littering
  :ensure t)

(setq auto-save-file-name-transforms
	`((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

(add-to-list 'recentf-exclude no-littering-var-directory)
(add-to-list 'recentf-exclude no-littering-etc-directory)

(setq custom-file (no-littering-expand-etc-file-name "custom.el"))

;; open files in last location
(save-place-mode 1)

;; update buffers when files have changed
(setq global-auto-revert-non-file-buffers t)
(global-auto-revert-mode 1)

;; save command history
(setq history-length 25)
(savehist-mode 1)

;; turn off dialogs
(setq use-dialog-box nil)

;;; macos
(customize-set-variable mac-right-option-modifier nil)
(customize-set-variable mac-command-modifier 'super)
(customize-set-variable ns-function-modifier 'hyper)

;;; window management
(winner-mode 1)

;;; vi emulation
(use-package evil
  :ensure t
  :init
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))

(use-package evil-collection
  :ensure t
  :after evil
  :config
  (evil-collection-init))

(use-package evil-nerd-commenter ; use M-; to comment line or region
  :ensure t
  :init
  (evilnc-default-hotkeys))

;;; programming
;; eglot
(add-hook 'typescript-ts-mode 'eglot-ensure)

;; formatting
(use-package apheleia
  :ensure t
  :init
  (apheleia-global-mode +1))

;; pair handling
(electric-pair-mode 1)
(show-paren-mode 1)

;; multiple cursors
(use-package multiple-cursors
  :ensure t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

;; expand region
(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

;; undo/redo
(use-package vundo
  :ensure t
  :bind (("C-x u" . vundo)))

;;; projects
;; project handling
(use-package projectile
  :ensure t
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (projectile-mode +1))

;; git integration
(use-package magit
  :ensure t)

;;; keys
(use-package which-key
  :ensure t
  :config
  (which-key-mode))

;;; completion
;; vertical completion
(use-package vertico
  :ensure t
  :bind (:map vertico-map
              ("C-j" . vertico-next)
              ("C-k" . vertico-previous))
  :custom
  (vertico-cycle t)
  :init
  (vertico-mode 1))

;; completion annotations
(use-package marginalia
  :ensure t
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-list nil))
  :init
  (marginalia-mode 1))

;; fuzzy completion
(use-package orderless
  :ensure t
  :init
  (setq completion-styles '(orderless basic)
	completion-category-defaults nil
	completion-category-overrides '((file (styles partial-completion)))))

;; completion ui
(use-package corfu
  :ensure t
  :bind (("M-p" . corfu-popupinfo-scroll-down)
	 ("M-n" . corfu-popupinfo-scroll-up)
	 ("M-d" . corfu-popupinfo-toggle))
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.0)
  (corfu-echo-documentation 0.25)
  :config
  (eldoc-add-command #'corfu-insert)
  :init
  (global-corfu-mode 1)
  (corfu-popupinfo-mode 1))

;; completion at point
(use-package cape
  :ensure t
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify))

(use-package consult
  :ensure t
  :bind (("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c k" . consult-kmacro)

         ("C-x M-:" . consult-complex-command)
         ("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x r b" . consult-bookmark)
         ("C-x p b" . consult-project-buffer)

         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)
         ("C-M-#" . consult-register)
         ("M-y" . consult-yank-pop)

         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-g o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)

         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ("M-s e" . consult-isearch-history)

         :map isearch-mode-map
         ("M-e" . consult-isearch-history) 
         ("M-s e" . consult-isearch-history) 
         ("M-s l" . consult-line)           
         ("M-s L" . consult-line-multi)    

         :map minibuffer-local-map
         ("M-s" . consult-history)
         ("M-r" . consult-history))
  :config
  (setq completion-in-region-function #'consult-completion-in-region)
  )

;; embark
(use-package embark
  :ensure t
  :bind (("C-." . embark-act)
         ("C-;" . embark-dwim)
         ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)

  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :ensure t
  :after (embark consult)
  :demand t
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;;; user interface
;; icons
(use-package all-the-icons
  :ensure t)

;; mode line
(use-package doom-modeline
  :ensure t
  :custom
  (doom-modeline-height 15)
  (doom-modeline-bar-width 6)
  (doom-modeline-minor-modes t)
  (doom-modeline-buffer-file-name-style 'truncate-except-project)
  :init (doom-modeline-mode 1))

;; font
(set-face-attribute 'default nil
                    :family "Input Mono" ; config will fail to load if font is not available
                    :height 160)

;; theme
(use-package emacs
  :init
  (setq modus-themes-italic-constructs nil
        modus-themes-bold-constructs nil

        modus-themes-region '(bg-only accented)
        modus-themes-hl-line '(accented)

        modus-themes-mode-line '(accented borderless 8 4)
        modus-themes-tabs-accented t

        modus-themes-prompts '(background)
        modus-themes-completions '((matches . (background))
                                   (selection . (accented text-also))
                                   (popup . (accented text-also)))

        modus-themes-markup '(bold background italic)
        modus-themes-syntax nil
        modus-themes-lang-checkers '(background text-also)
        modus-themes-paren-match '(intense)
        modus-themes-subtle-line-numbers t
        modus-themes-diffs 'desaturated
        modus-themes-intense-mouseovers nil

        modus-themes-links '(no-underline background)
        modus-themes-org-blocks 'tinted-background
        modus-themes-org-agenda
        '((header-block . (1.0 semibold))
          (header-date . (bold-today 1.0))
          (event . (accented varied))
          (scheduled . rainbow)
          (habit . traffic-light))
        modus-themes-headings
        '((t . (rainbow ultrabold))))

  :config
  (load-theme 'modus-operandi)
  :bind ("<f5>" . modus-themes-toggle))

;;; org
(setq org-agenda-files '("~/src/org/gtd/"))

(use-package org
  :bind (("C-c c" . org-capture)
         ("C-c a" . org-agenda))
  :config
  (setq org-startup-folded t
        org-startup-indented t
        org-cycle-separator-lines 0
        org-special-ctrl-a/e t
        org-special-ctrl-k t
        org-yank-adjusted-subtrees t
        org-log-done 'time
        org-log-into-drawer t
        org-return-follows-link t
        org-tags-match-list-sublevels t
        org-src-fontify-natively t
        org-src-preserve-indentation nil
        org-edit-src-content-indentation 0
        org-catch-invisible-edits 'error)

  (setq org-todo-keywords
        '((sequence "TODO(t)" "|" "DONE(d)")
          (sequence "WAIT(w@/!)" "|" "DONE(d)")))
  
  (setq org-capture-templates
        '(("i" "inbox" entry (file "~/src/org/gtd/inbox.org") "* TODO %?")))

  (setq org-tag-alist
        '((:startgrouptag)
          ("tasks")
          (:grouptags)
          ("brainstorm" . ?b) ("chore". ?c) ("develop" . ?d) ("learn" . ?l) ("meet" . ?m)
          ("mentor" . ?n) ("plan" . ?p) ("review" . ?r) ("schedule" . ?s) ("understand" . ?u)
          ("write" . ?w)
          (:endgrouptag)


          (:startgrouptag)
          ("focus")
          (:grouptags)
          ("shallow" . ?A) ("moderate" . ?B) ("deep" . ?C)
          (:endgrouptag)))

  ;; eisenhower matrix priorities
  (setq org-priority-highest 1
        org-priority-lowest 4
        org-priority-default 4)

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

;; dashboard
(use-package dashboard
  :ensure t
  :config
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-set-navigator t)
  (setq dashboard-projects-backend 'projectile)
  (setq dashboard-projects-switch-function 'counsel-projectile-switch-project-by-name)

  (setq dashboard-items '((recents  . 5)
                          (bookmarks . 5)
                          (projects . 5)
                          (agenda . 5)
                          (registers . 5)))

  (dashboard-setup-startup-hook))

(setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*"))) ; when using emacsclient

;; tabs
(use-package centaur-tabs
  :ensure t
  :demand t
  :init
  (setq centaur-tabs-enable-key-bindings t)
  :config
  (setq centaur-tabs-set-icons t)
  (setq centaur-tabs-set-close-button nil)
  (setq centaur-tabs-set-modified-marker t)
  (centaur-tabs-mode t)
  (centaur-tabs-group-by-projectile-project)
  :bind
  (:map evil-normal-state-map
        ("g t" . centaur-tabs-forward)
        ("g T" . centaur-tabs-backward)))

;;; file explorer
(use-package treemacs
  :ensure t
  :defer t
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-evil
  :after (treemacs evil)
  :ensure t)

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t)

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)
