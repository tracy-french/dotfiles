;;; init.el --- personal configuration -*- lexical-binding: t; -*-

;;; needs
;; revert files when they change outside of emacs
;; icons
;; (use-package projectile)
;; tab width
;; no littered files
;; eglot
;; consult
;; treesitter
;; apheleia
;; vertico
;; marginalia
;; orderless
;; corfu
;; cape
;; consult
;; magit
;; forge
;; doom modeline
;; doom theme
;; use package
;; ttypescript
;; org mode

;; Increase GC threshold size
(setq gc-cons-threshold (* 50 1000 1000))

;; Load package manager
(require 'package)

;; Link package repositories
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

;; Initialize package manager
(package-initialize)

;; Pull latest packages
(unless package-archive-contents
  (package-refresh-contents))

;; Install use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; Load use-package
(require 'use-package)

;; Default to downloading and installing packages from package manager
(setq use-package-always-ensure t)

;; Auto-update packages weekly
(use-package auto-package-update
  :custom
  (auto-package-update-interval 7)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe)
  (auto-package-update-at-time "09:00"))


;; Add newlines at the end of files with C-n
(setq next-line-add-newlines t)

;; Use command as meta on macos
(setq mac-option-key-is-meta nil
      mac-command-key-is-meta t
      mac-command-modifier 'meta
      mac-option-modifier 'none)


;; Remember recently edited files
(recentf-mode 1)

;; Remember last visited location in files
(save-place-mode)

;; No backup files please!
(setq make-backup-files nil)

;; https://github.com/emacsVcollective/no-littering
(use-package no-littering
  :after recentf
  :ensure t
  :init
  ;; Set cache location
  (setq user-emacs-directory "~/.cache/emacs")
  
  ;; https://github.com/emacscollective/no-littering#usage
  (setq no-littering-etc-directory
	(expand-file-name "config/" user-emacs-directory))
  (setq no-littering-var-directory
	(expand-file-name "data/" user-emacs-directory))

  :config
  ;; https://github.com/emacscollective/no-littering#recent-files
  (add-to-list 'recentf-exclude
	       (recentf-expand-file-name no-littering-var-directory))
  (add-to-list 'recentf-exclude
	       (recentf-expand-file-name no-littering-etc-directory))

  ;; https://github.com/emacscollective/no-littering#saved-customizations
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))

  (setq auto-save-file-name-transforms
	`((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

;; Hide the emacs logo screen
(setq inhibit-startup-message t)

;; Remove unused UI elements
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(tooltip-mode -1)

;; Add padding to buffers
(set-fringe-mode 10)

;; Use the visible bell
(setq visible-bell t)

;; Display column numbers in the modeline
(column-number-mode)

;; Use relative line numbers
(setq display-line-numbers 'relative)

;; Display line numbers everywhere
(global-display-line-numbers-mode t)

;; Disable line numbers in some modes
(dolist (mode '(org-mode-hook
		term-mode-hook
		shell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Set default font
(set-face-attribute 'default nil :family "Input Mono Condensed" :height 180)
(set-face-attribute 'fixed-pitch nil :family "Input Mono Condensed" :height 180)
(set-face-attribute 'variable-pitch nil :family "Input Serif Condensed" :height 180 :weight 'regular)

;; Setup theme
(use-package doom-themes
  :config
  (setq doom-themes-treemacs-enable-variable-pitch nil
	doom-themes-treemacs-theme "doom-colors")
  (load-theme 'doom-opera t)
  (doom-themes-visual-bell-config)
  (doom-themes-treemacs-config)
  (doom-themes-org-config))

;; Add pretty icons
;; make sure to run `all-the-icons-install-fonts`!
(use-package all-the-icons)

;; Add icons to dired
(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

;; Theme modeline
(use-package doom-modeline
  :init
  (setq display-time-default-load-average nil
	doom-modeline-bar-width 6
	doom-modeline-buffer-encoding nil
	doom-modeline-buffer-file-name-style 'file-name
	doom-modeline-height 15
	doom-modeline-modal nil
	doom-modeline-modal-icon nil)
  (display-battery-mode 1)
  (display-time-mode 1)
  (doom-modeline-mode))

;; Display available commands in the minibuffer
(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1))

;; Improve help page UI
(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

;; Use git in emacs
(use-package magit
  :commands magit-status
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; Interact with GitHub repos
;; Forge requires a token to connect to GitHub
(use-package forge
  :after magit)

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;;; vertical minibuffer completion
(use-package vertico
  :bind (:map vertico-map
	      ("C-j" . vertico-next)
	      ("C-k" . vertico-previous)
	      ("M-h" . vertico-directory-up))
  :hook (minibuffer-setup-hook . vertico-repeat-save)
  :init
  (setq vertico-resize nil
	vertico-count 10
	vertico-cycle t)
  (vertico-mode 1))

(use-package vertico-directory
  :ensure nil
  :after vertico
  :bind
  (:map vertico-map 
	("RET" . vertico-directory-enter)
	("DEL" . vertico-directory-delete-char)
	("M-DEL" . vertico-directory-delete-word)))

;; completion annotations
(use-package marginalia
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-list nil))
  :init (marginalia-mode 1))

;; fuzzy completion
(use-package orderless
  :init
  (progn
    (setq completion-styles '(orderless basic)
	  completion-category-overrides '((file (styles . (partial-completion)))))))

(use-package dabbrev
  :bind
  (("M-/" . dabbrev-completion)
   ("C-M-/" . dabbrev-expand))
  :custom
  (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))

;; completion ui
(use-package corfu
  :config
  ;; M-SPC inserts orderless separator
  (setq corfu-auto t
	corfu-auto-delay 0.0
	corfu-auto-prefix 2
	corfu-cycle t
	corfu-echo-documentation 0.25)
  (global-corfu-mode 1)

  (require 'corfu-popupinfo)
  (corfu-popupinfo-mode 1)
  
  (eldoc-add-command #'corfu-insert))

;; completion at point
(use-package cape
  :config
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev))

;; fuzzy lists for everything
(use-package consult
  :defer t
  :bind ("C-s" . consult-line)
  :init (setq completion-in-region-function #'consult-completion-in-region))

;; act at point
(use-package embark
  :defer t)

;; integrate embark and consult
(use-package embark-consult
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package all-the-icons-completion
  :config
  (all-the-icons-completion-mode))

(add-hook 'marginalia-mode-hook #'all-the-icons-completion-marginalia-setup)

(setq-default indent-tabs-mode nil
              tab-width 2)

(use-package eglot
  :ensure nil
  :defer t
  :hook ((eglot-managed-mode . tf/flymake-eslint-enable-maybe)
         (html-mode . eglot-ensure)
         (css-ts-mode . eglot-ensure)
         (js-ts-mode . eglot-ensure)
         (typescript-ts-base-mode . eglot-ensure)))

(use-package treesit
  :ensure nil
  :init
  (push '(css-mode . css-ts-mode) major-mode-remap-alist)
  (push '(javascript-mode . js-ts-mode) major-mode-remap-alist)
  (push '(js-mode . js-ts-mode) major-mode-remap-alist)
  (push '(js-json-mode . json-ts-mode) major-mode-remap-alist))

(use-package js-mode
  :ensure nil
  :defer t
  :mode (("\\.js\\'" . js-ts-mode)
         ("\\.jsx\\'" . js-ts-mode))
  :init
  (progn
    (setq js-indent-level 2
          js-jsx-indent-level 2)))

(use-package typescript-mode
  :ensure nil
  :defer t
  :mode (("\\.ts\\'" . typescript-ts-mode)
         ("\\.tsx\\'" . tsx-ts-mode)))

(use-package json-mode
  :ensure nil
  :defer t
  :mode ("\\.json\\'" . json-ts-mode))

(use-package ccs-mode
  :ensure nil
  :defer t
  :mode ("\\.css\\'" . css-ts-mode))

(use-package yaml-mode
  :ensure nil
  :defer t
  :mode (("\\.yml\\'" . yaml-ts-mode)
         ("\\.yaml\\'" . yaml-ts-mode)))


(use-package apheleia
  :init
  (apheleia-global-mode +1))

(defun tf/org-mode-setup ()
  (org-indent-mode)
  (visual-line-mode 1))

(use-package org
  :ensure nil
  :commands (org-capture org-agenda)
  :hook (org-mode . tf/org-mode-setup)
  :config
  (setq org-directory "~/org")

  (setq org-agenda-files
        '("~/org/things/calendar.org"
	  "~/org/things/actions.org"
	  "~/org/things/inbox.org"))

  (setq org-startup-folded t)

  (setq org-special-ctrl-a/e t)
  (setq org-special-ctrl-k t)
  
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)

  (setq org-todo-keywords
    '((sequence "TODO(t)" "|" "DONE(d!)")
      (sequence "WAIT(w@/!)" "|" "CANC(k@)")))

  (setq org-refile-targets
    '(("calendar.org" :maxlevel . 1)
      ("actions.org" :maxlevel . 1)))

  ;; Save Org buffers after refiling!
  (advice-add 'org-refile :after 'org-save-all-org-buffers)

  (setq org-tag-alist
    '((:startgroup)
       ; Put mutually exclusive tags here
       (:endgroup)
       ("@errand" . ?E)
       ("@home" . ?H)
       ("@work" . ?W)
       ("plan" . ?p)
       ("review" . ?r)
       ("note" . ?n)
       ("idea" . ?i)))

  ;; Configure custom agenda views
  (setq org-agenda-custom-commands
   '(("d" "Dashboard"
     ((agenda "" ((org-deadline-warning-days 7)))
      (todo "NEXT"
        ((org-agenda-overriding-header "Next Tasks")))
      (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))

    ("n" "Next Tasks"
     ((todo "NEXT"
        ((org-agenda-overriding-header "Next Tasks")))))

    ("W" "Work Tasks" tags-todo "+work-email")

    ;; Low-effort next actions
    ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
     ((org-agenda-overriding-header "Low Effort Tasks")
      (org-agenda-max-todos 20)
      (org-agenda-files org-agenda-files)))

    ("w" "Workflow Status"
     ((todo "WAIT"
            ((org-agenda-overriding-header "Waiting on External")
             (org-agenda-files org-agenda-files)))
      (todo "CANC"
            ((org-agenda-overriding-header "Cancelled Projects")
             (org-agenda-files org-agenda-files)))))))

  (setq org-capture-templates
    `(("t" "Tasks / Projects")
      ("tt" "Task" entry (file+olp "~/Projects/Code/emacs-from-scratch/OrgFiles/Tasks.org" "Inbox")
           "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)

      ("j" "Journal Entries")
      ("jj" "Journal" entry
           (file+olp+datetree "~/Projects/Code/emacs-from-scratch/OrgFiles/Journal.org")
           "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
           ;; ,(dw/read-file-as-string "~/Notes/Templates/Daily.org")
           :clock-in :clock-resume
           :empty-lines 1)
      ("jm" "Meeting" entry
           (file+olp+datetree "~/Projects/Code/emacs-from-scratch/OrgFiles/Journal.org")
           "* %<%I:%M %p> - %a :meetings:\n\n%?\n\n"
           :clock-in :clock-resume
           :empty-lines 1)

      ("w" "Workflows")
      ("we" "Checking Email" entry (file+olp+datetree "~/Projects/Code/emacs-from-scratch/OrgFiles/Journal.org")
           "* Checking Email :email:\n\n%?" :clock-in :clock-resume :empty-lines 1)

      ("m" "Metrics Capture")
      ("mw" "Weight" table-line (file+headline "~/Projects/Code/emacs-from-scratch/OrgFiles/Metrics.org" "Weight")
       "| %U | %^{Weight} | %^{Notes} |" :kill-buffer t)))

  (define-key global-map (kbd "C-c j")
    (lambda () (interactive) (org-capture nil "jj"))))

(defun tf/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . tf/org-mode-visual-fill))

