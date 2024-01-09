;;; init.el -*- lexical-binding: t; -*-

(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))

;; Simplify UI
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-splash-screen t)
(setq use-file-dialog nil)

;; -----------------------------------------------------------------------------
;; Package management
;; -----------------------------------------------------------------------------

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq use-package-always-defer t)

(use-package straight
  :custom (straight-use-package-by-default t))

;; -----------------------------------------------------------------------------
;; Paths
;; -----------------------------------------------------------------------------

(setenv "PATH" (concat (getenv "PATH") ":/bin"))
(setq exec-path (append exec-path '("/bin")))

;; set shell
(setenv "SHELL" (expand-file-name "~/bin/zsh"))

;; setup $PATH correctly
(use-package exec-path-from-shell
  :hook (after-init . exec-path-from-shell-initialize))

;; -----------------------------------------------------------------------------
;; Compilation
;; -----------------------------------------------------------------------------

;; Don't load outdated compiled files.
(setq load-prefer-newer t)

;; Suppress the *Warnings* buffer when native compilation shows warnings.
(setq native-comp-async-report-warnings-errors 'silent)

;; suppress warnings of using `cl'
;; TODO: change usage of `cl' to `cl-lib'
(setq byte-compile-warnings '(cl-functions))

;; -----------------------------------------------------------------------------
;; Defaults
;; -----------------------------------------------------------------------------

;; Start with clean scratch buffer
(use-package emacs
  :init
  (setq initial-scratch-message nil)
  (defun display-startup-echo-area-message ()
    (message "")))

;; Simplify dialogs
(use-package emacs
  :init
  (defalias 'yes-or-no-p 'y-or-n-p))

;; Use spaces over tabs
(use-package emacs
  :init
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 2))

(electric-pair-mode t)
(show-paren-mode 1)

;;; Buffer defaults

;; Revert non-file buffers, such as Dired
(customize-set-variable 'global-auto-revert-non-file-buffers t)

;; Revert file buffers
(global-auto-revert-mode 1)

;; Update Dired buffers when revisiting directory
(customize-set-variable 'dired-auto-revert-buffer t)

;; Pop up dedicated buffers in a different window
(customize-set-variable 'switch-to-buffer-in-dedicated-window 'pop)

;; Use ibuffer for managing buffers
(keymap-global-set "<remap> <list-buffers>" #'ibuffer-list-buffers)

;;; Completion defaults

;; Delete selection when delete is pressed
(delete-selection-mode)

;; Do not save duplicated in the kill ring
(customize-set-variable 'kill-do-not-save-duplicates t)

;; Better support for files with long lines
(setq-default bidi-paragraph-direction 'left-to-right)
(setq-default bidi-inhibit-bpa t)
(global-so-long-mode 1)

;;; Persistence between sessions

;; Turn on recentf mode
(add-hook 'after-init-hook #'recentf-mode)

;; Enable savehist-mode for command history
(savehist-mode 1)

;; Save the bookmarks file every time it is updated, instead
;; of waiting until Emacs is killed.
(customize-set-variable 'bookmark-save-flag 1)

;; -----------------------------------------------------------------------------
;; MacOS
;; -----------------------------------------------------------------------------

(customize-set-variable mac-right-option-modifier nil)
(customize-set-variable mac-command-modifier 'super)
(customize-set-variable ns-function-modifier 'hyper)

;; Visit files opened outside of Emacs in existing frame, not a new one
(setq ns-pop-up-frames nil)

;; sane trackpad/mouse scroll settings
(setq mac-redisplay-dont-reset-vscroll t
      mac-mouse-wheel-smooth-scroll nil)

;; Sets `ns-transparent-titlebar' and `ns-appearance' frame parameters so window
;; borders will match the enabled theme.
(and (or (daemonp)
         (display-graphic-p))
     (require 'ns-auto-titlebar nil t)
     (ns-auto-titlebar-mode +1))

(let ((gls (executable-find "gls")))
  (when gls
    (setq insert-dictionary-program gls)))

(use-package osx-trash
  :init (osx-trash-setup))

(when (fboundp 'set-fontset-font)
  (set-fontset-font "fontset-default"
		                '(#x1F600 . #x1F64F)
		                (font-spec :name "Apple Color Emoji") nil 'prepend))

;; -----------------------------------------------------------------------------
;; Evil mode
;; -----------------------------------------------------------------------------

(use-package evil
  :demand t
  :custom (evil-want-Y-yank-to-eol t)
  :init
  (setq evil-normal-state-cursor '("DarkGoldenrod2" box)
	      evil-insert-state-cursor '("chartreuse3" (bar . 2))
	      evil-emacs-state-cursor '("SkyBlue2" box)
	      evil-replace-state-cursor '("chocolate" (hbar . 2))
	      evil-visual-state-cursor '("gray" (hbar . 2))
	      evil-motion-state-cursor '("plum3" box)
	      evil-want-keybinding nil
        evil-want-C-u-scroll t
        evil-want-C-i-jump nil
        evil-want-C-h-delete t
	      evil-want-integration t)

  (setq-default evil-shift-width 2)
  :config
  (evil-mode 1)

  (evil-set-undo-system 'undo-redo)

  ;; Use C-M-u for the universal argument instead of C-u
  (keymap-global-set "C-M-u" 'universal-argument))

;; Evil everywhere
(use-package evil-collection
  :after evil
  :custom (evil-collection-setup-minibuffer t)
  :init (evil-collection-init))

(use-package evil-nerd-commenter
  :init
  ;; Use "," to access keymap (or M-; to perform comment action)
  (evilnc-default-hotkeys))

;; Use relative numbers in prog mode
(use-package emacs
  :init
  (defun tf/enable-line-numbers ()
    "Enable relative line numbers"
    (interactive)
    (display-line-numbers-mode)
    (setq display-line-numbers 'relative))
  (add-hook 'prog-mode-hook #'tf/enable-line-numbers))

;; Icons
(use-package nerd-icons)

;; -----------------------------------------------------------------------------
;; Keybindings
;; -----------------------------------------------------------------------------

;; Display keybindings
(use-package which-key
  :demand
  :init
  (setq which-key-idle-delay 0.5)
  :config
  (which-key-mode))

;; Simplify setting keybindings
(use-package general
  :demand
  :config
  (general-evil-setup t)

  ;; Use space as leader
  (general-create-definer tf/leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "C-SPC"))

;; Don't use escape as a modifier
(use-package emacs
  :init
	(global-set-key (kbd "<escape>") 'keyboard-escape-quit))

;; -----------------------------------------------------------------------------
;; User interface
;; -----------------------------------------------------------------------------

;; confirm before killing emacs for safety
(setq confirm-kill-emacs 'yes-or-no-p)

;; no prompting when creating new files and buffers
(setq confirm-nonexistent-file-or-buffer nil)

(setq uniquify-buffer-name-style 'forward
      ring-bell-function #'ignore
      visible-bell nil)

(setq hscroll-margin 2
      hscroll-step 1
      scroll-conservatively 101
      scroll-margin 0
      scroll-preserve-screen-position t
      auto-window-vscroll nil)

(blink-cursor-mode -1)

(setq blink-matching-paren nil)

(setq x-stretch-cursor nil)

(setq indicate-buffer-boundaries nil
      indicate-empty-lines nil)

(setq frame-title-format '("%b - Emacs")
      icon-title-format frame-title-format)

(setq frame-resize-pixelwise t
      window-resize-pixelwise nil)

(setq use-dialog-box nil)
(when (bound-and-true-p tooltip-mode)
  (tooltip-mode -1))

(setq window-divider-default-places t
      window-divider-default-bottom-width 1
      window-divider-default-right-width 1)
(add-hook 'tf/init-ui-hook #'window-divider-mode)

;; favor vertical splits
(setq split-width-threshold 160
      split-height-threshold nil)

(setq enable-recursive-minibuffers t)

(setq echo-keystrokes 0.02)

(setq resize-mini-windows 'grow-only)

(setq use-short-answers t)

;; keep the cursor out of readonly minibuffer sections
(setq minibuffer-prompt-properties '(read-only t
                                               intangible t
                                               cursor-intangible t
                                               face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;; -----------------------------------------------------------------------------
;; Garbage collection
;; -----------------------------------------------------------------------------

;; prevent gc from blocking thread when needed
(use-package gcmh
  :straight t
  :defer t
  :init
  (setq gcmh-idle-delay 'auto
        gcmh-auto-idle-delay-factor 10
        gcmh-high-cons-threshold (* 16 1024 2024))
  :config (gcmh-mode))

;; -----------------------------------------------------------------------------
;; Completion
;; -----------------------------------------------------------------------------

;;; Vertical minibuffer completion
(use-package vertico
  :custom
  (vertico-cycle t)
  :init
  (setq vertico-resize nil
	      vertico-count 10
	      vertico-cycle t)
  (vertico-mode 1))

(add-to-list 'load-path
             (expand-file-name "straight/build/vertico/extensions"
                               straight-base-dir))

;; Directory completion extension
(use-package vertico-directory
  :straight nil
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)
  :general
  (:keymaps 'vertico-map
            "C-j" 'vertico-next
            "C-k" 'vertico-previous
            "M-h" 'vertico-directory-up))

;; Completion annotations
(use-package marginalia
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-list nil))
  :init (marginalia-mode 1))

;; Fuzzy completion
(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
	      completion-category-overrides '((file (styles . (partial-completion))))))

(use-package dabbrev
  :general
  (:states 'normal
	         "M-/" 'dabbrev-completion
	         "C-M-/" 'dabbrev-expand)
  :custom
  (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))

(add-to-list 'load-path
	           (expand-file-name "straight/build/corfu/extensions"
			                         straight-base-dir))

;; completion ui
(use-package corfu
  :general
  (:keymaps 'corfu-map
	          "M-p" 'corfu-popupinfo-scroll-down
	          "M-n" 'corfu-popupinfo-scroll-up
	          "M-d" 'corfu-popupinfo-toggle)
  :init
  (progn
    ;; M-SPC inserts orderless separator
    (setq corfu-auto t
	        corfu-auto-delay 0.0
	        corfu-auto-prefix 2
	        corfu-cycle t
	        corfu-echo-documentation 0.25)
    (global-corfu-mode 1)

    (require 'corfu-popupinfo)
    (corfu-popupinfo-mode 1))
  :config
  (eldoc-add-command #'corfu-insert))

;; Completion at point
(use-package cape
  :init
  (progn
    (add-to-list 'completion-at-point-functions #'cape-file)
    (add-to-list 'completion-at-point-functions #'cape-dabbrev)))

;; Fuzzy lists for everything
(use-package consult
  :defer t
  :general
  ("C-s" 'consult-line)
  (:keymaps 'minibuffer-local-map
	          :override t
	          "C-r" 'consult-history)
  :init (setq completion-in-region-function #'consult-completion-in-region))

;; Act at point
(use-package embark
  :defer t
  :general
  ("C-." 'embark-act)
  ("C-h B" 'embark-bindings)
  :init
  (progn
    (setq prefix-help-command #'embark-prefix-help-command)))

;; Integrate embark and consult
(use-package embark-consult
  :after (embark consult)
  :demand t
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;; -----------------------------------------------------------------------------
;; Help
;; -----------------------------------------------------------------------------

(use-package emacs
  :general
  (:states 'normal
           :prefix "SPC"
           "h" '(:ignore t :wk "Help")
           "ha" '(apropos :wk "Apropos")
           "hc" '(helpful-command :wk "Describe command")
           "hf" '(helpful-function :wk "Describe function")
           "hi" '(info :wk "Emacs documentation")
           "hk" '(helpful-key :wk "Describe key")
           "hm" '(helpful-mode :wk "Describe mode")
           "hs" '(helpful-symbol :wk "Describe symbol")
           "hv" '(helpful-variable :wk "Describe variable")))

(use-package helpful)

;; -----------------------------------------------------------------------------
;; Font
;; -----------------------------------------------------------------------------

(set-face-attribute
 'default nil :family "MonaspiceKr NF" :height 160)

;; -----------------------------------------------------------------------------
;; Commands
;; -----------------------------------------------------------------------------

(use-package emacs
  :general
  (:states 'normal
           :prefix "SPC"
           "SPC" '(execute-extended-command :wk "Execute command")))

;; -----------------------------------------------------------------------------
;; Files
;; -----------------------------------------------------------------------------

(use-package emacs
  :general
  (:states 'normal
           :prefix "SPC"
           "f" '(:ignore t :wk "Files")
           "ff" '(find-file :wk "Find file")
           "fl" '(load-file :wk "Load file")
           "fr" '(consult-recent-file :wk "Find recent file")
           "fC" '(copy-file :wk "Copy file")
           "fD" '(delete-file :wk "Delete file")
           "fR" '(rename-file :wk "Rename file")))

;; keep directories clean of litter
(use-package no-littering
  :config
  ;; handle auto-save litter
  (setq auto-save-file-name-transforms
	      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

  ;; prevent customization litter
  (setq custom-file (no-littering-expand-etc-file-name "custom.el")))

;; -----------------------------------------------------------------------------
;; Buffers
;; -----------------------------------------------------------------------------

(use-package emacs
  :general
  (:states 'normal
           :prefix "SPC"
           "b" '(:ignore t :wk "Buffers")
           "bl" '(ibuffer :wk "List buffers")
           "bk" '(kill-buffer :wk "Kill buffer")
           "bb" '(consult-buffer :wk "Switch to open buffer")
           "bt" '(consult-buffer-other-tab :wk "Open buffer in other tab")
           "bw" '(consult-buffer-other-window :wk "Open buffer in other window")))

;; -----------------------------------------------------------------------------
;; Windows
;; -----------------------------------------------------------------------------

(use-package emacs
  :general
  (:states 'normal
           :prefix "SPC"
           "w" '(:ignore t :wk "Windows")
           "w0" '(delete-window :wk "Delete window")
           "w1" '(delete-other-windows :wk "Delete other windows")
           "wh" '(split-window-vertically :wk "Split window horizontal")
           "wv" '(split-window-horizontally :wk "Split window vertical")
           "wn" '(evil-window-next :wk "Next window")
           "wp" '(evil-window-prev :wk "Previous window")
           "wm" '(minimize-window :wk "Minimize window")
           "wM" '(maximize-window :wk "Maximize window")
           "w=" '(balance-windows :wk "Balance windows")))

;; -----------------------------------------------------------------------------
;; Tabs
;; -----------------------------------------------------------------------------

(use-package emacs
  :general
  (:states 'normal
           :prefix "SPC"
           "t" '(:ignore t :wk "Tabs")
           "tl" '(tab-last :wk "Last tab (ARG)")
           "tn" '(tab-next :wk "Next tab (ARG)")
           "tp" '(tab-previous :wk "Previous tab (ARG)")
           "ts" '(tab-select :wk "Select tab by ARG")
           "tm" '(tab-move :wk "Move tab by ARG")
           "tM" '(tab-move-to :wk "Move tab to ARG")
           "tC" '(tab-new :wk "Create tab")
           "tD" '(tab-close :wk "Delete tab")
           "tU" '(tab-undo :wk "Restore last deleted tab")
           "tR" '(tab-rename :wk "Rename tab")))

;; -----------------------------------------------------------------------------
;; Theme
;; -----------------------------------------------------------------------------

(use-package doom-themes
  :demand
  :config
  (setq doom-themes-treemacs-enable-variable-pitch nil
	      doom-themes-treemacs-theme "doom-atom")
  (load-theme 'doom-challenger-deep t)
  (doom-themes-visual-bell-config)
  (doom-themes-treemacs-config)
  (doom-themes-org-config))

;; -----------------------------------------------------------------------------
;; Modeline
;; -----------------------------------------------------------------------------

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

(use-package fancy-battery
  :init
  (setq fancy-batter-show-percentage t)
  (fancy-battery-mode))

(use-package nyan-mode
  :init
  (nyan-mode))

;; -----------------------------------------------------------------------------
;; File tree
;; -----------------------------------------------------------------------------

(use-package treemacs
  :commands (treemacs-select-window
	           treemacs-select-scope-type
	           treemacs--window-number-ten
	           treemacs-current-visibility)
  :defer t
  :hook (treemacs-mode . (lambda () (setq-local display-line-numbers-mode nil)))
  :general
  (:states 'normal
	         :prefix "SPC"
	         "ft"   'treemacs)
  :config
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-git-mode 'simple))

(use-package treemacs-evil
  :defer t
  :after treemacs
  :general
  (:keymaps 'evil-treemacs-state-map
	          [return] 'treemacs-RET-action
	          [tab] 'treemacs-TAB-action
	          "TAB" 'treemacs-TAB-action
	          "o v" 'treemacs-visit-node-horizontal-split
	          "o s" 'treemacs-visit-node-vertical-split))

(use-package treemacs-projectile
  :after treemacs
  :defer t
  :init (require 'treemacs-projectile))

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-mode))

(use-package treemacs-all-the-icons
  :hook ((treemacs-mode dired-mode) . (lambda ()
					                              (treemacs-load-theme 'all-the-icons))))

(use-package treemacs-magit
  :after treemacs magit
  :defer t)

;; -----------------------------------------------------------------------------
;; Formatting
;; -----------------------------------------------------------------------------

(setq-default indent-tabs-mode nil
              tab-width 2)

(setq-default tab-always-indent nil)

(setq tabify-regexp "^\t* [ \t]+")

(setq-default fill-column 80)

(setq-default word-wrap t)

(setq-default truncate-lines t)

(setq truncate-partial-width-windows nil)

(setq sentence-end-double-space nil)

(setq require-final-newline t)

(add-hook 'text-mode-hook #'visual-line-mode)

(use-package apheleia
  :general
  (:states 'normal
	         "C-c C-f" 'apheleia-format-buffer)
  :init
  (apheleia-global-mode +1))


;; -----------------------------------------------------------------------------
;; Editing
;; -----------------------------------------------------------------------------

(use-package aggressive-indent
  :hook (prog-mode . aggressive-indent-mode))

(use-package editorconfig
  :config
  (editorconfig-mode t))

(use-package persistent-scratch
  :defer t
  :init
  (persistent-scratch-autosave-mode t))

(use-package unkillable-scratch
  :defer t
  :init
  (setq unkillable-scratch-do-not-reset-scratch-buffer t)
  (unkillable-scratch t))

;; -----------------------------------------------------------------------------
;; Visual editing
;; -----------------------------------------------------------------------------

(use-package rainbow-delimiters
  :init
  (setq rainbow-delimiters-max-face-count 4)
  :hook (prog-mode . rainbow-delimiters-mode))

;; -----------------------------------------------------------------------------
;; Projects
;; -----------------------------------------------------------------------------

(use-package projectile
  :config
  (projectile-mode))

;; -----------------------------------------------------------------------------
;; Syntax
;; -----------------------------------------------------------------------------

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package bash-mode
  :straight (:type built-in)
  :defer t
  :mode ("\\.sh\\'" . bash-ts-mode))

(use-package js-mode
  :straight (:type built-in)
  :defer t
  :mode (("\\.js\\'" . js-ts-mode)
         ("\\.jsx\\'" . js-ts-mode))
  :init
  (progn
    (setq js-indent-level 2
          js-jsx-indent-level 2)))

(use-package typescript-mode
  :straight (:type built-in)
  :defer t
  :mode (("\\.ts\\'" . typescript-ts-mode)
         ("\\.tsx\\'" . tsx-ts-mode)))

(use-package json-mode
  :straight (:type built-in)
  :defer t
  :mode ("\\.json\\'" . json-ts-mode))

(use-package ccs-mode
  :straight (:type built-in)
  :defer t
  :mode ("\\.css\\'" . css-ts-mode))

(use-package yaml-mode
  :straight (:type built-in)
  :defer t
  :mode (("\\.yml\\'" . yaml-ts-mode)
         ("\\.yaml\\'" . yaml-ts-mode)))

;; -----------------------------------------------------------------------------
;; Linting
;; -----------------------------------------------------------------------------

(use-package flymake-eslint :defer t)

(defun tf/flymake-eslint-enable-maybe ()
  "enable `flymake-eslint' based on the project configuration."
  (flymake-eslint-enable)
  (setq-local flymake-eslint-project-root
              (locate-dominating-file buffer-file-name ".eslintrc.js")))

;; -----------------------------------------------------------------------------
;; Code
;; -----------------------------------------------------------------------------

(use-package emacs
  :general
  (:states 'normal
           :prefix "SPC"
           "c" '(:ignore t :wk "Code")
           "ca" '(eglot-code-actions :wk "Act at point")
           "cf" '(apheleia-format-buffer :wk "Format buffer")
           "ci" '(eglot-find-implementation :wk "Find implementation at point")
           "ct" '(eglot-find-typeDefinition :wk "Find type definition at point")
           "cR" '(eglot-rename :wk "Rename at point")))

(use-package eglot
  :straight (:type built-in)
  :defer t
  :custom
  (eglot-autoshutdown t)
  (eglot-events-buffer-size 0)
  :hook ((eglot-managed-mode . tf/flymake-eslint-enable-maybe)
         (html-mode . eglot-ensure)
         (css-ts-mode . eglot-ensure)
         (js-ts-mode . eglot-ensure)
         (typescript-ts-base-mode . eglot-ensure))
  :init
  (put 'eglot-server-programs 'safe-local-variable 'listp)
  :config
  (add-to-list 'eglot-stay-out-of 'eldoc-documentation-strategy)
  (put 'eglot-error 'flymake-overlay-control nil)
  (put 'eglot-warning 'flymake-overlay-control nil))

;; -----------------------------------------------------------------------------
;; Git
;; -----------------------------------------------------------------------------

(use-package git-commit
  :defer t)

(use-package git-timemachine
  :defer t)

(use-package git-modes
  :defer t)

(use-package magit
  :defer t
  :custom (magit-bury-buffer-function #'magit-restore-window-configuration)
  :general
  (:states 'normal
           :prefix "SPC"
           "g" '(:ignore t :mk "Git")
           "gs" '(magit-status :wk "Status")))

(use-package magit-delta
  :hook (magit-mode . magit-delta-mode))

(use-package magit-gitflow
  :hook (magit-mode . magit-gitflow-mode)
  :general
  (:keymaps 'magit-mode-map
	          "%" 'magit-gitflow-popup)
  :init (setq magit-gitflow-popup-key "%"))

(use-package magit-section
  :defer t)

(use-package magit-todos
  :hook (magit-mode . magit-todos-mode))

;; -----------------------------------------------------------------------------
;; Shell
;; -----------------------------------------------------------------------------

(use-package emacs
  :general
  (:states 'normal
           :prefix "SPC"
           "s" '(:ignore t :wk "Shell")
           "st" '(vterm :wk "Open terminal")
           "so" '(vterm-other-window :wk "Open terminal other window")
           "s!" '(async-shell-command :wk "Run shell command")))

(use-package vterm)

;; -----------------------------------------------------------------------------
;; Validation
;; -----------------------------------------------------------------------------

(use-package emacs
  :general
  (:states 'normal
           :prefix "SPC"
           "v" '(:ignore t :wk "Validate")
           "vt" '(:ignore t :wk "Test")
           "vtt" '(jest-run-all-tests :wk "Run all tests")
           "vtf" '(jest-run-tests-for-file :wk "Run tests for file")
           "vtp" '(jest-run-test-at-point :wk "Run test at point")))

(defun jest-run-all-tests ()
  "Run all tests for a project"
  (interactive)
  (async-shell-command "npx jest")
  (message "Running tests..."))

(defun jest-run-tests-for-file ()
  "Run tests for a file"
  (interactive)
  (let* ((file (buffer-file-name))
         (command (concat "npx jest " file)))
    (async-shell-command command)
    (message "Running tests...")))

(defun jest-run-test-at-point ()
  "Run test at point"
  (interactive)
  (let* ((file (buffer-file-name))
         (test-name (thing-at-point 'string))
         (command (concat "npx jest " file " -t " test-name)))
    (async-shell-command command)
    (message "Running test...")))
