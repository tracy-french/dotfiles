;;; module-completion.el --- module -*- lexical-binding: t -*-

;;; vertical minibuffer completion
(use-package vertico
  :hook (minibuffer-setup-hook . vertico-repeat-save)
  :config
  (setq vertico-resize nil
        vertico-count 17
        vertico-cycle t)
  :init
  (vertico-mode 1))

;; directory completion extension
(use-package vertico-directory
    :after vertico
    :ensure nil
    :general
    (:keymaps 'vertico-map
              "DEL" 'vertico-directory-delete-char)
    ;; Cleans up path when moving directories with shadowed paths syntax, e.g.
    ;; cleans ~/foo/bar/// to /, and ~/foo/bar/~/ to ~/.
    :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

;; completion annotations
(use-package marginalia
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-list nil))
  :config
  :init
  (marginalia-mode 1))

;; fuzzy completion
(use-package orderless
    :init
    (setq completion-styles '(orderless basic)
          completion-category-defaults nil
          completion-category-overrides '((file (styles orderless partial-completion)))
          orderless-component-separator "[ &]"))

(use-package emacs
  :init
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)
  ;; enable indentation+completion using the TAB key.
  (setq tab-always-indent 'complete))

;; completion ui
(use-package corfu
  :general
  ("M-d" 'corfu-popupinfo-toggle)
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0)
  (corfu-quit-no-match 'separator)
  :config
  (eldoc-add-command #'corfu-insert)
  :init
  (global-corfu-mode 1)
  (corfu-popupinfo-mode 1))

;; completion at point
(use-package cape
  :general
  ("M-+ p" 'completion-at-point)
  ("M-+ d" 'cape-dabbrev)
  ("M-+ h" 'cape-history)
  ("M-+ f" 'cape-file)
  ("M-+ k" 'cape-keyword)
  ("M-+ s" 'cape-symbol)
  ("M-+ a" 'cape-abbrev)
  ("M-+ i" 'cape-ispell)
  ("M-+ l" 'cape-line)
  ("M-+ w" 'cape-dict)
  :init
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-keyword))

;; fuzzy lists for everything
(use-package consult
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

;; act at point
(use-package embark
  :defer t
  :init
  (setq which-key-use-C-h-commands nil
        prefix-help-command #'embark-prefix-help-command)
    :general
    ("C-." 'embark-act)
    ("C-h B" 'embark-bindings)
    :init
    (setq prefix-help-command #'embark-prefix-help-command)
    :config
    ;; hide the mode line of the Embark live/completions buffers
    (add-to-list 'display-buffer-alist
                 '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                   nil
                   (window-parameters (mode-line-format . none)))))

;; integrate embark and consult
(use-package embark-consult
    :after (embark consult)
    :demand t
    :hook (embark-collect-mode . consult-preview-at-point-mode))

(provide 'module-completion)
