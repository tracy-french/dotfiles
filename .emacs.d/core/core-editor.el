;;; core-editor.el --- core -*- lexical-binding: t -*-

(setq create-lockfiles nil
      make-backup-files nil
      version-control t
      backup-by-copying t
      delete-old-versions t
      kept-old-versions 5
      kept-new-versions 5
      backup-directory-alist (list (cons "." (concat tf-emacs-cache-directory
                                                     "backup/")))
      tramp-backup-directory-alist backup-directory-alist)

;; -----------------------------------------------------------------------------
;; formatting
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

;; -----------------------------------------------------------------------------
;; kill ring
;; -----------------------------------------------------------------------------

(setq kill-do-not-save-duplicates t)

;; -----------------------------------------------------------------------------
;; extra file extensions
;; -----------------------------------------------------------------------------

(nconc
 auto-mode-alist
 '(("/LICENSE\\'" . text-mode)
   ("\\.log\\'" . text-mode)
   ("rc\\'" . conf-mode)
   ("\\.\\(?:hex\\|nes\\)\\'" . hexl-mode)))

(defvar tf//switch-buffer-hook nil
  "a list of hooks ran after changing the current buffer")

(defvar tf//switch-window-hook nil
  "a list of hooks ran after changing the current window")

(defvar tf//switch-frame-hook nil
  "a list of hooks ran after changing the current frame")

(defun tf//run-switch-buffer-hooks-h (&optional _)
  (let ((gc-cons-threshold most-positive-fixnum)
        (inhibit-redisplay t))
    (run-hooks 'tf//switch-buffer-hook)))

(defun tf//run-switch-window-or-frame-hooks-h (&optional _)
  (let ((gc-cons-threshold most-positive-fixnum)
        (inhibit-redisplay t))
    (unless (equal (old-selected-frame) (selected-frame))
      (run-hooks 'tf//switch-frame-hook))
    (unless (or (minibufferp)
                (equal (old-selected-window) (minibuffer-window)))
      (runhooks 'tf//switch-window-hook))))

(defun tf/visible-buffers (&optional buffer-list)
  "return list of visible buffers"
  (let ((buffers (delete-dups (mapcar #'window-buffer (window-list)))))
    (if buffer-list
        (cl-delete-if (lambda (b) (memq b buffer-list))
                      buffers)
      (delete-dups buffers))))

(use-package autorevert
  :hook ((focus-in . tf//auto-revert-buffers-h)
         (after-save . tf//auto-revert-buffers-h))
  :init
  (progn
    (defun tf//auto-revert-buffer-h ()
      (unless (or auto-revert-mode (active-minibuffer-window))
        (let ((auto-revert-mode t))
          (auto-revert-handler))))

    (defun tf//auto-revert-buffers-h ()
      (dolist (buf (tf/visible-buffers))
        (with-current-buffer buf
          (tf//auto-revert-buffer-h))))

    (setq auto-revert-verbose t
          auto-revert-use-notify nil
          auto-revert-stop-on-user-input nil
          revert-without-query (list "."))))

(use-package recentf
  :hook ((kill-emacs . recentf-cleanup)
         (dired-mode . tf//recentf-add-dired-directory-h)
         (tf//switch-window . tf//recentf-touch-buffer-h)
         (write-file-functions . tf//recentf-touch-buffer-h))
  :commands recentf-open-files
  :custom (recentf-save-file (concat tf-emacs-cache-directory "recentf"))
  :init
  (progn
    (add-hook 'find-file-hook (lambda () (unless recentf-mode
                                           (recentf-mode)
                                           (recentf-track-opened-file))))

    (defun tf//recentf-touch-buffer-h ()
      (when buffer-file-name
        (recentf-add-file buffer-file-name))
      ;; return nil for `write-file-functions'
      nil)

    (defun tf//recentf-add-dired-directory-h ()
      (recentf-add-file default-directory)))
  :config
  (progn
    (setq recentf-auto-cleanup nil
          recentf-max-saved-items 200
          recentf-show-abbreviated t)
    (add-to-list 'recentf-filename-handlers #'substring-no-properties)))

(use-package savehist
  :hook ((savehist-save . tf//savehist-unpropertize-variables-h)
         (savehist-save . tf//savehist-remove-unprintable-registers-h))
  :custom (savehist-file (concat tf-emacs-cache-directory "savehist"))
  :init
  (progn
    (defun tf//savehist-unpropertize-variables-h ()
      (setq kill-ring
            (mapcar #'substring-no-properties
                    (cl-remove-if-not #'stringp kill-ring))
            register-alist
            (cl-loop for (reg . item) in register-alist
                     if (stringp item)
                     collect (cons reg (substring-no-properties item))
                     else collect (cons reg item))))

    (defun tf//savehist-remove-unprintable-registers-h ()
      (setq-local register-alist
                  (cl-remove-if-not #'savehist-printable register-alist)))

    (savehist-mode t))
  :config
  (progn
    (setq savehist-save-minibuffer-history t
          savehist-autosave-interval nil
          savehist-additional-variables '(kill-ring
                                          register-alist
                                          mark-ring global-mark-ring
                                          search-ring regexp-search-ring))))

(use-package saveplace
  :custom (save-place-file (concat tf-emacs-cache-directory "places"))
  :init (save-place-mode))

(use-package server
  :defer t
  :config
  (when-let (name (getenv "EMACS_SERVER_NAME"))
    (setq server-name name))
  (unless (server-running-p)
    (server-start)))

(use-package dtrt-indent
  :config
  (defun tf//detect-indention-h ()
    (unless (or (not after-init-time)
                (member (substring (buffer-name) 0 1) '(" " "*")))
      (let ((inhibit-message (not init-file-debug)))
        (dtrt-indent-mode +1)))))

(use-package helpful
  :commands helpful--read-symbol
  :hook (helpful-mode . visual-line-mode)
  :init
  (setq apropos-do-all t)

  (global-set-key [remap describe-function] #'helpful-callable)
  (global-set-key [remap describe-command]  #'helpful-command)
  (global-set-key [remap describe-variable] #'helpful-variable)
  (global-set-key [remap describe-key]      #'helpful-key)
  (global-set-key [remap describe-symbol]   #'helpful-symbol))

(use-package so-long
  :config
  (progn
    (setq so-long-treshold 400)
    (global-so-long-mode)))

(use-package ws-butler
  :config
  (progn
    (setq ws-butler-keep-whitespace-before-point nil)
    (ws-butler-global-mode)))

(provide 'core-editor)
