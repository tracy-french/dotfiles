;;; module-projects.el --- module -*- lexical-binding: t -*-

(defun tf//projectile-directory-path ()
  "retrieve the directory path relative to project root

returns:
  - a string containing the directory path
  - `nil' in case the current buffer does not have a directory"
  (when-let (directory-name (if-let (file-name (buffer-file-name))
				(file-name-directory file-name)
			      list-buffers-directory))
    (file-relative-name
     (file-truename directory-name)
     (projectile-project-root))))

(defun tf//projectile-file-path ()
  "retrieve the file path relative to project root

returns:
  - a string containing the file path
  - `nil' in case the current buffer is not visiting a file"
  (when-let (file-name (buffer-file-name))
    (file-relative-name (file-truename file-name) (projectile-project-root))))

(defun tf//projectile-file-path-with-line ()
  "retrieve the file path relative to project root, including line number

returns:
  - a string containing the file path
  - `nil' in case the current buffer is not visiting a file"
  (when-let (file-path (tf//projectile-file-path))
    (concat file-path ":" (number-to-string (line-number-at-pos)))))

(defun tf//projectile-file-path-with-line-column ()
  "retrieve the file path relative to project root, including line and column
number"
  (when-let (file-path (tf//projectile-file-path-with-line))
    (concat
     file-path
     ":"
     (number-to-string (current-column)))))

(defun tf/projectile-copy-directory-path ()
  "copy and show the directory path relative to project root"
  (interactive)
  (if-let (directory-path (tf//projectile-directory-path))
      (progn
	(kill-new directory-path)
	(message "%s" directory-path))
    (message "WARNING: Current buffer does not have a directory!")))

(defun tf/projectile-copy-file-path ()
  "copy and show the file path relative to project root"
  (interactive)
  (if-let (file-path (tf//projectile-file-path))
      (progn
	(kill-new file-path)
	(message "%s" file-path))
    (messagee "WARNING: Current buffer is not visiting a file!")))

(defun tf/projectile-copy-file-path-with-line ()
  "copy and show the file path relative to project root, including line number"
  (interactive)
  (if-let (file-path (tf//projectile-file-path-with-line))
      (progn
	(kill-new file-path)
	(message "%s" file-path))
    (message "WARNING: Current buffer is not visiting a file!")))

(defun tf/projectile-copy-file-path-with-line-column ()
  "copy and show the file path relative to project root, including line and
column number"
  (interactive)
  (if-let (file-path (tf//projectile-file-path-with-line-column))
      (progn
	(kill-new file-path)
	(message "%s" file-path))
    (message "WARNING: Current buffer is not visiting a file!")))

(use-package projectile
  :commands (projectile-ack
	     projectile-ag
	     projectile-compile-project
	     projectile-dired
	     projectile-find-dir
	     projectile-find-file
	     projectile-find-tag
	     projectile-test-project
	     projectile-grep
	     projectile-invalidate-cache
	     projectile-kill-buffers
	     projectile-multi-occur
	     projectile-project-p
	     projectile-project-root
	     projectile-recentf
	     projectile-regenerate-tags
	     projectile-replace
	     projectile-replace-regexp
	     projectile-run-async-shell-command-in-root
	     projectile-run-shell-command-in-root
	     projectile-switch-project
	     projectile-switch-to-buffer
	     projectile-vc)
  :general
  (:states 'normal
	   :prefix "SPC"
	   ;; file path
	   "fyC" 'tf/projectile-copy-file-path-with-line-column
	   "fyD" 'tf/projectile-copy-directory-path
	   "fyL" 'tf/projectile-copy-file-path-with-line
	   "fyY" 'tf/projectile-copy-file-path
	   ;; project
	   "p!" 'projectile-run-shell-command-in-root
	   "p&" 'projectile-run-async-shell-command-in-root
	   "p%" 'projectile-replace-regexp
	   "pa" 'projectile-toggle-between-implementation-and-test
	   "pb" 'projectile-switch-project
	   "pc" 'projectile-compile-project
	   "pu" 'projectile-run-project
	   "pd" 'projectile-find-dir
	   "pD" 'projectile-dired
	   "pe" 'projectile-edit-dir-locals
	   "pf" 'projectile-find-file
	   "pF" 'projectile-find-file-dwim
	   "pE" 'projectile-find-references
	   "pg" 'projectile-find-tag
	   "pG" 'projectile-regenerate-tags
	   "pi" 'projectile-install-project
	   "pI" 'projectile-invalidate-cache
	   "pk" 'projectile-kill-buffers
	   "pp" 'projectile-switch-project
	   "pr" 'projectile-recentf
	   "pR" 'projectile-replace
	   "pT" 'projectile-test-project
	   "pv" 'projectile-vc)
  :init
  (progn
    (setq projectile-sort-order 'recentf
	  projectile-cache-file (concat tf-emacs-cache-directory
					"projectile.cache")
	  projectile-known-projects-file (concat tf-emacs-cache-directory
						 "projectile-bookmarks.eld")))
  :config
  (progn
    (projectile-mode)))

  (provide 'module-projects)
