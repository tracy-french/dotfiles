;;; module-ibuffer.el --- module -*- lexical-binding: t -*-

(defvar ibuffer-group-buffers-by 'modes
  "If non nil ibuffer will group the buffers according to the passed symbol.
The supported values are `modes' to group by major-modes and `projects' to
group by projectile projects.")

(defun tf//ibuffer-get-major-modes-ibuff-rules-list (mm-list result-list)
  (if mm-list
      (let* ((cur-mm (car mm-list))
	     (next-res-list-el `(,(symbol-name cur-mm) (mode . ,cur-mm))))
	(tf//ibuffer-get-major-modes-ibuff-rules-list
	 (cdr mm-list) (cons next-res-list-el result-list)))
    result-list))

(defun tf//ibuffer-get-major-modes-list ()
  (mapcar
   (function (lambda (buffer)
	       (buffer-local-value 'major-mode (get-buffer buffer))))
   (buffer-list (selected-frame))))

(defun tf//ibuffer-create-buffs-group ()
  (interactive)
  (let* ((ignore-modes '(Buffer-menu-mode
                         compilation-mode
                         minibuffer-inactive-mode
                         ibuffer-mode
                         magit-process-mode
                         messages-buffer-mode
                         fundamental-mode
                         completion-list-mode
                         help-mode
                         Info-mode))
         (cur-bufs
          (list (cons "Home"
                      (tf//ibuffer-get-major-modes-ibuff-rules-list
                       (cl-set-difference
                        (remove-duplicates
                         (tf//ibuffer-get-major-modes-list))
                        ignore-modes) '())))))
    (setq ibuffer-saved-filter-groups cur-bufs)
    (ibuffer-switch-to-saved-filter-groups "Home")))

(use-package ibuffer
  :defer t
  :general
  (:states 'normal
	   "SPC bI" 'ibuffer
	   "C-x C-b" 'ibuffer)
  (:keymaps 'ibuffer-mode-map
	    :states 'normal
	    "gr" 'ibuffer-update
	    "gj" 'ibuffer-forward-filter-group
	    "]" 'ibuffer-forward-filter-group
	    "gk" 'ibuffer-backward-filter-group
	    "[" 'ibuffer-backward-filter-group)
  :init
  (progn
    (defun tf//ibuffer-group-by-modes ()
      (when (eq 'modes ibuffer-group-buffers-by)
	(tf//ibuffer-create-buffs-group)))

    (add-hook 'ibuffer-hook 'tf//ibuffer-group-by-modes)

    (evil-ex-define-cmd "buffers" 'ibuffer)))

(use-package ibuffer-projectile
  :defer t
  :init
  (progn
    (defun tf//ibuffer-group-by-projects ()
      (when (eq 'projects ibuffer-group-buffers-by)
	(ibuffer-projectile-set-filter-groups)
	(unless (eq ibuffer-sorting-mode 'alphabetic)
	  (ibuffer-do-sort-by-alphabetic))))

    (add-hook 'ibuffer-hook 'tf//ibuffer-group-by-projects)))

(provide 'module-ibuffer)
