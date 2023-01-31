;;; core-hooks.el --- core -*- lexical-binding: t -*-

(defun tf/run-prog-mode-hooks ()
  (run-hooks 'prog-mode-hook))

(defun tf/run-text-mode-hooks ()
  (run-hooks 'text-mode-hook))

(defun tf/add-to-hooks (fun hooks &optional append local)
  "add function to hooks"
  (dolist (hook hooks)
    (add-hook hook fun append local)))

(defun tf/add-all-to-hook (hook &rest funs)
  "add functions to took"
  (taf-emacs/add-to-hook hook funs))

(defun tf/add-to-hook (hook funs)
  "add list of functions to hook"
  (dolist (fun funs)
    (add-hook hook fun)))

(provide 'core-hooks)
