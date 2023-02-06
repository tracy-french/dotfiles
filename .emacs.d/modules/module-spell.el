;;; module-spell.el --- module -*- lexical-binding: t -*-

(use-package auto-dictionary
  :defer t
  :hook ((flyspell-mode . auto-dictionary-mode))
  :init
  (progn
    (defun tf//adict-set-local-dictionary ()
      (when (and (fboundp 'adict-change-dictionary)
		 ispell-local-dictionary)
	(adict-change-dictionary ispell-local-dictionary)))

    (add-hook 'auto-dictionary-mode-hook
	      'tf//adict-set-local-dictionary 'append)))

(use-package flyspell
  :hook ((text-mode . flyspell-mode)
	 (prog-mode . flyspell-prog-mode))
  :general
  (:states 'normal
	   :prefix "SPC S"
	   "" '(:ignore t :wk "Spelling")
           "b" 'flyspell-buffer
           "r" 'flyspell-region
           "d" 'tf//spell-checking-change-dictionary
           "n" 'flyspell-goto-next-error
           "s" 'flyspell-correct-at-point

	   "a" '(:ignore t :wk "Add Work to Dict")
	   "ab" 'spacemacs/add-word-to-dict-buffer
           "ag" 'spacemacs/add-word-to-dict-global
           "as" 'spacemacs/add-word-to-dict-session)
  :init
  (progn
    (defun tf/spell-checking-change-dictionary ()
      (interactive)
      (if (fboundp 'adict-change-dictionary)
	  (adict-change-dictionary)
	(call-interactively 'ispell-change-dictionary)))

    (defun tf/add-word-to-dict-buffer ()
      (interactive)
      (tf//add-word-to-dict 'buffer))

    (defun tf/add-word-to-dict-global ()
      (interactive)
      (tf//add-word-to-dict 'save))

    (defun tf/add-word-to-dict-session ()
      (interactive)
      (tf//add-word-to-dict 'session))

    (defun tf//add-word-to-dict (scope)
      (let ((current-location (point))
	    (word (flyspell-get-word)))
	(when (consp word)
	  (if (tf//word-in-dict-p (car word))
	      (error "%s is already in dictionary" (car word))
	    (progn
	      (flyspell-do-correct scope nil (car word) current-location
				   (cadr word) (caddr word) current-location)
	      (ispell-pdict-save t))))))

    (defun tf//word-in-dict-p (word)
      (flyspell-accept-buffer-local-defs)
      (let (poss ispell-filter)
	(ispell-send-string "%\n")
	(ispell-send-string (concat "^" word "\n"))

	(while (progn
		 (accept-process-output ispell-process)
		 (not (string= "" (car ispell-filter)))))

	(setq ispell-filter (cdr ispell-filter))

	(or ispell-filter
	    (setq ispell-filter '(*)))

	(if (consp ispell-filter)
	    (setq poss (ispell-parse-output (car ispell-filter))))

	(or (eq poss t) (stringp poss))))))

(use-package flyspell-correct
  :commands (flyspell-correct-at-point
	     flyspell-correct-wrapper)
  :general
  (:states 'normal
	   "SPC Sc" 'flyspell-correct-wrapper))

(use-package flyspell-correct-popup
  :commands (flyspell-correct-popup)
  :init (setq flyspell-correct-interface #'flyspell-correct-popup))

(use-package flyspell-popup
  :defer t
  :hook (flyspell-mode . flyspell-popup-auto-correct-mode)
  :init (setq flyspell-popup-correct-delay 0.8))

(provide 'module-spell)
