;;; module-org.el --- module -*- lexical-binding: t -*-

(use-package evil-org
  :defer t
  :hook (org-mode . evil-org-mode)
  :init (setq evil-org-use-additional-insert t))

(use-package gnuplot
  :general
  (:prefix "SPC mt"
	   :keymaps 'org-mode
	   "p" 'org-plot/gnuplot))

(use-package htmlize
  :defer t)

(use-package org
  :general
  (:states 'normal
	   "SPC Cc" '(org-capture :wk "Org Capture"))
  (:keymaps 'org-mode
	    "RET" 'org-open-at-point)
  (:states 'normal
	   :prefix "SPC ao"
	   "" '(:ignore t :wk "Org Mode")
	   "#" '(org-agenda-list-stuck-projects :wk "Stuck Projects")
           "a" 'org-agenda-list
           "o" '(org-agenda :wk "Agenda")
           "c" '(org-capture :wk "Capture")
           "e" 'org-store-agenda-views
	   "l" 'org-store-link
	   "m" 'org-tags-view
	   "s" 'org-search-view
	   "t" 'org-todo-list

	   "C" '(:ignore t :wk "Clocks")
	   "Cc" '(org-clock-cancel :wk "Cancel")
           "Cg" '(org-clock-goto :wk "Goto")
           "Ci" '(org-clock-in :wk "Clock In")
	   "CI" '(org-clock-in-last :wk "Clock In Last")
	   "Cj" '(org-clock-jump-to-current-clock :wk "Jump to Clock")
	   "Co" '(org-clock-out :wk "Clock Out")
	   "Cr" '(org-resolve-clocks :wk "Resolve Clocks"))

  (:keymaps 'org-mode
	    :prefix "SPC m"
	    "'" 'org-edit-special
	    "c" '(org-capture :wk "Capture")

	    "a" 'org-agenda
	    "[" 'org-agenda-file-to-front
	    "]" 'org-remove-file

	    "p" 'org-priority

	    ;; More cycling options (timestamps, headlines, items, properties)
	    "L" 'org-shiftright
	    "H" 'org-shiftleft
	    "J" 'org-shiftdown
	    "K" 'org-shiftup

	    ;; Change between TODO sets
	    "C-S-l" 'org-shiftcontrolright
	    "C-S-h" 'org-shiftcontrolleft
	    "C-S-j" 'org-shiftcontroldown
	    "C-S-k" 'org-shiftcontrolup

	    ;; Multi-purpose keys
	    "m" 'org-ctrl-c-ctrl-c
	    "," 'org-ctrl-c-ctrl-c
	    "*" 'org-ctrl-c-star
	    "-" 'org-ctrl-c-minus
	    "#" 'org-update-statistics-cookies
	    "RET"   'org-ctrl-c-ret
	    "M-RET" 'org-meta-return

	    ;; attachments
	    "A" '(org-attach :wk "Attach..."))

  (:keymaps 'org-mode
	    :prefix "SPC mC"
	    "" '(:ignore t :wk "Clocks")
	    "c" '(org-clock-cancel :wk "Cancel")
	    "d" 'org-clock-display
	    "e" 'org-evaluate-time-range
	    "g" '(org-clock-goto :wk "Goto")
	    "i" 'org-clock-in
	    "I" 'org-clock-in-last
	    "j" 'org-clock-jump-to-current-clock
	    "o" 'org-clock-out
	    "R" 'org-clock-report
	    "r" 'org-resolve-clocks)

  (:keymaps 'org-map
	    :prefix "SPC md"
	    "" '(:ignore t :wk "Time")
	    "d" 'org-deadline
	    "s" 'org-schedule
	    "t" 'org-time-stamp
	    "T" 'org-time-stamp-inactive)

  (:keymaps 'org-map
	    :prefix "SPC me"
	    "" '(:ignore t :wk "Export")
	    "e" 'org-export-dispatch)

  (:keymaps 'org-mode
	    :prefix "SPC mf"
	    "" '(:ignore t :wk "Feed") 
	    "i" 'org-feed-goto-inbox
	    "u" 'org-feed-update-all)

  (:keymaps 'org-mode
	    :prefix "SPC mT"
	    "" '(:ignore t :wk "Toggle")
	    "c" 'org-toggle-checkbox
	    "e" 'org-toggle-pretty-entities
	    "i" 'org-toggle-inline-images
	    "n" 'org-num-mode
	    "l" 'org-toggle-link-display
	    "t" 'org-show-todo-tree
	    "T" 'org-todo)

  (:keymaps 'org-mode
	    :prefix "SPC ms"
	    "" '(:ignore t :wk "Subtrees")
	    "a" 'org-toggle-archive-tag
	    "A" 'org-archive-subtree-default
	    "b" 'org-tree-to-indirect-buffer
	    "d" 'org-cut-subtree
	    "y" 'org-copy-subtree
	    "p" 'org-paste-subtree
	    "h" 'org-promote-subtree
	    "j" 'org-move-subtree-down
	    "k" 'org-move-subtree-up
	    "l" 'org-demote-subtree
	    "n" 'org-narrow-to-subtree
	    "w" 'widen
	    "r" 'org-refile
	    "s" 'org-sparse-tree
	    "S" 'org-sort)

  (:keymaps 'org-mode
	    :prefix "SPC mt"
	    "" '(:ignore t :wk "Tables")
	    "a" 'org-table-align
	    "b" 'org-table-blank-field
	    "c" 'org-table-convert
	    "I" '(org-table-import :wk "Import...")
	    "j" 'org-table-next-row
	    "J" 'org-table-move-row-down
	    "K" 'org-table-move-row-up
	    "l" 'org-table-next-field
	    "L" 'org-table-move-column-right
	    "n" 'org-table-create
	    "N" 'org-table-create-with-table.el
	    "r" 'org-table-recalculate
	    "R" 'org-table-recalculate-buffer-tables
	    "s" 'org-table-sort-lines
	    "w" 'org-table-wrap-region
	    "e" 'org-table-eval-formula
	    "E" 'org-table-export
	    "f" 'org-table-field-info
	    "h" 'org-table-previous-field
	    "H" 'org-table-move-column-left

	    "d" '(:ignore t :wk "Delete")
	    "dc" 'org-table-delete-column
	    "dr" 'org-table-kill-row

	    "i" '(:ignore t :wk "Insert")
	    "ic" 'org-table-insert-column
	    "ih" 'org-table-insert-hline
	    "iH" 'org-table-hline-and-move
	    "ir" 'org-table-insert-row

	    "t" '(:ignore t :wk "Toggle")
	    "tf" 'org-table-toggle-formula-debugger
	    "to" 'org-table-toggle-coordinate-overlays)

  (:keymaps 'org-mode
	    :prefix "SPC mb"
	    "" '(:ignore t :wk "Babel")
	    "p"     'org-babel-previous-src-block
	    "n"     'org-babel-next-src-block
	    "e"     'org-babel-execute-maybe
	    "o"     'org-babel-open-src-block-result
	    "v"     'org-babel-expand-src-block
	    "u"     'org-babel-goto-src-block-head
	    "g"     'org-babel-goto-named-src-block
	    "r"     'org-babel-goto-named-result
	    "b"     'org-babel-execute-buffer
	    "s"     'org-babel-execute-subtree
	    "d"     'org-babel-demarcate-block
	    "t"     'org-babel-tangle
	    "f"     'org-babel-tangle-file
	    "c"     'org-babel-check-src-block
	    "j"     'org-babel-insert-header-arg
	    "l"     'org-babel-load-in-session
	    "i"     'org-babel-lob-ingest
	    "I"     'org-babel-view-src-block-info
	    "z"     'org-babel-switch-to-session
	    "Z"     'org-babel-switch-to-session-with-code
	    "a"     'org-babel-sha1-hash
	    "x"     'org-babel-do-key-sequence-in-edit-buffer)

  (:keymaps 'org-mode
	    :prefix "SPC mi"
	    "" '(:ignore t :wk "Insert")
	    "b" 'org-insert-structure-template
	    "d" 'org-insert-drawer
	    "e" 'org-set-effort
	    "f" 'org-footnote-new
	    "h" 'org-insert-heading
	    "H" 'org-insert-heading-after-current
	    "i" 'org-insert-item
	    "l" 'org-insert-link
	    "n" 'org-add-note
	    "p" 'org-set-property
	    "s" 'org-insert-subheading
	    "t" 'org-set-tags-command)
  :init
  (progn
    (setq org-clock-persist-file (concat tf-emacs-cache-directory
					 "org-clock-save.el")
	  ord-id-locations-file (concat tf-emacs-cache-directory
					".org-id-locations")
	  org-publish-timestamp-directory (concat tf-emacs-cache-directory
						  ".org-timestamps/")
	  org-directory "~/org"
	  org-default-notes-file (expand-file-name "notes.org" org-directory)
	  org-log-done 'time
	  org-startup-with-inline-images t
	  org-image-actual-width nil
	  org-src-fontify-natively t
	  org-src-tab-acts-natively t
	  org-imenu-depth 8
	  org-enforce-todo-dependencies t))
  :config
  (progn
    (font-lock-add-keywords
     'org-mode '(("\\(@@html:<kbd>@@\\) \\(.*\\) \\(@@html:</kbd>@@\\)"
                  (1 font-lock-comment-face prepend)
                  (2 font-lock-function-name-face)
                  (3 font-lock-comment-face prepend))))))

(use-package org-agenda
:straight nil
:general
(:keymaps 'org-agenda-mode
	  :prefix "SPC m"
	  "m" 'org-agenda-ctrl-c-ctrl-c
          "a" 'org-agenda
          "c" 'org-agenda-capture

	  "C" '(:ignore t :wk "Clocks")
          "Cc" 'org-agenda-clock-cancel
          "Ci" 'org-agenda-clock-in
          "Co" 'org-agenda-clock-out
          "Cj" 'org-agenda-clock-goto

	  "d" '(:ignore t :wk "Time")
          "dd" 'org-agenda-deadline
          "ds" 'org-agenda-schedule

	  "i" '(:ignore t :wk "Insert")
          "ie" 'org-agenda-set-effort
          "ip" 'org-agenda-set-property
          "iP" 'org-agenda-priority
          "it" 'org-agenda-set-tags

          "sr" 'org-agenda-refile)
:init
(progn
  (setq org-agenda-restore-windows-after-quit t)
  (with-eval-after-load 'org
    (add-to-list 'org-modules 'org-habit))))

(use-package org-wild-notifier
  :defer t
  :init
  (org-wild-notifier-mode))

(use-package org-brain
  :defer t
  :general
  (:states 'normal
	   :prefix "SPC aoB"
	   "" '(:ignore t :wk "Org Brain")
	   "v" 'org-brain-visualize
	   "a" 'org-brain-agenda)

  (:keymaps 'org-mode
	    :prefix "SPC mB"
	    "" '(:ignore t :wk "Org Brain")
	    "v" 'org-brain-visualize
            "R"  'org-brain-refile
            "x"  'org-brain-delete-entry

	    "a" '(:ignore t :wk "Add")
            "ac" 'org-brain-add-child
            "ah" 'org-brain-add-child-headline
            "ap" 'org-brain-add-parent
            "ar" 'org-brain-add-resource
            "af" 'org-brain-add-friendship

	    "g" '(:ignore t :wk "Goto")
            "gg" 'org-brain-goto
            "gc" 'org-brain-goto-child
            "gp" 'org-brain-goto-parent
            "gf" 'org-brain-goto-friend))

(use-package org-download
  :commands (org-download-enable
	     org-download-yank
	     org-download-screenshot)
  :hook (org-mode . org-download-enable)
  :general
  (:keymaps 'org-mode
	    :prefix "SPC miD"
	    "" '(:ignore t :wk "Download")
	    "y" 'org-download-yank
	    "s" 'org-download-screenshot))

(use-package org-mime
  :general
  (:prefix "SPC me"
	   :keymaps 'message-mode
	   "m" 'org-mime-htmlize
	   :keymaps 'org-mode
	   "m" 'org-mime-org-buffer-htmlize
	   "s" 'org-mime-org-subtree-htmlize))

(use-package org-pomodoro
  :general
  (:prefix "SPC mC"
	   :keymaps '(org-mode org-journal-mode org-agenda-mode)
	   "p" 'org-pomodoro)
  :init (setq org-pomodoro-audio-player "/usr/bin/afplay"))

(use-package org-cliplink
  :general
  (:keymaps 'org-mode
	    "SPC miL" 'org-cliplink))

(use-package org-rich-yank
  :demand t
  :general
  (:prefix "SPC mi"
	   :keymaps 'org-mode
	   "r" 'org-rich-yank))

(use-package org-projectile
  :general
  (:states 'normal
	   "SPC aop" 'org-projectile/capture
	   "SPC po" 'org-projectile/goto-todos)
  :init
  (defun org-projectile/capture (&optional arg)
    (interactive "P")
    (if arg
	(org-projectile-project-todo-completing-read :empty-lines 1)
      (org-projectile-capture-for-current-project :empty-lines 1)))

  (defun org-projectile/goto-todos ()
    (interactive)
    (org-projectile-goto-location-for-project (projectile-project-name)))

  (progn
    (with-eval-after-load 'org-capture
      (require 'org-projectile)))
  :config
  (if (file-name-absolute-p org-projectile-file)
      (progn
	(setq org-projectile-projects-file org-projectile-file)
	(push (org-projectile-project-todo-entry :empty-lines 1)
	      org-capture-templates))
    (org-projectile-per-project)
    (setq org-projectile-per-project-filepath org-projecile-file)))

(use-package org-contrib
  :defer t)

(use-package org-journal
  :general
  (:prefix "SPC aoj"
	   :states 'normal
	   "" '(:ignore t :wk "Org Journal")
	   "f" 'org-journal-open-current-journal-file
	   "j" 'org-journal-new-entry
	   "s" 'org-journal-search-forever
	   "t" 'org-journal-new-scheduled-entry
	   "v" 'org-journal-schedule-view)

  (:prefix "SPC m"
	   :keymaps 'calendar-mode
	   "r" 'org-journal-read-entry
	   "i" 'org-journal-new-date-entry
	   "n" 'org-journal-next-entry
	   "p" 'org-journal-previous-entry
	   "s" 'org-journal-search-forever
	   "w" 'org-journal-search-calendar-week
	   "m" 'org-journal-search-calendar-month
	   "y" 'org-journal-search-calendar-year

	   :keymaps 'org-journal-mode
	   "j" 'org-journal-new-entry
	   "n" 'org-journal-next-entry
	   "p" 'org-journal-previous-entry))

(use-package org-roam
  :general
  (:prefix "SPC aor"
	   :states 'normal
	   "" '(:ignore t :wk "Org Roam")
	   "c" 'org-roam-capture
	   "f" 'org-roam-node-find
	   "g" 'org-roam-graph
	   "i" 'org-roam-node-insert
	   "l" 'org-roam-buffer-toggle
	   "a" 'org-roam-alias-add

	   "d" '(:ignore t :wk "Dailies")
	   "dy" 'org-roam-dailies-goto-yesterday
	   "dt" 'org-roam-dailies-goto-today
	   "dT" 'org-roam-dailies-goto-tomorrow
	   "dd" 'org-roam-dailies-goto-date

	   "t" '(:ignore t :wk "Tags")
	   "ta" 'org-roam-tag-add
	   "tr" 'org-roam-tag-remove)

  (:keymaps 'org-mode
	    :prefix "SPC mr"
	    "" '(:ignore t :wk "Org Roam")
            "c" 'org-roam-capture
            "f" 'org-roam-node-find
            "g" 'org-roam-graph
            "i" 'org-roam-node-insert
            "l" 'org-roam-buffer-toggle
            "a" 'org-roam-alias-add

	    "d" '(:ignore t :wk "Dailies")
	    "dy" 'org-roam-dailies-goto-yesterday
            "dt" 'org-roam-dailies-goto-today
            "dT" 'org-roam-dailies-goto-tomorrow
            "dd" 'org-roam-dailies-goto-date

	    "t" '(:ignore t :wk "Tags")
            "ta" 'org-roam-tag-add
            "tr" 'org-roam-tag-remove)

  (:keymaps 'org-roam-mode
	    "o" 'link-hint-open-link
	    "r" 'org-roam-buffer-refresh)

  :config
  (use-package org-roam-protocol
    :after org-protocol))

(use-package org-sticky-header
  :defer t
  :hook (org-mode . org-sticky-header-mode))

(use-package verb
  :general
  (:prefix "SPC mr"
	   :keymaps 'org-mode
	   "" '(:ignore t :wk "Verb")
	   "f" 'verb-send-request-on-point
	   "s" 'verb-send-request-on-point-other-window
	   "r" 'verb-send-request-on-point-other-window-stay
	   "m" 'verb-send-request-on-point-no-window
	   "k" 'verb-kill-all-response-buffers
	   "e" 'verb-export-request-on-point
	   "u" 'verb-export-request-on-point-curl
	   "b" 'verb-export-request-on-point-verb
	   "v" 'verb-set-var

	   :keymaps 'verb-response-body-mode
	   "r" 'verb-toggle-show-headers
	   "k" 'verb-kill-response-buffer-and-window
	   "f" 'verb-re-send-request

	   :keymaps 'verb-response-headers-mode
	   "q" 'verb-kill-buffer-and-window))

(use-package valign
  :after org
  :hook (org-mode . valign-mode)
  :hook (valign-mode . (lambda () (unless valign-mode
				    (valign-remove-advice)))))

(use-package org-appear
  :defer t
  :hook (org-mode . org-appear-mode)
  :init
  (progn
    (setq org-appear-autolinks t
	  org-appear-autoemphasis t
	  org-appear-autosubmarkers t)))

(use-package org-transclusion
  :general
  (:prefix "SPC mu"
	   :keymaps 'org-mode
	   "" '(:ignore t :wk "Org Transclusion")
	   "u" 'org-transclusion-add
	   "U" 'org-transclusion-add-all
	   "d" 'org-transclusion-remove
	   "D" 'org-transclusion-remove-all
	   "l" 'org-transclusion-demote-subtree
	   "h" 'org-transclusion-promote-subtree
	   "r" 'org-transclusion-refresh
	   "g" 'org-transclusion-move-to-source))

(use-package ox-asciidoc
  :after ox)

(use-package org-superstar
  :defer t
  :init (add-hook 'org-mode-hook 'org-superstar-mode))

(use-package toc-org
  :defer t
  :hook (org-mode . toc-org-enable)
  :init (setq toc-org-max-depth 10))

(provide 'module-org)
