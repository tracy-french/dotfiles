;;; module-keys.el --- module -*- lexical-binding: t -*-

;; instantly display current keystrokes in mini buffer
(setq echo-keystrokes 0.02)
;; auto-indent on RET
(define-key global-map (kbd "RET") 'newline-and-indent)

;; improve delete-other-windows
(define-key global-map (kbd "C-x 1") 'tf/toggle-maximize-buffer)
;; adds two spacing modes while preserving just-one-space behaviour
(define-key global-map (kbd "M-SPC") 'cycle-spacing)

;; alternate binding to search next occurrence with isearch without
;; exiting isearch
(define-key isearch-mode-map (kbd "S-<return>") 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "M-S-<return>") 'isearch-repeat-backward)
;; Escape from isearch-mode("/" and "?" in evil-mode) like vim
(define-key isearch-mode-map (kbd "<escape>") 'isearch-cancel)

;; Make <escape> quit as much as possible
(define-key minibuffer-local-map (kbd "<escape>") 'keyboard-escape-quit)
(define-key minibuffer-local-ns-map (kbd "<escape>") 'keyboard-escape-quit)
(define-key minibuffer-local-completion-map (kbd "<escape>") 'keyboard-escape-quit)
(define-key minibuffer-local-must-match-map (kbd "<escape>") 'keyboard-escape-quit)
(define-key minibuffer-local-isearch-map (kbd "<escape>") 'keyboard-escape-quit)

;; Also bind C-n C-p in minibuffer
(define-key minibuffer-local-map (kbd "C-n") 'next-line-or-history-element)
(define-key minibuffer-local-map (kbd "C-p") 'previous-line-or-history-element)

(use-package which-key
  :config
  (progn
    (setq which-key-add-column-padding 1
	  which-key-allow-multiple-replacements t
	  which-key-echo-keystrokes 0.02
	  which-key-idle-delay 0.5
	  which-key-idle-secondary-delay 0.01
	  which-key-max-description-length 32
	  which-key-max-display-columns nil
	  which-key-min-display-lines 6
	  which-key-prevent-C-h-from-cycling t
	  which-key-sort-order 'which-key-prefix-then-key-order
	  which-key-sort-uppercase-first nil
	  which-key-special-keys nil
	  which-key-use-C-h-for-paging t
	  which-key-allow-evil-operators t))
  (which-key-mode))

(use-package general
  :config
  (general-define-key
   "RET" 'newline-and-indent
   "M-SPC" 'cycle-spacing)

  (general-define-key
   :keymaps 'isearch-mode-map
   ;; alternate binding to search next occurrence with isearch without
   ;; exiting isearch
   "S-<return>" 'isearch-repeat-forward
   "M-S-<return>" 'isearch-repeat-backward
   ;; Escape from isearch-mode("/" and "?" in evil-mode) like vim
   "<escape>" 'isearch-cancel)

  (general-define-key
   :states 'normal
   :prefix "SPC"
   :non-normal-prefix "M-SPC"
   "u" 'universal-argument
   "!" 'shell-command

   ;; --------------------------------------------------------------------------
   ;; Keyboard Macros
   ;; --------------------------------------------------------------------------

   "K" '(:ignore t :wk "Keyboard Macros")
   "Kk" '(kmacro-start-macro-or-insert-counter :wk "Start macro/Insert counter")
   "KK" '(kmacro-end-or-call-macro :wk "Stop or Run")
   "Kv" '(kmacro-view-macro-repeat :wk "View last macro")

   "Kc" '(:ignore t :wk "Counter")
   "Kca" '(kmacro-add-counter :wk "Increment counter")
   "Kcc" '(kmacro-insert-counter :wk "Insert counter")
   "KcC" '(kmacro-set-counter :wk "Set counter...")
   "Kcf" '(kmacro-set-format :wk "Set display format...")

   "Ke" '(:ignore t :wk "Edit")
   "Keb" '(kmacro-bind-to-key :wk "Assign key binding...")
   "Kee" '(kmacro-edit-macro-repeat :wk "Edit last macro")
   "Kel" '(kmacro-edit-lossage :wk "Create macro from lossage...")
   "Ken" '(kmacro-name-last-macro :wk "Name last macro...")
   "Ker" '(kmacro-to-register :wk "Write macro to register...")
   "Kes" '(kmacro-step-edit-macro :wk "Step by step edit...")

   "Kr" '(:ignore t :wk "Ring")
   "KrL" '(kmacro-view-ring-2nd :wk "Display ring head")
   "Krd" '(kmacro-delete-ring-head :wk "Delete ring head")
   "Krl" '(kmacro-call-ring-2nd-repeat :wk "Run 2nd macro in ring")
   "Krn" '(kmacro-cycle-ring-next :wk "Next in ring")
   "Krp" '(kmacro-cycle-ring-previous :wk "Previous in ring")
   "Krs" '(kmacro-swap-ring :wk "Swap first two")

   ;; --------------------------------------------------------------------------
   ;; Rectangles
   ;; --------------------------------------------------------------------------

   "C-v" '(:ignore t :wk "Rectangles")
   "C-v c" '(close-rectangle :wk "Delete whitespace after")
   "C-v d" '(delete-rectangle :wk "Delete text")
   "C-v e" '(rectangle-exchange-point-and-mark :wk "Go to corner")
   "C-v i" '(copy-rectangle-to-register :wk "Copy into register...")
   "C-v k" '(kill-rectangle :wk "Delete and save")
   "C-v l" '(rectangle-left-char :wk "Move left past EOL")
   "C-v m" '(rectangle-mark-mode :wk "Toggle region as rectangular")
   "C-v n" '(rectangle-next-line :wk "Go to next line past EOL")
   "C-v N" '(rectangle-number-lines :wk "Insert line number")
   "C-v o" '(open-rectangle :wk "Shift text right")
   "C-v p" '(rectangle-previous-line :wk "Go to prev. line past EOL")
   "C-v r" '(rectangle-right-char :wk "Move right past EOL")
   "C-v s" '(string-rectangle :wk "Replace lines with string...")
   "C-v x" '(clear-rectangle :wk "Blank out rectangle")
   "C-v y" '(yank-rectangle :wk "Paste last rectangle")

   ;; --------------------------------------------------------------------------
   ;; Applications
   ;; --------------------------------------------------------------------------

   "a" '(:ignore t :wk "Applications")
   "a*"  'calc-dispatch
   "ap"  'list-processes
   "aP"  'proced
   "au"  'undo-tree-visualize

   ;; --------------------------------------------------------------------------
   ;; Buffers
   ;; --------------------------------------------------------------------------

   "TAB" '(tf/alternate-buffer :wk "Last buffer")

   "b" '(:ignore t :wk "Buffers")
   "b1" '(buffer-to-window-1 :wk "Move buffer to window 1")
   "b2" '(buffer-to-window-2 :wk "Move buffer to window 2")
   "b3" '(buffer-to-window-3 :wk "Move buffer to window 3")
   "b4" '(buffer-to-window-4 :wk "Move buffer to window 4")
   "b5" '(buffer-to-window-5 :wk "Move buffer to window 5")
   "b6" '(buffer-to-window-6 :wk "Move buffer to window 6")
   "b7" '(buffer-to-window-7 :wk "Move buffer to window 7")
   "b8" '(buffer-to-window-8 :wk "Move buffer to window 8")
   "b9" '(buffer-to-window-9 :wk "Move buffer to window 9")
   "b C-d" '(tf/kill-other-buffers :wk "Kill other buffers...")
   "b C-S-d" '(tf/kill-matching-buffers-rudely :wk "Kill buffers...")
   "bd" '(tf/kill-this-buffer :wk "Kill buffer")
   "be" '(tf/safe-erase-buffer :wk "Erase...")
   ;; "bh" '(tf/home :wk "Tf home buffer")
   "bH" '(tf/switch-to-help-buffer :wk "Help buffer")
   "bn" '(next-buffer :wk "Next buffer")
   "bm" '(tf/switch-to-messages-buffer :wk "Messages buffer")
   "bP" '(tf/copy-clipboard-to-whole-buffer :wk "Paste and replace buffer")
   "bp" '(previous-buffer :wk "Previous buffer")
   "bR" '(tf/safe-revert-buffer :wk "Revert buffer...")
   "bs" '(tf/switch-to-scratch-buffer :wk "Scratch buffer")
   "bu" '(tf/reopen-killed-buffer :wk "Reopen last killed buffer")
   "bx" '(kill-buffer-and-window :wk "Kill buffer and close window")
   "bY" '(tf/copy-whole-buffer-to-clipboard :wk "Copy buffer")
   "bw" '(read-only-mode :wk "Toggle read-only")

   "bN" '(:ignore t :wk "New buffer")
   "bN C-i" '(make-indirect-buffer :wk "New indirect buffer...")
   "bNf" '(tf/new-empty-buffer-new-frame :wk "New buffer (new frame)")
   "bNh" '(tf/new-empty-buffer-left :wk "New buffer (left split)")
   "bNi" '(clone-indirect-buffer :wk "Clone buffer")
   "bNI" '(clone-indirect-buffer-other-window-without-purpose :wk "Clone buffer (other window)")
   "bNj" '(tf/new-empty-buffer-below :wk "New buffer (open below)")
   "bNk" '(tf/new-empty-buffer-above :wk "New buffer (open above)")
   "bNl" '(tf/new-empty-buffer-right :wk "New buffer (right split)")
   "bNn" '(tf/new-empty-buffer :wk "New buffer")

   ;; --------------------------------------------------------------------------
   ;; Files
   ;; --------------------------------------------------------------------------

   "f" '(:ignore t :wk "Files")
   "fA" '(tf/find-file-and-replace-buffer :wk "Set another file for buffer...")
   "fc" '(tf/save-as :wk "Save file or active region as a new file...")
   "fD" '(tf/delete-current-buffer-file :wk "Delete...")
   "fi" '(tf/insert-file :wk "Insert file content...")
   "fl" '(find-file-literally :wk "Open file literally...")
   "fE" '(tf/sudo-edit :wk "Open using sudo...")
   "fo" '(tf/open-file-or-directory-in-external-app :wk "Open with external app")
   "fR" '(tf/rename-current-buffer-file :wk "Rename...")
   "fS" '(evil-write-all :wk "Save all")
   "fs" '(save-buffer :wk "Save")

   "fv" '(:ignore t :wk "Variables")
   "fvd" '(add-dir-local-variable :wk "Add directory-local variable...")
   "fvf" '(add-file-local-variable :wk "Add bottom file variable...")
   "fvp" '(add-file-local-variable-prop-line :wk "Add top file property...")

   "fy" '(:ignore t :wk "Yank/Copy")
   "fyc" '(tf/copy-file-path-with-line-column :wk "File path with line and column")
   "fyd" '(tf/copy-directory-path :wk "Directory path")
   "fyl" '(tf/copy-file-path-with-line :wk "File path with line number")
   "fyn" '(tf/copy-file-name :wk "File name")
   "fyN" '(tf/copy-file-name-base :wk "File name without extension")
   "fyy" '(tf/copy-file-path :wk "File path")
   "fyb" '(tf/copy-buffer-name :wk "Buffer name")

   ;; --------------------------------------------------------------------------
   ;; Frames
   ;; --------------------------------------------------------------------------

   "F" '(:ignore t :wk "Frames")
   "Ff" '(tf/find-file-other-frame :wk "Find file other frame...")
   "Fd" '(delete-frame :wk "Delete frame")
   "FD" '(delete-other-frames :wk "Delete other frames")
   "Fb" '(tf/switch-to-buffer-other-frame :wk "Switch to buffer other frame...")
   "FB" '(tf/display-buffer-other-frame :wk "Display buffer other frame...")
   "Fo" '(other-frame :wk "Switch to other frame")
   "FO" '(tf/dired-other-frame :wk "Dired other frame...")
   "Fn" '(make-frame :wk "Make frame")

   ;; --------------------------------------------------------------------------
   ;; Git
   ;; --------------------------------------------------------------------------

   "g" '(:ignore t :wk "Git")

   ;; --------------------------------------------------------------------------
   ;; Help
   ;; --------------------------------------------------------------------------

   "h" '(:ignore t :wk "Help")
   "hI"  'tf/report-issue
   "hn"  'view-emacs-news

   "hd" '(:ignore t :wk "Documentation")
   "hdb" 'describe-bindings
   "hdc" 'describe-char
   "hdf" 'describe-function
   "hdk" 'describe-key
   "hdl" 'tf/describe-last-keys
   "hdp" 'describe-package
   "hdP" 'configuration-layer/describe-package
   "hds" 'tf/describe-system-info
   "hdt" 'describe-text-properties
   "hdT" 'describe-theme
   "hdv" 'describe-variable

   "hp" '(:ignore t :wk "Profiler")
   "hPs" 'profiler-start
   "hPk" 'profiler-stop
   "hPr" 'profiler-report
   "hPw" 'profiler-report-write-profile

   ;; --------------------------------------------------------------------------
   ;; Insert
   ;; --------------------------------------------------------------------------

   "i" '(:ignore t :wk "Insert")
   "iJ" 'tf/insert-line-below-no-indent
   "iK" 'tf/insert-line-above-no-indent
   "ik" 'tf/evil-insert-line-above
   "ij" 'tf/evil-insert-line-below
   "ib" 'insert-buffer

   ;; --------------------------------------------------------------------------
   ;; Format
   ;; --------------------------------------------------------------------------

   "j(" 'check-parens
   "j=" 'tf/indent-region-or-buffer
   "j+" 'tf/iwb-region-or-buffer
   "jo" 'open-line
   "jS" 'tf/split-and-new-line
   "jk" 'tf/evil-goto-next-line-and-indent

   ;; --------------------------------------------------------------------------
   ;; Navigate
   ;; --------------------------------------------------------------------------
   "j0" 'tf/push-mark-and-goto-beginning-of-line
   "j$" 'tf/push-mark-and-goto-end-of-line
   "jc" 'goto-last-change
   "jf" 'find-function
   "jv" 'find-variable

   ;; --------------------------------------------------------------------------
   ;; Compile
   ;; --------------------------------------------------------------------------
   "c" '(:ignore t :wk "Compile")
   "cC" 'compile
   "ck" 'kill-compilation
   "cr" 'recompile
   "cn" 'next-error
   "cN" 'previous-error
   "cd" 'tf/show-hide-compilation-window
   "cb" 'tf/switch-to-compilation-buffer

   ;; --------------------------------------------------------------------------
   ;; Toggles
   ;; --------------------------------------------------------------------------

   "t" '(:ignore t :wk "Toggles")

   ;; --------------------------------------------------------------------------
   ;; Narrow/Widen
   ;; --------------------------------------------------------------------------
   "n" '(:ignore t :wk "Narrow/Widen")
   "nr" 'narrow-to-region
   "np" 'narrow-to-page
   "nf" 'narrow-to-defun
   "nR" 'tf/narrow-to-region-indirect-buffer
   "nP" 'tf/narrow-to-page-indirect-buffer
   "nF" 'tf/narrow-to-defun-indirect-buffer
   "nw" 'widen

   ;; --------------------------------------------------------------------------
   ;; Quit
   ;; --------------------------------------------------------------------------
   "q" '(:ignore t :wk "Quit")
   "qs" 'tf/save-buffers-kill-emacs
   "qq" 'tf/prompt-kill-emacs
   "qQ" 'tf/kill-emacs
   "qf" 'tf/frame-killer

   ;; --------------------------------------------------------------------------
   ;; Windows
   ;; --------------------------------------------------------------------------
   "w" '(:ignore t :wk "Windows")
   "w TAB"  'tf/alternate-window
   "w1"  'tf/window-split-single-column
   "w2"  'tf/window-split-double-columns
   "w3"  'tf/window-split-triple-columns
   "w4"  'tf/window-split-grid
   "wb"  'tf/switch-to-minibuffer-window
   "wd"  'tf/delete-window
   "wt"  'tf/toggle-current-window-dedication
   "wf"  'follow-mode
   "wF"  'make-frame

   "wh"  'evil-window-left
   "wj"  'evil-window-down
   "wk"  'evil-window-up
   "wl"  'evil-window-right

   "wH"  'evil-window-move-far-left
   "wJ"  'evil-window-move-very-bottom
   "wK"  'evil-window-move-very-top
   "wL"  'evil-window-move-far-right

   "wm"  'tf/toggle-maximize-buffer
   "wo"  'other-frame
   "wr"  'tf/rotate-windows-forward
   "wR"  'tf/rotate-windows-backward
   "ws"  'split-window-below
   "wS"  'split-window-below-and-focus
   "w-"  'split-window-below
   "wU"  'winner-redo
   "wu"  'winner-undo
   "wv"  'split-window-right
   "wV"  'split-window-right-and-focus
   "ww"  'other-window
   "wx"  'kill-buffer-and-window
   "w/"  'split-window-right
   "w="  'balance-windows-area
   "w+"  'tf/window-layout-toggle
   "w_"  'tf/maximize-horizontally
   "w|"  'tf/maximize-vertically

   ;; --------------------------------------------------------------------------
   ;; Text
   ;; --------------------------------------------------------------------------
   "x"   '(:ignore t :wk "Text")
   "x TAB" 'indent-rigidly
   "xc"  'count-words-region
   "xU"  'upcase-region
   "xu"  'downcase-region
   
   "xa"  '(:ignore t :wk "Align")
   "xa%" 'tf/align-repeat-percent
   "xa&" 'tf/align-repeat-ampersand
   "xa(" 'tf/align-repeat-left-paren
   "xa)" 'tf/align-repeat-right-paren
   "xa{" 'tf/align-repeat-left-curly-brace
   "xa}" 'tf/align-repeat-right-curly-brace
   "xa[" 'tf/align-repeat-left-square-brace
   "xa]" 'tf/align-repeat-right-square-brace
   "xa," 'tf/align-repeat-comma
   "xa." 'tf/align-repeat-decimal
   "xa:" 'tf/align-repeat-colon
   "xa;" 'tf/align-repeat-semicolon
   "xa=" 'tf/align-repeat-equal
   "xa\\" 'tf/align-repeat-backslash
   "xaa" 'align
   "xac" 'align-current
   "xam" 'tf/align-repeat-math-oper
   "xar" 'tf/align-repeat
   "xa|" 'tf/align-repeat-bar

   "xd"  '(:ignore t :wk "Delete")
   "xd SPC" 'cycle-spacing
   "xdl" 'delete-blank-lines
   "xdw" 'delete-trailing-whitespace
   
   "xj"  '(:ignore t :wk "Justification")
   "xjc" 'set-justification-center
   "xjf" 'set-justification-full
   "xjl" 'set-justification-left
   "xjn" 'set-justification-none
   "xjr" 'set-justification-right

   "xl"  '(:ignore t :wk "Lines")
   "xlc" 'tf/sort-lines-by-column
   "xlC" 'tf/sort-lines-by-column-reverse
   "xld" 'tf/duplicate-line-or-region
   "xlk" 'tf/kill-back-to-indentation
   "xlr" 'tf/randomize-lines
   "xls" 'tf/sort-lines
   "xlS" 'tf/sort-lines-reverse
   "xlu" 'tf/uniquify-lines

   "xt"  '(:ignore t :wk "Transpose")
   "xtc" 'transpose-chars
   "xte" 'transpose-sexps
   "xtl" 'transpose-lines
   "xtp" 'transpose-paragraphs
   "xts" 'transpose-sentences
   "xtw" 'transpose-words

   "xw"  '(:ignore t :wk "Words")
   "xwc" 'tf/count-words-analysis
   "xwr" 'tf/randomize-words)

  (general-define-key
   :states 'normal
   ;; easy line movement
   "H" 'evil-first-non-blank
   "L" 'evil-end-of-line

   ;; easy paragraph movement
   "J" 'evil-forward-paragraph
   "K" 'evil-backward-paragraph)

  ;; move in insert mode
  (general-define-key
   :states 'insert
   "C-h" "<left>"
   "C-j" "<down>"
   "C-k" "<up>"
   "C-l" "<right>"))

(provide 'module-keys)
