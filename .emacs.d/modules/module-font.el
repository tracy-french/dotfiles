;;; module-font.el --- module -*- lexical-binding: t -*-

(set-face-attribute
 'default nil :family "Input Mono" :height 180)

(use-package unicode-fonts
  :init
  (progn
    (defun tf//setup-unicode-fonts (frame)
      (with-selected-frame frame
	(require 'unicode-fonts)
	(unicode-fonts-setup)
	(remove-hook 'after-make-frame-functions #'tf//setup-unicode-fonts)))

      (setq unicode-fonts-skil-font-groups `(decorative low-quality-glyphs))
    (tf//setup-unicode-fonts (selected-frame))))

(defconst tf--unicode-fonts-ligature-set '("|||>" "<|||" "<==>" "<!--" "####"
                                             "~~>" "***" "||=" "||>" ":::" "::="
                                             "=:=" "===" "==>" "=!=" "=>>" "=<<"
                                             "=/=" "!==" "!!." ">=>" ">>=" ">>>"
                                             ">>-" ">->" "->>" "-->" "---" "-<<"
                                             "<~~" "<~>" "<*>" "<||" "<|>" "<$>"
                                             "<==" "<=>" "<=<" "<->" "<--" "<-<"
                                             "<<=" "<<-" "<<<" "<+>" "</>" "###"
                                             "#_(" "..<" "..." "+++" "/==" "///"
                                             "_|_" "www" "&&" "^=" "~~" "~@"
                                             "~=" "~>" "~-" "**" "*>" "*/" "||"
                                             "|}" "|]" "|=" "|>" "|-" "{|" "[|"
                                             "]#" "::" ":=" ":>" ":<" "$>" "=="
                                             "=>" "!=" "!!" ">:" ">=" ">>" ">-"
                                             "-~" "-|" "->" "-<" "<~" "<*" "<|"
                                             "<:" "<$" "<=" "<>" "<-" "<<" "<+"
                                             "</" "#{" "#[" "#:" "#=" "#!" "##"
                                             "#(" "#?" "#_" "%%" ".=" ".-" ".."
                                             ".?" "+>" "++" "?:" "?=" "?." "??"
                                             ";;" "/*" "/**" "/=" "/>" "__" "~~"
                                             "(*" "*)" "://"))

(defconst tf--unicode-fonts-ligature-modes '(prog-mode))

(use-package ligature
  :init
  (dolist (mode tf--unicode-fonts-ligature-modes)
    (ligature-set-ligatures mode tf--unicode-fonts-ligature-set))
  (global-ligature-mode t))

(provide 'module-font)
