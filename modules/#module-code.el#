;;; module-code.el --- module -*- lexical-binding: t -*-

(use-package web-mode
    :custom-face
  (css-selector ((t (:inherit default :foreground "#66CCFF"))))
  (font-lock-comment-face ((t (:foreground "#828282")))))

;; javascript
(use-package js2-mode
    :mode "\\.js\\'"
    :interpreter "node")

;; typescript
(add-hook 'typescript-ts-mode-hook 'eglot-ensure)

(use-package emmet-mode
    :hook ((web-mode . emmet-mode)
           (css-mode . emmet-mode)))

(use-package json-mode
    :mode "\\.json\\'")

(provide 'module-code)
