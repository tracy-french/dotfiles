;;; core-start.el --- core -*- lexical-binding: t -*-

;; -----------------------------------------------------------------------------
;; defaults
;; -----------------------------------------------------------------------------

;;; runtime optimizations

;; second pass not needed
(setq auto-mode-case-fold nil)

;; disable bidirectional text scanning
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)

;; disable BPA to improve redisplay speed
(setq bidi-inhibit-bpa t)

;; don't render cursors or regions in other windows
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

;; lazily fontify text while scrolling
(setq fast-but-imprecise-scrolling t)

;; don't ping domains
(setq ffap-machine-p-known 'reject)

;; reduce rendering rate
(setq idle-update-delay 1.0)

;; don't compact fonts
(setq inhibit-compacting-font-caches t)

;; increase how much is read from processes in a single chunk
(setq read-process-output-max (* 64 1024))

;; inhibit fontification during input
(setq redisplay-skip-fontification-on-input t)

;; prevent gc from blocking thread when needed
(use-package gcmh
  :straight t
  :defer t
  :init
  (setq gcmh-idle-delay 'auto
        gcmh-auto-idle-delay-factor 10
        gcmh-high-cons-threshold (* 16 1024 2024))
  :config (gcmh-mode))

;;; disable ui early
(push '(menu-bar-lines . 0)   default-frame-alist)
(push '(tool-bar-lines . 0)   default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(setq menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil)

;;; encodings
(set-language-environment "UTF-8")
(setq default-input-method nil)

(provide 'core-start)
