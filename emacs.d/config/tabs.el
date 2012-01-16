;;; Tab management

;; Spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; If there is a tab, make it the size of 2 spaces
(setq-default tab-width 2)

;; Mode specific indent sizes
;; TODO: Consider putting these in their own mode specific inits
(setq c-basic-offset 4)
(setq css-indent-offset 2)
(setq sh-basic-offset 2)
(set-default 'javascript-indent-level 2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hippie expand.  Groovy vans with tie-dyes.

(setq hippie-expand-try-functions-list
      '(yas/hippie-try-expand
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name
        try-complete-lisp-symbol))

;; disable smart-tab when using sunrirse-commander
(add-to-list 'smart-tab-disabled-major-modes 'sr-mode)
