;; http://www.emacswiki.org/emacs/IbufferMode
;; Use human readable Size column instead of original one
;; (define-ibuffer-column size-h
;;   (:name "Size" :inline t)
;;   (cond
;;    ((> (buffer-size) 1000) (format "%7.3fk" (/ (buffer-size) 1000.0)))
;;    ((> (buffer-size) 1000000) (format "%7.3fM" (/ (buffer-size) 1000000.0)))
;;    (t (format "%8d" (buffer-size)))))

;; ;; Modify the default ibuffer-formats
;;   (setq ibuffer-formats
;;  '((mark modified read-only " "
;;    (name 18 18 :left :elide)
;;    " "
;;    (size-h 9 -1 :right)
;;    " "
;;    (mode 16 16 :left :elide)
;;    " "
;;    filename-and-process)))

(setq ibuffer-expert t)

(setq ibuffer-show-empty-filter-groups nil)

(add-hook 'ibuffer-mode-hook
          '(lambda ()
             ;; Auto refresh buffers list
             (ibuffer-auto-mode 1)
             (ibuffer-switch-to-saved-filter-groups "home")))
