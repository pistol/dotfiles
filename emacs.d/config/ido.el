;;; Interactive do, find-file and iswitchb replacement with fuzzy/flex matching.

;; Use C-f during file selection to switch to regular find-file
(ido-mode t)  ; use 'buffer rather than t to use only buffer switching
(ido-everywhere t)
(ido-ubiquitous t)

(setq
 ido-save-directory-list-file "~/.emacs.d/cache/ido.last"

 ;; ido-ignore-buffers ;; ignore these guys
 ;; '("\\` " "^\*Mess" "^\*Back" ".*Completion" "^\*Ido" "^\*trace"
 ;;     "^\*compilation" "^\*GTAGS" "^session\.*" "^\*")
 ido-work-directory-list '("~/.emacs.d" "~/Documents")
 ido-case-fold  t                 ; be case-insensitive

 ido-enable-last-directory-history t ; remember last used dirs
 ido-max-work-directory-list 30   ; should be enough
 ido-max-work-file-list      50   ; remember many
 ido-use-filename-at-point nil    ; don't use filename at point (annoying)
 ido-use-url-at-point nil         ; don't use url at point (annoying)

 ido-max-prospects 8

 ido-enable-flex-matching t
 ido-confirm-unique-completion nil ; wait for RET, even with unique completion

 ;; Allow the same buffer to be open in different frames
 ido-default-buffer-method 'selected-window

 ido-auto-merge-work-directories-length 0
 ;; This is awesome, opens previously closed buffers!
 ido-use-virtual-buffers t

 ;; when using ido, the confirmation is rather annoying...
 ido-show-confirm-message nil
 )

;; BUG: This causes a bug when going back up dir or in //
;; ;; sort ido filelist by mtime instead of alphabetically
;; (add-hook 'ido-make-file-list-hook 'ido-sort-mtime)
;; (add-hook 'ido-make-dir-list-hook 'ido-sort-mtime)
;; (defun ido-sort-mtime ()
;;   (setq ido-temp-list
;;         (sort ido-temp-list
;;               (lambda (a b)
;;                 (time-less-p
;;                  (sixth (file-attributes (concat ido-current-directory b)))
;;                  (sixth (file-attributes (concat ido-current-directory a)))))))
;;   (ido-to-end  ;; move . files to end (again)
;;    (delq nil (mapcar
;;               (lambda (x) (and (char-equal (string-to-char x) ?.) x))
;;               ido-temp-list))))


(add-to-list 'ido-ignore-files "\.DS_Store")
