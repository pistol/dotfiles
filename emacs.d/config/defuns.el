;;; Personal functions

;; For loading libraries from the vendor directory
;; Modified from defunkt's original version to support autoloading.
;; http://github.com/defunkt/emacs/blob/master/defunkt/defuns.el
(defun vendor (library &rest autoload-functions)
  (let* ((file (symbol-name library))
         (normal (concat "~/.emacs.d/vendor/" file))
         (suffix (concat normal ".el"))
         (personal (concat "~/.emacs.d/config/" file))
         (found nil))
    (cond
     ((file-directory-p normal) (add-to-list 'load-path normal) (set 'found t))
     ((file-directory-p suffix) (add-to-list 'load-path suffix) (set 'found t))
     ((file-exists-p suffix)  (set 'found t)))
    (when found
      (if autoload-functions
          (dolist (autoload-function autoload-functions)
            (autoload autoload-function (symbol-name library) nil t))
        (require library)))
    (when (file-exists-p (concat personal ".el"))
      (load personal))))

;; Arrows are common, especially in ruby
(defun insert-arrow ()
  (interactive)
  (delete-horizontal-space)
  (insert " => "))

;; Quickly jump back and forth between matching parens/brackets
;; http://www.grok2.com/vi-emacs.html
(defun match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert ."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
            (t (self-insert-command (or arg 1)))))

;; Make the whole buffer pretty and consistent
(defun iwb()
  "Indent Whole Buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

(defun delete-window-replacement (&optional p)
  "Kill current window.  If called with PREFIX, kill the buffer too."
  (interactive "P")
  (if p
      (kill-buffer nil))
  (delete-window))

(defun delete-other-windows-replacement (&optional p)
  "Make the selected window fill its frame.  If called with PREFIX,
kill all other visible buffers."
  (interactive "P")
  (if p
      (dolist (window (window-list))
        (unless (equal (window-buffer window) (current-buffer))
          (kill-buffer (window-buffer window)))))
  (delete-other-windows))

(defun lorem ()
  (interactive)
  (insert "Lorem ipsum dolor sit amet, consectetuer adipiscing elit. Praesent libero orci, auctor sed, faucibus vestibulum, gravida vitae, arcu. Nunc posuere. Suspendisse potenti. Praesent in arcu ac nisl ultricies ultricies. Fusce eros. Sed pulvinar vehicula ante. Maecenas urna dolor, egestas vel, tristique et, porta eu, leo. Curabitur vitae sem eget arcu laoreet vulputate. Cras orci neque, faucibus et, rhoncus ac, venenatis ac, magna. Aenean eu lacus. Aliquam luctus facilisis augue. Nullam fringilla consectetuer sapien. Aenean neque augue, bibendum a, feugiat id, lobortis vel, nunc. Suspendisse in nibh quis erat condimentum pretium. Vestibulum tempor odio et leo. Sed sodales vestibulum justo. Cras convallis pellentesque augue. In eu magna. In pede turpis, feugiat pulvinar, sodales eget, bibendum consectetuer, magna. Pellentesque vitae augue."))

;; ;; Use the text around point as a cue what it is that we want from the
;; ;; editor. Allowance has to be made for the case that point is at the
;; ;; edge of a buffer.
;; (defun indent-or-expand (arg)
;;   "Either indent according to mode, or expand the word preceding
;; point."
;;   (interactive "*P")
;;   (if (and
;;        (or (bobp) (= ?w (char-syntax (char-before))))
;;        (or (eobp) (not (= ?w (char-syntax (char-after))))))
;;       (dabbrev-expand arg)
;;     (indent-according-to-mode)))

;; This override for transpose-words fixes what I consider to be a flaw with the
;; default implementation in simple.el. To traspose chars or lines, you always
;; put the point on the second char or line to transpose with the previous char
;; or line.  The default transpose-words implementation does the opposite by
;; flipping the current word with the next word instead of the previous word.
;; The new implementation below instead makes transpose-words more consistent
;; with how transpose-chars and trasponse-lines behave.
(defun transpose-words (arg)
  "[Override for default transpose-words in simple.el]
Interchange words around point, leaving point at end of
them. With prefix arg ARG, effect is to take word before or
around point and drag it backward past ARG other words (forward
if ARG negative).  If ARG is zero, the words around or after
point and around or after mark are interchanged."
  (interactive "*p")
  (if (eolp) (forward-char -1))
  (transpose-subr 'backward-word arg)
  (forward-word (+ arg 1)))

;; Borrowed from https://gist.github.com/1415844
;; Also see http://emacsworld.blogspot.com/2011/12/moving-buffers-between-windows.html
(require 'cl)
(defun rotate-left (l)
  (append  (cdr l) (list (car l))))
(defun rotate-windows ()
  (interactive)
  (let ((start-positions (rotate-left (mapcar 'window-start (window-list))))
        (buffers (rotate-left (mapcar 'window-buffer (window-list)))))
    (mapcar* (lambda (window  buffer pos)
               (set-window-buffer window buffer)
               (set-window-start window pos))
             (window-list)
             buffers
             start-positions)))

;; Simple Vim-like duplicate line
;; http://stackoverflow.com/questions/88399/how-do-i-duplicate-a-whole-line-in-emacs
(defun duplicate-line (arg)
  "Duplicate current line, leaving point in lower line."
  (interactive "*p")

  ;; save the point for undo
  (setq buffer-undo-list (cons (point) buffer-undo-list))

  ;; local variables for start and end of line
  (let ((bol (save-excursion (beginning-of-line) (point)))
        eol)
    (save-excursion

      ;; don't use forward-line for this, because you would have
      ;; to check whether you are at the end of the buffer
      (end-of-line)
      (setq eol (point))

      ;; store the line and disable the recording of undo information
      (let ((line (buffer-substring bol eol))
            (buffer-undo-list t)
            (count arg))
        ;; insert the line arg times
        (while (> count 0)
          (newline)         ;; because there is no newline in 'line'
          (insert line)
          (setq count (1- count)))
        )

      ;; create the undo information
      (setq buffer-undo-list (cons (cons eol (point)) buffer-undo-list)))
    ) ; end-of-let

  ;; put the point in the lowest line and return
  (next-line arg))


;; Linum: Select lines my clicking
;; Select lines by click-dragging on the margin. Tested with GNU Emacs 23.3
;; http://www.emacswiki.org/emacs/LineNumbers
(defvar *linum-mdown-line* nil)

(defun line-at-click ()
  (save-excursion
    (let ((click-y (cdr (cdr (mouse-position))))
          (line-move-visual-store line-move-visual))
      (setq line-move-visual t)
      (goto-char (window-start))
      (next-line (1- click-y))
      (setq line-move-visual line-move-visual-store)
      ;; If you are using tabbar substitute the next line with
      ;; (line-number-at-pos))))
      (1+ (line-number-at-pos)))))

(defun md-select-linum ()
  (interactive)
  (goto-line (line-at-click))
  (set-mark (point))
  (setq *linum-mdown-line*
        (line-number-at-pos)))

(defun mu-select-linum ()
  (interactive)
  (when *linum-mdown-line*
    (let (mu-line)
      ;; (goto-line (line-at-click))
      (setq mu-line (line-at-click))
      (goto-line (max *linum-mdown-line* mu-line))
      (set-mark (line-end-position))
      (goto-line (min *linum-mdown-line* mu-line))
      (setq *linum-mdown*
            nil))))

;; Use ido in ibuffer
;; ibuffer mode doesn’t come with an option to use ido-find-file, so here’s a simple adaptation of ibuffer-find-file that uses ido instead of read-file:
;; http://www.emacswiki.org/emacs-es/InteractivelyDoThings
;; (defun ibuffer-ido-find-file ()
;;   "Like `ido-find-file', but default to the directory of the buffer at point."
;;   (interactive
;;    (let ((default-directory (let ((buf (ibuffer-current-buffer)))
;;             (if (buffer-live-p buf)
;;           (with-current-buffer buf
;;             default-directory)
;;         default-directory))))
;;      (ido-find-file-in-dir default-directory))))


;; http://emacslife.blogspot.com/2008/02/icicles.html
;; (defun ido-choose-from-recentf ()
;;   "Use ido to select a recently opened file from the `recentf-list'"
;;   (interactive)
;;   (if (and ido-use-virtual-buffers (fboundp 'ido-toggle-virtual-buffers))
;;       (ido-switch-buffer)
;;     (find-file (ido-completing-read "Open file: " recentf-list nil t))))

;; http://www.xsteve.at/prg/emacs/power-user-tips.html
;; (defun ido-choose-from-recentf ()
;;   "Use ido to select a recently opened file from the `recentf-list'"
;;   (interactive)
;;   (let ((home (expand-file-name (getenv "HOME"))))
;;     (find-file
;;      (ido-completing-read "Recentf open: "
;;                           (mapcar (lambda (path)
;;                                     (replace-regexp-in-string home "~" path))
;;                                   recentf-list)
;;                           nil t))))

;; Better approach than previous
;; http://www.masteringemacs.org/articles/2011/01/27/find-files-faster-recent-files-package/
(defun ido-choose-from-recentf ()
  "Use ido to select a recently opened file from the `recentf-list'"
  (interactive)
  (ido-completing-read
   "Recentf open: "
   (mapcar 'abbreviate-file-name recentf-list)
   nil t))

(defun my-hippie-expand-completions (&optional hippie-expand-function)
  "Return the full list of possible completions generated by `hippie-expand'.
    The optional argument can be generated with `make-hippie-expand-function'."
  (let ((this-command 'my-hippie-expand-completions)
        (last-command last-command)
        (buffer-modified (buffer-modified-p))
        (hippie-expand-function (or hippie-expand-function 'hippie-expand)))
    (flet ((ding)) ; avoid the (ding) when hippie-expand exhausts its options.
      (while (progn
               (funcall hippie-expand-function nil)
               (setq last-command 'my-hippie-expand-completions)
               (not (equal he-num -1)))))
    ;; Evaluating the completions modifies the buffer, however we will finish
    ;; up in the same state that we began, and (save-current-buffer) seems a
    ;; bit heavyweight in the circumstances.
    (set-buffer-modified-p buffer-modified)
    ;; Provide the options in the order in which they are normally generated.
    (delete he-search-string (reverse he-tried-table))))

;; (defmacro my-ido-hippie-expand-with (hippie-expand-function)
;;   "Generate an interactively-callable function that offers ido-based completion
;;     using the specified hippie-expand function."
;;   `(call-interactively
;;     (lambda (&optional selection)
;;       (interactive
;;        (let ((options (my-hippie-expand-completions ,hippie-expand-function)))
;;          (if options
;;              (list (ido-completing-read "Completions: " options)))))
;;       (if selection
;;           (he-substitute-string selection t)

;; (defun my-ido-hippie-expand ()
;;   "Offer ido-based completion for the word at point."
;;   (interactive)
;;   (my-ido-hippie-expand-with 'hippie-expand))

;; (defun my-ido-hippie-expand-filename ()
;;   "Offer ido-based completion for the filename at point."
;;   (interactive)
;;   (my-ido-hippie-expand-with
;;    (make-hippie-expand-function '(try-complete-file-name))))

;; Hack Ack-mode to allow Smali syntax highlighting
;; Also works for Grep-mode. Occurs doesn't need it
(defun hack-smali-highlight ()
  ;; "Hack ack-mode to show smali highlight (smali is major mode and conflicts)"
  (interactive)
  (smali-mode)
  (compilation-minor-mode 1))

;; Change back to default settings
(defun hack-smali-highlight-reset ()
  ;; "Hack ack-mode to show smali highlight (smali is major mode and conflicts)"
  ;; (ack-mode)
  ;; (smali-mode)
  (interactive)
  (ack-mode))

;; Open all files from ack/grep mode in buffers
;; Goal is to then be able do a Multi-Occur on all Files by loading them into buffers
(defun visit-all-find-results ()
  "do next-error until there are no more"
  (interactive)
  (condition-case nil
      (while t
        (next-error))
    (error nil)))

(defun revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive) (revert-buffer t t)
  (message "Refreshed current file."))

(defun revert-all-buffers ()
  "Refreshes all open buffers from their respective files."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (buffer-file-name) (not (buffer-modified-p)))
        (revert-buffer t t t) )))
  (message "Refreshed open files."))

(defun xterm-fix-keys ()
  (interactive)
  (define-key input-decode-map "\e[H"     [home])
  (define-key input-decode-map "\e[F"     [end])

  ;; Meta + PgUp/PgDn
  (define-key input-decode-map "\e[5;9~"  [M-prior])
  (define-key input-decode-map "\e[6;9~"  [M-next])

  ;; Shift + Arrows
  (define-key input-decode-map "\e[1;2A"  [S-up])
  ;; (define-key input-decode-map "\e[1;2B"  [S-down])
  ;; (define-key input-decode-map "\e[1;2C"  [S-right])
  ;; (define-key input-decode-map "\e[1;2D"  [S-left])

  ;; Control + Arrows
  (define-key input-decode-map "\e[1;5A"  [C-up])
  (define-key input-decode-map "\e[1;5B"  [C-down])
  (define-key input-decode-map "\e[1;5C"  [C-right])
  (define-key input-decode-map "\e[1;5D"  [C-left])

  ;; Control + Shift + Arrows
  (define-key input-decode-map "\e[1;6A"  [C-S-up])
  (define-key input-decode-map "\e[1;6B"  [C-S-down])
  (define-key input-decode-map "\e[1;6C"  [C-S-right])
  (define-key input-decode-map "\e[1;6D"  [C-S-left])

  ;; Meta + Arrows
  (define-key input-decode-map "\e[1;9A"  [M-up])
  (define-key input-decode-map "\e[1;9B"  [M-down])
  (define-key input-decode-map "\e[1;9C"  [M-right])
  (define-key input-decode-map "\e[1;9D"  [M-left])

  ;; Meta + Shift + Arrows
  (define-key input-decode-map "\e[1;10A" [M-S-up])
  (define-key input-decode-map "\e[1;10B" [M-S-down])
  (define-key input-decode-map "\e[1;10C" [M-S-right])
  (define-key input-decode-map "\e[1;10D" [M-S-left])

  ;; Control + Meta + Arrows
  (define-key input-decode-map "\e[1;13A" [C-M-up])
  (define-key input-decode-map "\e[1;13B" [C-M-down])
  (define-key input-decode-map "\e[1;13C" [C-M-right])
  (define-key input-decode-map "\e[1;13D" [C-M-left])

  ;; Shift + Fn
  ;; iTerm mapped and supported by Emacs
  ;; (define-key input-decode-map "\e[1;2P"  [S-f1])
  ;; (define-key input-decode-map "\e[1;2Q"  [S-f2])
  ;; (define-key input-decode-map "\e[1;2R"  [S-f3])
  ;; (define-key input-decode-map "\e[1;2S"  [S-f4])
  ;; (define-key input-decode-map "\e[15;2~" [S-f5])
  ;; (define-key input-decode-map "\e[17;2~" [S-f6])
  ;; (define-key input-decode-map "\e[18:2~" [S-f7])
  ;; (define-key input-decode-map "\e[19;2~" [S-f8])
  ;; (define-key input-decode-map "\e[20;2~" [S-f9])
  ;; (define-key input-decode-map "\e[21;2~" [S-f10])
  ;; (define-key input-decode-map "\e[23;2~" [S-f11])
  ;; (define-key input-decode-map "\e[24;2~" [S-f12])
  ;; (define-key input-decode-map "\e[25;2~" [S-f13])
  ;; (define-key input-decode-map "\e[26;2~" [S-f14])
  ;; (define-key input-decode-map "\e[28:2~" [S-f15])

  ;; Control + Fn
  ;; iTerm mapped and supported by Emacs
  ;; (define-key input-decode-map "\e[1;5P"  [C-f1])
  ;; (define-key input-decode-map "\e[1;5Q"  [C-f2])
  ;; (define-key input-decode-map "\e[1;5R"  [C-f3])
  ;; (define-key input-decode-map "\e[1;5S"  [C-f4])
  ;; (define-key input-decode-map "\e[15;5~" [C-f5])
  ;; (define-key input-decode-map "\e[17;5~" [C-f6])
  ;; (define-key input-decode-map "\e[18:5~" [C-f7])
  ;; (define-key input-decode-map "\e[19;5~" [C-f8])
  ;; (define-key input-decode-map "\e[20;5~" [C-f9])
  ;; (define-key input-decode-map "\e[21;5~" [C-f10])
  ;; (define-key input-decode-map "\e[23;5~" [C-f11])
  ;; (define-key input-decode-map "\e[24;5~" [C-f12])
  ;; (define-key input-decode-map "\e[25;5~" [C-f13])
  ;; (define-key input-decode-map "\e[26;5~" [C-f14])
  ;; (define-key input-decode-map "\e[28:5~" [C-f15])

  ;; Control + Shift + Fn
  ;; iTerm mapped and supported by Emacs
  ;; (define-key input-decode-map "\e[1;6P"  [C-S-f1])
  ;; (define-key input-decode-map "\e[1;6Q"  [C-S-f2])
  ;; (define-key input-decode-map "\e[1;6R"  [C-S-f3])
  ;; (define-key input-decode-map "\e[1;6S"  [C-S-f4])
  ;; (define-key input-decode-map "\e[15;6~" [C-S-f5])
  ;; (define-key input-decode-map "\e[17;6~" [C-S-f6])
  ;; (define-key input-decode-map "\e[18:6~" [C-S-f7])
  ;; (define-key input-decode-map "\e[19;6~" [C-S-f8])
  ;; (define-key input-decode-map "\e[20;6~" [C-S-f9])
  ;; (define-key input-decode-map "\e[21;6~" [C-S-f10])
  ;; (define-key input-decode-map "\e[23;6~" [C-S-f11])
  ;; (define-key input-decode-map "\e[24;6~" [C-S-f12])
  ;; (define-key input-decode-map "\e[25;6~" [C-S-f13])
  ;; (define-key input-decode-map "\e[26;6~" [C-S-f14])
  ;; (define-key input-decode-map "\e[28:6~" [C-S-f15])

  ;; Meta + Fn
  (define-key input-decode-map "\e[1;9P"  [M-f1])
  (define-key input-decode-map "\e[1;9Q"  [M-f2])
  (define-key input-decode-map "\e[1;9R"  [M-f3])
  (define-key input-decode-map "\e[1;9S"  [M-f4])
  (define-key input-decode-map "\e[15;9~" [M-f5])
  (define-key input-decode-map "\e[17;9~" [M-f6])
  (define-key input-decode-map "\e[18:9~" [M-f7])
  (define-key input-decode-map "\e[19;9~" [M-f8])
  (define-key input-decode-map "\e[20;9~" [M-f9])
  (define-key input-decode-map "\e[21;9~" [M-f10])
  (define-key input-decode-map "\e[23;9~" [M-f11])
  (define-key input-decode-map "\e[24;9~" [M-f12])
  (define-key input-decode-map "\e[25;9~" [M-f13])
  (define-key input-decode-map "\e[26;9~" [M-f14])
  (define-key input-decode-map "\e[28:9~" [M-f15])

  ;; Meta + Shift + Fn
  (define-key input-decode-map "\e[1;10P"  [M-S-f1])
  (define-key input-decode-map "\e[1;10Q"  [M-S-f2])
  (define-key input-decode-map "\e[1;10R"  [M-S-f3])
  (define-key input-decode-map "\e[1;10S"  [M-S-f4])
  (define-key input-decode-map "\e[15;10~" [M-S-f5])
  (define-key input-decode-map "\e[17;10~" [M-S-f6])
  (define-key input-decode-map "\e[18:10~" [M-S-f7])
  (define-key input-decode-map "\e[19;10~" [M-S-f8])
  (define-key input-decode-map "\e[20;10~" [M-S-f9])
  (define-key input-decode-map "\e[21;10~" [M-S-f10])
  (define-key input-decode-map "\e[23;10~" [M-S-f11])
  (define-key input-decode-map "\e[24;10~" [M-S-f12])
  (define-key input-decode-map "\e[25;10~" [M-S-f13])
  (define-key input-decode-map "\e[26;10~" [M-S-f14])
  (define-key input-decode-map "\e[28:10~" [M-S-f15])

  ;; Meta + Shift + Control - Fn
  ;; (define-key input-decode-map "\e[1;14P"  [M-S-C-f1])
  ;; (define-key input-decode-map "\e[1;14Q"  [M-S-C-f2])
  ;; (define-key input-decode-map "\e[1;14R"  [M-S-C-f3])
  ;; (define-key input-decode-map "\e[1;14S"  [M-S-C-f4])
  ;; (define-key input-decode-map "\e[15;14~" [M-S-C-f5])
  ;; (define-key input-decode-map "\e[17;14~" [M-S-C-f6])
  ;; (define-key input-decode-map "\e[18:14~" [M-S-C-f7])
  ;; (define-key input-decode-map "\e[19;14~" [M-S-C-f8])
  ;; (define-key input-decode-map "\e[20;14~" [M-S-C-f9])
  ;; (define-key input-decode-map "\e[21;14~" [M-S-C-f10])
  ;; (define-key input-decode-map "\e[23;14~" [M-S-C-f11])
  ;; (define-key input-decode-map "\e[24;14~" [M-S-C-f12])
  ;; (define-key input-decode-map "\e[25;14~" [M-S-C-f13])
  ;; (define-key input-decode-map "\e[26;14~" [M-S-C-f14])
  ;; (define-key input-decode-map "\e[28:14~" [M-S-C-f15])
  )
