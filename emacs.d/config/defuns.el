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
(defun match-paren (arg)
  "Go to the matching parenthesis if on parenthesis."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))))

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
(defun ibuffer-ido-find-file ()
  "Like `ido-find-file', but default to the directory of the buffer at point."
  (interactive
   (let ((default-directory (let ((buf (ibuffer-current-buffer)))
            (if (buffer-live-p buf)
          (with-current-buffer buf
            default-directory)
        default-directory))))
     (ido-find-file-in-dir default-directory))))
