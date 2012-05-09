;;; Generic emacs settings I cannot live without

;; Use command as the meta key (Only in GUI)
(setq ns-command-modifier 'meta)

;; Don't show the startup screen
(setq inhibit-startup-message t)

;; "y or n" instead of "yes or no"
(fset 'yes-or-no-p 'y-or-n-p)

;; Highlight regions and add special behaviors to regions.
;; "C-h d transient" for more info
(setq transient-mark-mode t)

;; Display column numbers in modeline
(setq column-number-mode  t)

;; Line numbers are only for Vi-users
(global-linum-mode 0)
(linum-mode 0)

;; Highlight current line
;; (global-hl-line-mode t) ;; disabled - high cpu usage!

;; Show time in modeline
;; (display-time-mode 1)
;; (display-battery-mode 1)

;; Small fringes
(set-fringe-mode '(5 . 5))

;; Nice justify but too slow
;(setq linum-format (lambda (line) (propertize (format (let ((w (length (number-to-string (count-lines (point-min) (point-max)))))) (concat " %" (number-to-string w) "d ")) line) 'face 'linum)))

;; Format for line numbers on left
(setq linum-format " %d ")

;; Emacs gurus don't need no stinking scroll bars
(when (fboundp 'toggle-scroll-bar)
  (toggle-scroll-bar -1))

;; Explicitly show the end of a buffer
;(toggle-indicate-empty-lines)

;; Line-wrapping
(set-default 'fill-column 80)

;; Prevent the annoying beep on errors
;; (setq visible-bell t)

;; Turn the alarm totally off
(setq ring-bell-function 'ignore)

;; Make sure all backup files only live in one place
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

;; Gotta see matching parens
(show-paren-mode t)

;; Truncate, do not wrap lines
(setq-default truncate-lines t)
(setq truncate-partial-width-windows nil)

;; Trailing whitespace is unnecessary
;; Disabled: a lot of lines get changed in git repo files
;; (add-hook 'before-save-hook (lambda () (whitespace-cleanup)))

;; Trash can support
(setq delete-by-moving-to-trash t)

;; `brew install aspell --lang=en` (instead of ispell)
(setq-default ispell-program-name "aspell")
(setq ispell-list-command "list")
(setq ispell-extra-args '("--sug-mode=ultra"))

;; Maximize decoration (e.g. for dired+)
(setq font-lock-maximum-decoration t)

;; Smooth scrolling
;; http://stackoverflow.com/questions/1128927/how-to-scroll-line-by-line-in-gnu-emacs
(setq scroll-step            1
      scroll-conservatively  10000)

;; (if (equal "xterm" (tty-type))
;;     (xterm-fix-keys)
;; )

;; (defadvice terminal-init-xterm (after select-shift-up activate)
;;   (xterm-fix-keys)
;; )

;; fix shift-up mark select in xterm
;; http://lists.gnu.org/archive/html/help-gnu-emacs/2011-05/msg00211.html
(if (equal "xterm" (tty-type))
      (define-key input-decode-map "\e[1;2A" [S-up]))

(defadvice terminal-init-xterm (after select-shift-up activate)
  (define-key input-decode-map "\e[1;2A" [S-up]))

;; keep a list of recently opened files
(recentf-mode 1)

;; zap-up-to-char, forward-to-word, backward-to-word, etc
(require 'misc)

;; cperl-mode is preferred to perl-mode
(defalias 'perl-mode 'cperl-mode)

;; For emacsclient
;(server-start)

;; Enable mouse in xterm
(unless window-system
  (xterm-mouse-mode t)
  (defun track-mouse (e))
  (setq mouse-sel-mode t)
)

;; Put autosave files (ie #foo#) and backup files (ie foo~) in ~/.emacs.d/.
(defvar backup-dir (expand-file-name "~/.emacs.d/backups/"))
(defvar autosave-dir (expand-file-name "~/.emacs.d/autosaves/"))
(setq backup-directory-alist (list (cons ".*" backup-dir)))
(setq auto-save-list-file-prefix autosave-dir)
(setq auto-save-file-name-transforms `((".*" ,autosave-dir t)))

;; create the autosave dir if necessary, since emacs won't.
(make-directory "~/.emacs.d/autosaves/" t)
(make-directory "~/.emacs.d/backups/" t)

;; Show keys as you type shortcuts
(setq echo-keystrokes 0.1)

;; Auto Yes to "Symbolic link to SVN-controlled source file; follow link? "
(setq vc-follow-symlinks t)

;; workgroups mode
(workgroups-mode 1)
