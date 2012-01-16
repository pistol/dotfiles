;; Global key bindigns
;; http://stackoverflow.com/questions/683425/globally-override-key-binding-in-emacs

(defvar my-keys-minor-mode-map (make-keymap) "my-keys-minor-mode keymap.")

;; Window manipulation
(define-key my-keys-minor-mode-map (kbd "C-M-<right>")  'enlarge-window-horizontally)
(define-key my-keys-minor-mode-map (kbd "C-M-<left>")   'shrink-window-horizontally)
(define-key my-keys-minor-mode-map (kbd "C-M-<up>")     'enlarge-window)
(define-key my-keys-minor-mode-map (kbd "C-M-<down>")   'shrink-window)

;; Find stuff
(define-key my-keys-minor-mode-map (kbd "<f2>")         'ack-default-directory)
(define-key my-keys-minor-mode-map (kbd "C-<f2>")       'ack-same)
(define-key my-keys-minor-mode-map (kbd "C-M-<f2>")     'ack)
(define-key my-keys-minor-mode-map (kbd "M-<f2>")       'find-name-dired)
(define-key my-keys-minor-mode-map (kbd "S-<f2>")       'occur)

;; Keyboard macros
(define-key my-keys-minor-mode-map (kbd "S-<f4>")       'kmacro-start-macro-or-insert-counter)
;; (define-key my-keys-minor-mode-map [kbd "<f4>")      'kmacro-end-or-call-macro)  ;; already defined

;; Refresh-like
(define-key my-keys-minor-mode-map (kbd "<f5>")         'revert-buffer)
(define-key my-keys-minor-mode-map (kbd "C-<f5>")       'revbufs)

;; Eval lisp region and buffer
(define-key my-keys-minor-mode-map (kbd "<f12>")        'eval-region)
(define-key my-keys-minor-mode-map (kbd "M-<f12>")      'eval-buffer)

;; Indenting and alignment
(define-key my-keys-minor-mode-map (kbd "<f8>")         'indent-region)
(define-key my-keys-minor-mode-map (kbd "C-<f8>")       'align)
(define-key my-keys-minor-mode-map (kbd "S-<f8>")       'align-current)
(define-key my-keys-minor-mode-map (kbd "M-<f8>")       'align-regexp)

;; map the window manipulation keys to meta 0, 1, 2, o
(define-key my-keys-minor-mode-map (kbd "M-3")          'split-window-horizontally) ; was digit-argument
(define-key my-keys-minor-mode-map (kbd "M-2")          'split-window-vertically) ; was digit-argument
(define-key my-keys-minor-mode-map (kbd "M-1")          'delete-other-windows) ; was digit-argument
(define-key my-keys-minor-mode-map (kbd "M-0")          'delete-window) ; was digit-argument
(define-key my-keys-minor-mode-map (kbd "M-o")          'other-window) ; was facemenu-keymap

(define-key my-keys-minor-mode-map (kbd "M-O")          'rotate-windows)

;; Repeat
(define-key my-keys-minor-mode-map (kbd "C-z")          'repeat) ; was suspend-frame

;; Mac OS X conventions
(define-key my-keys-minor-mode-map (kbd "M-a")          'mark-whole-buffer) ; was backward-sentence.

;; Find matching parens
(define-key my-keys-minor-mode-map (kbd "C-'")          'match-paren)

;; Easy inserts
(define-key my-keys-minor-mode-map (kbd "C-.")          'insert-arrow)

;; ibuffer > list-buffers
(define-key my-keys-minor-mode-map (kbd "C-x C-b")      'ibuffer)

;; Easier buffer killing
(global-unset-key (kbd "M-k"))
(define-key my-keys-minor-mode-map (kbd "M-K")          'kill-this-buffer)

;; Improved navigation and editing (assumes misc.el)
(define-key my-keys-minor-mode-map (kbd "M-Z")          'zap-up-to-char)
(define-key my-keys-minor-mode-map (kbd "M-F")          'forward-to-word)
(define-key my-keys-minor-mode-map (kbd "M-B")          'backward-to-word)

;; Personal textmate.el bindings
(define-key my-keys-minor-mode-map (kbd "C-c f")        'textmate-goto-file)
(define-key my-keys-minor-mode-map (kbd "C-<return>")         'textmate-next-line)

;; Tags
(define-key my-keys-minor-mode-map (kbd "M-,")          'pop-tag-mark) ; was tags-loop-continue

;; Vim-like duplicate current line
(define-key my-keys-minor-mode-map (kbd "C-c C-d")      'duplicate-line)

;; move-text bindings, move line/region using keys
(define-key my-keys-minor-mode-map (kbd "M-S-<up>")     'move-text-up)
(define-key my-keys-minor-mode-map (kbd "M-S-<down>")   'move-text-down)

;; iedit mode, mark all occurences of word at point and allow edit
(define-key my-keys-minor-mode-map (kbd "C-;")          'iedit-mode)

;; auto-highlight-symbol-mode A potentially better alternative to iedit with more features
(define-key my-keys-minor-mode-map (kbd "<f3>")         'global-auto-highlight-symbol-mode)
(define-key my-keys-minor-mode-map (kbd "M-S-<left>")   'ahs-backward)
(define-key my-keys-minor-mode-map (kbd "M-S-<right>")  'ahs-forward)

;; Window navigation
(windmove-default-keybindings                           'meta)
(define-key my-keys-minor-mode-map (kbd "M-<left>")     'windmove-left)
(define-key my-keys-minor-mode-map (kbd "M-<right>")    'windmove-right)
(define-key my-keys-minor-mode-map (kbd "M-<up>")       'windmove-up)
(define-key my-keys-minor-mode-map (kbd "M-<down>")     'windmove-down)

;; Select lines with mouse over line-nums bar
(define-key my-keys-minor-mode-map (kbd "<left-margin> <down-mouse-1>") 'md-select-linum)
(define-key my-keys-minor-mode-map (kbd "<left-margin> <mouse-1>")      'mu-select-linum)
(define-key my-keys-minor-mode-map (kbd "<left-margin> <drag-mouse-1>") 'mu-select-linum)

;; Smex and Ido, Smart Ido for M-X
(define-key my-keys-minor-mode-map (kbd "M-x")          'smex)
(define-key my-keys-minor-mode-map (kbd "M-X")          'smex-major-mode-commands)
;; This is your old M-x.
(define-key my-keys-minor-mode-map (kbd "C-c C-c M-x")  'execute-extended-command)

;; Move cursor back to last change
(define-key my-keys-minor-mode-map (kbd "C-x C-\\")     'goto-last-change)

;; Version control and change related
(define-key my-keys-minor-mode-map (kbd "<f9>") (lambda () (interactive) (magit-status default-directory)))

;; Replace dired's M-o
(add-hook 'dired-mode-hook   (lambda () (define-key dired-mode-map (kbd "M-o") 'other-window))) ; was dired-omit-mode
;; Replace ibuffer's M-o
(add-hook 'ibuffer-mode-hook (lambda () (define-key ibuffer-mode-map (kbd "M-o") 'other-window))) ; was ibuffer-visit-buffer-1-window
;; Use Ido in Ibuffer
(add-hook 'ibuffer-mode-hook (lambda () (define-key ibuffer-mode-map (kbd "C-x C-f") 'ibuffer-ido-find-file)))
;; To help Unlearn C-x 0, 1, 2, o
(global-unset-key (kbd "C-x 3")) ; was split-window-horizontally
(global-unset-key (kbd "C-x 2")) ; was split-window-vertically
(global-unset-key (kbd "C-x 1")) ; was delete-other-windows
(global-unset-key (kbd "C-x 0")) ; was delete-window
(global-unset-key (kbd "C-x o")) ; was other-window

(define-minor-mode my-keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  t " my-keys" 'my-keys-minor-mode-map)

(defun my-minibuffer-setup-hook ()
  (my-keys-minor-mode 0))

(add-hook 'minibuffer-setup-hook 'my-minibuffer-setup-hook)

(my-keys-minor-mode 1)
