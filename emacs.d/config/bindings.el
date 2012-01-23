;; Global key bindigns
;; http://stackoverflow.com/questions/683425/globally-override-key-binding-in-emacs

(defvar my-keys-minor-mode-map (make-keymap) "my-keys-minor-mode keymap.")

;; Fix xterm keys to use correct mappings (Mac keyboard)
(xterm-fix-keys)

;; Window manipulation
(define-key my-keys-minor-mode-map (kbd "C-M-<right>")   'enlarge-window-horizontally)
(define-key my-keys-minor-mode-map (kbd "C-M-<left>")    'shrink-window-horizontally)
(define-key my-keys-minor-mode-map (kbd "C-M-<up>")      'enlarge-window)
(define-key my-keys-minor-mode-map (kbd "C-M-<down>")    'shrink-window)

;; Compilation
(define-key my-keys-minor-mode-map (kbd "M-<prior>")     'previous-error)
(define-key my-keys-minor-mode-map (kbd "M-<next>")      'next-error)

;; Most helpful help command
(define-key my-keys-minor-mode-map (kbd "<f1>")          'apropos)

;; Find stuff
(define-key my-keys-minor-mode-map (kbd "<f2>")          'ack-default-directory)
(define-key my-keys-minor-mode-map (kbd "C-<f2>")        'ack-same)
(define-key my-keys-minor-mode-map (kbd "C-M-<f2>")      'ack)
(define-key my-keys-minor-mode-map (kbd "M-<f2>")        'find-name-dired)
(define-key my-keys-minor-mode-map (kbd "S-<f2>")        'occur)

;; AHS mode: A potentially better alternative to iedit with more features
(define-key my-keys-minor-mode-map (kbd "<f3>")          'auto-highlight-symbol-mode)
(define-key my-keys-minor-mode-map (kbd "M-<f3>")        'ahs-change-range) ; cycle between whole buffer, display area and beggining of defun
(define-key my-keys-minor-mode-map (kbd "M-S-<f3>")      'global-auto-highlight-symbol-mode)
(define-key my-keys-minor-mode-map (kbd "M-S-<left>")    'ahs-backward)
(define-key my-keys-minor-mode-map (kbd "M-S-<right>")   'ahs-forward)

;; Keyboard macros
(define-key my-keys-minor-mode-map (kbd "S-<f4>")        'kmacro-start-macro-or-insert-counter)
(define-key my-keys-minor-mode-map (kbd "<f4>")          'kmacro-end-or-call-macro)  ;; already default

;; Refresh-like
(define-key my-keys-minor-mode-map (kbd "<f5>")          'revert-buffer)
(define-key my-keys-minor-mode-map (kbd "C-<f5>")        'revbufs)

;; Force smali highlighting in ack/grep modes
;; (define-key my-keys-minor-mode-map (kbd "<f7>")          'hack-smali-highlight)
;; (define-key my-keys-minor-mode-map (kbd "M-<f7>")        'hack-smali-highlight-reset)
(define-key my-keys-minor-mode-map (kbd "<f7>")          'shell-pop)

;; Indenting and alignment
(define-key my-keys-minor-mode-map (kbd "<f8>")          'indent-region)
(define-key my-keys-minor-mode-map (kbd "C-<f8>")        'align)
(define-key my-keys-minor-mode-map (kbd "S-<f8>")        'align-current)
(define-key my-keys-minor-mode-map (kbd "M-<f8>")        'align-regexp)

;; Version control and change related
(define-key my-keys-minor-mode-map (kbd "<f9>") (lambda () (interactive) (magit-status default-directory)))

;; ido bindings
(define-key my-keys-minor-mode-map (kbd "M-<f11>")       'ido-choose-from-recentf)

;; Eval lisp region and buffer
(define-key my-keys-minor-mode-map (kbd "<f12>")         'eval-region)
(define-key my-keys-minor-mode-map (kbd "M-<f12>")       'eval-buffer)

;; map the window manipulation keys to meta 0, 1, 2, o
(define-key my-keys-minor-mode-map (kbd "M-3")           'split-window-horizontally) ; was digit-argument
(define-key my-keys-minor-mode-map (kbd "M-2")           'split-window-vertically) ; was digit-argument
(define-key my-keys-minor-mode-map (kbd "M-1")           'delete-other-windows) ; was digit-argument
(define-key my-keys-minor-mode-map (kbd "M-0")           'delete-window) ; was digit-argument
(define-key my-keys-minor-mode-map (kbd "M-o")           'other-window) ; was facemenu-keymap

;; Fullscreen on Mac GUI
(define-key my-keys-minor-mode-map (kbd "M-S-<return>")    'ns-toggle-fullscreen)

;; Disabled: Breaks Terminal Emacs arrow keys
;(define-key my-keys-minor-mode-map (kbd "M-O")          'rotate-windows)

;; Repeat
(define-key my-keys-minor-mode-map (kbd "C-z")           'repeat) ; was suspend-frame

;; Mac OS X conventions
(define-key my-keys-minor-mode-map (kbd "M-a")           'mark-whole-buffer) ; was backward-sentence .
;; Easy save
(define-key my-keys-minor-mode-map (kbd "M-s M-s")       'save-buffer)

;; Find matching parens
(define-key my-keys-minor-mode-map (kbd "%")             'match-paren)

;; Easy inserts
(define-key my-keys-minor-mode-map (kbd "C-.")           'insert-arrow)

;; ibuffer > list-buffers
(define-key my-keys-minor-mode-map (kbd "C-x C-b")       'ibuffer)

;; Easier buffer killing
;(global-unset-key (kbd "M-k"))
(define-key my-keys-minor-mode-map (kbd "M-K")           'kill-this-buffer)

;; Home/End to Start and End of line instead of Start/End of buffer
;(define-key my-keys-minor-mode-map (kbd "<home>")        'beginning-of-line)
;(define-key my-keys-minor-mode-map (kbd "<end>")         'end-of-line)

;; Improved navigation and editing (assumes misc.el)
(define-key my-keys-minor-mode-map (kbd "M-Z")           'zap-up-to-char)
(define-key my-keys-minor-mode-map (kbd "M-F")           'forward-to-word)
(define-key my-keys-minor-mode-map (kbd "M-B")           'backward-to-word)

;; Personal textmate.el bindings
;;(define-key my-keys-minor-mode-map (kbd "C-c f")       'textmate-goto-file)
(define-key my-keys-minor-mode-map (kbd "C-c f")         'ffap)

(define-key my-keys-minor-mode-map (kbd "C-<return>")    'textmate-next-line)
(define-key my-keys-minor-mode-map (kbd "C-c C-t")       'textmate-clear-cache)
(define-key my-keys-minor-mode-map (kbd "C-c C-a")       'align-regexp)
;;(define-key my-keys-minor-mode-map (kbd "M-S-[")       'indent-according-to-mode)
(define-key my-keys-minor-mode-map (kbd "C-<tab>")       'textmate-shift-right)
(define-key my-keys-minor-mode-map (kbd "C-S-<tab>")     'textmate-shift-left)
(define-key my-keys-minor-mode-map (kbd "M-?")           'comment-or-uncomment-region-or-line)
(define-key my-keys-minor-mode-map (kbd "M-l")           'textmate-select-line) ; was downcase-word
(define-key my-keys-minor-mode-map (kbd "M-t")           'textmate-goto-file)
(define-key my-keys-minor-mode-map (kbd "M-T")           'textmate-goto-symbol)
;; (define-key my-keys-minor-mode-map (kbd "M-<up>")     'textmate-column-up)
;; (define-key my-keys-minor-mode-map (kbd "M-<down>")   'textmate-column-down)
;; (define-key my-keys-minor-mode-map (kbd "M-S-<up>")   'textmate-column-up-with-select)
;; (define-key my-keys-minor-mode-map (kbd "M-S-<down>") 'textmate-column-down-with-select)

;; Tags
(define-key my-keys-minor-mode-map (kbd "M-S-,")         'pop-tag-mark)
(define-key my-keys-minor-mode-map (kbd "M-,")           'tags-loop-continue) ; also M-*
(define-key my-keys-minor-mode-map (kbd "M-,")           (lambda () (interactive) (find-tag nil t))) ; default: M-*
(define-key my-keys-minor-mode-map (kbd "M-.")           'find-tag)

;; Vim-like duplicate current line
(define-key my-keys-minor-mode-map (kbd "C-c C-d")       'duplicate-line)

;; move-text bindings, move line/region using keys
(define-key my-keys-minor-mode-map (kbd "M-S-<up>")      'move-text-up)
(define-key my-keys-minor-mode-map (kbd "M-S-<down>")    'move-text-down)

;; iedit mode, mark all occurences of word at point and allow edit
(define-key my-keys-minor-mode-map (kbd "M-\"")          'iedit-mode)

;; Window navigation
;;(windmove-default-keybindings                            'meta)
(define-key my-keys-minor-mode-map (kbd "M-<left>")      'windmove-left)
(define-key my-keys-minor-mode-map (kbd "M-<right>")     'windmove-right)
(define-key my-keys-minor-mode-map (kbd "M-<up>")        'windmove-up)
(define-key my-keys-minor-mode-map (kbd "M-<down>")      'windmove-down)

;; Win-switch
(define-key my-keys-minor-mode-map (kbd "C-x o")         'win-switch-dispatch)

;; Smex and Ido, Smart Ido for M-X
(define-key my-keys-minor-mode-map (kbd "M-x")           'smex)
(define-key my-keys-minor-mode-map (kbd "M-X")           'smex-major-mode-commands)
;; This is your old M-x.
(define-key my-keys-minor-mode-map (kbd "C-c C-c M-x")   'execute-extended-command)

;; Move cursor back to last change
(define-key my-keys-minor-mode-map (kbd "C-x C-\\")      'goto-last-change)

;(define-key my-keys-minor-mode-map (kbd "C-c /")        'my-ido-hippie-expand)
;(define-key my-keys-minor-mode-map (kbd "C-c C-f")      'my-ido-hippie-expand-filename)

;; Make the most common commands easier
(define-key my-keys-minor-mode-map (kbd "C-f")           'ido-find-file)
(define-key my-keys-minor-mode-map (kbd "C-b")           'ido-switch-buffer)
(define-key my-keys-minor-mode-map (kbd "C-v")           'ido-choose-from-recentf)

;; mk-project
(define-key my-keys-minor-mode-map (kbd "C-c p c")       'project-compile)
(define-key my-keys-minor-mode-map (kbd "C-c p l")       'project-load)
(define-key my-keys-minor-mode-map (kbd "C-c p a")       'project-ack)
(define-key my-keys-minor-mode-map (kbd "C-c p g")       'project-grep)
(define-key my-keys-minor-mode-map (kbd "C-c p o")       'project-multi-occur)
(define-key my-keys-minor-mode-map (kbd "C-c p u")       'project-unload)
(define-key my-keys-minor-mode-map (kbd "C-c p f")       'project-find-file-ido) ; or project-find-file-ido
(define-key my-keys-minor-mode-map (kbd "C-c p i")       'project-index)
(define-key my-keys-minor-mode-map (kbd "C-c p s")       'project-status)
(define-key my-keys-minor-mode-map (kbd "C-c p h")       'project-home)
(define-key my-keys-minor-mode-map (kbd "C-c p d")       'project-dired)
(define-key my-keys-minor-mode-map (kbd "C-c p t")       'project-tags)

;; Select lines with mouse over line-nums bar
(define-key my-keys-minor-mode-map (kbd "<left-margin> <down-mouse-1>") 'md-select-linum)
(define-key my-keys-minor-mode-map (kbd "<left-margin> <mouse-1>")      'mu-select-linum)
(define-key my-keys-minor-mode-map (kbd "<left-margin> <drag-mouse-1>") 'mu-select-linum)

;; Replace dired's M-o
(add-hook 'dired-mode-hook   (lambda () (define-key dired-mode-map (kbd "M-o") 'other-window))) ; was dired-omit-mode
;; Replace ibuffer's M-o
(add-hook 'ibuffer-mode-hook (lambda () (define-key ibuffer-mode-map (kbd "M-o") 'other-window))) ; was ibuffer-visit-buffer-1-window
;; Use Ido in Ibuffer
(add-hook 'ibuffer-mode-hook (lambda () (define-key ibuffer-mode-map (kbd "C-x C-f") 'ibuffer-ido-find-file)))
;; To help Unlearn C-x 0, 1, 2, o
;; (global-unset-key (kbd "C-x 3")) ; was split-window-horizontally
;; (global-unset-key (kbd "C-x 2")) ; was split-window-vertically
;; (global-unset-key (kbd "C-x 1")) ; was delete-other-windows
;; (global-unset-key (kbd "C-x 0")) ; was delete-window
;; (global-unset-key (kbd "C-x o")) ; was other-window

(define-minor-mode my-keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  t " my-keys" 'my-keys-minor-mode-map)

(defun my-minibuffer-setup-hook ()
  (my-keys-minor-mode 0))

(add-hook 'minibuffer-setup-hook 'my-minibuffer-setup-hook)

(my-keys-minor-mode 1)
