(add-to-list 'load-path "~/.emacs.d/vendor")

(setq custom-file "~/.emacs.d/config/custom.el")
(load custom-file 'noerror)

(load "config/pistol-theme")
(load "config/theme")
(load "config/env")
(load "config/global")
(load "config/defuns")
(load "config/bindings")
(load "config/tabs")
(load "config/disabled")
(load "config/fonts")
(load "config/utf-8")
(load "config/scratch")
(load "config/grep")
(load "config/diff")
(load "config/ido")
(load "config/dired")
(load "config/recentf")
(load "config/rectangle")
(load "config/org")
(load "config/zoom")
;(load "config/flymake")
;(load "config/javascript")
;(load "config/ri-emacs")
(load "config/mac")
(load "config/server-mode")
(load "config/shell-mode")
(load "config/private" 'noerror)

;; (load "config/hl-line")
;; (load "config/iswitchb")

;(vendor 'ruby-mode)
;(vendor 'rinari)
(vendor 'textmate)
;(vendor 'maxframe      'mf 'maximize-frame)
;(vendor 'filladapt)
;(vendor 'coffee-mode)
;(vendor 'haml-mode)
;(vendor 'sass-mode)
;(vendor 'htmlize)
(vendor 'full-ack      'ack 'ack-same 'ack-find-same-file 'ack-find-file 'ack-interactive)
;(vendor 'cdargs        'cv 'cdargs)
(vendor 'magit         'magit-status)
(vendor 'psvn          'svn-status)
;(vendor 'js2-mode      'js2-mode)
;(vendor 'markdown-mode 'markdown-mode)
;(vendor 'textile-mode  'textile-mode)
;(vendor 'csv-mode      'csv-mode)
;(vendor 'yaml-mode     'yaml-mode)
;(vendor 'inf-ruby      'inf-ruby)
;(vendor 'rcodetools    'xmp)
(vendor 'yasnippet)
;(vendor 'jekyll)
;(vendor 'lua-mode      'lua-mode)
;(vendor 'feature-mode)
;(vendor 'mode-line-bell)
;(vendor 'revbufs       'revbufs)
;(vendor 'shell-pop)
;(vendor 'mo-git-blame  'mo-git-blame-file 'mo-git-blame-current)
;(vendor 'ace-jump-mode 'ace-jump-mode 'ace-jump-word-mode 'ace-jump-char-mode 'ace-jump-line-mode)
;(vendor 'key-chord)

;; (vendor 'ruby-electric 'ruby-electric-mode)
;; (vendor 'auctex)
