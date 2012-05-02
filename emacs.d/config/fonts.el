;;; Fonts

;; From: http://community.schemewiki.org/cgi-bin/scheme.cgi?Emacs
;; (and window-system
;; (create-fontset-from-fontset-spec
;;  (concat
;;   "-apple-monaco-medium-r-normal--12-*-*-*-*-*-fontset-monaco,"
;;   "-apple-Menlo-medium-normal-normal-*-12-*-*-*-m-0-iso10646-")
;;   "ascii:-apple-monaco-medium-r-normal--12-100-*-*-m-100-mac-roman")
;; )

;(set-default-font "-apple-Menlo-medium-normal-normal-*-12-*-*-*-m-0-iso10646-")
(set-default-font "Menlo-9")

;; http://stackoverflow.com/questions/3984730/emacs-gui-with-emacs-daemon-not-loading-fonts-correctly
;; Correctly set font even when starting in Daemon mode
;; (setq default-frame-alist '((font . "Menlo-12")))

;; To change the font size interactively per buffer:
;; Up:    C-x C-+, C-x C-=
;; Down:  C-x C--
;; Reset: C-x C-0
