;; Disable fancy morhping
(setq wg-morph-on nil)
;; Do not load first workgroup automatically on load (breaks emacs --daemon startup)
(setq wg-switch-on-load nil)
(wg-load "~/.emacs.d/.workgroups")
