;;; Environment variables

(setq exec-path (cons "~/bin" exec-path))
(setenv "PATH" (concat "~/bin:" (getenv "PATH")))
(setq exec-path (cons "~/local/bin" exec-path))
(setenv "PATH" (concat "~/local/bin:" (getenv "PATH")))
(setq exec-path (cons "/usr/local/bin" exec-path))
(setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
(setq exec-path (cons "/usr/bin" exec-path))
(setenv "PATH" (concat "/usr/bin:" (getenv "PATH")))
(setq exec-path (cons "/bin" exec-path))
(setenv "PATH" (concat "/bin:" (getenv "PATH")))
(setq exec-path (cons "/usr/texbin" exec-path))
(setenv "PATH" (concat "/usr/texbin:" (getenv "PATH")))
(setq exec-path (cons "/Applications/Emacs.app/Contents/MacOS/bin" exec-path))
(setenv "PATH" (concat "/Applications/Emacs.app/Contents/MacOS/bin:" (getenv "PATH")))

;; WARNING: Disabled this as it seems to dramatically slow down all emacs shell commands
;; (setenv "BASH_ENV" "~/.bashrc")
