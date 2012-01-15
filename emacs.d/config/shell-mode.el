;;; Shell scripts

(add-to-list 'auto-mode-alist '("bashrc$" . sh-mode))
(add-to-list 'auto-mode-alist '("bash_profile$" . sh-mode))
(add-to-list 'auto-mode-alist '("bash_aliases$" . sh-mode))
(add-to-list 'auto-mode-alist '("bash_local$" . sh-mode))
(add-to-list 'auto-mode-alist '("bash_completion$" . sh-mode))

(add-to-list 'auto-mode-alist '("bash_aliases_linux$" . sh-mode))
(add-to-list 'auto-mode-alist '("bash_aliases_mac$" . sh-mode))
(add-to-list 'auto-mode-alist '("bash_aliases_private$" . sh-mode))
(add-to-list 'auto-mode-alist '("bash_aliases_private_linux$" . sh-mode))
(add-to-list 'auto-mode-alist '("bash_aliases_private_mac$" . sh-mode))

(add-to-list 'auto-mode-alist '("bashrc_linux$" . sh-mode))
(add-to-list 'auto-mode-alist '("bashrc_mac$" . sh-mode))
(add-to-list 'auto-mode-alist '("bashrc_private$" . sh-mode))
(add-to-list 'auto-mode-alist '("bashrc_private_linux$" . sh-mode))
(add-to-list 'auto-mode-alist '("bashrc_private_mac$" . sh-mode))

(add-hook 'shell-mode-hook (lambda () (setq tab-width 8)))
