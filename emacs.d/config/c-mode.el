(setq c-backslash-max-column 79)
(add-hook 'c-mode-hook (lambda () (setq comment-start "//"
                                        comment-end   ""
                                        comment-padding " "))
