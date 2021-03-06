;;; pistol-theme.el --- Custom face theme for Emacs
(deftheme pistol
  "Based on vim leo theme.")

(custom-theme-set-faces
 'pistol
 '(default                      ((t ( :background "Black"       :foreground "White"      :weight normal :underline nil ))))
 '(shadow                       ((t ( :background "#101010"     :foreground "DarkGray"   :weight normal :underline nil ))))
 '(highlight                    ((t ( :background "#cc0033"                                         ))))
 '(match                        ((t ( :background "#cc0033"     :foreground nil                                        ))))
 ;; '(cursor                       ((t ( :background "#cc0033"     :foreground "White"      :weight normal :underline nil ))))
 '(cursor                       ((t ( :background "Green"       :foreground "Black"      :weight normal :underline nil ))))
 '(region                       ((t ( :background "#333333"     :foreground nil          :weight normal                ))))
 '(mode-line                    ((t ( :background "#0052a3"     :foreground "White"      :weight normal :underline nil ))))
 ;; '(mode-line-inactive           ((t ( :background "#001f3d"     :foreground "Gray"       :weight normal :underline nil ))))
 '(mode-line-inactive           ((t ( :background "#333333"     :foreground "Gray"       :weight normal :underline nil ))))
 '(fringe                       ((t ( :background "Black"       :foreground "DodgerBlue" :weight normal :underline nil ))))
 '(hl-line                      ((t ( :background "#101010"     :foreground nil                                        ))))
 '(minibuffer-prompt            ((t ( :background nil           :foreground "Green"      :weight normal :underline nil ))))
 '(isearch                      ((t ( :background "DarkViolet"  :foreground "White"      :weight normal :underline nil ))))
 '(lazy-highlight               ((t ( :background "Orchid"      :foreground nil          :weight normal :underline nil ))))
 '(link                         ((t ( :background nil           :foreground "Yellow"     :weight normal :underline t   ))))
 '(link-visited                 ((t ( :background nil           :foreground "#ff871f"    :weight normal :underline t   ))))
 '(button                       ((t ( :background nil           :foreground "Lime"       :weight normal :underline nil ))))
 '(header-line                  ((t ( :background "#e5e5e5"     :foreground "#1a1a1a"    :weight normal :underline nil ))))
 '(font-lock-builtin-face       ((t ( :background nil           :foreground "DodgerBlue" :weight normal :underline nil ))))
 '(font-lock-comment-face       ((t ( :background nil           :foreground "DarkGray"   :weight normal :underline nil ))))
 '(font-lock-constant-face      ((t ( :background nil           :foreground "DarkOrange" :weight normal :underline nil ))))
 '(font-lock-function-name-face ((t ( :background "Black"       :foreground "#ff3300"    :weight normal :underline nil ))))
 '(font-lock-keyword-face       ((t ( :background nil           :foreground "Red"        :weight normal :underline nil ))))
 '(font-lock-string-face        ((t ( :background "#101010"     :foreground "Orchid"     :weight normal :underline nil ))))
 '(font-lock-type-face          ((t ( :background nil           :foreground "Green"      :weight normal :underline nil ))))
 '(font-lock-variable-name-face ((t ( :background nil           :foreground "Orange"     :weight normal :underline nil ))))
 '(font-lock-warning-face       ((t ( :background nil           :foreground "Red"        :weight normal :underline nil ))))
 '(font-lock-preprocessor-face  ((t ( :background nil           :foreground "LawnGreen"  :weight normal :underline nil ))))
 '(ahs-face                     ((t ( :background "#333333"     :foreground nil          :weight normal :underline nil ))))
 '(ahs-definition-face          ((t ( :background "CadetBlue"   :foreground "Black"      :weight normal :underline nil ))))
 '(ahs-warning-face             ((t ( :background nil           :foreground "Red"        :weight normal :underline nil ))))
 '(ahs-plugin-defalt-face       ((t ( :background "Orange1"     :foreground "Black"      :weight normal :underline nil ))))
 '(ahs-plugin-whole-buffer-face ((t ( :background "GreenYellow" :foreground "Black"      :weight normal :underline nil ))))
 '(ahs-plugin-bod-face          ((t ( :background "DodgerBlue"  :foreground "Black"      :weight normal :underline nil ))))
 '(ahs-plugin-edit-mode-face    ((t ( :background "Coral3"      :foreground "White"      :weight normal :underline nil ))))

 '(speedbar-button-face         ((t ( :background nil           :foreground "LawnGreen" :weight normal :underline nil ))))
 '(speedbar-selected-face       ((t ( :background "Red"         :foreground "White"      :weight normal :underline nil ))))
 '(speedbar-tag-face            ((t ( :background nil           :foreground "DarkOrange" :weight normal :underline nil ))))
 '(speedbar-highlight-face      ((t ( :background "Red"         :foreground "White"      :weight normal :underline nil ))))
 '(speedbar-directory-face      ((t ( :background nil           :foreground "DodgerBlue" :weight normal :underline nil ))))
 '(speedbar-file-face           ((t ( :background nil           :foreground "White"      :weight normal :underline nil ))))

 '(compilation-error            ((t ( :background nil           :foreground "DodgerBlue" :weight normal :underline nil ))))
 '(compilation-info             ((t ( :background nil           :foreground "DodgerBlue" :weight normal :underline nil ))))
 '(compilation-line-number      ((t ( :background nil           :foreground "DarkGray"   :weight normal :underline nil ))))
 ;; '(iedit-occurence-face         ((t ( :background "Purple"      :foreground "White"      :weight normal :underline nil ))))

 '(cperl-array-face             ((t ( :background "#101010"     :foreground "LawnGreen" :weight normal :underline nil ))))
 '(cperl-hash-face              ((t ( :background "#101010"     :foreground "DodgerBlue" :weight normal :underline nil ))))

 '(magit-header                 ((t ( :background "#0052a3" :foreground "White"))))
 '(magit-section-title          ((t ( :inherit magit-header ))))
 '(magit-branch                 ((t ( :inherit magit-header ))))
 '(magit-item-highlight         ((t ( :inherit highlight))))
 ;; '(magit-item-highlight         ((t ( nil))))
 '(magit-diff-file-header       ((t ( :inherit diff-file-header ))))
 '(magit-diff-add               ((t ( :inherit diff-added ))))
 '(magit-diff-del               ((t ( :inherit diff-removed ))))

 '(undo-tree-visualizer-active-branch-face ((t ( :background nil :foreground "Deep Pink"))))

 '(ido-first-match              ((t (:foreground "Orchid" :background nil))))
 '(ido-only-match               ((t (:foreground "deep pink" :background nil))))
 '(ido-subdir                   ((t (:foreground "DodgerBlue" :background nil))))
 '(ido-indicator                ((t (:foreground "black" :background "deep pink"))))

 '(rainbow-delimiters-depth-1-face ((t (:foreground "DodgerBlue"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "dark green"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "deep pink"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "yellow"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "green"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "light blue"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "orange"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "slate blue"))))
 '(rainbow-delimiters-depth-9-face ((t (:foreground "light gray"))))
 '(rainbow-delimiters-unmatched-face ((t (:foreground "red"))))

)

(provide-theme 'pistol)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; pistol-theme.el  ends here
