;;; pistol-theme.el --- Custom face theme for Emacs
(deftheme pistol
  "Based on vim leo theme.")

(custom-theme-set-faces
 'pistol
 '(default                      ((t ( :background "Black"       :foreground "White"      :weight normal :underline nil ))))
 '(shadow                       ((t ( :background "#101010"     :foreground "DarkGray"   :weight normal :underline nil ))))
 '(highlight                    ((t ( :background "#cc0033"     :foreground nil                         :underline nil ))))
 ;; '(cursor                       ((t ( :background "#cc0033"     :foreground "White"      :weight normal :underline nil ))))
 '(cursor                       ((t ( :background "Green"       :foreground "Black"      :weight normal :underline nil ))))
 '(region                       ((t ( :background "#333333"     :foreground nil          :weight normal :underline nil ))))
 '(mode-line                    ((t ( :background "#0052a3"     :foreground "White"      :weight normal :underline nil ))))
 '(mode-line-inactive           ((t ( :background "#001f3d"     :foreground "Gray"       :weight normal :underline nil ))))
 '(fringe                       ((t ( :background "Black"       :foreground "DodgerBlue" :weight normal :underline nil ))))
 '(hl-line                      ((t ( :background "#101010"     :foreground nil                         :underline nil ))))
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
 '(font-lock-warning-face       ((t ( :background nil           :foreground "Magenta"    :weight normal :underline nil ))))
 '(font-lock-preprocessor-face  ((t ( :background nil           :foreground "LawnGreen"  :weight normal :underline nil ))))
 '(ahs-face                     ((t ( :background "#181818"     :foreground nil          :weight normal :underline nil ))))
 '(ahs-definition-face          ((t ( :background "CadetBlue"   :foreground "Black"      :weight normal :underline nil ))))
 '(ahs-warning-face             ((t ( :background nil           :foreground "Red"        :weight normal :underline nil ))))
 '(ahs-plugin-defalt-face       ((t ( :background "Orange1"     :foreground "Black"      :weight normal :underline nil ))))
 '(ahs-plugin-whole-buffer-face ((t ( :background "GreenYellow" :foreground "Black"      :weight normal :underline nil ))))
 '(ahs-plugin-bod-face          ((t ( :background "DodgerBlue"  :foreground "Black"      :weight normal :underline nil ))))
 '(ahs-plugin-edit-mode-face    ((t ( :background "Coral3"      :foreground "White"      :weight normal :underline nil ))))
 ;; '(iedit-occurence-face         ((t ( :background "Purple"      :foreground "White"      :weight normal :underline nil ))))
)

(provide-theme 'pistol)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; pistol-theme.el  ends here
