;;; pistol-theme.el --- Custom face theme for Emacs

;; Copyright (C) 2010 Pierre Karashchuk.

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(deftheme pistol
  "Based on vim leo theme.")

(custom-theme-set-faces
 'pistol
 '(default                      ((t ( :background "Black"      :foreground "White"                                  ))))
 '(shadow                       ((t ( :background "#1a1a1a"    :foreground "DarkGray"                               ))))
 '(cursor                       ((t ( :background "#dc143c"    :foreground "White"                                  ))))
 '(region                       ((t ( :background "#333333"                             :weight normal              ))))
 '(mode-line                    ((t ( :background "#0052a3"    :foreground "White"                                  ))))
 '(mode-line-inactive           ((t ( :background "#001f3d"    :foreground "Gray"                                   ))))
 '(fringe                       ((t ( :background "Black"      :foreground "DodgerBlue"                             ))))
 '(hl-line                      ((t ( :background "#0052a3"                                                         ))))
 '(minibuffer-prompt            ((t (                          :foreground "Green"      :weight normal              ))))
 '(font-lock-builtin-face       ((t (                          :foreground "DodgerBlue" :weight normal              ))))
 '(font-lock-comment-face       ((t (                          :foreground "DarkGray"   :weight normal              ))))
 '(font-lock-constant-face      ((t (                          :foreground "DarkOrange" :weight normal              ))))
 '(font-lock-function-name-face ((t (                          :foreground "Gold"       :weight normal              ))))
 '(font-lock-keyword-face       ((t (                          :foreground "Red"        :weight normal              ))))
 '(font-lock-string-face        ((t (                          :foreground "Orange"     :weight normal              ))))
 '(font-lock-type-face          ((t (                          :foreground "Green"      :weight normal              ))))
 '(font-lock-variable-name-face ((t (                          :foreground "Orchid"     :weight normal              ))))
 '(font-lock-warning-face       ((t (                          :foreground "Crimson"    :weight normal              ))))
 '(font-lock-preprocessor-face  ((t (                          :foreground "LawnGreen"  :weight normal              ))))
 '(isearch                      ((t ( :background "DarkViolet" :foreground "White"      :weight normal              ))))
 '(lazy-highlight               ((t ( :background "Orchid"                              :weight normal              ))))
 '(link                         ((t (                          :foreground "Yellow"     :weight normal :underline t ))))
 '(link-visited                 ((t (                          :foreground "#ff871f"    :weight normal :underline t ))))
 '(button                       ((t (                          :foreground "Lime"       :weight normal :underline t ))))
 '(header-line                  ((t ( :background "#e5e5e5"    :foreground "#1a1a1a"    :weight normal              ))))
)

(provide-theme 'pistol)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; pistol-theme.el  ends here
