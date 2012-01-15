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
 '(default ((t (            :background "Black"   :foreground "White"))))
 '(cursor ((t (             :background "Green"   :foreground "White"))))
;; '(region ((t (             :background "#0d4519" :foreground "#000000"))))
 '(region ((t (             :background "#1a1a1a" :weight normal))))
 '(mode-line ((t (          :background "#0000af" :foreground "#ffffff"))))
 '(mode-line-inactive ((t ( :background "#e5e5e5" :foreground "#333333"))))
 '(fringe ((t (             :background "DarkGray"))))
 '(minibuffer-prompt ((t (                        :foreground "Green"      :weight normal))))
;; '(font-lock-builtin-face ((t (                   :foreground "#005fff"  :weight normal))))
 '(font-lock-builtin-face ((t (                   :foreground "DodgerBlue" :weight normal))))
;; '(font-lock-comment-face ((t (                   :foreground "#a8a8a8"  :weight normal))))
 '(font-lock-comment-face ((t (                   :foreground "DarkGray"   :weight normal))))
 '(font-lock-constant-face ((t (                  :foreground "DarkOrange" :weight normal))))
 '(font-lock-function-name-face ((t (             :foreground "Gold"       :weight normal))))
 '(font-lock-keyword-face ((t (                   :foreground "Red"        :weight normal))))
;; '(font-lock-string-face ((t (                    :foreground "#ad7fa8"  :weight normal))))
 '(font-lock-string-face ((t (                    :foreground "Orange"     :weight normal))))
 '(font-lock-type-face ((t (                      :foreground "Green"      :weight normal))))
 '(font-lock-variable-name-face ((t (             :foreground "Orchid"     :weight normal))))
 '(font-lock-warning-face ((t (                   :foreground "Crimson"    :weight normal))))
 '(font-lock-preprocessor-face ((t (              :foreground "LawnGreen"  :weight normal))))
 '(isearch ((t (            :background "#de14a1" :foreground "White"      :weight normal))))
 '(lazy-highlight ((t (     :background "#020aee"                          :weight normal))))
 '(link ((t (                                     :foreground "#ffff00"    :weight normal :underline t))))
 '(link-visited ((t (                             :foreground "#ff871f"    :weight normal :underline t))))
 '(button ((t (                                   :foreground "#00ff00"    :weight normal :underline t))))
 '(header-line ((t (        :background "#e5e5e5" :foreground "#312f33"    :weight normal)))))

(provide-theme 'pistol)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; pistol-theme.el  ends here
