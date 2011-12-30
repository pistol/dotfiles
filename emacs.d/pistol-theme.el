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
 '(default ((t (:background "#000000" :foreground "#ffffff"))))
 '(cursor ((t (:background "#000000" :foreground "#ffffff"))))
 '(region ((t (:background "#0d4519" :foreground "#000000"))))
 '(mode-line ((t (:background "#0000af" :foreground "#ffffff"))))
 '(mode-line-inactive ((t (:background "#e5e5e5" :foreground "#333333"))))
 '(fringe ((t (:background "#ffffff"))))
 '(minibuffer-prompt ((t (:foreground "#00d700" :weight bold))))
 '(font-lock-builtin-face ((t (:foreground "#005fff" :weight bold))))
 '(font-lock-comment-face ((t (:foreground "#a8a8a8"))))
 '(font-lock-constant-face ((t (:foreground "#fe7200"))))
 '(font-lock-function-name-face ((t (:foreground "#005fff" :weight bold))))
 '(font-lock-keyword-face ((t (:foreground "#005fff" :weight bold))))
 '(font-lock-string-face ((t (:foreground "#ad7fa8"))))
 '(font-lock-type-face ((t (:foreground "#00d700" :weight bold))))
 '(font-lock-variable-name-face ((t (:foreground "#d75fff"))))
 '(font-lock-warning-face ((t (:foreground "#ff0000" :weight bold))))
 '(isearch ((t (:background "#de14a1" :foreground "#ffffff"))))
 '(lazy-highlight ((t (:background "#020aee"))))
 '(link ((t (:foreground "#ffff00" :underline t))))
 '(link-visited ((t (:foreground "#ff871f" :underline t))))
 '(button ((t (:foreground "#00ff00" :underline t))))
 '(header-line ((t (:background "#e5e5e5" :foreground "#312f33")))))

(provide-theme 'pistol)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; pistol-theme.el  ends here
