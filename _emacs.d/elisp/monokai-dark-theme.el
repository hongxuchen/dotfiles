;;; monokai-dark-theme.el --- REQUIRES EMACS 24: monokai-dark Color Theme for Emacs.

;; Authors: Hongxu Chen <leftcopy.chx@gmail.com> Lorenzo Villani <lorenzo@villani.me>
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, version 3 of the License.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.

(deftheme monokai-dark "monokai-dark theme")

(custom-theme-set-faces
 'monokai-dark
 ;; Frame
 `(default ( (((class color) (min-colors 256)) (:foreground "#eee" :background "#222"))
             ((:foreground "#eee" :background "#000"))))
 ;; `(default ((t (:foreground "#f8f8f2" :background "#141411))))
 `(cursor ((t (:foreground "#f92672"))))
 `(hl-line ((t (:background "#141411"))))
 `(minibuffer-prompt ((t (:foreground "#1e90ff"))))
 `(mode-line ((t (:background "#1e1e1e" :foreground "#e6e6e6"))))
 `(region ((t (:background "#383830"))))
 `(show-paren-match-face ((t (:background "#383830"))))
 ;; Main
 `(font-lock-builtin-face ((t (:foreground "#a6e22a"))))
 `(font-lock-comment-face ((t (:foreground "#1e90ff"))))
 `(font-lock-constant-face ((t (:foreground "#ae81ff"))))
 `(font-lock-doc-string-face ((t (:foreground "#e6db74"))))
 `(font-lock-function-name-face ((t (:foreground "#a6e22a"))))
 `(font-lock-keyword-face ((t (:foreground "#f92672"))))
 `(font-lock-string-face ((t (:foreground "#e6db74"))))
 `(font-lock-type-face ((t (:foreground "#89bdff"))))
 `(font-lock-variable-name-face ((t (:foreground "#f92672"))))
 `(font-lock-warning-face ((t (:bold t :foreground "#fd5ff1"))))
 ;; ;; CUA
 `(cua-rectangle ((t (:background "#141411"))))
 ;; IDO
 `(ido-first-match ((t (:foreground "#ae81ff"))))
 `(ido-only-match ((t (:foreground "#a6e22a"))))
 `(ido-subdir ((t (:foreground "#89bdff"))))
 ;; Whitespace
 `(whitespace-space ((t (:foreground "#595959"))))
 ;; Yasnippet
 `(yas/field-highlight-face ((t (:background "#383830"))))
 ;; woman
 `(woman-bold ((((class color)) :foreground "#ff2200" :bold t)))
 `(woman-italic ((((class color)) :foreground "#00ee00" :italic nil)))
 `(woman-unknwon-face ((t (:foreground "#eeee00"))))
 `(woman-addition-face ((t (:foreground "#4169e1"))))
 ;; man
 `(Man-overstrike ((((class color)) :foreground "#ff2200" :bold t)))
 `(Man-underline ((((class color)) :foreground "#00ee33" :italic nil :bold t)))
 `(Man-reverse ((t (:foreground "#eeee00"))))
 ;; TODO which-func
 )

;;; ###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'monokai-dark)
(provide 'monokai-dark-theme)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; monokai-dark-theme.el ends here
