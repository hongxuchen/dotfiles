;;; monokai-dark-theme.el --- REQUIRES EMACS 24: monokai-dark Color Theme for Emacs.

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
(defconst THRESHOLD 1024)

(custom-theme-set-faces
 'monokai-dark
 ;; Frame
 `(default ( (((class color) (min-colors ,THRESHOLD)) (:foreground "#f8f8f2" :background "#202020"))
             (t (:foreground "#adadad" :background "#000"))))
 ;; ;; Grep
 `(grep-context-face ((((class color) (min-colors ,THRESHOLD)) (:foreground "#f8f8f2" :background "#272822"))
             (t (:foreground "#adadad" :background "#222"))))

 `(grep-error-face ((t (:foreground "#f92672" :weight bold :underline t))))
 `(grep-hit-face ((t (:foreground "#fd5ff0"))))
 `(grep-match-face ((t (:foreground "#fd971f" :weight bold))))
 `(match ((t (:foreground "#a6e22e" :background "171a0b" :weight bold))))
 ;; only works for x
 `(linum ((t (:foreground "#ae81ff" :background "#202020"))))
 `(cursor ( (((class color) (min-colors ,THRESHOLD)) (:background "#e0e0e0"))) (t (:background "#eee")))
 `(fringe ( (((class color) (min-colors ,THRESHOLD)) (:background "#1a1a1a"))))
 `(hl-line ( (((class color) (min-colors ,THRESHOLD)) (:bold nil :background "#141411")) (t (:background "#0f0"))))
 `(menu ((((class color) (min-colors ,THRESHOLD)) (:bold t :foreground "#afafaf" :background "#777777"))))
 `(minibuffer-prompt ((((class color) (min-colors ,THRESHOLD)) (:bold nil :foreground "#1e90ff"))
                      (t (:foreground "#005f57" :background "#000"))))
 `(mode-line ((((class color) (min-colors ,THRESHOLD)) (:foreground "#f8f8f2" :background "#303030" :box (:line-width -1 :style released-button))) (t (:background "#000" :foreground "#024"))))
 `(region ((((class color) (min-colors ,THRESHOLD)) (:bold nil :background "#003b3b" :inverse-video nil)) (t (:background "#001"))))
 `(show-paren-match-face ((((class color) (min-colors ,THRESHOLD)) (:bold nil :background "#aaaaaa"))(t (:background "#999"))))
 ;; Main
 `(font-lock-builtin-face ((((class color) (min-colors ,THRESHOLD)) (:foreground "#aee22e"))(t (:foreground "#11ff00"))))
 `(font-lock-preprocessor-face ((((class color) (min-colors ,THRESHOLD)) (:foreground "#16c62a"))(t (:foreground "#2d3"))))
 `(font-lock-comment-face ((((class color) (min-colors ,THRESHOLD)) (:foreground "#2f70ff"))(t (:foreground "#ae81ff"))))
 `(font-lock-constant-face ((((class color) (min-colors ,THRESHOLD)) (:foreground "#ae81ff")) (t (:foreground "#11f"))))
 `(font-lock-doc-string-face ((((class color) (min-colors ,THRESHOLD)) (:foreground "#e6db74")) (t (:foreground "#e6db74"))))
 `(font-lock-function-name-face ((((class color) (min-colors ,THRESHOLD)) (:foreground "#a6e22e")) (t (:foreground "#67930f"))))
 `(font-lock-keyword-face ((((class color) (min-colors ,THRESHOLD)) (:foreground "#f92672")) (t (:foreground "#f92672"))))
 `(font-lock-string-face ((((class color) (min-colors ,THRESHOLD)) (:foreground "#f6eb00")) (t (:foreground "#e6db74"))))
 `(font-lock-type-face ((((class color) (min-colors ,THRESHOLD)) (:foreground "#89bdff")) (t (:foreground "#00d7ff"))))
 `(font-lock-variable-name-face ((((class color) (min-colors ,THRESHOLD)) (:foreground "#f92672")) (t (:foreground "#f92672"))))
 `(font-lock-warning-face ((((class color) (min-colors ,THRESHOLD)) (:bold t :foreground "#fd5ff1")) (t (:foreground "#afaf87"))))
 ;; CUA
 `(cua-rectangle ((((class color) (min-colors ,THRESHOLD)) (:background "#141411")) (t (:background "#101010"))))
 ;; IDO
 `(ido-first-match ((((class color) (min-colors ,THRESHOLD)) (:foreground "#ae81ff")) (t (:foreground "#ae81ff"))))
 `(ido-only-match ((((class color) (min-colors ,THRESHOLD)) (:foreground "#a6e22a")) (t (:foreground "#a6e22a"))))
 `(ido-subdir ((((class color) (min-colors ,THRESHOLD)) (:foreground "#89bdff")) (t (:foreground "#89bdff"))))
 ;; Whitespace
 `(whitespace-space ((((class color) (min-colors ,THRESHOLD)) (:foreground "#595959")) (t (:foreground "#595959"))))
 ;; Yasnippet
 `(yas-field-highlight-face ((((class color) (min-colors ,THRESHOLD)) (:background "#383830")) (t (:background "#838"))))
 ;; popup
 `(popup-face ((t (:background "cyan" :foreground "blue"))))
 `(popup-tip-face ((t (:background "#99cccc" :foreground "#cc1111"))))
 ;; woman
 `(woman-bold ((((class color) (min-colors ,THRESHOLD)) (:foreground "#ff2200" :bold t)) (t (:foreground "#ff2200"))))
 `(woman-italic ((((class color) (min-colors ,THRESHOLD)) (:foreground "#00ee00" :italic nil)) (t (:foreground "#00ee00"))))
 `(woman-unknwon-face ((((class color) (min-colors ,THRESHOLD)) (:foreground "#eeee00")) (t (:foreground "#eeee00"))))
 `(woman-addition-face ((((class color) (min-colors ,THRESHOLD)) (:foreground "#4169e1")) (t (:foreground "#4169e1"))))
 ;; man
 `(Man-overstrike ((((class color) (min-colors ,THRESHOLD)) (:foreground "#ff2200" :bold t)) (t (:foreground "#ff2200"))))
 `(Man-underline ((((class color) (min-colors ,THRESHOLD)) (:foreground "#00ee00" :italic nil)) (t (:foreground "#00ee00"))))
 `(Man-reverse ((((class color) (min-colors ,THRESHOLD)) (:foreground "#eeee00")) (t (:foreground "#eeee00"))))
 ;; which-func
 `(which-func((((class color) (min-colors ,THRESHOLD)) (:foreground "#e6db74")) (t (:foreground "#e6db74"))))
 )

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(custom-theme-set-variables
 'monokai-dark
 `(ansi-color-faces-vector [default bold shadow italic underline bold bold-italic bold])
 `(ansi-color-names-vector ["#2d3743" "#ff4242" "#74af68" "#dbdb95" "#34cae2" "#008b8b" "#00ede1" "#e1e1e0"])
 `(ansi-term-color-vector ["#3f3f3f" "#cc9393" "#7f9f7f" "#f0dfaf" "#8cd0d3" "#dc8cc3" "#93e0e3" "#dcdccc"] t)
 )

(provide-theme 'monokai-dark)
(provide 'monokai-dark-theme)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; monokai-dark-theme.el ends here
