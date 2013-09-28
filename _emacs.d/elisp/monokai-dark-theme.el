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

(defvar monokai-colors
  '((((class color) (min-colors 65535))
     (monokai-bg-1      . "#171A0B")
     (monokai-bg        . "#272822")
     (monokai-bg+1      . "#3E3D31")
     (monokai-bg+2      . "#49483E")
     (monokai-red-1     . "#A20C41")
     (monokai-red       . "#F92672")
     (monokai-red+1     . "#FC5C94")
     (monokai-red+2     . "#FC87B0")
     (monokai-green-1   . "#67930F")
     (monokai-green     . "#A6E22E")
     (monokai-green+1   . "#C1F161")
     (monokai-green+2   . "#CDF187")
     (monokai-orange-1  . "#A45E0A")
     (monokai-orange    . "#FD971F")
     (monokai-orange+1  . "#FEB257")
     (monokai-orange+2  . "#FEC683")
     (monokai-yellow-1  . "#968B26")
     (monokai-yellow    . "#E6DB74")
     (monokai-yellow+1  . "#F3EA98")
     (monokai-yellow+2  . "#F3ECB0")
     (monokai-blue-1    . "#21889B")
     (monokai-blue      . "#66D9EF")
     (monokai-blue+1    . "#8DE6F7")
     (monokai-blue+2    . "#A9EBF7")
     (monokai-purple-1  . "#562AA6")
     (monokai-purple    . "#AE81FF")
     (monokai-purple+1  . "#C2A1FF")
     (monokai-purple+2  . "#D2BAFF")
     (monokai-magenta-1 . "#A41F99")
     (monokai-magenta   . "#FD5FF0")
     (monokai-magenta+1 . "#FE87F4")
     (monokai-magenta+2 . "#FEA7F7")
     (monokai-cyan-1    . "#349B8D")
     (monokai-cyan      . "#A1EFE4")
     (monokai-cyan+1    . "#BBF7EF")
     (monokai-cyan+2    . "#CBF7F1")
     (monokai-fg-1      . "#75715E")
     (monokai-fg        . "#F8F8F2")
     (monokai-fg+1      . "#F8F8F0"))
    (t
     (monokai-bg-1      . "#141414")
     (monokai-bg        . "#1B1E1C")
     (monokai-bg+1      . "#212121")
     (monokai-bg+2      . "#303030")
     (monokai-red-1     . "#5F0000")
     (monokai-red       . "#FF1493")
     (monokai-red+1     . "#EE6AA7")
     (monokai-red+2     . "#FF82AB")
     (monokai-green-1   . "#6B8E23")
     (monokai-green     . "#87D700")
     (monokai-green+1   . "#B3EE3A")
     (monokai-green+2   . "#CCFF99")
     (monokai-orange-1  . "#A0522D")
     (monokai-orange    . "#FF8C00")
     (monokai-orange+1  . "#FFA54F")
     (monokai-orange+2  . "#FFD39B")
     (monokai-yellow-1  . "#968B26")
     (monokai-yellow    . "#FFEC8B")
     (monokai-yellow+1  . "#F3EA98")
     (monokai-yellow+2  . "#F3ECB0")
     (monokai-blue-1    . "#21889B")
     (monokai-blue      . "#00d7ff")
     (monokai-blue+1    . "#8DE6F7")
     (monokai-blue+2    . "#A9EBF7")
     (monokai-purple-1  . "#562AA6")
     (monokai-purple    . "#AE81FF")
     (monokai-purple+1  . "#C2A1FF")
     (monokai-purple+2  . "#D2BAFF")
     (monokai-magenta-1 . "#A41F99")
     (monokai-magenta   . "#D700D7")
     (monokai-magenta+1 . "#FE87F4")
     (monokai-magenta+2 . "#FEA7F7")
     (monokai-cyan-1    . "#349B8D")
     (monokai-cyan      . "#5FFFFF")
     (monokai-cyan+1    . "#AFEEEE")
     (monokai-cyan+2    . "#CBF7F1")
     (monokai-fg-1      . "#8b8970")
     (monokai-fg        . "#F5F5F5")
     (monokai-fg+1      . "#FFFAFA")))
  "List of Monokai colors.
Each element has the form (NAME . HEX).

`+N' suffixes indicate a color is lighter.
`-N' suffixes indicate a color is darker.")

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
 `(linum ((((class color) (min-colors ,THRESHOLD)) (:foreground "#ae81ff" :background "#202020")) (t (:foreground "#cd00cd"))))
 `(cursor ( (((class color) (min-colors ,THRESHOLD)) (:background "#e0e0e0"))) (t (:background "#eee")))
 `(fringe ( (((class color) (min-colors ,THRESHOLD)) (:background "#1a1a1a"))))
 `(hl-line ( (((class color) (min-colors ,THRESHOLD)) (:bold nil :background "#141411")) (t (:background "#0f0"))))
 `(menu ((((class color) (min-colors ,THRESHOLD)) (:bold t :foreground "#afafaf" :background "#777777"))))
 `(minibuffer-prompt ((((class color) (min-colors ,THRESHOLD)) (:bold nil :foreground "#1e90ff"))
                      (t (:foreground "#005f57" :background "#000"))))
 `(mode-line ((((class color) (min-colors ,THRESHOLD)) (:foreground "#f8f8f2" :background "#303030" :box (:line-width -1 :style released-button))) (t (:background "#000" :foreground "#024"))))
 `(region ((((class color) (min-colors ,THRESHOLD)) (:bold nil :background "#003b3b" :inverse-video nil)) (t (:background "#004b4b" :foreground "#1e1eee" :inverse-video t))))
 `(show-paren-match-face ((((class color) (min-colors ,THRESHOLD)) (:bold nil :background "#aaaaaa"))(t (:background "#999"))))
 ;; Main
 `(font-lock-builtin-face ((((class color) (min-colors ,THRESHOLD)) (:foreground "#aee22e"))(t (:foreground "#00cdcd"))))
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
 `(popup-face ((t (:background "#00cdcd" :foreground "#0000ee"))))
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
 `(which-func ((((class color) (min-colors ,THRESHOLD)) (:foreground "#e6db74")) (t (:foreground "#e6db74"))))
 ;; flymake
 `(flymake-errline ((((class color) (min-colors ,THRESHOLD)) (:underline (:style line) :inherit error)) (t (:foreground "#A20C41" :weight bold :underline t))))
 `(flymake-warnline ((((class color) (min-colors ,THRESHOLD)) (:foreground "#FD971F" :underline (:style line) :inherit warning)) (t (:foreground "#00cdcd" :weight bold :underline t))))
 `(flycheck-error ((((class color) (min-colors ,THRESHOLD)) (:underline (:style line) :inherit error)) (t (:foreground "#A20C41" :weight bold :underline t))))
 `(flycheck-warning ((((class color) (min-colors ,THRESHOLD)) (:foreground "#FD971F" :underline (:style line) :inherit warning)) (t (:foreground "#00cdcd" :weight bold :underline t))))
 `(irony-flycheck-error ((((class color) (min-colors ,THRESHOLD)) (:underline (:style line) :inherit error)) (t (:foreground "#A20C41" :weight bold :underline t))))
 `(irony-flycheck-warning ((((class color) (min-colors ,THRESHOLD)) (:foreground "#FD971F" :underline (:style line) :inherit warning)) (t (:foreground "#00cdcd" :weight bold :underline t))))
 ;; ac-irony
 `(ac-irony-candidate-face ((((class color) (min-colors ,THRESHOLD)) (:background "lightgray" :foreground "navy")) (t :background "#e9ffff" :foreground "#212121")))
 `(ac-irony-selection-face ((((class color) (min-colors ,THRESHOLD)) (:background "navy" :foreground "white")) (t :background "blue" :foreground "white")))
 ;; auto-complete
 `(ac-candidate-face ((((class color) (min-colors ,THRESHOLD)) (:background "lightgray" :foreground "navy")) (t :background "#00cdcd" :foreground "#ffffff")))
 `(ac-selection-face ((((class color) (min-colors ,THRESHOLD)) (:background "navy" :foreground "white")) (t :background "blue" :foreground "white")))
 ;; `(ac-complete-face ((((class color) (min-colors ,THRESHOLD)) (:background "navy" :foreground "white")) (t :background "#00cdcd" :foreground "#111111")))
 ;;
 `(eldoc-highlight-function-argument ((((class color) (min-colors ,THRESHOLD)) (:inherit bold)) (t :foreground "green")))
 )

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(custom-theme-set-variables
 'monokai-dark
 `(ansi-color-faces-vector [default bold shadow italic underline bold bold-italic bold])
 ;; `(ansi-color-names-vector ["#2d3743" "#ff4242" "#74af68" "#dbdb95" "#34cae2" "#008b8b" "#00ede1" "#e1e1e0"])
 '(ansi-color-names-vector ["black" "#d55e00" "#009e73" "#f8ec59" "#0072b2" "#cc79a7" "#56b4e9" "white"])
 ;; `(ansi-term-color-vector ["#3f3f3f" "#cc9393" "#7f9f7f" "#f0dfaf" "#8cd0d3" "#dc8cc3" "#93e0e3" "#dcdccc"])
 )

(provide-theme 'monokai-dark)
(provide 'monokai-dark-theme)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; monokai-dark-theme.el ends here
