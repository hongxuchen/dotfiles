(mapc (lambda (mode)
        (add-hook 'LaTeX-mode-hook mode))
      (list 'auto-fill-mode
            'LaTeX-math-mode
            'turn-on-reftex
            'linum-mode))

(setq TeX-view-program-selection '((output-pdf "Evince")))
(setq LaTeX-command "latex -synctex=1")

(setq TeX-view-program-list
      '(("SumatraPDF" "SumatraPDF.exe %o")
        ("Gsview" "gsview32.exe %o")
        ("Evince" "evince --page-index=%(outpage) %o")
        ("Okular" "okular --unique %o#src:%n%b")
        ("Firefox" "firefox %o")))

(setq-default TeX-engine 'pdflatex)

(setq LaTeX-section-hook
      '(LaTeX-section-heading
        LaTeX-section-title
        LaTeX-section-toc
        LaTeX-section-section
        LaTeX-section-label))
(define-key minibuffer-local-map [escape] 'keyboard-quit)

(setq reftex-plug-into-AUCTeX t)
(setq TeX-byte-compile t)
(setq TeX-math-close-double-dollar t)
(setq TeX-fold-type-list '(env macro math comment))

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

(add-hook 'LaTeX-mode-hook
          (lambda ()
            ;; auto-complete
            (require 'ac-math)
            (add-to-list 'ac-modes 'latex-mode)
            (setq ac-math-unicode-in-math-p t)
            (setq ac-sources
                  (append
                   '(ac-source-math-unicode ac-source-math-latex ac-source-latex-commands)
                   ac-sources))
            ;; preview
            (require 'preview)
            (setq preview-auto-cache-preamble t)

            (setq TeX-auto-untabify t     ; remove all tabs before saving
                  TeX-engine 'default    ; use pdflatex default
                  TeX-show-compilation t) ; display compilation windows
            (TeX-global-PDF-mode t)       ; PDF mode enable, not plain
            ;; (tex-fold-mode -1)
            (define-key LaTeX-mode-map (kbd "C-c C-p") 'preview-buffer)
            (define-key LaTeX-mode-map (kbd "C-c p") 'preview-clearout-buffer)
            (imenu-add-menubar-index)
            (define-key LaTeX-mode-map (kbd "TAB") 'TeX-complete-symbol)))

(setq TeX-debug-bad-boxes t
      TeX-debug-warnings nil)

(setq TeX-source-correlate-mode t
      TeX-source-correlate-start-server t)

(setq TeX-fold-unfold-around-mark nil)
(setq TeX-fold-auto t)
(setq TeX-save-query nil)

(provide 'init-tex)
