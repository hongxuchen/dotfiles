(mapc (lambda (mode)
        (add-hook 'LaTeX-mode-hook mode))
      (list 'auto-fill-mode
            'LaTeX-math-mode
            'turn-on-reftex
            'linum-mode))

(setq TeX-view-program-list
      '(("SumatraPDF" "SumatraPDF.exe %o")
        ("Gsview" "gsview32.exe %o")
        ("Okular" "okular --unique %o")
        ("Evince" "evince %o")
        ("Firefox" "firefox %o")))

(setq LaTeX-section-hook
      '(LaTeX-section-heading
        LaTeX-section-title
        LaTeX-section-toc
        LaTeX-section-section
        LaTeX-section-label))

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master "main")

(add-hook 'LaTeX-mode-hook
          (lambda ()
            (setq TeX-auto-untabify t     ; remove all tabs before saving
                  TeX-engine 'xetex       ; use xelatex default
                  TeX-show-compilation t) ; display compilation windows
            (TeX-global-PDF-mode t)       ; PDF mode enable, not plain
            (setq TeX-save-query nil)
            (tex-fold-mode 1)
            (imenu-add-menubar-index)
            (define-key LaTeX-mode-map (kbd "TAB") 'TeX-complete-symbol)))

;;; auto-complete
(require 'ac-math)
(add-to-list 'ac-modes 'latex-mode)   ; make auto-complete aware of `latex-mode`

(defun ac-LaTeX-mode-setup () ; add ac-sources to default ac-sources
  (setq ac-sources
        (append '(ac-source-math-unicode ac-source-math-latex ac-source-latex-commands)
                ac-sources))
  )
(add-hook 'LaTeX-mode-hook 'ac-LaTeX-mode-setup)
(global-auto-complete-mode t)

(setq ac-math-unicode-in-math-p t)

(require 'preview)

(provide 'init-tex)
