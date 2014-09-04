(defun my-tex-mode-ac-setup ()
  (require 'ac-math)
  (add-to-list 'ac-modes 'latex-mode)
  (setq ac-math-unicode-in-math-p t)
  (setq ac-sources
        (append
         '(ac-source-math-unicode ac-source-math-latex ac-source-latex-commands) ac-sources))
  )

(defun my-tex-mode-view-setup ()
  (require 'preview)
  (setq preview-auto-cache-preamble t)
  (define-key LaTeX-mode-map (kbd "C-c C-p") 'preview-buffer)
  (define-key LaTeX-mode-map (kbd "C-c p") 'preview-clearout-buffer)
  (setq TeX-view-program-selection '((output-pdf "Evince"))
        TeX-view-program-list
        '(("SumatraPDF" "SumatraPDF.exe %o")
          ("Gsview" "gsview32.exe %o")
          ("Evince" "evince --page-index=%(outpage) %o")
          ("Okular" "okular --unique %o#src:%n%b")
          ("Firefox" "firefox %o")))
  (setq TeX-fold-unfold-around-mark nil
        TeX-fold-auto t
        TeX-fold-type-list '(env macro math comment))
  )

(defun my-tex-mode-basic-setup ()
  (auto-fill-mode 1)
  (LaTeX-math-mode 1)
  (reftex-mode 1)
  (linum-mode 1)
  (TeX-fold-mode -1)
  (TeX-global-PDF-mode 1)
  (imenu-add-menubar-index)

  (setq-default TeX-engine 'pdflatex
                TeX-master nil)

  (setq reftex-plug-into-AUCTeX t
        TeX-byte-compile t
        TeX-math-close-double-dollar t
        TeX-save-query nil
        TeX-auto-save t
        TeX-parse-self t
        TeX-debug-bad-boxes t
        TeX-debug-warnings nil
        TeX-source-correlate-mode t
        TeX-source-correlate-start-server t)

  (setq LaTeX-command "latex -synctex=1")
  (setq LaTeX-section-hook
        '(LaTeX-section-heading
          LaTeX-section-title
          LaTeX-section-toc
          LaTeX-section-section
          LaTeX-section-label))
  (setq TeX-auto-untabify t
        TeX-show-compilation t)
  )

(provide 'init-tex)
