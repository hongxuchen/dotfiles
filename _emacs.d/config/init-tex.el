(defun my-tex-mode-view-setup ()
  (require 'preview)
  (setq preview-auto-cache-preamble t)
  (define-key LaTeX-mode-map (kbd "C-c C-p") 'preview-buffer)
  (define-key LaTeX-mode-map (kbd "C-c p") 'preview-clearout-buffer)
  (setq TeX-view-program-selection '((output-pdf "Open"))
        TeX-view-program-list
        '(("Evince" "atril --page-index=%(outpage) %o")
          ("Open" "Open %o")
          ))
  (setq TeX-fold-unfold-around-mark nil
        TeX-fold-auto t
        TeX-fold-type-list '(env macro math comment))
  )

(defun my-tex-mode-basic-setup ()

  (setq-default TeX-engine 'default
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

  (reftex-mode 1)
  (auto-fill-mode 1)
  (LaTeX-math-mode 1)
  (linum-mode 1)
  (TeX-fold-mode -1)
  (TeX-global-PDF-mode 1)
  ;; (imenu-add-menubar-index)

  (auctex-latexmk-setup)

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

(add-hook 'LaTex-mode-hook
          (progn
            (my-tex-mode-view-setup)
            (my-tex-mode-basic-setup)
            ))

(provide 'init-tex)
