(setq auto-mode-alist
      (append '(("SConstruct\\'" . python-mode)
                ("SConscript\\'" . python-mode)
                ("\\.pyw\\'" . python-mode))
              auto-mode-alist))

(defun my-python-mode-elpy-setup ()
  (require 'py-autopep8)
  (require 'elpy)
  (elpy-enable)
  (setq elpy-default-minor-modes '(eldoc-mode flymake-mode yas-minor-mode auto-complete-mode)
        python-shell-interpreter "python"
        elpy-rpc-python-command "python"
        elpy-rpc-backend "jedi")
  (add-hook 'before-save-hook 'py-autopep8-before-save)
  (if (fboundp 'evil-mode)
      (progn
        (evil-define-key 'normal python-mode-map "gd" 'elpy-goto-definition)
        (evil-define-key 'normal python-mode-map "\C-]" 'elpy-rgrep-symbol)
        )))


(setq elpy-modules '(elpy-module-sane-defaults
                          elpy-module-company
                          elpy-module-eldoc
                          elpy-module-highlight-indentation
                          elpy-module-pyvenv
                          elpy-module-yasnippet))

(eval-after-load "python"
  '(progn
     ;; (my-python-mode-elpy-setup)
     (autoload 'doctest-mode "doctest-mode" "Python doctest editing mode." t)
     ))

(provide 'init-python)
