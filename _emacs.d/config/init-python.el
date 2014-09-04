(setq auto-mode-alist
      (append '(("SConstruct\\'" . python-mode)
                ("SConscript\\'" . python-mode)
                ("\\.pyw\\'" . python-mode))
              auto-mode-alist))

(defun my-python-mode-elpy-setup ()
  (require 'elpy)
  (setq elpy-default-minor-modes '(eldoc-mode flymake-mode yas-minor-mode auto-complete-mode)
        python-shell-interpreter "python"
        elpy-rpc-python-command "python"
        elpy-rpc-backend "jedi")
  (if (fboundp 'evil-mode)
      (progn
        (evil-define-key 'normal python-mode-map "gd" 'elpy-goto-definition)
        (evil-define-key 'normal python-mode-map "\C-]" 'elpy-rgrep-symbol)
        )))

(eval-after-load "python"
  '(progn
     (my-python-mode-elpy-setup)
     (autoload 'doctest-mode "doctest-mode" "Python doctest editing mode." t)
     ))

(provide 'init-python)
