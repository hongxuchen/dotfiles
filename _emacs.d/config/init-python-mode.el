(autoload 'doctest-mode "doctest-mode" "Python doctest editing mode." t)

(setq auto-mode-alist
      (append '(("SConstruct\\'" . python-mode)
                ("SConscript\\'" . python-mode))
              auto-mode-alist))

(evil-define-key 'normal python-mode-map "gd" 'elpy-goto-definition)
(evil-define-key 'normal python-mode-map "\C-]" 'elpy-rgrep-symbol)

(eval-after-load 'python '(elpy-enable))
(setq python-shell-interpreter "python3")
(setq elpy-rpc-python-command "python3")
(setq elpy-rpc-backend "jedi")

(provide 'init-python-mode)
