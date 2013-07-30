(autoload 'doctest-mode "doctest-mode" "Python doctest editing mode." t)

(setq auto-mode-alist
      (append '(("SConstruct\\'" . python-mode)
		("SConscript\\'" . python-mode))
              auto-mode-alist))

;; (evil-define-key 'normal python-mode-map "\C-t" 'elpy-goto-definition)
(evil-define-key 'normal python-mode-map "\C-]" 'elpy-goto-definition)
(eval-after-load 'python '(elpy-enable))
(setq elpy-rpc-python-command "python")
(setq elpy-rpc-backend "jedi")

(provide 'init-python-mode)
