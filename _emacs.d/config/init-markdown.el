(autoload 'markdown-mode "markdown-mode" "Mode for editing Markdown documents" t)
(setq auto-mode-alist (cons '("\\.\\(md\\|markdown\\|mdown\\)\\'"
                              . markdown-mode) auto-mode-alist))

(add-hook 'markdown-mode-hook 'turn-on-auto-fill)

(setq markdown-enable-math t)
(setq markdown-css-path (expand-file-name "~/.emacs.d/others/css/github.css"))
(setq markdown-command "redcarpet --parse-fenced_code_blocks --parse-autolink --parse-no_intra_emphasis --parse-tables --parse-with_toc_data --parse-strikethrough --parse-lax_spacing --parse-superscript --parse-pygments")

(provide 'init-markdown)
