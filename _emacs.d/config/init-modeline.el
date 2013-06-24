;; which-func
(progn
  (require 'which-func)
  (which-function-mode t)
  (setq which-func-modes '(c-mode c++-mode python-mode makefile-mode sh-mode org-mode)))

;; display time
(setq display-time-24hr-format nil
      display-time-day-and-date t)

(setq global-mode-string '((vc-mode vc-mode))) ;; to make it updated

(setq minor-mode-alist
      '((dired-omit-mode
         (:eval
          (if
              (eq major-mode 'dired-mode)
              " Omit" "")))
        (vc-parent-buffer vc-parent-buffer-name)
        (orgtbl-mode " OrgTbl")
        (org-table-follow-field-mode " TblFollow")
        (org-indent-mode " Ind")
        (view-mode " View")
        (doxymacs-mode " doxy")
        (flyspell-mode flyspell-mode-line-string)
        (ispell-minor-mode " Spell")
        (flymake-mode " FlyMake")
        (irony-mode " Irony")
        (gnus-mailing-list-mode " Mailing-List")
        (gnus-agent-summary-mode
         #(" Plugged" 0 8
           (mouse-face mode-line-highlight local-map
                       (keymap
                        (mode-line keymap
                                   (mouse-2 . gnus-agent-toggle-plugged))))))
        (gnus-agent-group-mode
         #(" Plugged" 0 8
           (mouse-face mode-line-highlight local-map
                       (keymap
                        (mode-line keymap
                                   (mouse-2 . gnus-agent-toggle-plugged))))))
        (org-capture-mode " Rem")
        (gnus-dead-summary-mode " Dead")
        (org-cdlatex-mode " OCDL")
        (orgstruct-mode " OrgStruct")
        (outline-minor-mode " Outl")
        (mml-mode " MML")
        (eldoc-mode eldoc-minor-mode-string)
        (paredit-mode " Par")
        (checkdoc-minor-mode checkdoc-minor-mode-string)
        (hs-minor-mode " hs")
        (rainbow-delimiters-mode "")
        (fic-mode " FIC")
        (server-buffer-clients " Server")
        (auto-complete-mode " AC")
        (yas-minor-mode " yas")
        (autopair-mode " pair")
        (global-auto-revert-mode global-auto-revert-mode-text)
        (auto-revert-tail-mode auto-revert-tail-mode-text)
        (auto-revert-mode auto-revert-mode-text)
        (next-error-follow-minor-mode " Fol")
        (overwrite-mode overwrite-mode)
        (auto-fill-function " Fill")
        ))

;; use setq-default to set it for all modes
(setq-default mode-line-format
              (list

               '(:eval (propertize "%b " 'face 'font-lock-keyword-face
                                   'help-echo (buffer-file-name)))
               ;; column, row
               "(" (propertize "%02l" 'face 'font-lock-type-face) ","
               (propertize "%02c" 'face 'font-lock-type-face) ") "

               ;; percent, size
               "[" (propertize "%p" 'face 'font-lock-constant-face)
               "/" (propertize "%I" 'face 'font-lock-constant-face) "] "

               "[" '(:eval (propertize "%m" 'face 'font-lock-string-face
                                       'help-echo buffer-file-coding-system)) "] "


                                       '(:eval (when evil-mode
                                                 (propertize evil-mode-line-tag
                                                             'face 'font-lock-type-face
                                                             'help-echo "Evil Mode")))

                                       "["
                                       '(:eval (propertize (if overwrite-mode "Ovr" "Ins")
                                                           'face 'font-lock-preprocessor-face
                                                           'help-echo (concat "Buffer "
                                                                              (if overwrite-mode "overwrite" "insert"))))

                                       '(:eval (when (buffer-modified-p)
                                                 (concat ","  (propertize "Mod"
                                                                          'face 'font-lock-warning-face
                                                                          'help-echo "Buffer Modified"))))


                                       '(:eval (when buffer-read-only
                                                 (concat ","  (propertize "RO"
                                                                          'face 'font-lock-type-face
                                                                          'help-echo "Buffer is read-only"))))
                                       "] "

                                       mode-line-misc-info

                                       '(:eval (propertize (format-time-string "%D %R")
                                                           'face 'font-lock-type-face
                                                           'help-echo
                                                           (concat (format-time-string "%c; ")
                                                                   (emacs-uptime "Uptime:%hh"))))


                                       " {"
                                       minor-mode-alist  ;; list of minor modes
                                       " } %-" ;; fill with '-'
                                       ))

(provide 'init-modeline)
