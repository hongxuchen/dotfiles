(require 'escreen)
(escreen-install)

(defun find-escreen-data-by-number (number)
  (car (delq nil
             (mapcar (lambda (x) (and (= (car x) number) x))
                     escreen-configuration-alist))))

(defun escreen-buffer-name (number)
  (let* ((screen-data (find-escreen-data-by-number number))
         (data-map (escreen-configuration-data-map screen-data)))
    (escreen-configuration-data-map-critical-buffer-name
     (escreen-configuration-data-map-critical (car data-map)))))

(defun escreen-display-screens ()
  "what the name says"
  (interactive)
  (let ((escreens (escreen-get-active-screen-numbers))
        (screen-msg ""))

    (dolist (s escreens)
      (setq screen-msg
            (concat screen-msg
                    (let ((display-str (concat (number-to-string s)
                                               ":"
                                               (escreen-buffer-name s))))
                      (if (= escreen-current-screen-number s)
                          (propertize display-str 'face 'font-lock-warning-face)
                        display-str))
                    " ")))
    (message "%s" screen-msg)))

(add-hook 'escreen-goto-screen-hook 'escreen-display-screens)

(evil-ex-define-cmd "tabn" 'escreen-goto-next-screen)
(evil-ex-define-cmd "tabp" 'escreen-goto-prev-screen)
(evil-ex-define-cmd "tabl" 'escreen-goto-last-screen)
(evil-ex-define-cmd "tabc" 'escreen-kill-screen)
(evil-ex-define-cmd "tabs" 'escreen-display-screens)

(provide 'init-escreen)
