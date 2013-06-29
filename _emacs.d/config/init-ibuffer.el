(defun ibuffer-set-up-preferred-filters ()
  (ibuffer-vc-set-filter-groups-by-vc-root)
  (unless (eq ibuffer-sorting-mode 'filename/process)
    (ibuffer-do-sort-by-filename/process)))

;; Modify the default ibuffer-formats
(setq ibuffer-formats
      '((mark modified read-only vc-status-mini " "
              (name 18 18 :left :elide)
              " "
              (size-h 9 -1 :right)
              " "
              (mode 16 16 :left :elide)
              " "
              (vc-status 16 16 :left)
              " "
              filename-and-process)))

(setq ibuffer-sorting-mode t)
(setq ibuffer-last-sorting-mode t)
(setq ibuffer-expert t)
(setq ibuffer-filter-group-name-face 'font-lock-doc-face)

(eval-after-load 'ibuffer
  '(progn
     (require 'ibuffer-vc)
     ;; Use human readable Size column instead of original one
     (define-ibuffer-column size-h
       (:name "Size" :inline t)
       (cond
        ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
        ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
        (t (format "%8d" (buffer-size))))))
  )
(add-hook 'ibuffer-hook 'ibuffer-set-up-preferred-filters)
;; keep buffer list up-to-date
(add-hook 'ibuffer-mode-hook
          '(lambda ()
             (ibuffer-auto-mode 1)
             (ibuffer-switch-to-saved-filter-groups "home")))

;; TODO workaround
(add-hook 'Buffer-menu-mode-hook 'buffer-disable-undo)

(provide 'init-ibuffer)
