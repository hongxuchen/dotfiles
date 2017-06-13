;; ------------------------------------------------------------------------------
;; dired
;; ------------------------------------------------------------------------------
(setq auto-mode-alist (cons '("[^/]\\.dired$" . dired-virtual-mode)
                            auto-mode-alist))

(eval-after-load "dired-aux"
  '(progn
     (add-to-list 'dired-compress-file-suffixes
                  '("\\.zip\\'" ".zip" "unzip"))
     (add-to-list 'dired-compress-file-suffixes
                  '("\\.rar\\'" ".rar" "unrar"))
(setq openwith-associations '(("\\.pdf\\'" "open" (file))))
     (require 'openwith)
     (openwith-mode 1)
     ))

(require 'dired-x)
(require 'dired-k)
(setq-default dired-omit-files-p t)
(setq dired-recursive-copies t               ;copy recursively
      dired-recursive-deletes t              ;delete recursively
      dired-recursive-deletes 'always        ;delete confirm once
      dired-recursive-copies 'always         ;no confirm when copy
      dired-details-hidden-string "[ ... ] " ;chars denoting hidden info
      dired-listing-switches "-aluh"         ;args passed to `ls`
      directory-free-space-args "-Pkh"       ;dir free space arg
      dired-omit-size-limit nil              ;no omit size limit
      dired-omit-verbose nil
      dired-dwim-target t                    ;guess target dir
      dired-auto-revert-buffer t
      dired-no-confirm '(byte-compile chgrp chmod chown compress copy hardlink load move print shell symlink touch uncompress))

(setq dired-omit-files "^\\.$\\|^\\.\\.$\\|^GTAGS$\\|^Thumbs.db$\\|^Thumbs.db:encryptable$\\|^GSYMS$\\|^GRTAGS$\\|^GPATH$\\|\\.bc$|\\.o$\\|\\.out$\\|\\.aux$\\|\\.log$\\|\\.fdb_latexmk$\\|\\.synctex.gz$\\|\\.fls$\\|\\.cmx$\\|\\.cmxa$\\|\\.byte$\\|\\.native$\\|\\.cmo$\\|\\.cmi$\\|^\\._d$\\|^\\._ncdi$\\|^\\._bcdi$")

(define-key dired-mode-map "W" 'wdired-change-to-wdired-mode)

(defvar my-dired-sort-map (make-sparse-keymap))
(define-key dired-mode-map "s" my-dired-sort-map)
(define-key my-dired-sort-map "s" (lambda () "sort by Size" (interactive) (dired-sort-other (concat dired-listing-switches " -S"))))
(define-key my-dired-sort-map "x" (lambda () "sort by eXtension" (interactive) (dired-sort-other (concat dired-listing-switches " -X"))))
(define-key my-dired-sort-map "t" (lambda () "sort by Time" (interactive) (dired-sort-other (concat dired-listing-switches " -t"))))
(define-key my-dired-sort-map "n" (lambda () "sort by Name" (interactive) (dired-sort-other dired-listing-switches)))
(define-key my-dired-sort-map "?" (lambda () "sort help" (interactive) (message "s Size; x eXtension; t Time; n Name")))

(setq find-ls-option '("-ls" . "-dilsbh"))

;; (add-hook 'dired-mode-hook 'dired-k--highlight)
(define-key dired-mode-map (kbd "K") 'dired-k)

;; ------------------------------------------------------------------------------
;; session & recentf
;; ------------------------------------------------------------------------------
(setq session-use-package t
      session-save-file (expand-file-name "~/.emacs.d/.session"))
(add-hook 'after-init-hook 'session-initialize)

(recentf-mode 1)
(setq recentf-keep '(file-remote-p file-readable-p))
(setq recentf-max-saved-items 1000
      recentf-exclude '("/tmp/"
                        "/ssh:"
                        "/sudo:"
                        "/home/[a-z]\+/\\."
                        "\\.el.gz\\'"
                        "\\.out\\'"
                        ))

;; ------------------------------------------------------------------------------
;; ibuffer
;; ------------------------------------------------------------------------------

(defun ibuffer-set-up-preferred-filters ()
  (ibuffer-vc-set-filter-groups-by-vc-root)
  (unless (eq ibuffer-sorting-mode 'filename/process)
    (ibuffer-do-sort-by-filename/process)))

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

(setq ibuffer-sorting-mode t
      ibuffer-expert t
      ibuffer-filter-group-name-face 'font-lock-doc-face)

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

(provide 'init-files)
