;; (eval-after-load "ffap" '(require 'ffap-gcc-path))

(require 'ffap)

(defvar ffap-gcc-program "gcc"
  "The name of the gcc program for `ffap-gcc-path-setup'.")

(defun ffap-gcc-path-setup ()
  "Set `ffap-c-path' to the include path used by gcc.
The gcc program is taken from `ffap-gcc-program'. You can change
that and re-run ffap-gcc-path-setup' while cross-compiling or
using a particular gcc version.

See the home page for updates etc,
URL `http://user42.tuxfamily.org/ffap-gcc-path/index.html'"

  (with-temp-buffer
    (setq default-directory "/") ;; in case inherit non-existent
    (let ((ret (let ((process-environment (copy-sequence process-environment))
                     (process-connection-type nil)) ;; pipe
                 ;; The messages "search starts here" etc are probably
                 ;; translated, avoid that.
                 (setenv "LANGUAGES" nil)
                 (setenv "LANG" nil)
                 (setenv "LC_ALL" nil)
                 (setenv "LC_MESSAGES" nil)
                 (condition-case err
                     (call-process ffap-gcc-program
                                   nil ;; input
                                   t ;; output to current buffer
                                   nil ;; display
                                   "-v"
                                   "-E"
                                   "--language=c"
                                   (if (boundp 'null-device) ;; Emacs 20.3
                                       null-device
                                     "/dev/null"))
                   ;; `ret' gets message string on error
                   (error (error-message-string err))))))
      (if (not (equal 0 ret))
          ;; want to tolerate no gcc available at all, so just `message' here
          (message "Error running %s: %s" ffap-gcc-program ret)

        ;; note the regexp subexpr here avoids matching last \n because
        ;; emacs 22 made an incompatible change to split-string, a trailing
        ;; separator there results in an empty string at the end
        (goto-char (point-min))
        (or (re-search-forward "#include <\\.\\.\\.> search starts here:
\\(.*\\(\n.*\\)*\\)\nEnd of search list." nil t)
            (error "%s search path output unrecognised" ffap-gcc-program))

        (narrow-to-region (match-beginning 1) (match-end 1))
        (goto-char (point-min))
        (while (re-search-forward "^ +" nil t) ;; lose spaces at start of each
          (replace-match "" t t))
        (setq ffap-c-path (split-string (buffer-string) "\n"))))))

;; do the setup now
(ffap-gcc-path-setup)

(provide 'ffap-gcc-path)

;;; ffap-gcc-path.el ends here
