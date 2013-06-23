(require 'compile)

;;;###autoload
(defgroup cppcheck nil
  "*cppcheck"
  :group 'cppcheck)

;;;###autoload
(defcustom cppcheck-executable "cppcheck"
  "Path to the cppcheck executable"
  :type '(file :must-match t)
  :group 'cppcheck)

;;;###autoload
(defcustom cppcheck-template "{file}:{line}:({severity}): {message}"
  "Format the error messages. E.g.
'{file}:{line},{severity},{id},{message}' or
'{file}({line}):({severity}) {message}'"
  :type 'string
  :group 'cppcheck)

;;;###autoload
(defcustom cppcheck-platform nil
  "Specifies platform specific types and sizes. The
available platforms are:
* unix32
32 bit unix variant
* unix64
64 bit unix variant
* win32A
32 bit Windows ASCII character encoding
* win32W
32 bit Windows UNICODE character encoding
* win64
64 bit Windows"
  :type 'string
  :group 'cppcheck)

;;;###autoload
(defcustom cppcheck-std-list
  nil
  "*Alist of standards to test.
The available options are:
* posix
POSIX compatible code
* c89
C code is C89 compatible
* c99
C code is C99 compatible
* c11
C code is C11 compatible (default)
* c++03
C++ code is C++03 compatible
* c++11
C++ code is C++11 compatible (default)"
  :group 'list
  :group 'cppcheck)

;;;###autoload
(defcustom cppcheck-enable-list
  all
  "Enable additional checks. The available ids are:
* all
Enable all checks
* style
Enable all coding style checks. All messages with the severities 'style', 'performance' and 'portability' are enabled.
* performance
Enable performance messages
* portability
Enable portability messages
* information
Enable information messages
* unusedFunction
Check for unused functions
* missingInclude
Warn if there are missing includes."
  :group 'list
  :group 'cppcheck)

;;;###autoload
(defcustom cppcheck-relative-paths-list
  nil
  "Use relative paths in output. When given, <paths> are
used as base. We use string comparison to create relative
paths, so using e.g. ~ for home folder does not work. It
is currently only possible to apply the base paths to
files that are on a lower level in the directory tree."
  :group 'list
  :group 'cppcheck)

;;;###autoload
(defcustom cppcheck-include-path-list
  nil
  "*Alist of directories to search for include files."
  :group 'list
  :group 'cppcheck)

;;;###autoload
(defcustom cppcheck-hide-configuration-list
  nil
  "*Alist of configurations to hide '#ifdef <ID>'"
  :group 'list
  :group 'cppcheck)

;;;###autoload
(defcustom cppcheck-use-configuration-list
  nil
  "*Alist of configurations to use '#ifdef <ID>'"
  :group 'list
  :group 'cppcheck)

(defun cppcheck-args-get-include-dirs ()
  "Returns the options for the '-I' flag"
  (if (and (listp cppcheck-include-path-list) (> (length cppcheck-include-path-list) 0))
      (let (result)
        (dolist (element cppcheck-include-path-list)
          (setq result (if (not (stringp result))
                           (format " -I %s" element)
                         (format "%s -I %s" result element)))
          ) result) ""))

(defun cppcheck-args-get-hidden-configurations ()
  "Returns the options for the '-U' flag"
  (if (and (listp cppcheck-hide-configuration-list) (> (length cppcheck-hide-configuration-list) 0))
      (let (result)
        (dolist (element cppcheck-hide-configuration-list)
          (setq result (if (not (stringp result))
                           (format " -U%s" (shell-quote-argument element))
                         (format "%s -U%s" result (shell-quote-argument element))))
          ) result) ""))

(defun cppcheck-args-get-use-configurations ()
  "Returns the options for the '-U' flag"
  (if (and (listp cppcheck-use-configuration-list) (> (length cppcheck-use-configuration-list) 0))
      (let (result)
        (dolist (element cppcheck-use-configuration-list)
          (setq result (if (not (stringp result))
                           (format " -D%s" (shell-quote-argument element))
                         (format "%s -D%s" result (shell-quote-argument element))))
          ) result) ""))

(defun cppcheck-args-get-enable ()
  "Returns the options for the '--enable=' flag"
  (if (and (listp cppcheck-enable-list) (> (length cppcheck-enable-list) 0))
      (let (result)
        (dolist (element cppcheck-enable-list)
          (setq result (if (not (stringp result))
                           (format " --enable=%s" element)
                         (format "%s,%s" result element)))
          ) result) ""))

(defun cppcheck-args-get-relative-paths ()
  "Returns the options for the '--relative-paths' flag"
  (if (and (listp cppcheck-relative-paths-list) (> (length cppcheck-relative-paths-list) 0))
      (let (result)
        (dolist (element cppcheck-relative-paths-list)
          (setq result (if (not (stringp result))
                           (format " --relative-paths=%s" element)
                         (format "%s;%s" result element)))
          ) result) ""))

(defun cppcheck-args-get-std ()
  "Returns the options for the '--std' flag"
  (if (and (listp cppcheck-std-list) (> (length cppcheck-std-list) 0))
      (let (result)
        (dolist (element cppcheck-std-list)
          (setq result (if (not (stringp result))
                           (format " --std=%s" element)
                         (format "%s --std=%s" result element)))
          ) result) ""))

(defun cppcheck-get-commandline-args (filename &optional additional-args)
  "Returns a string containing the a string for running cppcheck based on the various options."
  (format "%s%s%s%s%s%s%s%s%s%s %s"
          (shell-quote-argument cppcheck-executable)
          (cppcheck-args-get-std)
          (cppcheck-args-get-enable)
          (cppcheck-args-get-relative-paths)
          (cppcheck-args-get-include-dirs)
          (cppcheck-args-get-hidden-configurations)
          (cppcheck-args-get-use-configurations)
          (if (stringp cppcheck-platform) (concat " --platform=" cppcheck-platform) "")
          (if (stringp cppcheck-template) (concat " --template='" cppcheck-template "'") "" )
          (if (stringp additional-args) (concat " " additional-args) "")
          filename ))

;;;###autoload
(define-compilation-mode cppcheck-results-mode "cppcheck"
  "Sets `cppcheck-last-buffer' and `compilation-window-height'."
  (setq cppcheck-last-buffer (current-buffer))
  (set (make-local-variable 'compilation-directory-matcher) '("\\`a\\`"))
  (set (make-local-variable 'compilation-disable-input) t) )

;;;###autoload
(defun cppcheck-file (filename &optional additional-args)
  "Runs cppcheck on the specified file. Additional arguments can be used with the ADDITIONAL-ARGS paramater"
  (unless (file-exists-p filename) (error (format "cppcheck: File does not exist: %s" filename)))
  (unless (file-readable-p filename) (error (format "cppcheck: File is not readable: %s" filename)))
  (compilation-start
   (cppcheck-get-commandline-args filename additional-args) 'cppcheck-results-mode) )

;;;###autoload
(defun cppcheck-current-file ()
  "Runs cppcheck on the current file."
  (interactive)
  (cppcheck-file (buffer-file-name)) )

;;;###autoload
(defun cppcheck-current-file-check-config ()
  "Runs cppcheck with the '--check-config' option"
  (interactive)
  (cppcheck-file (buffer-file-name) "--check-config") )
