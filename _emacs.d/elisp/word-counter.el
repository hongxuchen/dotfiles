;; word-counter.el: --- Count words in the buffer and in region

;; Time-stamp: "2002-04-18 13:52:05 drs"
;; Version: 0.2
;; $Revision:
;; $Id:
;; $Source:

;; Author: Dr. Rafael Sepúlveda. <drs@gnulinux.org.mx>
;; Mantainer: Dr. Rafael Sepúlveda. <drs@gnulinux.org.mx>

;; Copyright (C) 2001-2002, Dr. Rafael Sepúlveda <drs@gnulinux.org.mx>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software Foundation,
;; Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307, USA.

;;; Commentary:

;; This file has all the functions that create a word counter. It has an
;; interactive word-counter and can display in the modeline the number of
;; words in the buffer and in the current region, plus more details.

;;; Usage:

;; To use the word counter, several steps have to be done. The
;; `word-counter.el' has to be in the load-path; let's say you'll put it in
;; `~/etc/emacs/elisp/word-counter.el', so your load path should be (suposing
;; that this is your only change to load-path):
;; (setq load-path (cons "~/etc/emacs/elisp" load-path))
;;
;; and also you can add this snippet to your `.emacs' to load and configure the
;; word-counter:
;; -----
;; (require 'word-counter) ;load the word counter
;;
;; ;; run word counter after each command (including key presses), the
;; ;; word counter code, will know when exactly to run and don't overload
;; ;; (too much) the computer.
;; (add-hook 'post-command-hook 'word-count)
;;
;; ;; set limit count to 10,000 words in every major mode; set limit to
;; ;; 50,000 words in text-mode.
;; (setq wc-max-words-alist
;; '((text-mode . 50000)
;; (t . 10000)))
;;
;; ;; toggle on the word-counter on every mode, except for `eshell'.
;; (setq wc-toggle-mode-alist
;; '((eshell-mode . nil)
;; (t . t)))
;; -----
;;
;; Finally you have to add a variable named `word-counter' to the mode-line
;; so it can be displayed there. If you use the default mode-line, use this
;; in your `.emacs':
;; -----
;; (setq default-mode-line-format
;; (list "-"
;; 'mode-line-mule-info
;; 'mode-line-modified
;; 'mode-line-frame-identification
;; 'mode-line-buffer-identification
;; " "
;; 'global-mode-string
;; " %[("
;; 'mode-name
;; 'mode-line-process
;; 'minor-mode-alist
;; "%n"
;; ")%]--"
;; '(which-func-mode ("" which-func-format "--"))
;; '(line-number-mode "L%l--")
;; '(column-number-mode "C%c--")
;; 'word-counter ;this is the only change.
;; '(-3 . "%p")
;; "-%-"))
;; (setq mode-line-format default-mode-line-format)
;; -----

;;; TODO:

;; - Valorate if a fix-point-goal-counter will be needed. (not point-min,
;; and not in a region; mainly to exclude a header or footer of
;; the buffer).
;; - Check why the hooks in html-mode (psgml) stop updating of the
;; mode-line counter.
;; - Check why the buffer doesn't make an update if called from a command
;; in Eshell until a key is pressed (Maybe Eshell bug).
;; - See if using a function eval instead of mayor modes in
;; `wc-max-words-alist', `wc-regexp-alist', `wc-toggle-mode-alist'
;; will be useful.

;;; Code:

;; User-definable Variables

(defvar wc-max-words-alist
  '((t . 10000))
  "Define a limit for Word counter depending on the major mode.
This limit is used so that computation time takes not too long.
The bigger the buffer is, the longer the word count will take.

The alist is formed like this:

(MAJOR-MODE . MAX-WORDS)

MAJOR-MODE: The name of the selected major mode.
MAX-WORDS: Integer that defines the maximum number of words before
word count stop the count.

If word count is bigger than MAX-WORDS, the count will stop. This
limit is only for a non-interactive call. Interactive calls count all
words without any overflow limit.")


(defvar wc-max-words-local nil
  "Define a limit for word counter in the local buffer.
If word count is bigger than INTEGER, stop the count. Prevent overflow.
This limit is only for a non-interactive call. Interactive calls count
all words without any overflow limit.

If this variable is set, `wc-max-words-alist' will be ignored on that
buffer.")
(make-variable-buffer-local 'wc-max-words-local)


(defvar wc-reached-goal-function
  (function
   (lambda ()
     (beep)))
   "Variable that runs this SEXP when goal-words limit is reached.
If nil, only the mark '!' in the minibuffer will be shown when the
limit is reached or passed.

To set the goal words (buffer-local) use `wc-set-goal-words' interactive
function.")


(defvar wc-regexp-alist
  '((t . "\\w+\\W*"))
  "Regexp that match words on a given major mode.
This is an alist so it can define a different regexp for any given
major mode, if a different criteria for 'word' is needed.")


(defvar wc-regexp-local nil
  "*Regexp that match words in the local buffer.
This buffer-local variable can hold a regexp definition different for
that buffer without considering the major mode used.

If this variable is set, `wc-regexp-alist' will be ignored on that
buffer.")
(make-variable-buffer-local 'wc-regexp-local)


(defvar wc-toggle-mode-alist
  '((t . t))
"List major modes that will activate or deactivate the word counter.
The alist is formed like this:

(MAJOR-MODE . t/nil)

MAJOR-MODE: the name of the selected mayor mode.
t/nil: 't' will activate the counter on that mode; 'nil' will deactivate
the counter on that mode.

If the last line is '(t . t)' it means that the counter will activate
on every not-listed mode. If '(t . nil)' the counter will deactivate
the counter on every not-listed mode.")


;;; No user costumizations beyond this point.


;; Control Variables

(defvar last-buffer-modified-tick -1
  "Check if buffer needs a 'refresh' of the word count.
Used to prevent the word count on every keystroke and to force an update
in the modeline (usually after a region word count).

The variable's value '-1' is only for convention; uses an integer as the
default value type; uses a negative number to avoid any conflict with a
possible value and to avoid any conflict using 'nil'.")
(make-variable-buffer-local 'last-buffer-modified-tick)

(defvar word-counter nil
  "Show in each buffer it's own word count.")
(make-variable-buffer-local 'word-counter)

(defvar wc-goal-words nil
  "Maximum number of words in the buffer before an alert is made.
This variable is set by the `wc-set-goal-words' function.")
(make-variable-buffer-local 'wc-goal-words)

(defvar wc-words-in-buffer-flag nil
  "Non-nil means display current buffer word count.")
(make-variable-buffer-local 'wc-words-in-buffer-flag)

(defvar wc-words-in-region-flag nil
  "Non-nil means display current region word count.")
(make-variable-buffer-local 'wc-words-in-region-flag)

(defvar wc-time-control 0
  "Don't run the word counter less than a second from the last count.
This way Emacs doesn't have to evaluate the word counter too often;
like when you keep pressed a key.")
(make-variable-buffer-local 'word-time-control)


;; Functions

;;; Main function.
(defun word-count (&optional force)
  "Count the number of words in a buffer and region.
Count words in buffer if region is not set; if region is set, then
count the words in both the region and the buffer.

If FORCE is non-nil then force the word count overriding
`wc-time-control' variable (only used in non-interactive calls)."
  (interactive)
  (let ((overflow ;set the overflow limit
(or
wc-max-words-local
(cond ((assq major-mode wc-max-words-alist)
(cdr (assq major-mode wc-max-words-alist)))
((assq t wc-max-words-alist)
(cdr (assq t wc-max-words-alist))))));)
(regexp ;set the regexp
(or wc-regexp-local
(cond ((assq major-mode wc-regexp-alist)
(cdr (assq major-mode wc-regexp-alist)))
((assq t wc-regexp-alist)
(cdr (assq t wc-regexp-alist)))))))

    (if (interactive-p)
(word-count-interactive regexp)
      (word-count-non-interactive regexp overflow force))))



(defun word-count-interactive (regexp)
  "Report the word count on the echo area.
This function is executed only when 'word-count' is called
interactively.

REGEXP: Regular expression used to match words."
  (let ((region-message
(if mark-active
"Region: %d words. "
""))

(buffer-message
"Buffer: %d words.")

(goal-message
(if wc-goal-words
(if (> wc-words-in-buffer-flag wc-goal-words)
" Goal count: %d Exceded!"
" Goal count: %d.")
"")))

    (message "Counting words, please wait...")
    (message (concat region-message buffer-message goal-message)
    (if (not (zerop (length region-message)))
(setq wc-words-in-region-flag
(wc-region (region-beginning) (region-end) regexp nil))
      (setq wc-words-in-buffer-flag
(wc-region (point-min) (point-max) regexp nil)))
    (if (not (zerop (length region-message)))
(setq wc-words-in-buffer-flag
(wc-region (point-min) (point-max) regexp nil))
      wc-goal-words)
    wc-goal-words)))



(defun word-count-non-interactive (regexp overflow force)
  "Report the word count on the modeline.
This function is executed only when 'word-count' is called
non-interactively.

REGEXP: Regexp used to match words in the buffer.
OVERFLOW: Integer that defines the maximum number of words before
word count stop the count.
FORCE: Force the counter to run."
  (let ((activate-counter ;see if word-counter will run on this major-mode.
(cond ((assq major-mode wc-toggle-mode-alist)
(cdr (assq major-mode wc-toggle-mode-alist)))
((assq t wc-toggle-mode-alist)
(cdr (assq t wc-toggle-mode-alist)))))
(wc-format-string	;set the minibuffer string
(if mark-active
"%sW%s/%s--"
"%sW%s--")))
    
    (when
(or force	;if non-nil, force update of counter
(and activate-counter ;counter will run, depending on `wc-toggle-mode-alist'
(not (active-minibuffer-window)) ;don't count words on minibuffer
(or mark-active
(and
(not (= last-buffer-modified-tick	;if buffer has been modified
(buffer-modified-tick)))
(< wc-time-control (cadr (current-time)))) ;don't count too often
(= last-buffer-modified-tick -1)))) ; eval 't' when quit from region

      (if mark-active
(progn
(setq wc-words-in-region-flag
(wc-region (region-beginning) (region-end) regexp overflow))
(wc-show wc-words-in-buffer-flag
wc-words-in-region-flag
wc-format-string
wc-goal-words)
(setq last-buffer-modified-tick -1))
(progn
(setq wc-words-in-buffer-flag
(wc-region (point-min) (point-max) regexp overflow))
(wc-show wc-words-in-buffer-flag
wc-words-in-region-flag
wc-format-string
wc-goal-words)
(setq last-buffer-modified-tick (buffer-modified-tick))))

      (setq wc-time-control (cadr (current-time))))))



;;; Update interactively the `wc-goal-words' variable.
(defun wc-set-goal-words ()
  "Set the maximum number of words in the buffer before an alert is made.
The number asked in the minibuffer has to be an integer.

If the value entered is not a number, zero is asumed.
If the value entered is a number but not an integer, only the integer
part is used.
If the value entered is a negative integer, the goal counter is
deactivated."
  (interactive)
   (let ((goal ;set the goal word number.
(string-to-number
(read-string
(concat
(and wc-goal-words (format "Actual goal: %d. " wc-goal-words))
"New Goal: (a negative integer deactivates goal count) ")))))
     (cond ((< goal 0)
(message "Word goal counter deactivated!"
(setq wc-goal-words nil)))
((= goal 0) ;if given a non-numeric value, '0' is assumed.
(if (y-or-n-p (format "0 is not recomended. Are you sure? " goal))
(message "New goal set to %d."
(setq wc-goal-words goal))
(wc-set-goal-words)))
(t
(message "New goal set to %d."
(setq wc-goal-words goal))))
     (word-count t)))



;;; Update the `word-counter' variable info; will be updated in the modeline.
(defun wc-show (buffer-count region-count format-string goal-words)
  "Set the correct values in the `word-counter' variable.
All parameters (BUFFER-COUNT, REGION-COUNT, FORMAT-STRING and GOAL-WORDS)
are passed from other functions.
This variable will be displayed in the mode-line."
  (let ((first (if mark-active
(or region-count "??")
(or buffer-count "??")))
(second (if mark-active
(or buffer-count "??")))
(goal-mark (if (and goal-words (> buffer-count goal-words))
(progn
(funcall wc-reached-goal-function)
"!")
"")))
    (setq word-counter
(format
format-string
goal-mark
first
second))
    (force-mode-line-update)))



;;; Actual word counter.
(defun wc-region (beg end regexp overflow)
  "Count the number of words between BEG and END.
The argument REGEXP is the actual regular expresion used in this buffer.
The argument OVERFLOW marks the maximum number of words found before
stopping, to prevent an overflow."
  (let ((count 0))
    (save-excursion
      (goto-char beg)
      (while (and (< (point) end)
(re-search-forward regexp end t)
(if overflow
(or (< count overflow) ;return 'nil' if overflow.
(progn (setq count nil) nil))
t))
(setq count (1+ count))))
    count))

(provide 'word-counter)

;;; word-counter.el ends here
