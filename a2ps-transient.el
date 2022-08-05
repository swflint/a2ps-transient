;;; a2ps-transient.el --- A transient interface to a2ps -*- lexical-binding: t -*-

;; Author: Samuel W. Flint <swflint@flintfam.org>
;; URL: https://github.com/swflint/a2ps-transient
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.0"))
;; Keywords: printer, transient, syntax-highlighting

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; This package provites a transient menu with common options for a2ps.

(require 'transient)

;;; Code:

;; Utilities

(defun a2ps-transient-read-file (prompt _initial-input _history)
  "Read a file name using PROMPT.

Returns the name of an existing file in a list."
  (list (file-local-name (expand-file-name (read-file-name prompt nil nil t)))))

(defun a2ps-transient--get-default-file-or-buffer ()
  "Return list of files/buffer to print.

In `dired-mode' get marked files.  Otherwise, if there's a
filename, get it, else, the buffer."
  (if (derived-mode-p 'dired-mode)
      (dired-get-marked-files)
    (or (when-let (file-name (buffer-file-name))
          (list file-name))
        (current-buffer))))

(defun a2ps-transient--read-printer (prompt initial-input history)
  "Read printer name using PROMPT, respecting INITIAL-INPUT and HISTORY.

Uses a2ps --list=printers to determine configured printers."
  (let ((filter-lines
         (lambda (lines)
           (remove-if #'null
                      (mapcar (lambda (x)
                                (when (and (string-match-p "^-" x)
                                           (not (string-match-p "Default" x))
                                           (not (string-match-p "Unknown" x)))
                                  (substring x 2)))
                              lines)))))
    (completing-read prompt
                     (funcall filter-lines
                              (split-string (with-temp-buffer
                                              (call-process "a2ps" nil t nil "--list=printers")
                                              (buffer-string))
                                            "\n" 'omit-nulls))
                     nil nil initial-input history)))

(defun a2ps-transient--read-medium (prompt initial-input history)
  "Read a2ps medium using PROMPT.

Respects INITIAL-INPUT and HISTORY."
  (let* ((lines (split-string (with-temp-buffer
                                (call-process "a2ps" nil t nil "--list=media")
                                (buffer-string))
                              "\n" 'omit-nulls))
         (media-lines (remove-if-not (lambda (line) (and (string-match-p "^  " line)
                                                         (not (string-match-p "^  Name" line))))
                                     lines))
         (media (mapcar (lambda (line)
                          (let* ((line (substring line 2))
                                 (end (string-match "[[:space:]]" line)))
                            (substring line nil end)))
                        media-lines)))
    (completing-read prompt media nil nil initial-input history)))

(defun a2ps-transient--read-pretty-printer (prompt initial-input history)
  "PROMPT for pretty printer style sheet.

Respects INITIAL-INPUT and HISTORY."
  (let* ((lines (split-string (with-temp-buffer
                                (call-process "a2ps" nil t nil "--list=style-sheets")
                                (buffer-string))
                              "\n" 'omit-nulls))
         (sheet-lines (remove-if-not (lambda (line) (string-match-p "\\.ssh" line)) lines))
         (sheets (remove-if #'null
                            (mapcar (lambda (line)
                                      (when-let ((start (string-match "(" line))
                                                 (end (string-match "\\.ssh" line)))
                                        (substring line (1+ start) end)))
                                    sheet-lines))))
    (completing-read prompt sheets nil nil initial-input history)))

(defun a2ps-transient--read-highlight-level (prompt initial-input history)
  "PROMPT for highlight level.

Respects INITIAL-INPUT and HISTORY."
  (completing-read prompt '("none" "normal" "heavy") nil nil initial-input history))

(defun a2ps-transient--read-sidedness (prompt initial-input history)
  "PROMPT for sidedness in printing.

Respects INITIAL-INPUT and HISTORY."(completing-read prompt '("simplex" "1" "duplex" "tumble" "2") nil nil initial-input history))

(defun a2ps-transient--read-alignment (prompt initial-input history)
  "PROMPT for page alignment.

Respects INITIAL-INPUT and HISTORY."
  (completing-read prompt '("fill" "rank" "page" "sheet") nil nil initial-input history))


;; Options

(defclass a2ps-transient-files-or-buf (transient-infix)
  ((key         :initform "--")
   (argument    :initform "--")
   (reader      :initform #'a2ps-transient-read-file)
   (always-read :initform t))
  "A transient class to read list of files.
The slot `value' is either a list of files or a single buffer.

Taken from `lp-transient'.")

(cl-defmethod transient-format-value ((obj a2ps-transient-files-or-buf))
  "Format OBJ's value for display and return the result.

Taken from `lp-transient'."
  (let ((argument (oref obj argument)))
    (if-let ((value (oref obj value)))
        (propertize
         (if (listp value)
             ;; Should be list of files.
             (mapconcat (lambda (x)
                          (file-relative-name
                           (abbreviate-file-name (string-trim x "\"" "\""))))
                        value " ")
           ;; Should be a buffer
           (prin1-to-string value))
         'face 'transient-value)
      (propertize argument 'face 'transient-inactive-value))))

(transient-define-argument a2ps-transient--files ()
  :description "Files"
  :init-value (lambda (obj)
                (oset obj value (a2ps-transient--get-default-file-or-buffer)))
  :class 'a2ps-transient-files-or-buf)


;; Save Options

(defvar a2ps-transient-saved-options nil
  "List of options to be passed by default for `a2ps'.")

(defun a2ps-transient-save-options (args)
  "Save a2ps ARGS."
  (interactive (list (cdr (transient-args 'a2ps-transient-menu))))
  (setq a2ps-transient-saved-options args)
  (message "Arguments saved."))


;; Run Program

(defun a2ps-transient-run (files &optional args)
  "Call `a2ps' with files/buffer.

FILES is a buffer or list of files.  ARGS are other arguments
passed to `a2ps'."
  (interactive (list (a2ps-transient--get-default-file-or-buffer)))
  (unless (or (bufferp files)
              (listp files))
    (user-error "`files' must be a buffer or list"))
  (if-let ((program (executable-find "a2ps")))
      (let* ((cmd (nconc (list program)
                         args
                         (and (listp files)
                              files)))
             (process (make-process
                       :name "a2ps-print"
                       :buffer nil
                       :connection-type 'pipe
                       :command cmd)))
        (when (bufferp files)
          (process-send-string process (with-current-buffer files (buffer-string)))
          (process-send-eof process))
        (message "Started print job: %s"
                 (mapconcat #'identity cmd " ")))
    (error "No `a2ps' executable available")))

(defun a2ps-transient-do-run (arguments)
  "Call `a2ps-transient-run-a2ps' with `transient' ARGUMENTS."
  (interactive (list (transient-args 'a2ps-transient-menu)))
  (a2ps-transient-run (car arguments) (cdr arguments)))

(defun a2ps-transient ()
  "Start the a2ps transient menu."
  (interactive)
  (call-interactively #'a2ps-transient-menu))


;; Build the Transient

(transient-define-prefix a2ps-transient-menu (filename)
  "Call `a2ps' with various arguments and options."
  :init-value (lambda (obj)
                (oset obj value a2ps-transient-saved-options))
  :man-page "a2ps"
  :info-manual "a2ps"

  [(a2ps-transient--files)]

  [["General"
    ("P" "Printer" "-P"
     :prompt "Printer? "
     :class transient-option
     :always-read t
     :reader a2ps-transient--read-printer)
    ("p" "Pages" "--pages="
     :prompt "Pages? "
     :class transient-option
     :reader read-string)
    ("t" "Job Title" "--title="
     :prompt "Job Title? "
     :class transient-option
     :reader read-string)
    ("n" "Number of Copies" "--copies="
     :prompt "Number of Copies? "
     :class transient-option
     :reader transient-read-number-N+)
    ("s" "Sidedness" "--sides="
     :prompt "Sidedness? "
     :class transient-option
     :reader a2ps-transient--read-sidedness)]

   ["Sheets"
    ("M" "Medium" "--medium="
     :prompt "Medium? "
     :class transient-option
     :always-read t
     :reader a2ps-transient--read-medium)
    ("r" "Portrait?" ("-R" "--portrait"))
    ("C" "Columns" "--columns="
     :prompt "Columns? "
     :class transient-option
     :reader transient-read-number-N+)
    ("R" "Rows" "--rows="
     :prompt "Rows? "
     :class transient-option
     :reader transient-read-number-N+)
    ("a" "Alignment" "--file-align="
     :prompt transient-option
     :reader a2ps-transient--read-alignment)]

   ["Pages"
    ("C-l" "Line Numbers" "--line-numbers="
     :prompt "Line Numbers? "
     :class transient-option
     :reader transient-read-number-N+)
    ("f" "Font Size" "--font-size="
     :prompt "Font Size? "
     :class transient-option
     :reader read-number)
    ("L" "Lines per Page" "--lines-per-page="
     :prompt "Lines per Page? "
     :class transient-option
     :reader transient-read-number-N+)
    ("l" "Characters per Line" "--chars-per-line="
     :prompt "Characters per Line? "
     :class transient-option
     :reader transient-read-number-N+)
    ("T" "Tab Size" "--tab-size="
     :prompt "Tab Size? "
     :class transient-option
     :reader transient-read-number-N+)]]

  [["Headings"
    ("B" "No Header" ("-B" "--no-header"))
    ("b" "Header" "--header="
     :prompt "Header? "
     :class transient-option
     :reader read-string)
    ("u" "Underlay" "--underlay="
     :prompt "Underlay? "
     :class transient-option
     :reader read-string)]
   [""
    ("hc" "Centered Heading" "--center-title="
     :prompt "Centered Title? "
     :class transient-option
     :reader read-string)
    ("hl" "Left Title" "--left-title="
     :prompt "Left Title? "
     :class transient-option
     :reader read-string)
    ("hr" "Right Title" "--right-title="
     :prompt "Right Title? "
     :class transient-option
     :reader read-string)]
   [""
    ("Fc" "Centered Footer" "--center-footer="
     :prompt "Centered Footer? "
     :class transient-option
     :reader read-string)
    ("Fl" "Left Footer" "--left-footer="
     :prompt "Left Footer? "
     :class transient-option
     :reader read-string)
    ("Fr" "Right Footer" "--right-footer="
     :prompt "Right Footer? "
     :class transient-option
     :reader read-string)]]

  [["Pretty-Printing"
    ("E" "Highlight?" "--pretty-print="
     :prompt "Pretty Printer? "
     :class transient-option
     :reader a2ps-transient--read-pretty-printer)]
   [""
    ("H" "Highlight Level" "--highlight-level="
     :prompt "Highlight Level? "
     :class transient-option
     :reader a2ps-transient--read-highlight-level)]]

  [["Commands"
    ("C-c C-c" "Run"
     a2ps-transient-do-run
     :transient nil)]
   ["" ("C-c C-s" "Save Options"
        a2ps-transient-save-options
        :transient t)]])

(with-eval-after-load 'savehist
  (add-to-list 'savehist-additional-variables
               'a2ps-transient-saved-options))

(provide 'a2ps-transient)

;;; a2ps-transient.el ends here
