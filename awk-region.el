;;; awk-region.el --- Transform regions with awk -*- lexical-binding: t; -*-

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/awk-region
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))
;; Created: 27 September 2024
;; Keywords: convenience, awk

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Interactively run awk code on region from inline command prompt.
;;
;; Inspired by https://www.emacswiki.org/emacs/awk-it.el.
;;
;; TODO(09/27/24):
;; - transient menu:
;;  - toggle command mode
;;  - set match regexp
;;  - set FS, OFS, etc.
;; - Switch to `edit-indirect-region' with raw mode
;; - Better undo handling, `yas-snippet-revival'
;; - Edit & rerun last awk command
;; - Disable delete at start of command prompt
;; - Run awk code from file
;; - Use field with mode FS as data field for prompt?
;; - When switching b/w modes, worth trying to transform code?
;;
;;; Code:


(defgroup awk-region nil
  "Run awk on regions interactively."
  :group 'external
  :prefix "awk-region-")

(defcustom awk-region-command "awk"
  "Command to run Awk."
  :type 'string)

(defcustom awk-region-commit-action 'replace
  "Default action to take when committing awk results."
  :type '(choice (const :tag "Delete" nil)
                 (const :tag "Replace original" replace)
                 (const :tag "Insert in buffer" insert)
                 (const :tag "Kill" kill)))

(defcustom awk-region-undo t
  "Non-nil to group changes during `awk-region-minor-mode' as single undo."
  :type 'boolean)

(defcustom awk-region-match-re "$0 !~ /^$/"
  "Regexp to match input line."
  :type 'regexp)

(defcustom awk-region-fs " "
  "Regexp for awk field separator FS."
  :type 'regexp)

(defcustom awk-region-mode 'print
  "Awk command mode.

Print mode runs \"print <command>\" on lines matching `awk-region-match-re' with
some preprocessing of the command:
 - Single/double quotes, backslashes, and newlines are escaped.
 - Fields are concatenated with the rest of the text.

Simple mode runs the input command as the body of the rule matching
`awk-region-match-re'.

Raw mode is full awk syntax."
  :type '(choice (const :tag "Print" print)
                 (const :tag "Simple" simple)
                 (const :tag "Raw" raw)))

(defvar awk-region-simple-template
  "/^$/ { print }
%s {
  %s
}")

(defface awk-region-command-face
  '((t (:background "#181418")))
  "Face to highlight awk command.")

(defface awk-region-prompt-face
  '((t (:foreground "#5af" :slant italic)))
  "Face to use to highlight prompts.")

;; `smerge-lower'
(defface awk-region-output-face
  '((((class color) (min-colors 88) (background light))
     :background "#ddffdd" :extend t)
    (((class color) (min-colors 88) (background dark))
     :background "#335533" :extend t)
    (((class color))
     :foreground "green" :extend t))
  "Face for awk output.")

;; `smerge-upper'
(defface awk-region-input-face
  '((((class color) (min-colors 88) (background light))
     :background "#ffdddd" :extend t)
    (((class color) (min-colors 88) (background dark))
     :background "#553333" :extend t)
    (((class color))
     :foreground "red" :extend t))
  "Face for input region.")

(defvar awk-region-debug t)


;;; Internal data structures

(defvar-local awk-region--current nil
  "Current active region.")

(cl-defstruct (awk--region (:constructor awk-region--make)
                           (:copier nil))
  "Active region."
  example command error output input
  (stdout (format "*awk-region[%s]::stdout*" (buffer-name)))
  (stderr (format "*awk-region[%s]::stderr*" (buffer-name))))

(defun awk-region--make-current (start end)
  "Setup active region for input from START to END."
  (cl-assert (null awk-region--current))
  ;; FIXME(09/27/24): what was this?
  (setq awk-region--current (awk-region--make)) ;; (current-buffer)
  (save-excursion
    (goto-char start)
    (let ((example-data
           ;; FIXME: get longest line by field separators (like awk-it)
           (buffer-substring-no-properties start (line-end-position))))
      (awk-region--make-io-overlays start end)
      (awk-region--make-command-overlay start example-data))))


;;; Overlays

(defvar-keymap awk-region-command-map
  :doc "Keymap active in awk-region command overlay.")

(defun awk-region--make-overlay (start end type &rest props)
  "Create overlay of TYPE from START to END and apply PROPS."
  (let* ((front (memq type '(example error input output)))
         (rear (memq type '(command)))
         (ov (make-overlay start end nil front rear)))
    (overlay-put ov 'awk-region type)
    (cl-loop for (k v) on props by #'cddr
             do (overlay-put ov k v))
    ov))

(defun awk-region--make-io-overlays (start end)
  "Create input/output overlays for input region from START to END."
  (setf (awk--region-output awk-region--current)
        (awk-region--make-overlay start start 'output 'invisible t))
  (setf (awk--region-error awk-region--current)
        (awk-region--make-overlay start start 'error 'invisible t))
  (setf (awk--region-input awk-region--current)
        (awk-region--make-overlay
         start end 'input
         'face '(:inherit awk-region-input-face :extend t))))

;;; TODO: disable backward delete at beginning of command prompt
(defun awk-region--make-command-overlay (start &optional example-data)
  "Create command overlay at START.
When EXAMPLE-DATA is non-nil, include a \"Data: EXAMPLE-DATA\" line before
command prompt."
  ;; Hide added prompt from undo
  (with-silent-modifications
    (goto-char start)
    ;; FIXME: do the whole operation with modifying buffer?
    (insert-before-markers "\n"))
  (let ((example (awk-region--make-overlay start start 'example))
        (command
         (awk-region--make-overlay
          start (point) 'command
          'keymap awk-region-command-map
          'face '(:inherit awk-region-command-face :extend t))))
    (when example-data
      (overlay-put
       example 'after-string
       (concat (propertize
                "Data: " 'font-lock-face 'awk-region-prompt-face)
               (propertize example-data 'font-lock-face 'font-lock-string-face)
               "\n")))
    (overlay-put
     command 'before-string
     (propertize
      "Awk> " 'font-lock-face '(:inherit awk-region-prompt-face :weight bold)))
    (setf (awk--region-example awk-region--current) example)
    (setf (awk--region-command awk-region--current) command)
    command))

(defun awk-region--update-output (overlay &optional res)
  "Update OVERLAY with RES."
  (overlay-put overlay 'after-string res)
  (overlay-put overlay 'invisible (null res)))

;; (defun awk-region--make-text-prompt (start &optional example)
;;   (save-excursion
;;     ;; Hide from undo
;;     (combine-change-calls start (point)
;;       (goto-char start)
;;       (when example
;;         (insert "Data: " example "\n"))
;;       (insert "Awk> ")
;;       (add-text-properties start (point) '( field prompt
;;                                             rear-nonsticky t
;;                                             read-only t)))))

(defun awk-region--cleanup-region (&optional region)
  "Cleanup active REGION or default `awk-region--current'."
  (when-let* ((r (or region awk-region--current)))
    (pcase-let (((cl-struct awk--region
                            example command error input output stdout stderr)
                 r))
      (let ((inhibit-read-only t))
        (dolist (ov (delq nil (list example command error input output)))
          (let ((type (overlay-get ov 'awk-region)))
            (when (eq 'command type)
              (delete-region (overlay-start ov) (overlay-end ov))))
          (delete-overlay ov)))
      (dolist (b (list stdout stderr))
        (when-let* ((buf (get-buffer b)))
          (kill-buffer buf))))
    (or region
        (setq awk-region--current nil))))

(defun awk-region--commit (&optional region)
  "Commit output, replacing original according to `awk-region-commit-action'.
Use REGION if non-nil."
  (when-let* ((r (or region awk-region--current)))
    (pcase-let (((cl-struct awk--region input output command) r))
      (let ((str (substring-no-properties (overlay-get output 'after-string))))
        (cond
         ((eq 'kill awk-region-commit-action)))
        (save-excursion
          (pcase awk-region-commit-action
            ;; TODO(4/24/24): after replace, if there is an `undo', replace
            ;; input with previous input
            ((or 'insert 'replace)
             (unless str
               (user-error "Output is currently empty: ignoring"))
             (let ((cmd-end (overlay-end command))
                   (input-beg (overlay-start input)))
               (goto-char input-beg)
               (insert str)
               ;; Dont extend command
               (move-overlay command (overlay-start command) cmd-end)
               (when (eq 'replace awk-region-commit-action)
                 (delete-region (point) (overlay-end input))
                 (move-overlay input input-beg (point)))))
            ('kill (and str (kill-new str)))
            (_ nil))))
      (awk-region--update-output output nil))))


;;; Minor Mode

(defun awk-region-commit (&optional action)
  "Commit current output according to `awk-region-commit-action'.
With prefix, choose commit ACTION."
  (interactive
   (list (and current-prefix-arg
              (intern
               (completing-read
                 "Action on output: " '("replace" "insert" "kill" "delete")
                 nil t)))))
  (let ((awk-region-commit-action (or action awk-region-commit-action)))
    (awk-region--commit)))

(defun awk-region-abort ()
  "Abort current change and exit `awk-region-minor-mode'."
  (interactive)
  (let (awk-region-commit-action)
    (awk-region-minor-mode -1)))

(defun awk-region-back-to-prompt ()
  "Move to awk region command prompt."
  (interactive)
  (pcase-let (((cl-struct awk--region command) awk-region--current))
    (goto-char (overlay-start command))))

(defvar-keymap awk-region-minor-mode-map
  :doc "Active keymap during `awk-region-minor-mode'."
  "C-x C-s" #'awk-region-commit
  "C-c C-p" #'awk-region-back-to-prompt
  "C-c C-c" #'awk-region-run
  "C-c C-k" #'awk-region-abort)

(define-minor-mode awk-region-minor-mode
  "Minor mode active when using `awk-region'."
  :lighter " AwkR"
  (if (null awk-region-minor-mode)
      (progn
        (awk-region--cleanup-region)
        (awk-region--cleanup-undo))
    (when awk-region-undo
      (setq buffer-undo-list (cons 'AWK-REGION buffer-undo-list)))))

;;; FIXME(4/23/24): better way?
;; Shouldnt error when trying to undo after entering `awk-region-minor-mode'
;; and bumps into 'AWK-REGION entry on list
(defun awk-region--cleanup-undo ()
  "Reset `buffer-undo-list'."
  (when awk-region-undo
    (setq buffer-undo-list
          (let (found)
            (cl-loop for x in buffer-undo-list
                     when (eql x 'AWK-REGION) do (setq found t)
                     when (and (or found x) (not (eql x 'AWK-REGION)))
                     collect x)))))


;;; Process

(defun awk-region--escape-quote (code)
  "Replace \"'\" with \\='auto_quote in CODE."
  (replace-regexp-in-string "'" "\" auto_quote \"" code))

(defun awk-region--build-begin ()
  "Build BEGIN rule."
  (if (string= " " awk-region-fs) ""
    (format "BEGIN { FS=\"%s\"; }\n" awk-region-fs)))

(defun awk-region--preprocess-print (raw-code)
  "Preprocess RAW-CODE for print mode."
  (thread-last raw-code
               (replace-regexp-in-string "\\\\" "\\\\\\\\")
               (replace-regexp-in-string "\"" "\\\\\"")
               (replace-regexp-in-string "\\(\\$[0-9]+\\)" "\" \\1 \"")
               (replace-regexp-in-string "\n" "\\\\n\\\\\n")
               (format "print \"%s\"")))

(defun awk-region--preprocess-simple (raw-code)
  "Preprocess RAW-CODE for simple mode."
  raw-code)

(defun awk-region--build-code (&optional overlay)
  "Build awk code from command in OVERLAY."
  (let* ((overlay (or overlay (awk--region-command awk-region--current)))
         (raw-code (string-chop-newline
                    (buffer-substring-no-properties
                     (overlay-start overlay) (overlay-end overlay)))))
    (awk-region--escape-quote
     (concat (awk-region--build-begin)
             (if (eq 'raw awk-region-mode)
                 raw-code
               (format awk-region-simple-template
                       awk-region-match-re
                       (pcase awk-region-mode
                         ('print (awk-region--preprocess-print raw-code))
                         ('simple (awk-region--preprocess-simple raw-code))
                         ('raw raw-code))))))))

;;; TODO: allow editing last code and rerun
;;; In raw mode, option to use `edit-indirect-region' to edit command input
(defvar-local awk-region--last-code nil)

(defun awk-region-run (&optional region)
  "Run current command on input.
If REGION is non-nil, run instead of default active region."
  (interactive)
  (pcase-let (((cl-struct awk--region command input output error stdout stderr)
               (or region awk-region--current)))
    (let* ((shell-command-dont-erase-buffer)
           (max-mini-window-height)     ; no message from `shell-command'
           (awk-code (awk-region--build-code command))
           (status (save-window-excursion
                     (shell-command-on-region
                      (overlay-start input) (overlay-end input)
                      (format "%s -v auto_quote=\"'\" '%s'"
                              awk-region-command awk-code)
                      stdout nil
                      (with-current-buffer (get-buffer-create stderr)
                        (erase-buffer)
                        (current-buffer)))))
           (error-p (not (zerop status)))
           (res (with-current-buffer (if error-p stderr stdout)
                  (buffer-string))))

      (setq awk-region--last-code awk-code)
      (when awk-region-debug
        (message "Code:\n%s" awk-region--last-code))

      (awk-region--update-output
       error (and error-p (propertize res 'font-lock-face 'font-lock-warning-face)))
      (awk-region--update-output
       output (unless error-p
                (propertize res 'font-lock-face 'awk-region-output-face))))))


;;; Commands

;;;###autoload
(defun awk-region (start end &optional _fs _file)
  "Run awk commands interactively on region from START to END.
Optionally specify FS or FILE to run code from.

This command creates a temporary input prompt for awk code before the selected
region and enables `awk-region-minor-mode'.

Output from running the awk code on the region is displayed below the prompt in
an overlay. The initial region isn't modified until running `awk-region-commit'."
  (interactive "r")
  (setq deactivate-mark t)
  (condition-case-unless-debug _err
      (progn
        (awk-region--make-current start end)
        (awk-region-back-to-prompt)
        (awk-region-minor-mode 1))
    (error
     (let (awk-region-commit-action)
       (awk-region-minor-mode -1)))
    (quit
     (let (awk-region-commit-action)
       (awk-region-minor-mode -1)))))

(provide 'awk-region)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; awk-region.el ends here
