;;; awk-region.el --- Transform regions with awk -*- lexical-binding: t; -*-

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/awk-region
;; Version: 0.0.1
;; Package-Requires: ((emacs "28.1"))
;; Created: 23 April 2024
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
;;; Code:


(defgroup awk-region nil
  "Run awk on regions interactively."
  :group 'external
  :prefix "awk-region-")

(defface awk-region-command-face
  '((t (:background "#181418")))
  "Face to highlight awk command.")

(defface awk-region-prompt-face
  '((t (:foreground "#5af" :slant italic)))
  "Face to use to highlight prompts.")

(defcustom awk-region-replace-original t
  "Non-nil to replace original region with modified region."
  :type 'boolean)

(defcustom awk-region-undo t
  "Non-nil to treat modified region as single undo."
  :type 'boolean)

(defcustom awk-region-match-re "$0 !~ /^$/"
  "Regexp to match input line."
  :type 'regexp)

(defcustom awk-region-fs " "
  "Regexp for awk field separator FS."
  :type 'regexp)


;;; Overlays

(cl-defstruct (awk--region (:constructor awk--make-region))
  "Active region."
  command prompt output data
  (stdout (format "*awk-region[%s]::stdout*" (buffer-name)))
  (stderr (format "*awk-region[%s]::stderr*" (buffer-name))))

(defvar-local awk-region--current nil)

(defvar-keymap awk-region-command-map
  :doc "Keymap active in awk-region command overlay.")

(defun awk-region--make-data-overlay (start end)
  "Create data overlay from START to END."
  (goto-char start)
  (insert-before-markers "\n")
  (let ((out (make-overlay start start nil nil t))
        (data (make-overlay (point) end)))
    (overlay-put out 'awk-region 'output)
    (overlay-put out 'face '(:inherit region))
    (setf (awk--region-output awk-region--current) out)
    (overlay-put data 'awk-region 'data)
    (overlay-put data 'face '(:inherit region))
    (setf (awk--region-data awk-region--current) data)
    data))

(defun awk-region--make-command-overlay (start &optional data)
  "Create command overlay at START.
When DATA is non-nil, include a \"Data: DATA\" line before command line."
  (save-excursion
    ;; Hide added prompt from undo
    (combine-change-calls start (point)
      (goto-char start)
      (when data
        (insert "Data: " data "\n"))
      (insert "Awk> ")
      (add-text-properties
       start (point)
       '( field prompt
          rear-nonsticky t
          read-only t)))
    (let ((prompt (make-overlay start (point)))
          (ov (make-overlay (point) (point) nil nil t)))
      (overlay-put prompt 'awk-region 'prompt)
      (overlay-put prompt 'font-lock-face '(:inherit awk-region-prompt-face))
      (overlay-put prompt 'read-only t)
      (setf (awk--region-prompt awk-region--current) prompt)
      (overlay-put ov 'awk-region 'command)
      (overlay-put ov 'keymap awk-region-command-map)
      (overlay-put ov 'face '(:inherit awk-region-command-face :extend t))
      (setf (awk--region-command awk-region--current) ov)
      ov)))

(defun awk-region--command-input (&optional region)
  "Get the current command."
  (pcase-let (((cl-struct awk--region command) (or region awk-region--current)))
    (buffer-substring-no-properties
     (overlay-start command) (overlay-end command))))

(defun awk-region--data-bounds (&optional region)
  "Get the bounds of current data region."
  (pcase-let (((cl-struct awk--region data) (or region awk-region--current)))
    (cons (overlay-start data) (overlay-end data))))

(defun awk-region--cleanup-region (&optional region)
  "Cleanup active REGION or default `awk-region--current'."
  (when-let ((r (or region awk-region--current)))
    (pcase-let (((cl-struct awk--region prompt command data output) r))
      (let ((inhibit-read-only t))
        (dolist (ov (list prompt command data))
          (when (or awk-region-replace-original
                    (not (eq 'data (overlay-get ov 'awk-region))))
            (delete-region (overlay-start ov) (overlay-end ov)))
          (delete-overlay ov))))
    (or region
        (setq awk-region--current nil))))


;;; Minor Mode

(defun awk-region-abort ()
  (interactive)
  (let (awk-region-replace-original)
    (awk-region-minor-mode -1)))

(defvar-keymap awk-region-minor-mode-map
  :doc "Active keymap during `awk-region-minor-mode'."
  "C-c C-c" #'awk-region-run
  "C-c C-k" #'awk-region-abort)

(define-minor-mode awk-region-minor-mode
  "Minor mode active when using `awk-region'."
  :lighter " AwkR"
  (if (null awk-region-minor-mode)
      (progn
        (remove-hook 'yas-after-exit-snippet-hook 'awk-region--yas-completed t)
        (awk-region--cleanup-region)
        (awk-region--cleanup-undo))
    (unless yas-minor-mode
      (yas-minor-mode))
    (cl-assert (null awk-region--current))
    (setq awk-region--current (awk--make-region))
    (when awk-region-undo
      (setq buffer-undo-list (cons 'AWK-REGION buffer-undo-list)))
    (add-hook 'yas-after-exit-snippet-hook 'awk-region--yas-completed nil t)))

(defun awk-region--yas-completed ()
  "Hook run in `yas-after-exit-snippet-hook'."
  (awk-region-minor-mode -1))

;;; FIXME(4/23/24): better way?
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

(defun awk-region--sanitize (code)
  (format "print %s" code)
  ;; (concat
  ;;  "print \"" 
  ;;  (->> code (replace-regexp-in-string "\"" "\\\\\"" )
  ;;       (replace-regexp-in-string "\\(\\$[0-9]+\\)" "\" \\1 \""))
  ;;  "\"")
  )

(defun awk-region--awk-code (&optional ov)
  (let* ((ov (or ov (awk--region-command awk-region--current)))
         (code
          (awk-region--sanitize
           (buffer-substring-no-properties
            (overlay-start ov) (overlay-end ov))))
         (begin (if (string= " " awk-region-fs)
                    ""
                  (format "BEGIN { FS=\"%s\"; }\n" awk-region-fs))))
    (format "%s%s { %s }\n/^$/ { print }"
            begin awk-region-match-re code)))

(defun awk-region--insert-output (text output)
  (save-excursion
    (let ((start (overlay-start output)))
      (goto-char start)
      (insert text)
      (delete-region (point) (overlay-end output)))))

(defun awk-region-run (&optional region)
  (interactive)
  (pcase-let (((cl-struct awk--region command data output stdout stderr)
               (or region awk-region--current)))
    (let* ((awk-code (awk-region--awk-code command))
           ;; (stdout (with-current-buffer (get-buffer-create stdout)
           ;;           (erase-buffer)))
           ;; (stderr (with-current-buffer (get-buffer-create stderr)
           ;;           (erase-buffer)))
           (status
            (shell-command-on-region
             (overlay-start data)
             (overlay-end data)
             (format "awk '%s'" awk-code)
             stdout
             nil
             stderr)))
      (if (zerop status)
          (let ((res (with-current-buffer stdout
                       (buffer-string))))
            (awk-region--insert-output res output))
        (pop-to-buffer stderr)))))


;;; Commands

(defun awk-region (start end &optional fs file)
  "Run awk commands on region from START to END.
Optionally specify FS or FILE to run code from."
  (interactive "r")
  (awk-region-minor-mode 1)
  (condition-case-unless-debug _err
      (progn
        (awk-region--make-data-overlay start end)
        (goto-char start)
        (insert-before-markers "\n")
        (let ((ov (awk-region--make-command-overlay start)))
          (goto-char (overlay-end ov))
          ;; (awk-region--run)
          ))
    (error
     (let (awk-region-replace-original)
       (awk-region-minor-mode -1)))
    (quit
     (let (awk-region-replace-original)
       (awk-region-minor-mode -1)))))

  ;; (let ((ov (awk-region--make-command-overlay start)))
  ;;   (goto-char (overlay-end ov)))
  
(provide 'awk-region)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; awk-region.el ends here
