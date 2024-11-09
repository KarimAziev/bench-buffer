;;; bench-buffer.el --- Benchmark utils -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/bench-buffer
;; Version: 0.1.0
;; Keywords: lisp
;; Package-Requires: ((emacs "28.1"))
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Benchmark elisp forms in editable buffer

;;; Code:

(require 'fp)

(defcustom bench-buffer--history-file (expand-file-name
                                       "bench-buffer.history"
                                       user-emacs-directory)
  "File to save history."
  :type 'file
  :group 'bench-buffer)

(defcustom bench-buffer--history-max-size 100
  "Max size for history."
  :type 'integer
  :group 'bench-buffer)

(defvar bench-buffer--history nil)
(defvar bench-buffer--history-idx 0)
(defvar bench-buffer-result nil)

(define-derived-mode bench-buffer-report-mode tabulated-list-mode
  "Bench-buffer"
  "Show report gathered about unused definitions."
  (setq tabulated-list-format
        [("Name" 30 t)
         ("Time" 20 t)
         ("GC-count" 10 t)
         ("GC-time" 10 t)
         ("" 0 t)])
  (tabulated-list-init-header))

(defun bench-buffer-print-results ()
  "Show ITEMS in tabulated list mode with HEADER and PADDING."
  (let ((buffer (get-buffer-create "*bench-buffer-results*")))
    (with-current-buffer buffer
      (unless (derived-mode-p 'bench-buffer-report-mode)
        (bench-buffer-report-mode))
      (setq tabulated-list-entries
            (bench-buffer-format-results
             bench-buffer-result))
      (tabulated-list-print)
      (pop-to-buffer buffer))))


(defun bench-buffer-jump-to-entry (beg)
  "Jump to a specific entry in the benchmark buffer and highlight it.

Argument BEG is a position in the buffer, typically a number, where the function
will jump to."
  (let* ((buff (get-buffer "*bench-buffer*"))
         (wnd (or (get-buffer-window buff)
                  (get-buffer-window (pop-to-buffer buff)))))
    (with-selected-window wnd
      (goto-char beg)
      (pulse-momentary-highlight-one-line))))

(defun bench-buffer-format-results (result)
  "Format RESULT to org table."
  (let* ((by-fastest (seq-sort-by (fp-partial nth 3) #'< result))
         (fastest (nth 3 (car by-fastest)))
         (slowest (nth 3 (car (last by-fastest)))))
    (mapcar
     (pcase-lambda (`(,name ,beg ,_end ,time ,count-gc ,time-gc))
       (let ((label
              (pcase time
                ((pred (= fastest)) "Fastest")
                ((pred (= slowest)) "Slowest")
                (_ ""))))
         (list (format "%s" name)
               (apply #'vector (mapcar
                                (fp-when
                                  (fp-not listp)
                                  (apply-partially #'format "%s"))
                                (list
                                 (list
                                  (format "%s" name)
                                  'action
                                  #'bench-buffer-jump-to-entry 'button-data
                                  beg)
                                 (bench-buffer-format-to-ms time)
                                 count-gc
                                 time-gc
                                 label))))))
     result)))

;;;###autoload
(defun bench-buffer-print-result ()
  "Print result of benchmarks in tabulated mode."
  (interactive)
  (bench-buffer-print-results))

(defun bench-buffer-unquote (exp)
  "Return EXP unquoted."
  (declare (pure t)
           (side-effect-free t))
  (while (memq (car-safe exp) '(quote function))
    (setq exp (cadr exp)))
  exp)


(defun bench-buffer-map-names (items)
  "Return list of uniq symbols from ITEMS."
  (let ((names)
        (flatten-items
         (mapcar (fp-compose
                  (apply-partially #'mapcar #'bench-buffer-unquote)
                  (apply-partially
                   #'seq-filter
                   (fp-and
                    symbolp
                    functionp
                    (fp-compose
                     not
                     (fp-or special-form-p
                            subrp
                            (fp-rpartial
                             memq
                             '(let let* defun default-directory
                                   car cdr nth aref
                                   elt if and or + -
                                   1+ 1- min max
                                   car-safe cdr-safe
                                   progn prog1 prog2
                                   * / % length memq
                                   list vector
                                   vectorp
                                   < > <= >= = error))))))
                  reverse
                  flatten-list)
                 items))
        (key-parts)
        (processed)
        (idx 0))
    (while (setq key-parts (pop flatten-items))
      (let ((sym (or (seq-find (lambda (key)
                                 (not (memq key
                                            (flatten-list
                                             (append processed
                                                     flatten-items)))))
                               key-parts)
                     idx)))
        (setq names (push sym names))
        (setq idx (1+ idx))
        (push key-parts processed)))
    (reverse names)))

(defvar bench-buffer-buffer-default-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-x 0") #'kill-this-buffer)
    (define-key map (kbd "C-c C-k") #'kill-this-buffer)
    (define-key map (kbd "C-c C-c")
                #'bench-buffer-edit-buffer-edit-done)
    (define-key map (kbd "C-c C-p")
                #'bench-buffer-setup-edit-prev-history-element)
    (define-key map (kbd "C-c C-n")
                #'bench-buffer-setup-edit-next-history-element)
    (define-key map (kbd "C-x s") #'bench-buffer--save-history)
    (define-key map (kbd "C-x C-s") #'bench-buffer--save-current-element)
    map))

(defun bench-buffer-setup-edit-get-next-or-prev-history (n)
  "Return the N element of `km-setup-edit--buffer-name'."
  (let* ((values
          (or bench-buffer--history
              (when bench-buffer--history-file
                (setq bench-buffer--history (bench-buffer--unserialize
                                             bench-buffer--history-file)))))
         (max (1- (length values)))
         (sum (+ n bench-buffer--history-idx))
         (next-idx (if (and (>= sum 0)
                            (<= sum max))
                       sum
                     (if (> n 0) 0 (abs max)))))
    (setq bench-buffer--history-idx next-idx)
    (nth bench-buffer--history-idx bench-buffer--history)))

;;;###autoload
(defun bench-buffer-setup-edit-prev-history-element ()
  "Insert previous history content."
  (interactive)
  (erase-buffer)
  (when-let* ((str (bench-buffer-setup-edit-get-next-or-prev-history -1)))
    (goto-char (point-min))
    (save-excursion
      (insert str))))

;;;###autoload
(defun bench-buffer-setup-edit-next-history-element ()
  "Insert next history content."
  (interactive)
  (erase-buffer)
  (when-let* ((str (bench-buffer-setup-edit-get-next-or-prev-history 1)))
    (goto-char (point-min))
    (save-excursion
      (insert str))))

(defun bench-buffer--serialize (data filename)
  "Serialize DATA to FILENAME.

The saved data can be restored with `bench-buffer--unserialize'."
  (when (file-writable-p filename)
    (with-temp-file filename
      (insert
       (let (print-length)
         (prin1-to-string data))))))

(defun bench-buffer--unserialize (filename)
  "Read data serialized by `bench-buffer--serialize' from FILENAME."
  (with-demoted-errors
      "Error during file deserialization: %S"
    (when (file-exists-p filename)
      (with-temp-buffer
        (insert-file-contents filename)
        (read (buffer-string))))))

(defun bench-buffer--cleanup-history ()
  "Cleanup history."
  (interactive)
  (setq bench-buffer--history
        nil)
  (bench-buffer--save-history))

(defun bench-buffer--save-history ()
  "Save history."
  (interactive)
  (when bench-buffer--history-file
    (bench-buffer--serialize bench-buffer--history bench-buffer--history-file)))

(defun bench-buffer--save-current-element ()
  "Save current content in \"*bench-buffer*\" to `bench-buffer--history-file'."
  (interactive)
  (let ((elem (string-trim (buffer-substring-no-properties (point-min)
                                                           (point-max))))
        (history (bench-buffer--unserialize bench-buffer--history-file)))
    (if (string-empty-p elem)
        (message "Nothing to save")
      (setq history (delete elem history))
      (setq history (append history (list elem)))
      (bench-buffer--serialize history bench-buffer--history-file)
      (message "Saved"))))

(defun bench-buffer-setup-edit-buffer (&optional content setup-args)
  "Return editable buffer with CONTENT in popup window.
If ON-DONE is a function, invoke it with buffer content.
SETUP-ARGS can includes keymaps, syntax table, filename and function.
A filename can be opened with \\<bench-buffer-buffer-default-keymap>\\.
A function will be called without args inside quit function.

If SETUP-ARGS contains syntax table, it will be used in the inspect buffer."
  (let ((buffer (get-buffer-create (concat "*bench-buffer*")))
        (keymaps (seq-filter #'keymapp setup-args)))
    (with-current-buffer buffer
      (erase-buffer)
      (setq buffer-read-only nil)
      (progn
        (when (stringp content)
          (insert content))
        (emacs-lisp-mode)
        (use-local-map
         (let ((map (copy-keymap bench-buffer-buffer-default-keymap)))
           (when keymaps
             (setq map (make-composed-keymap
                        keymaps
                        map)))
           (set-keymap-parent map (current-local-map))
           map)))
      (setq buffer-undo-list nil)
      (set-buffer-modified-p nil)
      (setq header-line-format
            (substitute-command-keys "\\<bench-buffer-buffer-default-keymap>\
Use `\\[bench-buffer-edit-buffer-edit-done]' when done")))
    buffer))



(defun bench-buffer-scan-top-level-lists ()
  "Return all Lisp lists at outermost position in current buffer.
An \"outermost position\" means one that it is outside of any syntactic entity:
outside of any parentheses, comments, or strings encountered in the scan."
  (let ((sexps)
        (sexp))
    (goto-char (point-min))
    (while (setq sexp (ignore-errors (read (current-buffer))))
      (let ((sexp-end (point))
            (sexp-start))
        (let ((name (save-excursion
                      (ignore-errors
                        (backward-sexp 1)
                        (setq sexp-start (point))
                        (when (save-excursion
                                (skip-chars-backward "\s\t\n\r\f")
                                (nth 4 (syntax-ppss (point))))
                          (let ((beg)
                                (end (point)))
                            (forward-comment most-negative-fixnum)
                            (setq beg (point))
                            (string-trim
                             (buffer-substring-no-properties beg end))))))))
          (push (list name sexp-start sexp-end sexp) sexps))))
    (reverse sexps)))

(defvar-local bench-buffer-scan-timer nil)

(defvar bench-buffer-forms nil)

;;;###autoload
(defun bench-buffer-scan-cancel-timer ()
  "Reset `bench-buffer-scan-timer'."
  (interactive)
  (when (timerp bench-buffer-scan-timer)
    (cancel-timer bench-buffer-scan-timer))
  (setq bench-buffer-scan-timer nil))


(defun bench-buffer-format-to-ms (value)
  "Format the given VALUE to milliseconds if it's in scientific notation.

Argument VALUE is any number that will be formatted as a string."
  (let ((str (format "%s" value)))
    (if (string-match-p
         "^[+-]?\\([0-9]+\\.?[0-9]*\\|[0-9]*\\.?[0-9]+\\)[eE][+-]?[0-9]+$" str)
        (format "%s" (* value 1000))
      str)))

(defun bench-buffer-render-chunk-in-buffer (buffer spec repetitions)
  "Render symbols in SPEC used in other forms to BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (bench-buffer-scan-cancel-timer)
      (when spec
        (pcase-let ((`(,name ,beg ,end ,form) spec))
          (let* ((lexical-binding t)
                 (compile-fn (if (native-comp-available-p)
                                 #'native-compile
                               #'byte-compile))
                 (fn `(lambda ()
                        (,@form)))
                 (compiled (funcall compile-fn fn)))
            (garbage-collect)
            (let ((res (benchmark-call compiled repetitions)))
              (setq bench-buffer-result (nconc
                                         bench-buffer-result
                                         (list (append (list
                                                        name
                                                        beg
                                                        end)
                                                       res)))))
            (bench-buffer-print-results))))
      (if-let* ((next-form (pop bench-buffer-forms)))
          (setq bench-buffer-scan-timer
                (run-with-idle-timer 0.1 nil
                                     #'bench-buffer-render-chunk-in-buffer
                                     (current-buffer)
                                     next-form
                                     repetitions))
        (bench-buffer-print-results)))))

;;;###autoload
(defun bench-buffer-edit-buffer-edit-done (repetitions)
  "Run benchmarks REPETITIONS times and show results."
  (interactive (list (read-number "Repetions: " 10)))
  (bench-buffer-scan-cancel-timer)
  (setq bench-buffer-result nil)
  (let* ((full-forms (bench-buffer-scan-top-level-lists))
         (forms (mapcar (apply-partially #'nthcdr 3) full-forms))
         (names (bench-buffer-map-names
                 forms))
         (final-items (seq-map-indexed (lambda (form i)
                                         (if (car form)
                                             form
                                           (setcar form (nth i names))
                                           form))
                                       full-forms)))
    (add-to-history 'bench-buffer--history
                    (buffer-substring-no-properties (point-min)
                                                    (point-max))
                    bench-buffer--history-max-size)
    (setq bench-buffer-forms final-items)
    (setq bench-buffer-scan-timer
          (run-with-idle-timer 0.1 nil
                               #'bench-buffer-render-chunk-in-buffer
                               (current-buffer)
                               (pop bench-buffer-forms)
                               repetitions))))

;;;###autoload
(defun bench-buffer ()
  "Create editable buffer for benching elisp top forms.
\\<bench-buffer-buffer-default-keymap>
When done, exit with `\\[bench-buffer-edit-buffer-edit-done]'.  This \
will run benchmark on every elisp form."
  (interactive)
  (pop-to-buffer (bench-buffer-setup-edit-buffer)))

(provide 'bench-buffer)
;;; bench-buffer.el ends here