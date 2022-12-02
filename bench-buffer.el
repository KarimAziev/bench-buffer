;;; bench-buffer.el --- Benchmark utils -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/bench-buffer
;; Version: 0.1.0
;; Keywords: lisp
;; Package-Requires: ((emacs "27.1"))

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
(defvar bench-buffer-diff-rows-names nil)

(defvar-local bench-buffer-setup-edit-buffer-on-done-fn nil)
(defun bench-buffer-tabulated-map-entries (items)
  "Return cons with transformed ITEMS and columns indexes for sort as numbers."
  (when (functionp items)
    (setq items (funcall items)))
  (let ((sort-cols)
        (rows))
    (dotimes (idx (length items))
      (let* ((row (nth idx items))
             (entry
              (cond ((and (listp row)
                          (vectorp (nth 1 row)))
                     (list (car row)
                           (apply #'vector
                                  (seq-map-indexed
                                   (lambda (v i)
                                     (if (not (numberp v))
                                         v
                                       (push i sort-cols)
                                       (format "%s" i)))
                                   (append row nil)))))
                    (t (list
                        (format "%s" idx)
                        (apply #'vector
                               (seq-map-indexed
                                (lambda (v i)
                                  (let* ((key
                                          (when (consp v)
                                            (car v)))
                                         (value (if key
                                                    (cdr v)
                                                  v))
                                         (res
                                          (cond ((numberp value)
                                                 (unless (member i
                                                                 sort-cols)
                                                   (push i sort-cols))
                                                 (format "%s" value))
                                                ((not value)
                                                 "")
                                                ((and (listp value)
                                                      (or (stringp (car
                                                                    value))
                                                          (symbolp (car
                                                                    value))
                                                          (numberp
                                                           (car
                                                            value))))
                                                 (mapconcat
                                                  (lambda (it)
                                                    (if (symbolp it)
                                                        (symbol-name
                                                         it)
                                                      (if (stringp
                                                           it)
                                                          it
                                                        (format
                                                         "%s"
                                                         it))))
                                                  " "))
                                                ((listp value)
                                                 (unless (member i
                                                                 sort-cols)
                                                   (push i sort-cols))
                                                 (format "%s"
                                                         (length value)))
                                                ((stringp value)
                                                 value)
                                                (t (format "%s" value)))))
                                    res))
                                (if (vectorp row)
                                    (append row nil)
                                  row))))))))
        (push entry rows)))
    (cons rows sort-cols)))

(defun bench-buffer-tabulated-make-header-columns (header rows sort-cols)
  "Convert or create HEADER from ROWS in tabulated format.
SORT-COLS is indexes of columns to add sorting as numbers."
  (apply #'vector
         (seq-map-indexed
          (lambda (hrow i)
            (when (stringp hrow)
              (setq hrow (list hrow
                               (/ 100
                                  (length
                                   (nth 1
                                        (car
                                         rows))))
                               t)))
            (let ((title (seq-find #'stringp hrow))
                  (width (or (seq-find #'numberp hrow)
                             (/ 100
                                (length
                                 (nth 1
                                      (car
                                       rows))))))
                  (sorter (nth 2 hrow)))
              (setq sorter (if (and (member i sort-cols)
                                    (not (functionp
                                          sorter)))
                               (lambda (row-a row-b)
                                 (let* ((fn (lambda (row)
                                              (when-let* ((arr
                                                           (seq-find
                                                            #'vectorp
                                                            row))
                                                          (val
                                                           (aref
                                                            arr
                                                            (1-
                                                             (length
                                                              arr)))))
                                                (if (stringp
                                                     val)
                                                    (string-to-number
                                                     val)
                                                  val))))
                                        (val-a
                                         (funcall
                                          fn
                                          row-a))
                                        (val-b
                                         (funcall
                                          fn
                                          row-b))
                                        (result
                                         (- (or val-a 0)
                                            (or val-b 0))))
                                   (> result 0)))
                             sorter))
              (list title width sorter)))
          (if (vectorp header)
              (append header nil)
            (or header
                (seq-map-indexed
                 (lambda (v i)
                   (format "%s %s" v i))
                 (append (make-vector
                          (length
                           (nth 1 (car rows)))
                          "Column")
                         nil)))))))

(defun bench-buffer-list-to-tabulated-entries (items &optional header)
  "Return cons with mapped HEADER and ITEMS as tabulated list entries."
  (when (functionp items)
    (setq items (funcall items)))
  (when-let ((hline-pos (seq-position items 'hline)))
    (let ((head-pos (1- hline-pos)))
      (when (and (>= head-pos 0)
                 (nth head-pos items))
        (setq header (nth head-pos items)))
      (setq items (seq-drop items (1+ hline-pos)))))
  (let*
      ((table-data (bench-buffer-tabulated-map-entries items))
       (rows (car table-data))
       (sort-cols (cdr table-data))
       (header-columns (bench-buffer-tabulated-make-header-columns header
                                                                   rows
                                                                   sort-cols)))
    (cons header-columns rows)))

(defun bench-buffer-show-tabulated-results (items &optional header padding)
  "Show ITEMS in tabulated list mode with HEADER and PADDING."
  (let* ((table
          (bench-buffer-list-to-tabulated-entries items
                                                  header))
         (entries (cdr table)))
    (if-let ((buffer (get-buffer "*bench-buffer-result*")))
        (with-current-buffer buffer
          (setq-local tabulated-list-entries
                      entries)
          (tabulated-list-print)
          (pop-to-buffer (current-buffer)))
      (with-current-buffer (get-buffer-create "*bench-buffer-result*")
        (let ((table
               (bench-buffer-list-to-tabulated-entries items
                                                       header)))
          (tabulated-list-mode)
          (setq tabulated-list-padding (or padding 0))
          (setq-local tabulated-list-format
                      (car table))
          (setq-local tabulated-list-entries
                      entries)
          (tabulated-list-init-header)
          (setq-local imenu-prev-index-position-function
                      (lambda ()
                        (unless (bobp)
                          (forward-line -1))))
          (setq-local imenu-extract-index-name-function
                      (lambda ()
                        (buffer-substring-no-properties
                         (line-beginning-position)
                         (line-end-position))))
          (tabulated-list-print))
        (pop-to-buffer (current-buffer))))))

(defun bench-buffer-format-results (result)
  "Format RESULT to org table."
  (let* ((by-fastest (seq-sort-by (fp-partial nth 2) '< result))
         (fastest (car by-fastest))
         (slowest (car (last by-fastest))))
    (mapcar
     (lambda (it)
       (let ((label (if (= (car it)
                           (car fastest))
                        "Fastest"
                      (if (= (car it)
                             (car slowest))
                          "Slowest"
                        ""))))
         (append (seq-subseq it 0 2)
                 (list (format "%.6f" (nth 2 it)))
                 (list label)
                 (seq-subseq it 3))))
     result)))

;;;###autoload
(defun bench-buffer-print-result ()
  "Print result of benchmarks in tabulated mode."
  (interactive)
  (bench-buffer-show-tabulated-results (bench-buffer-format-results
                                        bench-buffer-result)
                                       '(("Idx" 5)
                                         ("Form" 70) "Time"
                                         ""
                                         "GC-count"  "GC-time")))

(defun bench-buffer-unquote (exp)
  "Return EXP unquoted."
  (declare (pure t)
           (side-effect-free t))
  (while (memq (car-safe exp) '(quote function))
    (setq exp (cadr exp)))
  exp)

(defun bench-buffer-map-names (items)
  "Return list of uniq symbols from ITEMS."
  (let ((names))
    (dotimes (idx (length items))
      (let* ((key-parts
              (seq-drop-while
               (lambda (k)
                 (memq (bench-buffer-unquote k) names))
               (seq-filter
                (fp-and
                 symbolp
                 (fp-compose
                  not
                  (fp-or special-form-p
                         (fp-rpartial 'memq
                                      '(let defun default-directory)))))
                (reverse (flatten-list (nth idx items))))))
             (sym (bench-buffer-unquote (car key-parts))))
        (setq names (push sym names))))
    (reverse names)))

(defmacro bench-buffer-diff-funcs (repetitions &rest functions)
  "Run benchmark REPETITIONS times for FUNCTIONS."
  (when (and (= (length functions) 1)
             (listp (car functions)))
    (setq functions (car functions)))
  `(progn
     (setq bench-buffer-result nil)
     (setq bench-buffer-diff-rows-names
           '(,@(mapcar #'symbol-name
                       (bench-buffer-map-names
                        functions))))
     ,@(seq-map-indexed
        (lambda (v i)
          `(progn
             (garbage-collect)
             (let ((res (append
                         (list
                          ,i
                          (nth ,i
                               bench-buffer-diff-rows-names))
                         (benchmark-run-compiled
                             ,repetitions ,v))))
               (setq bench-buffer-result
                     (nconc
                      bench-buffer-result
                      (list
                       res))))))
        functions)
     (bench-buffer-format-results bench-buffer-result)))



(defvar bench-buffer-buffer-default-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-x 0") 'kill-this-buffer)
    (define-key map (kbd "C-c C-k") 'kill-this-buffer)
    (define-key map (kbd "C-c C-c")
                'bench-buffer-edit-buffer-edit-done)
    (define-key map (kbd "C-c C-p")
                'bench-buffer-setup-edit-prev-history-element)
    (define-key map (kbd "C-c C-n")
                'bench-buffer-setup-edit-next-history-element)
    (define-key map (kbd "C-x s") 'bench-buffer--save-history)
    (define-key map (kbd "C-x C-s") 'bench-buffer--save-current-element)
    map))

(defun bench-buffer-setup-edit-get-next-or-prev-history (n)
  "Return the N element of `km-setup-edit--buffer-name'."
  (let* ((values bench-buffer--history)
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
  (when-let ((str (bench-buffer-setup-edit-get-next-or-prev-history -1)))
    (goto-char (point-min))
    (save-excursion
      (insert str))))

;;;###autoload
(defun bench-buffer-setup-edit-next-history-element ()
  "Insert next history content."
  (interactive)
  (erase-buffer)
  (when-let ((str (bench-buffer-setup-edit-get-next-or-prev-history 1)))
    (goto-char (point-min))
    (save-excursion
      (insert str))))

(defun bench-buffer--serialize (data filename)
  "Serialize DATA to FILENAME.

The saved data can be restored with `bench-buffer--unserialize'."
  (when (file-writable-p filename)
    (with-temp-file filename
      (insert (let (print-length)
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


(defun bench-buffer--ensure-history-size ()
  "Check history size."
  (setq bench-buffer--history
        (if (> (length bench-buffer--history) bench-buffer--history-max-size)
            (seq-take bench-buffer--history bench-buffer--history-max-size)
          bench-buffer--history)))

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
A filename can be opened with \\<igist-edit-buffer-default-keymap>\.
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

;;;###autoload
(defun bench-buffer-edit-buffer-edit-done ()
  "Run benchmarks and show results."
  (interactive)
  (let ((elem (buffer-substring-no-properties
               (point-min)
               (point-max))))
    (add-to-history 'bench-buffer--history elem
                    bench-buffer--history-max-size)
    (let ((reps (read-number "Repetions: ")))
      (with-temp-buffer
        (erase-buffer)
        (insert (format
                 "(bench-buffer-diff-funcs %d %s) (bench-buffer-print-result)"
                 reps elem))
        (eval-buffer)))))

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