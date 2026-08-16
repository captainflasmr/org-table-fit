;;; org-table-fit.el --- Fit org-mode tables to the window width -*- lexical-binding: t; -*-

;; Copyright (C) 2026 James Dyer

;; Author: James Dyer <james@dyerdwelling.family>
;; URL: https://github.com/captainflasmr/org-table-fit
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: org, tables, convenience

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:
;;
;; Rewrite the org-mode table at point so it fits within the current
;; window width, instead of overflowing the right edge (where
;; visual-line-mode soft-wraps each row and the alignment looks
;; corrupt).
;;
;;   M-x org-table-fit-window
;;
;; When a table is wider than the window, `org-table-fit-window'
;; word-wraps cell content across lines using org's native multi-line
;; cells, then re-aligns.  The result is a perfectly valid org table:
;; TAB navigation, formulas, font-lock and export all handle wrapped
;; cells.  The buffer is modified in place; `undo' (C-x u) restores
;; the original layout, or use:
;;
;;   M-x org-table-fit-unwrap
;;
;; which merges wrapped (continuation) rows back into single lines.
;; The usual workflow for re-fitting after a window resize is:
;; unwrap, then fit again.
;;
;; Column widths are computed like agent-shell's markdown table
;; renderer and markdown-table-wrap:
;;
;;   1. natural width  = widest cell in the column
;;   2. minimum width  = widest unbreakable word in the column
;;   3. when the natural total exceeds the target width, columns are
;;      shrunk proportionally towards their minimums, and
;;   4. cell text is wrapped at word boundaries; words longer than
;;      their column are force-broken so the table still fits.
;;
;; Cells that start with `=' (calc formulas) are never wrapped, since
;; formulas must stay on one line.  Width cookies (<N>) are preserved
;; and respected by `org-table-align' as usual.

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'seq)
(require 'org)

(defgroup org-table-fit nil
  "Fit org-mode tables to the window width."
  :group 'org
  :prefix "org-table-fit-")

(defcustom org-table-fit-width-fraction 0.95
  "Fraction of the window body width used as the fit target.
The whole fitted table (borders and padding included) is kept within
this fraction of the window, leaving room for the fringe and a scroll
margin."
  :type 'number
  :group 'org-table-fit)

(defcustom org-table-fit-min-column-width 1
  "Floor for column widths when the table cannot fit otherwise.
Columns never shrink below this, even when even the longest word
would not fit; such words are force-broken instead."
  :type 'integer
  :group 'org-table-fit)

;; -> width-measurement

(defun org-table-fit--longest-word-width (str)
  "Return display width of the longest unbreakable run in STR.

Line-breakable characters (category `|': CJK ideographs, kana,
Hangul, etc.) can wrap anywhere, so they each bound runs and
contribute only their own `char-width' — otherwise a
whitespace-free CJK sentence would count as one word and pin its
column at the full sentence width.

For example, \"foo bar\" yields 3 (\"foo\"), \"日本語\" yields 1,
and \"日本のfoo語\" yields 3 (\"foo\")."
  (let ((longest 0)
        (run 0)
        (i 0)
        (len (length str)))
    (while (< i len)
      (let* ((ch (aref str i))
             (breakable (or (memq ch '(?\s ?\t ?\n))
                            (aref (char-category-set ch) ?|))))
        (if breakable
            (setq longest (max longest run)
                  run 0)
          (setq run (+ run (char-width ch)))))
      (setq i (1+ i)))
    (max longest run)))

;; -> width-allocation

(defun org-table-fit--table-total-width (widths)
  "Total display width of an aligned org table with column WIDTHS.
Accounts for borders and padding (`| X | Y |' = 2 padding + 1 pipe
per column, plus one leading pipe)."
  (+ 1 (* 3 (length widths)) (seq-reduce #'+ widths 0)))

(defun org-table-fit--allocate-widths (natural min-widths target)
  "Shrink NATURAL-WIDTHS proportionally to fit TARGET, respecting MIN-WIDTHS.

MIN-WIDTHS holds each column's longest unbreakable word, the width
below which word-wrapping alone keeps content intact.  When even
those minimums cannot fit TARGET, columns shrink down to
`org-table-fit-min-column-width' and the cell wrapper hard-breaks
long words across lines, so the table still fits rather than
overflowing and line-wrapping as a whole.

Guarantees the resulting total width is <= TARGET (one column per
shrinkable column is trimmed at a time when the proportional
allocation rounds up)."
  (let* ((total (org-table-fit--table-total-width natural))
         (excess (- total target))
         (floors (if (> (org-table-fit--table-total-width min-widths) target)
                     (make-list (length min-widths)
                                org-table-fit-min-column-width)
                   min-widths)))
    (if (<= excess 0)
        natural
      (let* ((shrinkable (cl-mapcar (lambda (w m) (max 0 (- w m)))
                                    natural floors))
             (total-shrinkable (seq-reduce #'+ shrinkable 0))
             (widths (if (<= total-shrinkable 0)
                         floors
                       (let ((ratio (min 1.0 (/ (float excess)
                                                total-shrinkable))))
                         (cl-mapcar (lambda (w m s)
                                      (max m (floor (- w (* s ratio)))))
                                    natural floors shrinkable)))))
        ;; Trim the widest shrinkable column until the table fits.
        (while (and (> (org-table-fit--table-total-width widths) target)
                    (cl-some (lambda (i) (> (nth i widths) (nth i floors)))
                             (number-sequence 0 (1- (length widths)))))
          (let* ((i (cl-loop for k from 0 below (length widths)
                             when (> (nth k widths) (nth k floors))
                             maximize k into best
                             finally (return best)))
                 (w (nth i widths)))
            (setf (nth i widths) (1- w))))
        widths))))

;; -> text-wrapping

(defun org-table-fit--wrap-text (text width)
  "Wrap TEXT to fit within WIDTH display columns.

Word-wraps at whitespace; a word wider than WIDTH is force-broken
at character boundaries so no line exceeds WIDTH.  Returns a list of
line strings.  An empty TEXT returns a single empty line."
  (if (<= (string-width text) width)
      (list text)
    (let ((words (split-string text "[ \t]+" t))
          (lines nil)
          (cur ""))
      (dolist (word words)
        (if (> (string-width word) width)
            (progn
              (unless (string-empty-p cur)
                (push cur lines)
                (setq cur ""))
              (let ((pos 0)
                    (len (length word)))
                (while (< pos len)
                  (let ((end (min len (+ pos width))))
                    (while (and (> end pos)
                                (> (string-width (substring word pos end)) width))
                      (setq end (1- end)))
                    (when (= end pos)
                      (setq end (min len (1+ pos))))
                    (push (substring word pos end) lines)
                    (setq pos end)))))
          (if (string-empty-p cur)
              (setq cur word)
            (if (<= (+ (string-width cur) 1 (string-width word)) width)
                (setq cur (concat cur " " word))
              (push cur lines)
              (setq cur word)))))
      (unless (string-empty-p cur)
        (push cur lines))
      (or (nreverse lines) (list "")))))

(defun org-table-fit--wrap-row (cells widths)
  "Wrap CELLS to the column WIDTHS, returning physical row cell-lists.

A wrapped cell continues on following rows as org multi-line cells:
continuation rows carry the remaining wrapped text in the same column
and empty strings elsewhere.  Cells whose text starts with `='
\(calc formulas) are never wrapped."
  (let* ((wrapped (cl-mapcar (lambda (cell width)
                               (if (or (null cell) (string-empty-p cell)
                                       (string-prefix-p "=" cell))
                                   (list cell)
                                 (org-table-fit--wrap-text cell width)))
                             cells widths))
         (max-lines (apply #'max 1 (mapcar #'length wrapped))))
    (cl-loop for line-idx below max-lines
             collect (mapcar (lambda (cell-lines)
                               (if (< line-idx (length cell-lines))
                                   (nth line-idx cell-lines)
                                 ""))
                             wrapped))))

;; -> table-io

(defun org-table-fit--split-row (line)
  "Split an org table LINE into trimmed cell strings.
A literal `|' in a cell is written `\\vert' in org, so no escape
handling is needed.  Empty cells (including continuation-row
padding) are preserved."
  (let ((cells (split-string (string-trim line) "[ \t]*|[ \t]*")))
    ;; The leading and trailing bounding pipes produce empty elements.
    (when (and cells (string-empty-p (car cells)))
      (setq cells (cdr cells)))
    (when (and cells (string-empty-p (car (last cells))))
      (setq cells (butlast cells)))
    cells))

(defun org-table-fit--collect-rows (beg end)
  "Collect the org table rows between BEG and END.
Each row is (:cells CELLS) or (:hline t)."
  (let ((rows nil))
    (save-excursion
      (goto-char beg)
      (while (< (point) end)
        (let ((line (buffer-substring-no-properties
                     (line-beginning-position) (line-end-position))))
          (push (if (string-match-p org-table-hline-regexp line)
                    '(:hline t)
                  (list :cells (org-table-fit--split-row line)))
                rows))
        (forward-line 1)))
    (nreverse rows)))

(defun org-table-fit--merge-continuations (rows)
  "Merge continuation rows in ROWS into logical rows.

Within a logical row, the set of non-empty columns can only shrink
(columns finish and go empty).  When a previously-empty column
reappears with content, a new logical row has started.  Content from
continuation cells is joined with a single space.  Hline rows end the
current logical row and are preserved in the result.

The heuristic is ambiguous for tables that merely have consecutive
rows with empty cells, so only call this on tables tagged
`org-table-fit-wrapped' (see `org-table-fit--wrapped-table-p')."
  (let ((logical nil)
        (current nil)
        (prev-non-empty nil)
        (num-cols 0))
    (dolist (row rows)
      (if (eq (car row) :hline)
          (progn
            (when current
              (push current logical))
            (push row logical)
            (setq current nil prev-non-empty nil))
        (let* ((cells (map-elt row :cells))
               (non-empty (cl-loop for c in cells
                                   for i from 0
                                   unless (string-empty-p c)
                                   collect i)))
          (setq num-cols (max num-cols (length cells)))
          (let ((continuation (and prev-non-empty
                                   (cl-every (lambda (i) (memq i prev-non-empty))
                                             non-empty))))
            (if continuation
                (progn
                  (unless current
                    (setq current (make-list (length cells) "")))
                  (cl-loop for c in cells
                           for i from 0
                           unless (string-empty-p c)
                           do (setf (nth i current)
                                    (string-trim-right
                                     (concat (nth i current) " " c)))))
              (when current
                (push current logical))
              (setq current (copy-sequence cells))))
          (setq prev-non-empty non-empty))))
    (when current
      (push current logical))
    (nreverse
     (cl-loop for row in logical
              collect (if (eq (car row) :hline)
                          row
                        (list :cells (append row
                                             (make-list (max 0 (- num-cols
                                                                  (length row)))
                                                        ""))))))))

(defun org-table-fit--wrapped-table-p (beg end)
  "Return non-nil when the table between BEG and END was wrapped by us."
  (text-property-any beg end 'org-table-fit-wrapped t))

(defun org-table-fit--render-rows (rows indent)
  "Render ROWS as org table lines prefixed with INDENT.
Returns a string ending with a newline."
  (mapconcat
   (lambda (row)
     (if (eq (car row) :hline)
         (concat indent "|-")
       (concat indent "| " (mapconcat #'identity (map-elt row :cells) " | ")
               " |")))
   rows
   "\n"))

(defun org-table-fit--table-bounds ()
  "Return (BEG . END) for the table at point, or nil if not in a table."
  (when (org-at-table-p)
    (cons (org-table-begin) (org-table-end))))

(defun org-table-fit--table-indent (beg)
  "Return the whitespace indenting the table line at BEG."
  (save-excursion
    (goto-char beg)
    (when (looking-at "[ \t]*")
      (buffer-substring-no-properties (match-beginning 0) (match-end 0)))))

;; -> commands

;;;###autoload
(defun org-table-fit-window (&optional width)
  "Fit the org table at point to the current window width.

When WIDTH (a numeric prefix argument) is given and is at least 10,
fit to WIDTH columns instead.  If the table was already wrapped by
`org-table-fit-window' in this session, continuation rows are merged
first and the table is re-wrapped at the new width.

The table is rewritten in place using org's native multi-line cells;
`undo' restores the previous layout.  After reloading the file, run
`org-table-fit-unwrap' before re-fitting."
  (interactive "P")
  (let* ((target (if (and (integerp width) (>= width 10))
                     width
                   (floor (* (window-body-width)
                             org-table-fit-width-fraction))))
         (bounds (org-table-fit--table-bounds)))
    (unless bounds
      (user-error "Not in an org table"))
    (let* ((beg (car bounds))
           (end (cdr bounds))
           (indent (org-table-fit--table-indent beg))
           (rows (org-table-fit--collect-rows beg end)))
      ;; Merge continuation rows from a previous fit of this buffer.
      (when (org-table-fit--wrapped-table-p beg end)
        (setq rows (org-table-fit--merge-continuations rows)))
      (let* ((data-rows (cl-remove-if (lambda (r) (eq (car r) :hline)) rows))
             (cell-count (apply #'max 1 (mapcar (lambda (r)
                                                  (length (map-elt r :cells)))
                                                data-rows)))
             (natural (make-list cell-count 0))
             (minimum (make-list cell-count 0)))
        ;; Compute per-column natural and minimum widths.
        (dolist (row data-rows)
          (cl-loop for cell in (map-elt row :cells)
                   for i from 0
                   do (setf (nth i natural)
                            (max (nth i natural) (string-width cell))
                            (nth i minimum)
                            (max (nth i minimum)
                                 (org-table-fit--longest-word-width cell)))))
        (if (<= (org-table-fit--table-total-width natural) target)
            (message "org-table-fit: table already fits (%d <= %d columns)"
                     (org-table-fit--table-total-width natural) target)
          (let ((widths (org-table-fit--allocate-widths
                         natural minimum target)))
            (let ((wrapped-rows
                   (cl-loop for row in rows
                            append
                            (if (eq (car row) :hline)
                                (list row)
                              (let* ((cells (map-elt row :cells))
                                     (padded (append cells
                                                     (make-list
                                                      (max 0 (- cell-count
                                                                (length cells)))
                                                      "")))
                                     (physical (org-table-fit--wrap-row
                                                padded widths)))
                                (mapcar (lambda (prow) (list :cells prow))
                                        physical))))))
              (delete-region beg end)
              (goto-char beg)
              (insert (org-table-fit--render-rows wrapped-rows indent))
              (let ((new-end (point)))
                (put-text-property beg new-end 'org-table-fit-wrapped t))
              (org-table-align)
              (message "org-table-fit: wrapped table to %d columns"
                       (org-table-fit--table-total-width widths)))))))))

;;;###autoload
(defun org-table-fit-unwrap ()
  "Merge wrapped (continuation) rows in the table at point.

Each continuation row's cells are joined onto the row above, and the
table is re-aligned.  Useful before re-fitting at a different width
after the file was reloaded, or to restore the original single-line
layout.  Note that `undo' restores the exact original text."
  (interactive)
  (let ((bounds (org-table-fit--table-bounds)))
    (unless bounds
      (user-error "Not in an org table"))
    (let* ((beg (car bounds))
           (end (cdr bounds))
           (indent (org-table-fit--table-indent beg))
           (rows (org-table-fit--collect-rows beg end))
           (replacement (org-table-fit--merge-continuations rows)))
      (delete-region beg end)
      (goto-char beg)
      (insert (org-table-fit--render-rows replacement indent))
      (org-table-align)
      (remove-text-properties beg (point) '(org-table-fit-wrapped t))
      (message "org-table-fit: unwrapped table"))))

(provide 'org-table-fit)

;;; org-table-fit.el ends here
