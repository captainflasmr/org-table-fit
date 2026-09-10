;;; org-table-fit.el --- Fit org-mode tables to the window width -*- lexical-binding: t; -*-

;; Copyright (C) 2026 James Dyer

;; Author: James Dyer <james@dyerdwelling.family>
;; URL: https://github.com/captainflasmr/org-table-fit
;; Version: 0.1.4
;; Package-Requires: ((emacs "29.1"))
;; Keywords: org, tables, convenience

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'seq)
(require 'subr-x)
(require 'org)

(defgroup org-table-fit nil
  "Fit org-mode tables to the window width."
  :group 'org
  :prefix "org-table-fit-")

(defvar org-table-fit-mode nil
  "Non-nil when `org-table-fit-mode' is enabled.")

(defvar org-table-fit-overlay-mode nil
  "Non-nil when `org-table-fit-overlay-mode' is enabled.")

(defvar-local org-table-fit--resize-timer nil
  "Debounced timer for scheduling a table refit.")

(defcustom org-table-fit-width-fraction 0.95
  "Fraction of the window body width used as the fit target."
  :type 'number
  :group 'org-table-fit)

(defcustom org-table-fit-min-column-width 1
  "Floor for column widths when the table cannot fit otherwise."
  :type 'integer
  :group 'org-table-fit)

(defcustom org-table-fit-overlay-reveal-on-point t
  "When non-nil, reveal a table's source while point is inside it.
Mirrors `org-table-widget-reveal-on-point': the overlay returns as
soon as point leaves the table."
  :type 'boolean
  :group 'org-table-fit)

(defcustom org-table-fit-overlay-relayout-delay 0.15
  "Idle seconds to wait before redisplaying overlays after resize/edits.
A value of zero or less means redisplay immediately."
  :type 'number
  :group 'org-table-fit)

(defvar-local org-table-fit--display-overlays nil
  "Overlays displaying fitted tables in the current buffer.")

(defvar-local org-table-fit--display-width nil
  "Window width in columns the display overlays were last laid out for.")

(defvar-local org-table-fit--display-timer nil
  "Idle timer for a pending overlay relayout.")

(defvar-local org-table-fit--display-inside nil
  "Non-nil while point is inside a revealed (overlay-hidden) table.")

(defvar-local org-table-fit--display-previous-point nil
  "Marker recording point before the current command.")

(defvar-local org-table-fit--keep-display-once nil
  "When non-nil, keep display overlays for the next post-command.
Set by the row-stepping commands so traversing a rendered table does
not reveal its source even when reveal-on-point is enabled.
Consumed (reset to nil) by `org-table-fit--display-post-command'.")

;; -> fit-planning (shared by replace and overlay paths)

(defun org-table-fit--target-width (&optional width)
  "Resolve WIDTH to a fit target in columns.
A numeric WIDTH >= 10 is used directly; otherwise use the current
window body width scaled by `org-table-fit-width-fraction'.
The body width is measured in characters of the buffer's (possibly
remapped, e.g. text-scaled) default face, so a font scale change
adjusts the target."
  (if (and (integerp width) (>= width 10))
      width
    (floor (* (window-body-width nil 'remap)
              org-table-fit-width-fraction))))

;; -> width-measurement

(defun org-table-fit--longest-word-width (str)
  "Return display width of the longest unbreakable run in STR."
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
Accepts either a list or a vector of widths."
  (let ((sum 0))
    (if (vectorp widths)
        (dotimes (i (length widths))
          (cl-incf sum (aref widths i)))
      (dolist (w widths)
        (cl-incf sum w)))
    (+ 1 (* 3 (length widths)) sum)))

(defun org-table-fit--allocate-widths (natural min-widths target)
  "Shrink NATURAL widths proportionally to fit TARGET using vector operations."
  (let* ((len (length natural))
         (vec-nat (vconcat natural))
         (vec-min (vconcat min-widths))
         (tot-nat (org-table-fit--table-total-width vec-nat)))
    (if (<= tot-nat target)
        natural
      (let* ((tot-min (org-table-fit--table-total-width vec-min))
             (floors (if (> tot-min target)
                         (make-vector len org-table-fit-min-column-width)
                       vec-min))
             (shrinkable (make-vector len 0))
             (tot-shrinkable 0))
        (dotimes (i len)
          (let ((s (max 0 (- (aref vec-nat i) (aref floors i)))))
            (aset shrinkable i s)
            (cl-incf tot-shrinkable s)))
        (let ((result (make-vector len 0)))
          (if (<= tot-shrinkable 0)
              (setq result floors)
            (let* ((excess (- tot-nat target))
                   (ratio (min 1.0 (/ (float excess) tot-shrinkable))))
              (dotimes (i len)
                (aset result i (max (aref floors i)
                                    (floor (- (aref vec-nat i) (* (aref shrinkable i) ratio))))))))
          ;; Trim excess single columns directly on vector
          (let ((current-tot (org-table-fit--table-total-width result)))
            (while (and (> current-tot target)
                        (cl-loop for i below len thereis (> (aref result i) (aref floors i))))
              (let ((best-idx -1)
                    (max-w -1))
                (dotimes (i len)
                  (when (and (> (aref result i) (aref floors i))
                             (> (aref result i) max-w))
                    (setq max-w (aref result i)
                          best-idx i)))
                (when (>= best-idx 0)
                  (aset result best-idx (1- (aref result best-idx)))
                  (cl-decf current-tot)))))
          (append result nil))))))

;; -> text-wrapping

(defconst org-table-fit--inline-markup-regexp
  (rx (or (seq "=" (group (one-or-more (not (any "=" "\n")))) "=")
          (seq "~" (group (one-or-more (not (any "~" "\n")))) "~")
          (seq "[[" (group (one-or-more (not (any "]" "\n"))))
               (optional (seq "[" (zero-or-more (not (any "]" "\n"))) "]"))
               "]]")))
  "Match an org inline markup span that must stay intact while wrapping.")

(defun org-table-fit--tokenize (text)
  "Split TEXT into wrap tokens, keeping org inline markup spans whole."
  (let ((tokens nil)
        (pos 0)
        (len (length text)))
    (while (< pos len)
      (let* ((span-info (save-match-data
                          (when (string-match
                                 org-table-fit--inline-markup-regexp text pos)
                            (cons (match-beginning 0) (match-end 0)))))
             (ws-info (save-match-data
                        (when (string-match "[ \t]+" text pos)
                          (cons (match-beginning 0) (match-end 0)))))
             (span (car span-info)))
        (cond
         ((eq span pos)
          (let ((end (cdr span-info)))
            (while (and (< end len)
                        (let ((ch (aref text end)))
                          (and (not (memq ch '(?\s ?\t ?= ?~ ?\[ ?* ?_ ?/ ?+)))
                               (< (char-width ch) 2))))
              (setq end (1+ end)))
            (push (substring text pos end) tokens)
            (setq pos end)))
         ((and ws-info (eq (car ws-info) pos))
          (setq pos (cdr ws-info)))
         (t
          (let ((end (min (or span len) (or (car ws-info) len))))
            (push (substring text pos end) tokens)
            (setq pos end))))))
    (nreverse tokens)))

(defun org-table-fit--wrap-text (text width)
  "Wrap TEXT to fit within WIDTH display columns."
  (if (<= (string-width text) width)
      (list text)
    (let ((words (org-table-fit--tokenize text))
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
  "Wrap CELLS to the column WIDTHS."
  (let* ((wrapped (cl-mapcar (lambda (cell width)
                               (if (or (null cell) (string-empty-p cell)
                                       (string-prefix-p ":=" cell))
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

(defun org-table-fit--logical-rows (beg end rows)
  "Return logical ROWS for the table between BEG and END.
If the region was wrapped by this package, merge continuation rows
using the stored row-start tags; otherwise return ROWS unchanged."
  (if (org-table-fit--wrapped-table-p beg end)
      (org-table-fit--merge-continuations
       rows (org-table-fit--collect-row-starts beg end))
    rows))

(defun org-table-fit--column-count (rows)
  "Return the number of columns spanned by the data rows in ROWS."
  (let ((data (cl-remove-if (lambda (r) (eq (car r) :hline)) rows)))
    (apply #'max 1 (mapcar (lambda (r) (length (map-elt r :cells))) data))))

(defun org-table-fit--natural-minimum-widths (data-rows cell-count)
  "Return (NATURAL MINIMUM) column widths for DATA-ROWS."
  (let ((natural (make-list cell-count 0))
        (minimum (make-list cell-count 0)))
    (dolist (row data-rows)
      (cl-loop for cell in (map-elt row :cells)
               for i from 0
               do (setf (nth i natural)
                        (max (nth i natural) (string-width cell))
                        (nth i minimum)
                        (max (nth i minimum)
                             (org-table-fit--longest-word-width cell)))))
    (list natural minimum)))

(defun org-table-fit--wrap-all-rows (rows cell-count widths)
  "Wrap ROWS to WIDTHS.  Return (WRAPPED-ROWS ROW-STARTS).
ROW-STARTS marks the first physical line of each logical row and is
stored as text properties by the destructive path so unwrapping can
recover exact rows."
  (let* ((row-starts nil)
         (wrapped
          (cl-loop for row in rows
                  append
                  (if (eq (car row) :hline)
                      (progn (push nil row-starts) (list row))
                    (let* ((cells (map-elt row :cells))
                           (padded (append cells
                                           (make-list
                                            (max 0 (- cell-count (length cells)))
                                            "")))
                           (physical (org-table-fit--wrap-row padded widths)))
                      (cl-loop for i from 0 below (length physical)
                               do (push (= i 0) row-starts))
                      (mapcar (lambda (prow) (list :cells prow)) physical))))))
    (list wrapped (nreverse row-starts))))

;; -> table-io

(defun org-table-fit--split-row (line)
  "Split an org table LINE into trimmed cell strings."
  (let ((cells (split-string (string-trim line) "[ \t]*|[ \t]*")))
    (when (and cells (string-empty-p (car cells)))
      (setq cells (cdr cells)))
    (when (and cells (string-empty-p (car (last cells))))
      (setq cells (butlast cells)))
    cells))

(defun org-table-fit--collect-rows (beg end)
  "Collect table rows from buffer between BEG and END."
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

(defun org-table-fit--merge-continuations (rows &optional row-starts)
  "Merge continuation rows in ROWS into logical rows."
  (let ((logical nil)
        (current nil)
        (prev-non-empty nil)
        (num-cols 0)
        (idx -1))
    (dolist (row rows)
      (setq idx (1+ idx))
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
                                   collect i))
               (continuation (and (not (and row-starts
                                            (nth idx row-starts)))
                                  prev-non-empty
                                  (cl-every (lambda (i) (memq i prev-non-empty))
                                            non-empty))))
          (setq num-cols (max num-cols (length cells)))
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
            (setq current (copy-sequence cells)))
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
  (text-property-any beg end 'org-table-fit-wrapped t))

(defun org-table-fit--collect-row-starts (beg end)
  (let ((starts nil))
    (save-excursion
      (goto-char beg)
      (while (< (point) end)
        (push (get-text-property (point) 'org-table-fit-row-start) starts)
        (forward-line 1)))
    (nreverse starts)))

(defun org-table-fit--tag-row-starts (beg row-starts)
  (save-excursion
    (goto-char beg)
    (dolist (start row-starts)
      (when start
        (put-text-property (point) (1+ (point)) 'org-table-fit-row-start t))
      (forward-line 1))))

(defun org-table-fit--render-rule (widths indent)
  "Render a horizontal rule for WIDTHS prefixed with INDENT."
  (concat indent "|"
          (mapconcat (lambda (w) (make-string (+ w 2) ?-))
                     (vconcat widths)
                     "+")
          "|"))

(defun org-table-fit--render-data-row (cells widths indent)
  "Render CELLS as one aligned Org table line padded to WIDTHS."
  (let ((vec-widths (vconcat widths)))
    (let ((padded-cells
           (cl-loop for i below (length vec-widths)
                    for cell = (or (nth i cells) "")
                    for w = (aref vec-widths i)
                    for pad = (max 0 (- w (string-width cell)))
                    collect (concat cell (make-string pad ?\s)))))
      (concat indent "| " (mapconcat #'identity padded-cells " | ") " |"))))

(defun org-table-fit--render-rows (rows widths indent)
  "Render ROWS as aligned Org table lines padded to WIDTHS and prefixed with INDENT."
  (mapconcat
   (lambda (row)
     (if (eq (car row) :hline)
         (org-table-fit--render-rule widths indent)
       (org-table-fit--render-data-row (map-elt row :cells) widths indent)))
   rows
   "\n"))

(defun org-table-fit--chunk-wrapped (rows wrapped row-starts widths indent)
  "Group flat WRAPPED lines back under their logical ROWS.
ROW-STARTS marks the first physical line of each logical row (see
`org-table-fit--wrap-all-rows').  Returns a list with one (KIND .
LINES) entry per logical row, where KIND is `hline' or `data' and
LINES holds that row's rendered strings."
  (let ((chunks nil)
        (p 0)
        (n (length wrapped)))
    (dolist (row rows)
      (if (eq (car row) :hline)
          (progn
            (push (cons 'hline (list (org-table-fit--render-rule widths indent)))
                  chunks)
            (setq p (1+ p)))
        (let ((h 1))
          (while (and (< (+ p h) n)
                      (not (eq (car (nth (+ p h) wrapped)) :hline))
                      (not (nth (+ p h) row-starts)))
            (setq h (1+ h)))
          (push (cons 'data
                      (cl-loop for j from p below (+ p h)
                               collect (org-table-fit--render-data-row
                                        (map-elt (nth j wrapped) :cells)
                                        widths indent)))
                chunks)
          (setq p (+ p h)))))
    (nreverse chunks)))

(defun org-table-fit--remove-display-overlay (overlay)
  "Delete display OVERLAY and forget it."
  (delete-overlay overlay)
  (setq org-table-fit--display-overlays
        (delq overlay org-table-fit--display-overlays)))

(defun org-table-fit--clear-display-overlays ()
  "Remove every display overlay from the current buffer."
  (mapc #'delete-overlay org-table-fit--display-overlays)
  (setq org-table-fit--display-overlays nil))

(defun org-table-fit--overlay-at (position)
  "Return the display overlay covering POSITION, or nil.
A row overlay typically ends at the line's end, so POSITION on that
line counts as covered even though it is not strictly inside: use
the overlay that covers the line, found via the line beginning."
  (seq-find (lambda (overlay) (overlay-get overlay 'org-table-fit-display))
            (overlays-in
             (save-excursion (goto-char position)
                             (min (point) (line-beginning-position)))
             (1+ position))))

(defun org-table-fit--remove-display-overlays-in (beg end)
  "Remove display overlays intersecting BEG..END without scheduling."
  (dolist (overlay (overlays-in beg end))
    (when (overlay-get overlay 'org-table-fit-display)
      (org-table-fit--remove-display-overlay overlay))))

(defun org-table-fit--display-hold-point ()
  "Keep point on a displayed row from being pushed to the row end.
Redisplay's point adjustment treats the overlays' empty `display'
property as intangible and moves point out of the hidden source.
Setting `disable-point-adjustment' suppresses that for the current
command, so a landing at the start of a row survives redisplay and
the row's `cursor' property shows the cursor at the rendered row's
left edge.  Overlays without a `cursor' anchor (hidden continuation
rows) are left to the normal adjustment."
  (let* ((overlay (org-table-fit--overlay-at (point)))
         (before (and overlay (overlay-get overlay 'before-string))))
    (when (and (stringp before)
               (> (length before) 0)
               (not (eq (aref before 0) ?\n))
               (get-text-property 0 'cursor before))
      (setq disable-point-adjustment t))))

(defun org-table-fit--replace-table (beg end rendered wrapped row-starts)
  "Replace table in BEG..END with RENDERED, preserving point relative to table start."
  (org-table-fit--remove-display-overlays-in beg end)
  (let* ((pt (point))
         (inside (and (>= pt beg) (< pt end)))
         (line-offset (when inside
                        (count-lines beg (line-beginning-position))))
         (col-offset (when inside
                       (current-column))))
    (atomic-change-group
      (save-excursion
        (goto-char beg)
        (delete-region beg end)
        (let ((start (point)))
          (insert rendered)
          (if wrapped
              (progn
                (put-text-property start (point) 'org-table-fit-wrapped t)
                (org-table-fit--tag-row-starts start row-starts))
            (remove-text-properties start (point)
                                    '(org-table-fit-wrapped t
                                      org-table-fit-row-start t))))))
    (when inside
      (goto-char beg)
      (forward-line line-offset)
      (move-to-column col-offset))))

(defun org-table-fit--table-bounds ()
  (save-excursion
    (let ((pos (point)))
      (or (when (org-at-table-p)
            (cons (org-table-begin) (org-table-end)))
          (progn
            (goto-char pos)
            (when (re-search-backward "^|" nil t)
              (when (org-at-table-p)
                (cons (org-table-begin) (org-table-end)))))
          (progn
            (goto-char pos)
            (when (re-search-forward "^|" nil t)
              (when (org-at-table-p)
                (cons (org-table-begin) (org-table-end)))))))))

(defun org-table-fit--table-indent (beg)
  (save-excursion
    (goto-char beg)
    (when (looking-at "[ \t]*")
      (buffer-substring-no-properties (match-beginning 0) (match-end 0)))))

(defun org-table-fit--after-change-or-window-size (&optional frame-or-window)
  "Queue a refit for the table when the window size changes."
  (when org-table-fit-mode
    (let* ((win (if (windowp frame-or-window)
                    frame-or-window
                  (selected-window)))
           (buf (current-buffer)))
      (when org-table-fit--resize-timer
        (cancel-timer org-table-fit--resize-timer))
      (setq org-table-fit--resize-timer
            (run-with-timer
             0.05 nil
             (lambda ()
               (setq org-table-fit--resize-timer nil)
               (when (and (buffer-live-p buf)
                          (window-live-p win)
                          (eq (window-buffer win) buf))
                 (with-selected-window win
                   (with-current-buffer buf
                     (when (and org-table-fit-mode
                                (org-at-table-p))
                       (org-table-fit-window)))))))))))

;;;###autoload
(define-minor-mode org-table-fit-mode
  "Automatically fit the current Org table to the current window width."
  :lighter " OrgFit"
  :global nil
  (if org-table-fit-mode
      (progn
        (add-hook 'window-size-change-functions #'org-table-fit--after-change-or-window-size nil t)
        (add-hook 'text-scale-mode-hook #'org-table-fit--after-change-or-window-size nil t)
        (org-table-fit--after-change-or-window-size))
    (when org-table-fit--resize-timer
      (cancel-timer org-table-fit--resize-timer)
      (setq org-table-fit--resize-timer nil))
    (remove-hook 'window-size-change-functions #'org-table-fit--after-change-or-window-size t)
    (remove-hook 'text-scale-mode-hook #'org-table-fit--after-change-or-window-size t)))

;; -> commands

;;;###autoload
(defun org-table-fit-window (&optional width)
  "Fit the org table at point to the current window width."
  (interactive "P")
  (let* ((target (org-table-fit--target-width width))
         (bounds (org-table-fit--table-bounds)))
    (unless bounds
      (user-error "Not in an org table"))
    (let* ((beg (car bounds))
           (end (cdr bounds))
           (indent (org-table-fit--table-indent beg))
           (rows (org-table-fit--logical-rows
                  beg end (org-table-fit--collect-rows beg end))))
      (let* ((data-rows (cl-remove-if (lambda (r) (eq (car r) :hline)) rows))
             (cell-count (org-table-fit--column-count rows))
             (measured (org-table-fit--natural-minimum-widths
                        data-rows cell-count))
             (natural (nth 0 measured))
             (minimum (nth 1 measured)))
        (if (<= (org-table-fit--table-total-width natural) target)
            (message "org-table-fit: table already fits (%d <= %d columns)"
                     (org-table-fit--table-total-width natural) target)
          (let* ((widths (org-table-fit--allocate-widths natural minimum target))
                 (wrapped (org-table-fit--wrap-all-rows rows cell-count widths))
                 (wrapped-rows (nth 0 wrapped))
                 (row-starts (nth 1 wrapped))
                 (rendered (concat (org-table-fit--render-rows
                                    wrapped-rows widths indent)
                                   "\n")))
            (org-table-fit--replace-table beg end rendered t row-starts)
            (message "org-table-fit: wrapped table to %d columns"
                     (org-table-fit--table-total-width widths))))))))

;;;###autoload
(defun org-table-fit-unwrap ()
  "Merge wrapped (continuation) rows in the table at point."
  (interactive)
  (let ((bounds (org-table-fit--table-bounds)))
    (unless bounds
      (user-error "Not in an org table"))
    (let* ((beg (car bounds))
           (end (cdr bounds))
           (indent (org-table-fit--table-indent beg))
           (rows (org-table-fit--collect-rows beg end))
           (row-starts (when (org-table-fit--wrapped-table-p beg end)
                         (org-table-fit--collect-row-starts beg end)))
           (replacement (org-table-fit--merge-continuations rows row-starts))
           (data-rows (cl-remove-if (lambda (r) (eq (car r) :hline)) replacement))
           (cell-count (apply #'max 1 (mapcar (lambda (r)
                                                (length (map-elt r :cells)))
                                              data-rows)))
           (widths (make-list cell-count 0)))
      (dolist (row data-rows)
        (cl-loop for cell in (map-elt row :cells)
                 for i from 0
                 do (setf (nth i widths)
                          (max (nth i widths) (string-width cell)))))
      (let ((rendered (concat (org-table-fit--render-rows replacement widths indent) "\n")))
        (org-table-fit--replace-table beg end rendered nil nil)
        (message "org-table-fit: unwrapped table")))))

;; -> overlay display (non-destructive, cf. org-table-widget)
;; The buffer text is never modified.  Each source line of a fitted
;; table gets its own overlay whose `before-string' holds that line's
;; wrapped rendering, so export, formulas and Babel keep seeing the
;; original table.  Every row overlay carries an integer `cursor'
;; property covering exactly its source line, so when
;; `org-table-fit-overlay-reveal-on-point' is nil and point traverses
;; the hidden source, the cursor follows the matching rendered row.
;; With reveal enabled (the default), moving point into a table
;; removes its overlays and reveals the source for ordinary
;; `org-table' editing; moving point out lays it out again.

(defun org-table-fit--tables ()
  "Return (BEG . END) for every Org table in the accessible buffer."
  (let (tables)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward org-table-any-line-regexp nil t)
        (if (org-at-table-p)
            (let ((beg (org-table-begin))
                  (end (org-table-end)))
              (push (cons beg end) tables)
              (goto-char end))
          (forward-line 1))))
    (nreverse tables)))

(defun org-table-fit--display-layout-width ()
  "Return the window body width available for display overlays.
Widths are counted in characters of the buffer's (possibly remapped,
e.g. text-scaled) default face, so a font scale change adjusts the
layout.  Uses the narrowest window showing the buffer, falling back
to the selected window so commands also work in batch buffers."
  (let ((windows (get-buffer-window-list (current-buffer) nil t)))
    (if windows
        (apply #'min (mapcar (lambda (window)
                               (window-body-width window 'remap))
                             windows))
      (window-body-width nil 'remap))))

(defun org-table-fit--display-table-bounds-at (position)
  "Return the table (BEG . END) containing POSITION, or nil."
  (save-excursion
    (goto-char position)
    (org-table-fit--table-bounds)))

(defun org-table-fit--display-last-line-bol (end)
  "Return the beginning of the last source line ending at END."
  (save-excursion
    (goto-char end)
    (when (bolp)
      (forward-line -1))
    (line-beginning-position)))

(defun org-table-fit--display-make-row-overlay (beg end before)
  "Cover the source line BEG..END with BEFORE as its rendering.
BEFORE is shown as the overlay's `before-string' while `display \"\"'
hides the source.  Its first character gets an integer `cursor'
property spanning exactly this overlay, so the display engine shows
the cursor on this row while point traverses the hidden line.
Returns the overlay."
  (let ((overlay (make-overlay beg end nil t nil)))
    (overlay-put overlay 'org-table-fit-display t)
    ;; Unlike replacement strings, before-strings honor pixel spaces
    ;; and keep the source positions intact for editing/export.
    (overlay-put overlay 'display "")
    (when (and (> (length before) 0)
                 (not (eq (aref before 0) ?\n)))
      ;; No `cursor' on newlines: they have no graphic representation
      ;; for the engine to find.  Renderings always start with indent
      ;; or a border, so the first character is a safe anchor.
      (put-text-property 0 1 'cursor (- end beg) before))
    (overlay-put overlay 'before-string before)
    (overlay-put overlay 'evaporate t)
    (overlay-put overlay 'modification-hooks
                 (list #'org-table-fit--display-modified))
    (overlay-put overlay 'insert-in-front-hooks
                 (list #'org-table-fit--display-modified))
    (push overlay org-table-fit--display-overlays)
    overlay))

(defun org-table-fit--display-modified (overlay &rest _)
  "Reveal the source when the table under OVERLAY is modified."
  (when (overlay-buffer overlay)
    (with-current-buffer (overlay-buffer overlay)
      (let ((bounds (save-excursion
                      (goto-char (overlay-start overlay))
                      (org-table-fit--table-bounds))))
        (if bounds
            (org-table-fit--remove-display-overlays-in
             (car bounds) (cdr bounds))
          (org-table-fit--remove-display-overlay overlay)))
      (org-table-fit--display-schedule))))

(defun org-table-fit--display-table-in (beg end target)
  "Cover each source line of the table between BEG and END with overlays.
TARGET is the fit width in columns.  Returns the list of overlays,
or nil when the table already fits TARGET."
  (org-table-fit--remove-display-overlays-in beg end)
  (let* ((raw (org-table-fit--collect-rows beg end))
         (wrapped-source-p (org-table-fit--wrapped-table-p beg end))
         (starts (if wrapped-source-p
                     (org-table-fit--collect-row-starts beg end)
                   (mapcar (lambda (row)
                             (if (eq (car row) :hline) nil t))
                           raw)))
         (rows (if wrapped-source-p
                   (org-table-fit--merge-continuations raw starts)
                 raw))
         (data-rows (cl-remove-if (lambda (r) (eq (car r) :hline)) rows)))
    (when data-rows
      (let* ((cell-count (org-table-fit--column-count rows))
             (measured (org-table-fit--natural-minimum-widths
                        data-rows cell-count))
             (natural (nth 0 measured))
             (minimum (nth 1 measured)))
        (unless (<= (org-table-fit--table-total-width natural) target)
          (let* ((widths (org-table-fit--allocate-widths
                          natural minimum target))
                 (wrapped (org-table-fit--wrap-all-rows rows cell-count widths))
                 (chunks (org-table-fit--chunk-wrapped
                          rows (nth 0 wrapped) (nth 1 wrapped)
                          widths (org-table-fit--table-indent beg)))
                 (data-chunks (seq-filter (lambda (chunk)
                                            (eq (car chunk) 'data))
                                          chunks))
                 (overlays nil))
            ;; RAW has one entry per source line, so walk the buffer
            ;; lines in lockstep.  Newline accounting: a row whose
            ;; range hides its newline terminates its rendering with
            ;; "\n" so following text starts on a fresh line; any
            ;; other row leaves its visible source newline to do
            ;; that.  Continuation lines of a previously wrapped
            ;; table hide along with their newline and show nothing.
            (save-excursion
              (goto-char beg)
              (cl-loop for rawrow in raw
                       for start in starts
                       do (let* ((lb (line-beginning-position))
                                 (le (line-end-position))
                                 (next (save-excursion (forward-line 1) (point)))
                                 (last (save-excursion (forward-line 1)
                                                       (>= (point) end)))
                                 (extended (and last (> end le)))
                                 (ov-end (if extended end le))
                                 (terminator (if extended "\n" "")))
                            (cond
                             ((eq (car rawrow) :hline)
                              (push (org-table-fit--display-make-row-overlay
                                     lb ov-end
                                     (concat (org-table-fit--render-rule
                                              widths (org-table-fit--table-indent beg))
                                             terminator))
                                    overlays))
                             (start
                              (let* ((chunk (and data-chunks (pop data-chunks)))
                                     (lines (or (cdr chunk) (list ""))))
                                (push (org-table-fit--display-make-row-overlay
                                       lb ov-end
                                       (concat (mapconcat #'identity lines "\n")
                                               terminator))
                                      overlays)))
                             ((< lb next)
                              (push (org-table-fit--display-make-row-overlay
                                     lb next "")
                                    overlays))))
                       (forward-line 1)))
            (nreverse overlays)))))))

;;;###autoload
(defun org-table-fit-display-table (&optional width)
  "Display the table at point fitted to WIDTH columns as an overlay.
The buffer text is left untouched, unlike `org-table-fit-window'.
With no prefix, use the window body width scaled by
`org-table-fit-width-fraction'."
  (interactive "P")
  (let ((bounds (org-table-fit--table-bounds)))
    (unless bounds
      (user-error "Not in an org table"))
    (let* ((target (if (and (integerp width) (>= width 10))
                       width
                     (floor (* (org-table-fit--display-layout-width)
                               org-table-fit-width-fraction))))
           (beg (car bounds))
           (end (cdr bounds)))
      (if (org-table-fit--display-table-in beg end target)
          (message "org-table-fit: displaying fitted table")
        (message "org-table-fit: table already fits (%d columns)" target)))))

;;;###autoload
(defun org-table-fit-hide-table ()
  "Reveal the source of the table at point by removing its overlays."
  (interactive)
  (let ((overlay (org-table-fit--overlay-at (point))))
    (if overlay
        (let ((bounds (org-table-fit--display-table-bounds-at (point))))
          (if bounds
              (org-table-fit--remove-display-overlays-in
               (car bounds) (cdr bounds))
            (org-table-fit--remove-display-overlay overlay))
          (setq org-table-fit--display-inside t)
          (message "org-table-fit: revealed table source"))
      (message "org-table-fit: no display overlay here"))))

;;;###autoload
(defun org-table-fit-toggle-reveal-on-point ()
  "Toggle whether display overlays reveal their table when point enters.
Flips `org-table-fit-overlay-reveal-on-point': turn it on to edit
tables normally, off to traverse the fitted rendering instead.
Refreshes the current buffer's overlays when
`org-table-fit-overlay-mode' is active so the change applies at once."
  (interactive)
  (setq org-table-fit-overlay-reveal-on-point
        (not org-table-fit-overlay-reveal-on-point))
  (when org-table-fit-overlay-mode
    (org-table-fit-refresh-overlays))
  (message "org-table-fit: reveal-on-point %s"
           (if org-table-fit-overlay-reveal-on-point "on" "off")))

;;;###autoload
(defun org-table-fit-next-row (&optional arg)
  "Move forward ARG source lines, landing at the beginning of the line.
Unlike line motion this steps by buffer lines, so it traverses a
displayed table one source row at a time instead of skipping across
its rendering.  Each row's `cursor' property then shows the cursor on
the matching rendered row.  This command alone never reveals the
source, even with `org-table-fit-overlay-reveal-on-point' enabled."
  (interactive "p")
  (setq org-table-fit--keep-display-once t)
  (forward-line (or arg 1))
  (beginning-of-line)
  (org-table-fit--display-hold-point))

;;;###autoload
(defun org-table-fit-prev-row (&optional arg)
  "Move backward ARG source lines, landing at the beginning of the line.
Like `org-table-fit-next-row' but upward.  This command alone never
reveals a displayed table's source."
  (interactive "p")
  (setq org-table-fit--keep-display-once t)
  (forward-line (- (or arg 1)))
  (beginning-of-line)
  (org-table-fit--display-hold-point))

(defun org-table-fit--line-motion-direction ()
  "Return 1 or -1 when this command is line motion, else nil.
Covers `next-line', `previous-line' and the row-or-line commands
bound by `org-table-fit-overlay-mode'."
  (cond ((memq this-command '(next-line org-table-fit-next-row-or-line)) 1)
        ((memq this-command '(previous-line org-table-fit-previous-row-or-line)) -1)
        (t nil)))

;;;###autoload
(defun org-table-fit-next-row-or-line (&optional arg)
  "Step down one row in a displayed table, else move like `next-line'.
When point is on a displayed table row, move to the next source row
and keep point at the start of the line (see `org-table-fit-next-row').
Otherwise behave exactly like `next-line'."
  (interactive "p")
  (if (org-table-fit--overlay-at (point))
      (org-table-fit-next-row arg)
    ;; Deliberate interactive delegation: inherit goal column,
    ;; shift-selection and everything else `next-line' does.
    (with-no-warnings (next-line arg))))

;;;###autoload
(defun org-table-fit-previous-row-or-line (&optional arg)
  "Step up one row in a displayed table, else move like `previous-line'.
When point is on a displayed table row, move to the previous source
row and keep point at the start of the line (see
`org-table-fit-prev-row').  Otherwise behave exactly like
`previous-line'."
  (interactive "p")
  (if (org-table-fit--overlay-at (point))
      (org-table-fit-prev-row arg)
    ;; Deliberate interactive delegation: see above.
    (with-no-warnings (previous-line arg))))

;;;###autoload
(defun org-table-fit-toggle-display (&optional width)
  "Toggle the fitted overlay for the table at point."
  (interactive "P")
  (let ((overlay (or (org-table-fit--overlay-at (point))
                     ;; Point may sit just past the overlay end after a
                     ;; toggle that moved it out of a revealed table.
                     (and (> (point) (point-min))
                          (org-table-fit--overlay-at (1- (point)))))))
    (cond
     (overlay
      (goto-char (overlay-start overlay))
      (org-table-fit-hide-table))
     ((org-at-table-p)
      (let* ((bounds (org-table-fit--table-bounds))
             (target (if (and (integerp width) (>= width 10))
                         width
                       (floor (* (org-table-fit--display-layout-width)
                                 org-table-fit-width-fraction)))))
        (if (org-table-fit--display-table-in (car bounds) (cdr bounds) target)
            (progn
              ;; With reveal-on-point active, point inside the newly
              ;; covered table would make the post-command hook remove
              ;; the overlay at once; move point out so the toggle
              ;; sticks.  Without the mode, leave point alone.
              (when org-table-fit-overlay-mode
                (goto-char (cdr bounds))
                (setq org-table-fit--display-inside nil))
              (message "org-table-fit: displaying fitted table"))
          (message "org-table-fit: table already fits (%d columns)" target))))
     (t (user-error "Not at an Org table")))))

;;;###autoload
(defun org-table-fit-refresh-overlays ()
  "Lay out every table in the buffer as a fitted overlay.
A table containing point is left as source when
`org-table-fit-overlay-reveal-on-point' is non-nil.  Tables that
already fit need no overlay and are left alone."
  (interactive)
  (org-table-fit--display-cancel)
  (org-table-fit--clear-display-overlays)
  (let* ((width (org-table-fit--display-layout-width))
         (target (floor (* width org-table-fit-width-fraction)))
         (point (point)))
    (save-excursion
      (dolist (table (org-table-fit--tables))
        (unless (and org-table-fit-overlay-reveal-on-point
                     (>= point (car table))
                     (< point (cdr table)))
          (org-table-fit--display-table-in (car table) (cdr table) target))))
    (setq org-table-fit--display-width width)))

(defun org-table-fit--display-missing ()
  "Display overlays for tables that have none, except the one at point."
  (let* ((width (or org-table-fit--display-width
                    (org-table-fit--display-layout-width)))
         (target (floor (* width org-table-fit-width-fraction)))
         (point (point)))
    (save-excursion
      (dolist (table (org-table-fit--tables))
        (unless (or (org-table-fit--overlay-at (car table))
                    (and org-table-fit-overlay-reveal-on-point
                         (>= point (car table))
                         (< point (cdr table))))
          (org-table-fit--display-table-in (car table) (cdr table) target))))))

(defun org-table-fit--display-cancel ()
  "Cancel a pending overlay relayout."
  (when org-table-fit--display-timer
    (cancel-timer org-table-fit--display-timer)
    (setq org-table-fit--display-timer nil)))

(defun org-table-fit--display-run (buffer)
  "Redisplay BUFFER's overlays when its window width changed."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq org-table-fit--display-timer nil)
      (when org-table-fit-overlay-mode
        (org-table-fit-refresh-overlays)))))

(defun org-table-fit--display-schedule ()
  "Redisplay overlays after the window configuration settles."
  (when org-table-fit-overlay-mode
    (org-table-fit--display-cancel)
    (if (<= org-table-fit-overlay-relayout-delay 0)
        (org-table-fit--display-run (current-buffer))
      (setq org-table-fit--display-timer
            (run-with-idle-timer org-table-fit-overlay-relayout-delay nil
                                 #'org-table-fit--display-run
                                 (current-buffer))))))

(defun org-table-fit--display-window-changed ()
  "Schedule a relayout when the window width changed."
  (let ((width (org-table-fit--display-layout-width)))
    (unless (equal width org-table-fit--display-width)
      (org-table-fit--display-schedule))))

(defun org-table-fit--display-after-change (_beg _end _len)
  "Schedule a relayout after the buffer text changes."
  (when org-table-fit-overlay-mode
    (org-table-fit--display-schedule)))

(defun org-table-fit--display-pre-command ()
  "Record point so the direction of entry into a preview is known."
  (when org-table-fit-overlay-mode
    (set-marker org-table-fit--display-previous-point (point))))

(defun org-table-fit--display-step-to-line (line-bol)
  "Move point to the start of the source line at LINE-BOL.
Landings stay at the beginning of the line so point rests on hidden
text covered by that row's `cursor' property (rather than on the
visible newline, which would show the cursor at the end of the
rendered row)."
  (goto-char line-bol)
  (beginning-of-line))

(defun org-table-fit--display-correct-line-motion ()
  "Step line motion through displayed tables one source line at a time.
Visual line motion can skip across an overlay's `before-string',
landing past the table instead of inside it.  When
`org-table-fit-overlay-reveal-on-point' is nil the overlays stay, so
correct `next-line' and `previous-line' here: entering from outside
lands on the first or last source line, and leaving from inside only
happens one row at a time.  Landings always go to the beginning of
the line.  Anything that does not match a clean single-step skip
(prefix-argument jumps, search, clicks) is left alone."
  (let ((dir (and org-table-fit--display-previous-point
                  (marker-position org-table-fit--display-previous-point)
                  (org-table-fit--line-motion-direction))))
    (when dir
      (let* ((prev (marker-position org-table-fit--display-previous-point))
             (here (point)))
      (unless (= prev here)
        (let ((at-here (org-table-fit--overlay-at here))
              (at-prev (org-table-fit--overlay-at prev)))
          (cond
           ;; Entering from below: landed exactly at the table's start
           ;; after starting below it.  Step onto its last source line
           ;; instead of resting on the first row.
           ((and (not at-prev) (< dir 0) at-here
                 (let ((bounds (save-excursion
                                 (goto-char here)
                                 (org-table-fit--table-bounds))))
                   (and bounds (> prev (cdr bounds)) (= here (car bounds)))))
            (let ((bounds (save-excursion
                            (goto-char here)
                            (org-table-fit--table-bounds))))
              (org-table-fit--display-step-to-line
               (org-table-fit--display-last-line-bol (cdr bounds)))))
           ;; Resting inside a displayed table: keep point at the start
           ;; of the line.  A preserved goal column would park point at
           ;; the end of the hidden line, outside its row's `cursor'
           ;; range, and the cursor would jump to the row end.
           (at-here
            (beginning-of-line))
           (at-prev
            ;; Started inside, now outside: a correct single step
            ;; either stays inside or exits exactly one line.  Landing
            ;; exactly at the far boundary from a non-exit row means
            ;; the motion skipped; pull it back one source line.
            (let ((bounds (save-excursion
                            (goto-char prev)
                            (org-table-fit--table-bounds))))
            (when (and bounds (null (org-table-fit--overlay-at here)))
              (let ((first-line (save-excursion
                                  (goto-char (car bounds))
                                  (line-number-at-pos)))
                    (last-line (save-excursion
                                 (goto-char (cdr bounds))
                                 (when (bolp) (forward-line -1))
                                 (line-number-at-pos)))
                    (prev-line (save-excursion
                                 (goto-char prev)
                                 (line-number-at-pos))))
                (cond
                 ((and (> dir 0) (= here (cdr bounds)) (< prev-line last-line))
                  (goto-char prev)
                  (forward-line 1)
                  (beginning-of-line))
                 ((and (< dir 0) (= here (car bounds)) (> prev-line first-line))
                  (goto-char prev)
                  (forward-line -1)
                  (beginning-of-line)))))))
         ((> dir 0)
          ;; Started outside and moving down: landing exactly at a
          ;; table's end after starting above it means the motion
          ;; skipped the table; step onto its first source line.
          (when (and (> here (point-min))
                     (null (org-table-fit--overlay-at here))
                     (org-table-fit--overlay-at (1- here)))
            (let ((bounds (save-excursion
                            (goto-char (1- here))
                            (org-table-fit--table-bounds))))
              (when (and bounds (< prev (car bounds)) (= here (cdr bounds)))
                (org-table-fit--display-step-to-line (car bounds))))))))
)))))

(defun org-table-fit--display-post-command ()
  "Reveal the table under point and restore overlays point has left."
  (when org-table-fit-overlay-mode
    (if (not org-table-fit-overlay-reveal-on-point)
        (progn
          (org-table-fit--display-correct-line-motion)
          (org-table-fit--display-hold-point))
      (if org-table-fit--keep-display-once
          ;; A row-stepping command asked to traverse without revealing.
          (progn
            (setq org-table-fit--keep-display-once nil)
            (org-table-fit--display-hold-point))
        (let ((vertical-motion
             (and (org-table-fit--line-motion-direction)
                  (bound-and-true-p line-move-visual)
                  org-table-fit--display-previous-point
                  (marker-position org-table-fit--display-previous-point)))
            (overlay (org-table-fit--overlay-at (point))))
        ;; Down can skip the entire replacement and land just past its end.
        (when (and (not overlay) vertical-motion (> (point) (point-min)))
          (let ((crossed (org-table-fit--overlay-at (1- (point)))))
            (when (and crossed
                       (= (point) (overlay-end crossed))
                       (< org-table-fit--display-previous-point
                          (overlay-start crossed)))
              (setq overlay crossed)
              (goto-char (overlay-start overlay)))))
        (cond
         (overlay
          (let ((bounds (org-table-fit--display-table-bounds-at (point))))
            ;; Display-based vertical motion can land anywhere on the
            ;; hidden source depending on direction, so entering a
            ;; displayed table by line motion lands on the boundary
            ;; source line's start (first line from above, last from
            ;; below).  Compare against the table bounds rather than
            ;; this row's overlay.  Do not redirect searches or other
            ;; explicit jumps into the table.
            (when (and vertical-motion bounds
                       org-table-fit--display-previous-point)
              (cond
               ((< org-table-fit--display-previous-point (car bounds))
                (goto-char (car bounds)))
               ((>= org-table-fit--display-previous-point (cdr bounds))
                ;; Table overlays include the final newline, unlike LaTeX previews.
                (goto-char (1- (cdr bounds)))
                (beginning-of-line))))
            (if bounds
                (org-table-fit--remove-display-overlays-in
                 (car bounds) (cdr bounds))
              (org-table-fit--remove-display-overlay overlay)))
          (setq org-table-fit--display-inside t))
         ((and org-table-fit--display-inside
               (not (org-at-table-p)))
          (setq org-table-fit--display-inside nil)
          (org-table-fit--display-missing))
         ((org-at-table-p)
          (setq org-table-fit--display-inside t))))))))


;;;###autoload
(define-minor-mode org-table-fit-overlay-mode
  "Show Org tables as fitted, non-destructive overlays.
The buffer text is left untouched; each source line of a wide table
is covered by an overlay displaying its wrapped rendering.  Moving
point into a table reveals its source for ordinary editing.  With
`org-table-fit-overlay-reveal-on-point' set to nil the overlays stay
and point can still traverse the table: each row guides the cursor to
its rendered line.  Line motion keys step row by row inside displayed
tables and behave normally everywhere else."
  :lighter " OrgFitD"
  :global nil
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map [remap next-line]
                        #'org-table-fit-next-row-or-line)
            (define-key map [remap previous-line]
                        #'org-table-fit-previous-row-or-line)
            map)
  (if org-table-fit-overlay-mode
      (progn
        (unless (derived-mode-p 'org-mode)
          (setq org-table-fit-overlay-mode nil)
          (user-error "Org table overlays require Org mode"))
        (unless org-table-fit--display-previous-point
          (setq org-table-fit--display-previous-point (make-marker)))
        (add-hook 'after-change-functions
                  #'org-table-fit--display-after-change nil t)
        (add-hook 'pre-command-hook
                  #'org-table-fit--display-pre-command nil t)
        (add-hook 'post-command-hook
                  #'org-table-fit--display-post-command nil t)
        (add-hook 'window-configuration-change-hook
                  #'org-table-fit--display-window-changed nil t)
        (add-hook 'text-scale-mode-hook
                  #'org-table-fit--display-schedule nil t)
        (org-table-fit-refresh-overlays))
    (remove-hook 'after-change-functions
                 #'org-table-fit--display-after-change t)
    (remove-hook 'pre-command-hook
                 #'org-table-fit--display-pre-command t)
    (when org-table-fit--display-previous-point
      (set-marker org-table-fit--display-previous-point nil)
      (setq org-table-fit--display-previous-point nil))
    (remove-hook 'post-command-hook
                 #'org-table-fit--display-post-command t)
    (remove-hook 'window-configuration-change-hook
                 #'org-table-fit--display-window-changed t)
    (remove-hook 'text-scale-mode-hook
                 #'org-table-fit--display-schedule t)
    (org-table-fit--display-cancel)
    (org-table-fit--clear-display-overlays)))

(provide 'org-table-fit)

;;; org-table-fit.el ends here

