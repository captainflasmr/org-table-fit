;;; org-table-fit.el --- Fit org-mode tables to the window width -*- lexical-binding: t; -*-

;; Copyright (C) 2026 James Dyer

;; Author: James Dyer <james@dyerdwelling.family>
;; URL: https://github.com/captainflasmr/org-table-fit
;; Version: 0.1.3
;; Package-Requires: ((emacs "29.1"))
;; Keywords: org, tables, convenience

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'seq)
(require 'org)

(defgroup org-table-fit nil
  "Fit org-mode tables to the window width."
  :group 'org
  :prefix "org-table-fit-")

(defvar org-table-fit-mode nil
  "Non-nil when `org-table-fit-mode' is enabled.")

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

(defun org-table-fit--render-rows (rows widths indent)
  "Render ROWS as aligned Org table lines padded to WIDTHS and prefixed with INDENT."
  (let ((vec-widths (vconcat widths)))
    (mapconcat
     (lambda (row)
       (if (eq (car row) :hline)
           (concat indent "|"
                   (mapconcat (lambda (w) (make-string (+ w 2) ?-))
                              vec-widths
                              "+")
                   "|")
         (let* ((cells (map-elt row :cells))
                (padded-cells
                 (cl-loop for i below (length vec-widths)
                          for cell = (or (nth i cells) "")
                          for w = (aref vec-widths i)
                          for pad = (max 0 (- w (string-width cell)))
                          collect (concat cell (make-string pad ?\s)))))
           (concat indent "| " (mapconcat #'identity padded-cells " | ") " |"))))
     rows
     "\n")))

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

(defun org-table-fit--after-change-or-window-size (&rest _)
  "Queue a refit for the table in the current window when window size changes."
  (when (and org-table-fit-mode
             (not org-table-fit--resize-timer))
    (let ((buf (current-buffer))
          (win (selected-window)))
      (setq org-table-fit--resize-timer
            (run-with-idle-timer
             0.1 nil
             (lambda ()
               (setq org-table-fit--resize-timer nil)
               (when (buffer-live-p buf)
                 (with-current-buffer buf
                   (when (and org-table-fit-mode
                              (window-live-p win)
                              (eq (window-buffer win) buf)
                              (with-selected-window win (org-at-table-p)))
                     (with-selected-window win
                       (org-table-fit-window)))))))))))

;;;###autoload
(define-minor-mode org-table-fit-mode
  "Automatically fit the current Org table to the current window width."
  :lighter " OrgFit"
  :global nil
  (if org-table-fit-mode
      (progn
        (add-hook 'window-size-change-functions #'org-table-fit--after-change-or-window-size nil t)
        (org-table-fit--after-change-or-window-size))
    (when org-table-fit--resize-timer
      (cancel-timer org-table-fit--resize-timer)
      (setq org-table-fit--resize-timer nil))
    (remove-hook 'window-size-change-functions #'org-table-fit--after-change-or-window-size t)))

;; -> commands

;;;###autoload
(defun org-table-fit-window (&optional width)
  "Fit the org table at point to the current window width."
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
      (when (org-table-fit--wrapped-table-p beg end)
        (setq rows (org-table-fit--merge-continuations
                    rows (org-table-fit--collect-row-starts beg end))))
      (let* ((data-rows (cl-remove-if (lambda (r) (eq (car r) :hline)) rows))
             (cell-count (apply #'max 1 (mapcar (lambda (r)
                                                  (length (map-elt r :cells)))
                                                data-rows)))
             (natural (make-list cell-count 0))
             (minimum (make-list cell-count 0)))
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
          (let ((widths (org-table-fit--allocate-widths natural minimum target)))
            (let* ((row-starts nil)
                   (wrapped-rows
                    (cl-loop for row in rows
                             append
                             (if (eq (car row) :hline)
                                 (progn
                                   (push nil row-starts)
                                   (list row))
                               (let* ((cells (map-elt row :cells))
                                      (padded (append cells
                                                      (make-list
                                                       (max 0 (- cell-count
                                                                 (length cells)))
                                                       "")))
                                      (physical (org-table-fit--wrap-row
                                                 padded widths)))
                                 (cl-loop for i from 0 below (length physical)
                                          do (push (= i 0) row-starts))
                                 (mapcar (lambda (prow) (list :cells prow))
                                         physical)))))
                   (rendered (concat (org-table-fit--render-rows wrapped-rows widths indent) "\n")))
              (atomic-change-group
                (delete-region beg end)
                (goto-char beg)
                (let ((start (point)))
                  (insert rendered)
                  (put-text-property start (point) 'org-table-fit-wrapped t)
                  (org-table-fit--tag-row-starts start (nreverse row-starts))))
              (message "org-table-fit: wrapped table to %d columns"
                       (org-table-fit--table-total-width widths)))))))))

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
        (atomic-change-group
          (delete-region beg end)
          (goto-char beg)
          (insert rendered)
          (remove-text-properties beg (point)
                                  '(org-table-fit-wrapped t
                                    org-table-fit-row-start t))))
      (message "org-table-fit: unwrapped table"))))

(provide 'org-table-fit)

;;; org-table-fit.el ends here
