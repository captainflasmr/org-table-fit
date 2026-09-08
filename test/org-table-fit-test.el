;; -*- lexical-binding: t; -*-
(require 'org)
(require 'org-table-fit)

(defvar org-table-fit--failures 0)

(defun org-table-fit--check (label condition)
  (if condition
      (princ (format "PASS: %s\n" label))
    (setq org-table-fit--failures (1+ org-table-fit--failures))
    (princ (format "FAIL: %s\n" label))))

(defun org-table-fit--max-line-width ()
  "Max display width of table lines in current buffer."
  (let ((maxw 0))
    (goto-char (point-min))
    (while (re-search-forward "^|" nil t)
      (setq maxw (max maxw (- (line-end-position) (line-beginning-position)))))
    maxw))

(defun org-table-fit--buffer-text ()
  (buffer-substring-no-properties (point-min) (point-max)))

;; --- Test 1: wide table fits target width -----------------------------
(with-temp-buffer
  (org-mode)
  (insert "| Header One | Header Two | Header Three | Header Four |\n")
  (insert "|------------+------------+--------------+-------------|\n")
  (insert "| alpha beta gamma delta epsilon zeta eta | 1 | short | long unbrokenwordthatrunsonthescreen |\n")
  (insert "| omega | 2 | medium text here | fine |\n")
  (goto-char (point-min))
  (org-table-align)
  (org-table-fit-window 60)
  (org-table-fit--check "wide table fits width 60"
                        (<= (org-table-fit--max-line-width) 60))
  (org-table-fit--check "wrapped table still has all cells"
                        (string-match-p "unbrokenwordthat"
                                        (org-table-fit--buffer-text)))
  (org-table-fit--check "header content preserved"
                        (string-match-p "Header One" (org-table-fit--buffer-text)))
  ;; every row has same column count as first
  (let ((cols (length (org-table-fit--split-row
                       (buffer-substring-no-properties (line-beginning-position)
                                                       (line-end-position)))))
        (ok t))
    (goto-char (point-min))
    (while (and ok (re-search-forward "^|" nil t))
      (let ((line (buffer-substring-no-properties (line-beginning-position)
                                                  (line-end-position))))
        (unless (string-match-p org-table-hline-regexp line)
          (setq ok (= cols (length (org-table-fit--split-row line)))))))
    (org-table-fit--check "all rows same column count" ok)))

;; --- Test 2: unwrap restores single-line rows --------------------------
(with-temp-buffer
  (org-mode)
  (insert "| h1 | h2 | h3 |\n")
  (insert "|---+----+----|\n")
  (insert "| a b c d | e f | g |\n")
  (insert "| q | r s t u v w x y z | m |\n")
  (goto-char (point-min))
  (org-table-align)
  (org-table-fit-window 30)
  (let ((fitted (org-table-fit--buffer-text)))
    (org-table-fit-unwrap)
    (org-table-fit--check "unwrap merges continuation rows"
                          (string-match-p "a b c d" (org-table-fit--buffer-text)))
    (org-table-fit--check "unwrap recovers long cell text"
                          (string-match-p "r s t u v w x y z"
                                          (org-table-fit--buffer-text)))
    (org-table-fit--check "unwrap produces single-line rows"
                          (let ((txt (org-table-fit--buffer-text)))
                            (= 3 (with-temp-buffer
                                   (insert txt)
                                   (goto-char (point-min))
                                   (let ((n 0))
                                     (while (re-search-forward "^|" nil t)
                                       (unless (save-excursion
                                                 (beginning-of-line)
                                                 (looking-at org-table-hline-regexp))
                                         (setq n (1+ n))))
                                     n)))))))

;; --- Test 3: already-fitting table is left alone -----------------------
(with-temp-buffer
  (org-mode)
  (insert "| a | b |\n")
  (insert "|---+---|\n")
  (insert "| 1 | 2 |\n")
  (goto-char (point-min))
  (org-table-align)
  (let ((before (org-table-fit--buffer-text)))
    (org-table-fit-window 60)
    (org-table-fit--check "narrow table unchanged"
                          (string= before (org-table-fit--buffer-text)))))

;; --- Test 4: indented table keeps indentation --------------------------
(with-temp-buffer
  (org-mode)
  (insert "- item\n\n  | a b c d e f g h i j k l m n o p q r s t u v | x |\n")
  (goto-char (point-min))
  (search-forward "| a")
  (org-table-align)
  (org-table-fit-window 30)
  (org-table-fit--check "indented table keeps indent"
                        (string-match-p "^  |" (org-table-fit--buffer-text))))

;; --- Test 5: formula cells are not wrapped -----------------------------
(with-temp-buffer
  (org-mode)
  (insert "| a b c d e f g h i j k l m | =1+2 |\n")
  (goto-char (point-min))
  (org-table-align)
  (org-table-fit-window 20)
  (org-table-fit--check "formula cell intact"
                        (string-match-p "| *=1\\+2 *|" (org-table-fit--buffer-text))))

;; --- Test 6: fit -> unwrap -> refit at new width ------------------------
(with-temp-buffer
  (org-mode)
  (insert "| a b c d e f g h i j k l m n o p q r s t u | v w x y z |\n")
  (insert "| 1 2 3 4 5 6 7 8 9 0 | plain |\n")
  (goto-char (point-min))
  (org-table-align)
  (org-table-fit-window 40)
  (org-table-fit-unwrap)
  (org-table-fit-window 25)
  (let* ((txt (org-table-fit--buffer-text))
         (squashed (replace-regexp-in-string "[ \t\n|]" "" txt)))
    (org-table-fit--check "refit at narrower width"
                          (and (<= (org-table-fit--max-line-width) 25)
                               (string-match-p "abcdef" squashed)
                               (string-match-p "ghijkl" squashed)
                               (string-match-p "123456" squashed)
                               (string-match-p "7890" squashed)
                               (string-match-p "vwx" squashed)
                               (string-match-p "plain" squashed)))))

;; --- Test 7: hline after data rows preserved ---------------------------
(with-temp-buffer
  (org-mode)
  (insert "| a b c d e f g h i j k l m n o p q r s | x |\n")
  (insert "|---+---|\n")
  (insert "| 1 2 3 4 5 6 7 8 9 | y |\n")
  (insert "|---+---|\n")
  (insert "| last | z |\n")
  (goto-char (point-min))
  (org-table-align)
  (org-table-fit-window 30)
  (let* ((txt (org-table-fit--buffer-text))
         (squashed (replace-regexp-in-string "[ \t\n|]" "" txt))
         (n (with-temp-buffer
              (insert txt)
              (goto-char (point-min))
               (let ((c 0))
                 (while (re-search-forward org-table-hline-regexp nil t)
                   (setq c (1+ c)))
                 c))))
    (org-table-fit--check "two hlines preserved" (= n 2))
    (org-table-fit--check "fit with hlines keeps data"
                          (and (string-match-p "last" squashed)
                               (string-match-p "123456789" squashed)))))

;; --- Test 8: org verbatim markup spans stay intact ---------------------
(with-temp-buffer
  (org-mode)
  (insert "| =C-x C-s= / =C-x C-c= | save and quit (=C-x C-s= then =C-x C-c=) |\n")
  (insert "| =C-g C-/= (or =M-/=) | redo the last undone change |\n")
  (goto-char (point-min))
  (org-table-align)
  (org-table-fit-window 30)
  (let ((odd 0))
    (goto-char (point-min))
    (while (re-search-forward "^|" nil t)
      (let ((line (buffer-substring-no-properties (line-beginning-position)
                                                  (line-end-position))))
        (when (cl-oddp (cl-count ?= line))
          (setq odd (1+ odd)))))
    (org-table-fit--check "no broken =...= spans" (zerop odd)))
  (org-table-fit--check "multi-word span intact"
                        (string-match-p "=C-x C-s=" (org-table-fit--buffer-text)))
  (org-table-fit--check "span with internal space intact"
                        (string-match-p "=C-g C-/=" (org-table-fit--buffer-text))))

;; --- Test 9: unwrap recovers exact rows via tags ------------------------
(with-temp-buffer
  (org-mode)
  (insert "| =C-a= / =C-e= | start / end of line |\n")
  (insert "| =M-f= =M-b= | move forward / backward by word |\n")
  (insert "| =C-l= | recenter the window on the cursor line (middle, then top, then bottom) |\n")
  (goto-char (point-min))
  (org-table-align)
  (org-table-fit-window 40)
  (org-table-fit-unwrap)
  (let ((txt (org-table-fit--buffer-text)))
    (org-table-fit--check "exact unwrap keeps rows separate"
                          (string-match-p "=C-a= / =C-e= *| start / end of line"
                                          txt))
    (org-table-fit--check "exact unwrap row 2"
                          (string-match-p "=M-f= =M-b= *| move forward / backward by word"
                                          txt))
    (org-table-fit--check "exact unwrap row 3"
                          (string-match-p "=C-l= *| recenter the window"
                                          txt))))

;; --- Test 10: dynamic mode installs resize hooks ------------------------
(with-temp-buffer
  (org-mode)
  (insert "| a b c d e f g h i j k l m n o p q r s t | x |\n")
  (insert "|---+---|\n")
  (insert "| 1 2 3 4 5 6 7 8 9 0 | y |\n")
  (goto-char (point-min))
  (org-table-align)
  (org-table-fit-mode 1)
  (org-table-fit--check "minor mode enables"
                        org-table-fit-mode)
  (org-table-fit--check "minor mode registers window-resize hook"
                        (memq #'org-table-fit--after-change-or-window-size
                              window-size-change-functions))
    (org-table-fit-mode -1)
  (org-table-fit--check "minor mode unregisters hooks"
                        (not (memq #'org-table-fit--after-change-or-window-size
                                   window-size-change-functions))))

;; --- Test 11: overlay display keeps source intact --------------------
(with-temp-buffer
  (org-mode)
  (insert "| Header One | Header Two | Header Three | Header Four |\n")
  (insert "|------------+------------+--------------+-------------|\n")
  (insert "| alpha beta gamma delta epsilon zeta eta | 1 | short | long unbrokenwordthatrunsonthescreen |\n")
  (insert "| omega | 2 | medium text here | fine |\n")
  (goto-char (point-min))
  (org-table-align)
  (let ((before (org-table-fit--buffer-text)))
    (goto-char (point-min))
    (org-table-fit-display-table 60)
    (org-table-fit--check "display creates one overlay per source line"
                          (= 4 (length org-table-fit--display-overlays)))
    (let ((all-hidden t)
          (cursors-ok t)
          (rendered "")
          (maxw 0))
      (dolist (overlay org-table-fit--display-overlays)
        (unless (equal (overlay-get overlay 'display) "")
          (setq all-hidden nil))
        (let ((chunk (overlay-get overlay 'before-string)))
          (setq rendered (concat rendered chunk))
          (unless (and (> (length chunk) 0)
                       (not (eq (aref chunk 0) ?\n))
                       (eq (get-text-property 0 'cursor chunk)
                           (- (overlay-end overlay) (overlay-start overlay))))
            (setq cursors-ok nil))
          (dolist (line (split-string chunk "\n" t))
            (setq maxw (max maxw (string-width line))))))
      (org-table-fit--check "overlays hide source with display" all-hidden)
      (org-table-fit--check "overlay found at table point"
                            (org-table-fit--overlay-at (point-min)))
      (org-table-fit--check "overlay rendering fits width 60" (<= maxw 60))
      (org-table-fit--check "overlay keeps header text"
                            (string-match-p "Header One" rendered))
      (org-table-fit--check "overlay keeps long word text"
                            (string-match-p "unbrokenwordthat" rendered))
      (org-table-fit--check "row cursor spans cover their source line"
                            cursors-ok))
    (org-table-fit--check "display leaves buffer text unchanged"
                          (string= before (org-table-fit--buffer-text)))))

;; --- Test 12: narrow table needs no overlay ---------------------------
(with-temp-buffer
  (org-mode)
  (insert "| a | b |\n")
  (insert "|---+---|\n")
  (insert "| 1 | 2 |\n")
  (goto-char (point-min))
  (org-table-align)
  (org-table-fit-display-table 60)
  (org-table-fit--check "narrow table creates no overlay"
                        (null org-table-fit--display-overlays)))

;; --- Test 13: hide/toggle roundtrip ------------------------------------
(with-temp-buffer
  (org-mode)
  (insert "| a b c d e f g h i j k l m n o p q r s t u | v w x y z |\n")
  (insert "| 1 2 3 4 5 6 7 8 9 0 | plain |\n")
  (goto-char (point-min))
  (org-table-align)
  (let ((source (org-table-fit--buffer-text)))
    (goto-char (point-min))
    (org-table-fit-display-table 40)
    (org-table-fit-hide-table)
    (org-table-fit--check "hide removes overlay"
                          (null org-table-fit--display-overlays))
    (org-table-fit--check "hide leaves source unchanged"
                          (string= source (org-table-fit--buffer-text)))
    (goto-char (point-min))
    (org-table-fit-toggle-display 40)
    (org-table-fit--check "toggle shows overlays"
                          (= 2 (length org-table-fit--display-overlays)))
    (org-table-fit--check "toggle keeps point at table"
                          (org-at-table-p))
    (org-table-fit-toggle-display)
    (org-table-fit--check "toggle hides overlay again"
                          (null org-table-fit--display-overlays))))

;; --- Test 14: overlay mode refresh respects point ----------------------
(with-temp-buffer
  (org-mode)
  (let ((org-table-fit-width-fraction 0.3)
        (org-table-fit-overlay-relayout-delay 0))
    (insert "intro text\n\n")
    (insert "| alpha beta gamma delta epsilon zeta eta | 1 | short | long unbrokenwordthatrunsonthescreen |\n")
    (insert "| omega | 2 | medium text here | fine |\n")
    (insert "\ntrailing text\n")
    (goto-char (point-min))
    (search-forward "intro")
    (org-table-fit-overlay-mode 1)
    (org-table-fit--check "overlay mode enables" org-table-fit-overlay-mode)
    (org-table-fit--check "overlay mode displays table away from point"
                          (= 2 (length org-table-fit--display-overlays)))
    (goto-char (point-min))
    (search-forward "| alpha")
    (org-table-fit-refresh-overlays)
    (org-table-fit--check "refresh skips table at point"
                          (null org-table-fit--display-overlays))
    (let ((org-table-fit-overlay-reveal-on-point nil))
      (org-table-fit-refresh-overlays)
      (org-table-fit--check "refresh without reveal displays table"
                            (= 2 (length org-table-fit--display-overlays))))
    (org-table-fit--check "overlay mode registers post-command hook"
                          (memq #'org-table-fit--display-post-command
                                post-command-hook))
    (org-table-fit-overlay-mode -1)
    (org-table-fit--check "overlay mode clears overlays on disable"
                          (null org-table-fit--display-overlays))
    (org-table-fit--check "overlay mode unregisters hooks"
                          (not (memq #'org-table-fit--display-post-command
                                     post-command-hook)))))

;; --- Test 15: destructive refit clears stale overlays ------------------
(with-temp-buffer
  (org-mode)
  (insert "| a b c d e f g h i j k l m n o p q r s t u | v |\n")
  (insert "| 1 2 3 4 5 6 | plain |\n")
  (goto-char (point-min))
  (org-table-align)
  (goto-char (point-min))
  (org-table-fit-display-table 40)
  (org-table-fit--check "overlay present before refit"
                        (= 2 (length org-table-fit--display-overlays)))
  (goto-char (point-min))
  (org-table-fit-window 40)
  (org-table-fit--check "refit clears display overlay"
                        (null org-table-fit--display-overlays)))

;; --- Test 16: overlay keeps formula cells intact ------------------------
(with-temp-buffer
  (org-mode)
  (insert "| a b c d e f g h i j k l m | =1+2 |\n")
  (goto-char (point-min))
  (org-table-align)
  (goto-char (point-min))
  (org-table-fit-display-table 20)
  (let ((rendered (mapconcat
                   (lambda (overlay) (overlay-get overlay 'before-string))
                   org-table-fit--display-overlays "")))
    (org-table-fit--check "overlay keeps formula cell intact"
                          (string-match-p "| *=1\\+2 *|" rendered))))

;; --- Test 17: row overlays tile the table -----------------------------
(with-temp-buffer
  (org-mode)
  (insert "| Header One | Header Two |\n")
  (insert "|------------ +------------|\n")
  (insert "| alpha beta gamma delta epsilon zeta eta theta | 1 |\n")
  (insert "| omega | 2 |\n")
  (goto-char (point-min))
  (org-table-align)
  (goto-char (point-min))
  (search-forward "| Header")
  (let ((bounds (org-table-fit--table-bounds)))
    (org-table-fit-display-table 40)
    (let ((ordered (sort (copy-sequence org-table-fit--display-overlays)
                         (lambda (a b) (< (overlay-start a) (overlay-start b)))))
          (tiles t))
      (org-table-fit--check "one overlay per source line"
                            (= 4 (length ordered)))
      (cl-loop for overlay in ordered
               for i from 0
               do (unless (and (> (overlay-end overlay) (overlay-start overlay))
                               (if (< i 3)
                                   (= (overlay-start (nth (1+ i) ordered))
                                      (1+ (overlay-end overlay)))
                                 (= (overlay-end overlay) (cdr bounds))))
                    (setq tiles nil)))
      (org-table-fit--check "row overlays tile table lines" tiles))))

;; --- Tests 18-20: navigate with reveal disabled ------------------------
(with-temp-buffer
  (org-mode)
  (let ((org-table-fit-width-fraction 0.3)
        (org-table-fit-overlay-relayout-delay 0)
        (org-table-fit-overlay-reveal-on-point nil))
    (insert "above text\n\n")
    (insert "| alpha beta gamma delta epsilon zeta eta theta iota | 1 |\n")
    (insert "| omega sigma tau upsilon phi chi psi omega sigma | 2 |\n")
    (insert "| short row here | 3 |\n")
    (insert "\nbelow text\n")
    (goto-char (point-min))
    (search-forward "above")
    (beginning-of-line)
    (let ((above (point)))
      (org-table-fit-overlay-mode 1)
      (goto-char (point-min))
      (search-forward "| alpha")
      (beginning-of-line)
      (let ((row1 (point))
            (bounds (org-table-fit--table-bounds))
            row2 row3)
        (forward-line 1)
        (setq row2 (point))
        (forward-line 1)
        (setq row3 (point))
        (org-table-fit--check "reveal-off displays all rows"
                              (= 3 (length org-table-fit--display-overlays)))
        ;; Test 18: line motion down into the table steps to row 1.
        (set-marker org-table-fit--display-previous-point above)
        (goto-char (cdr bounds))
        (let ((this-command 'next-line))
          (org-table-fit--display-post-command))
        (org-table-fit--check "down motion enters first row"
                              (= (point) row1))
        (org-table-fit--check "overlays survive entry with reveal off"
                              (= 3 (length org-table-fit--display-overlays)))
        ;; Landings stay at the start of the line even when motion
        ;; started mid-line, so the cursor shows at the row start.
        (set-marker org-table-fit--display-previous-point
                    (save-excursion (goto-char above) (end-of-line) (point)))
        (goto-char (cdr bounds))
        (let ((this-command 'next-line))
          (org-table-fit--display-post-command))
        (org-table-fit--check "entry lands at line start, not mid-line"
                              (and (= (point) row1) (bolp)))
        ;; Test 19: motion from a middle row steps to the next row.
        (set-marker org-table-fit--display-previous-point row1)
        (goto-char (cdr bounds))
        (let ((this-command 'next-line))
          (org-table-fit--display-post-command))
        (org-table-fit--check "down motion steps to second row"
                              (= (point) row2))
        ;; Leaving from the last row is allowed.
        (set-marker org-table-fit--display-previous-point row3)
        (goto-char (cdr bounds))
        (let ((this-command 'next-line))
          (org-table-fit--display-post-command))
        (org-table-fit--check "down motion may leave from last row"
                              (= (point) (cdr bounds)))
        ;; Test 20: line motion up into the table steps to the last row.
        (goto-char (point-min))
        (search-forward "below")
        (beginning-of-line)
        (let ((below (point)))
          (set-marker org-table-fit--display-previous-point below)
          (goto-char (car bounds))
          (let ((this-command 'previous-line))
            (org-table-fit--display-post-command))
          (org-table-fit--check "up motion enters last row"
                                (= (point) row3))
          (org-table-fit--check "overlays survive up entry"
                                (= 3 (length org-table-fit--display-overlays)))))
      (org-table-fit-overlay-mode -1))))

;; --- Test 21: rendered rows have no blank lines between them -------
(with-temp-buffer
  (org-mode)
  (insert "above\n\n| Header One | Header Two |\n")
  (insert "|------------+------------|\n")
  (insert "| alpha beta gamma delta epsilon zeta eta theta iota kappa | 1 |\n")
  (insert "| omega sigma | 2 |\n")
  (insert "\nbelow\n")
  (goto-char (point-min))
  (org-table-align)
  (goto-char (point-min))
  (search-forward "| Header")
  (let ((bounds (org-table-fit--table-bounds)))
    (org-table-fit-display-table 40)
    ;; Reconstruct what the display engine shows for the table region:
    ;; each overlay's before-string plus the visible gaps between them.
    (let ((sim "")
          (pos (car bounds))
          (ordered (sort (copy-sequence org-table-fit--display-overlays)
                         (lambda (a b) (< (overlay-start a)
                                          (overlay-start b))))))
      (dolist (overlay ordered)
        (setq sim (concat sim
                          (buffer-substring-no-properties
                           pos (overlay-start overlay))
                          (overlay-get overlay 'before-string)))
        (setq pos (overlay-end overlay)))
      (let ((lines (split-string sim "\n")))
        ;; A single trailing newline terminates the last rendered row.
        (when (and lines (string-empty-p (car (last lines))))
          (setq lines (butlast lines)))
        (org-table-fit--check "no blank lines inside rendered table"
                              (cl-every (lambda (line)
                                          (not (string-empty-p line)))
                                        lines))
        (org-table-fit--check "rendered table keeps every row"
                              (> (length lines) 4))))))

;; --- Test 22: reveal toggle flips and refreshes -----------------------
(with-temp-buffer
  (org-mode)
  (let ((org-table-fit-width-fraction 0.3)
        (org-table-fit-overlay-relayout-delay 0)
        (org-table-fit-overlay-reveal-on-point t))
    (insert "above\n\n| alpha beta gamma delta epsilon zeta eta theta iota | 1 |\n| omega sigma tau upsilon phi chi psi omega sigma | 2 |\n\nbelow\n")
    (goto-char (point-min))
    (search-forward "| alpha")
    (org-table-fit-overlay-mode 1)
    (org-table-fit--check "point table skipped with reveal on"
                          (null org-table-fit--display-overlays))
    (org-table-fit-toggle-reveal-on-point)
    (org-table-fit--check "toggle turns reveal off"
                          (null org-table-fit-overlay-reveal-on-point))
    (org-table-fit--check "toggle displays point table at once"
                          (= 2 (length org-table-fit--display-overlays)))
    (org-table-fit-toggle-reveal-on-point)
    (org-table-fit--check "toggle turns reveal back on"
                          org-table-fit-overlay-reveal-on-point)
    (org-table-fit--check "toggle reveals point table at once"
                          (null org-table-fit--display-overlays))
    (org-table-fit-overlay-mode -1)))

;; --- Tests 23-24: programmatic row stepping ---------------------------
(with-temp-buffer
  (org-mode)
  (let ((org-table-fit-width-fraction 0.3)
        (org-table-fit-overlay-relayout-delay 0)
        (org-table-fit-overlay-reveal-on-point nil))
    (insert "above\n\n| alpha beta gamma delta epsilon zeta eta theta iota | 1 |\n| omega sigma tau upsilon phi chi psi omega sigma | 2 |\n| short row here | 3 |\n\nbelow\n")
    (goto-char (point-min))
    (search-forward "above")
    (org-table-fit-overlay-mode 1)
    (goto-char (point-min))
    (search-forward "| alpha")
    (beginning-of-line)
    (let ((row1 (point)))
      (forward-line 1)
      (let ((row2 (point)))
        (forward-line 1)
        (let ((row3 (point)))
          ;; Test 23: forward-line stepping stays on column 0.
          (goto-char row1)
          (end-of-line)
          (org-table-fit-next-row)
          (org-table-fit--check "next-row steps one row"
                                (= (point) row2))
          (org-table-fit--check "next-row lands at line start"
                                (bolp))
          (org-table-fit--check "stepping keeps overlays"
                                (= 3 (length org-table-fit--display-overlays)))
          (org-table-fit-next-row 1)
          (org-table-fit--check "next-row with arg steps rows"
                                (= (point) row3))
          (org-table-fit-prev-row)
          (org-table-fit--check "prev-row steps back"
                                (= (point) row2))
          (org-table-fit-prev-row 1)
          (org-table-fit--check "prev-row with arg steps rows"
                                (= (point) row1))
          ;; Test 24: stepping never reveals, even with reveal on.
          ;; (Batch calls run no command loop, so invoke the
          ;; post-command hook explicitly, as real keystrokes would.)
          (let ((org-table-fit-overlay-reveal-on-point t))
            (goto-char row1)
            (org-table-fit-next-row)
            (let ((this-command 'org-table-fit-next-row))
              (org-table-fit--display-post-command))
            (org-table-fit--check "stepping does not reveal with reveal on"
                                  (= (point) row2))
            (org-table-fit--check "keep flag is consumed"
                                  (null org-table-fit--keep-display-once))
            (org-table-fit--check "stepping keeps overlays with reveal on"
                                  (= 3 (length org-table-fit--display-overlays)))))))
    (org-table-fit-overlay-mode -1)))

;; --- Tests 25-26: context-sensitive C-n / C-p -------------------------
(org-table-fit--check "remap next-line to row stepping"
                      (eq (lookup-key org-table-fit-overlay-mode-map
                                      [remap next-line])
                          #'org-table-fit-next-row-or-line))
(org-table-fit--check "remap previous-line to row stepping"
                      (eq (lookup-key org-table-fit-overlay-mode-map
                                      [remap previous-line])
                          #'org-table-fit-previous-row-or-line))
(org-table-fit--check "line motion direction helper"
                      (and (= 1 (let ((this-command 'next-line))
                                  (org-table-fit--line-motion-direction)))
                           (= 1 (let ((this-command 'org-table-fit-next-row-or-line))
                                  (org-table-fit--line-motion-direction)))
                           (= -1 (let ((this-command 'previous-line))
                                   (org-table-fit--line-motion-direction)))
                           (= -1 (let ((this-command 'org-table-fit-previous-row-or-line))
                                   (org-table-fit--line-motion-direction)))
                           (null (let ((this-command 'forward-char))
                                   (org-table-fit--line-motion-direction)))))

(with-temp-buffer
  (org-mode)
  (let ((org-table-fit-width-fraction 0.3)
        (org-table-fit-overlay-relayout-delay 0)
        (org-table-fit-overlay-reveal-on-point nil))
    (insert "above\n\n| alpha beta gamma delta epsilon zeta eta theta iota | 1 |\n| omega sigma tau upsilon phi chi psi omega sigma | 2 |\n| short row here | 3 |\n\nbelow\n")
    (goto-char (point-min))
    (search-forward "above")
    (beginning-of-line)
    (let ((above (point)))
      (org-table-fit-overlay-mode 1)
      ;; Outside a table the remapped keys move by plain lines.
      (goto-char above)
      (org-table-fit-next-row-or-line)
      (org-table-fit--check "row-or-line delegates outside tables"
                            (= (point) (save-excursion
                                         (goto-char above)
                                         (forward-line 1)
                                         (point))))
      ;; Inside a displayed table they step rows on column 0, from any
      ;; column of the hidden line (overlay-at matches per line).
      (goto-char (point-min))
      (search-forward "| alpha")
      (end-of-line)
      (org-table-fit--check "overlay-at finds row from mid-line"
                            (org-table-fit--overlay-at (point)))
      (let ((row1 (save-excursion (beginning-of-line) (point))))
        (org-table-fit-next-row-or-line)
        (org-table-fit--check "row-or-line steps rows in tables"
                              (and (org-table-fit--overlay-at (point))
                                   (bolp)
                                   (= (save-excursion
                                        (forward-line -1) (point))
                                      row1)))
        (org-table-fit--check "row-or-line keeps overlays"
                              (= 3 (length org-table-fit--display-overlays)))
        (org-table-fit-previous-row-or-line)
        (org-table-fit--check "row-or-line steps back"
                              (= (point) row1)))
      ;; Skip correction also applies to the remapped commands.
      (goto-char (point-min))
      (search-forward "| alpha")
      (let ((bounds (org-table-fit--table-bounds)))
        (set-marker org-table-fit--display-previous-point above)
        (goto-char (cdr bounds))
        (let ((this-command 'org-table-fit-next-row-or-line))
          (org-table-fit--display-post-command))
        (org-table-fit--check "remapped motion enters first row"
                              (= (point) (car bounds)))))
    (org-table-fit-overlay-mode -1)))

(princ (format "\n%d failure(s)\n" org-table-fit--failures))
(kill-emacs org-table-fit--failures)
