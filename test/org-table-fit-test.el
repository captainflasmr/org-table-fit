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

(princ (format "\n%d failure(s)\n" org-table-fit--failures))
(kill-emacs org-table-fit--failures)
