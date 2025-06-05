;;; org-roam-cjk.el --- Org Roam CJK Extensions  -*- lexical-binding: t -*-
;;
;; Copyright (C) 2025 Taro Sato
;;
;; Author: Taro Sato <okomestudio@gmail.com>
;; URL: https://github.com/okomestudio/org-roam-cjk
;; Version: 0.1.1
;; Keywords: org-mode, org-roam, plug-in
;; Package-Requires: ((emacs "30.1") (org "9.7") (org-roam "2.3.0") (magit-section "3.0.0"))
;;
;;; License:
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; This library provides a version of
;; `org-roam-unlinked-references-section' for `org-roam' notes written
;; in a CJK (Chinese, Japanese, Korean) language.
;;
;;; Code:

(require 'magit-section)
(require 'org-roam)

(defcustom org-roam-cjk-unlinked-references-word-boundary-re "|(\\b%1$s\\b)"
  "The word bounday regex used by ripgrep for unlinked references.
Regex's word boundary (\b) does not correctly determine how words and
phrases should be tokenized in a CJK language. This custom variable
allows customization."
  :group 'org-roam
  :type 'string)

(defcustom org-roam-cjk-unlinked-references-max-results-count 1000
  "The max number of items in the unlinked references section.
Rendering of the unlinked references section can appear to freeze when
the match count is very large. This number limits the maximum number of
matched unlinked references to show to prevent the issue."
  :group 'org-roam
  :type 'integer)

(defcustom org-roam-cjk-unlinked-references-ignore-lines '("begin_src.+"
                                                           "filetags:.+"
                                                           "property:.+")
  "The regexp for excluding matched lines from result."
  :group 'org-roam
  :type '(repeat string))

(defun org-roam-cjk-unlinked-references--result-filter-p
    (matched-text matched-file row col titles node)
  "Filter if the match is considered an unlinked reference.
Return non-nil if MATCHED-TEXT at ROW and COL in MATCHED-FILE is an
unlinked reference, or return nil. TITLES and NODE are supplied for use
in the conditional expression."
  (let ((linked-re (format "\\[\\[id:%s\\]\\[.*\\]\\]" (org-roam-node-id node))))
    (if (not (file-equal-p (org-roam-node-file node) matched-file))
        ;; The matched text is in a different file from the current node.
        (not (string-match linked-re matched-text)) ; Is this reference unlinked?
      ;; The matched text is in the same file of the current node.
      (let* ((other-node (save-match-data
                           (with-current-buffer (find-file-noselect matched-file)
                             (save-excursion
                               (goto-char (point-min))
                               (forward-line (1- row))
                               (move-to-column col)
                               (org-roam-node-at-point))))))
        (if (not (string-equal (org-roam-node-id node)
                               (org-roam-node-id other-node)))
            ;; The matched text is in a different node from the
            ;; current node within the same file.
            (not (string-match linked-re matched-text)) ; Is this reference unlinked?
          )))))

(defun org-roam-cjk-unlinked-references--preview-line
    (file row col file-prev row-prev col-prev)
  "Return the preview line from FILE.
The line was matched with text at ROW and COL. FILE-PREV, ROW-PREV, and
COL-PREV points to the previous line and can be used to control
rendering."
  (if (and (string= file file-prev) (= row row-prev))
      ;; Use a "ditto" mark if the current line is
      ;; similar to the previous line.
      "⎯〃⎯"
    (with-temp-buffer
      (insert-file-contents file)
      (forward-line (1- row))
      (buffer-substring-no-properties
       (save-excursion (beginning-of-line) (point))
       (save-excursion (end-of-line) (point))))))

(defun org-roam-cjk-unlinked-references--file-glob-args ()
  "Construct file glob arguments for ripgrep."
  (mapconcat (lambda (glob) (concat "-g " glob))
             (org-roam--list-files-search-globs org-roam-file-extensions)
             " "))

(defun org-roam-cjk-unlinked-references--title-regex (titles)
  "Construct a ripgrep regex pattern from TITLES.
The output expression should be sanitized for the shell use."
  (let* (;; Apply word boundaries to each title
         (bounded-re
          (substring
           (mapconcat #'org-roam-cjk-unlinked-references--apply-word-boundary-re
                      titles "")
           1))

         ;; For variable-lengths lookbehinds in PCRE, see:
         ;;
         ;;   - www.drregex.com/2019/02/variable-length-lookbehinds-actually.html
         ;;
         ;; `plb' is for positive lookbehind; `nlb' is for negative lookbehind.
         (plb "(?=(?'a'[\\s\\S]*))(?'b'(%s)(?=\\k'a'\\z)|(?<=(?=x^|(?&b))[\\s\\S]))")
         (nlb "(?!(?=(?<a>[\\s\\S]*))(?<b>(%s)(?=\\k<a>\\z)|(?<=(?=x^|(?&b))[\\s\\S])))")

         ;; The list of substrings for negative matching:
         (ignore-lines org-roam-cjk-unlinked-references-ignore-lines))
    (format "'\\[\\[id:[0-9a-f-]+\\]\\[[^][]*(%s)[^][]*\\]\\]|%s(%s)'"
            bounded-re
            (format nlb (string-join ignore-lines "|"))
            bounded-re)))

(defun org-roam-cjk-unlinked-references--apply-word-boundary-re (title)
  "Wrap TITLE with word boundary regex."
  (let* (;; Expand quote variants:
         (s (replace-regexp-in-string " ['\‘]\\(\\w\\)" " ['\‘]\\1" title))
         (s (replace-regexp-in-string "\\(\\w\\)['\’]" "\\1['\’]" s))
         (s (replace-regexp-in-string " [\"\“]\\(\\w\\)" " [\"\“]\\1" s))
         (s (replace-regexp-in-string "\\(\\w\\)[\"\”]" "\\1[\"\”]" s))

         (s (format org-roam-cjk-unlinked-references-word-boundary-re
                    (org-roam-cjk-unlinked-references--sanitize-title title)))

         ;; Some special chars needs unescaping after `shell-quotes':
         (s (replace-regexp-in-string "[\\][[]\\([^][]+\\)[\\][]]" "[\\1]" s)))
    s))

(defun org-roam-cjk-unlinked-references--sanitize-title (title)
  "Sanitize TITLE for shell use."
  (mapconcat #'shell-quote-argument (split-string title "'") "'\"'\"'"))

(defun org-roam-cjk-unlinked-references--rg-command (titles)
  "Return the ripgrep command searching for TITLES."
  (concat "rg --follow --only-matching --vimgrep --pcre2 --ignore-case "
          (org-roam-cjk-unlinked-references--file-glob-args) " "
          (org-roam-cjk-unlinked-references--title-regex titles) " "
          (shell-quote-argument org-roam-directory)))

(defun org-roam-cjk-unlinked-references-section (node)
  "The unlinked references section for NODE.
References from FILE are excluded."
  (when (and
         (executable-find "rg")
         (org-roam-node-title node)
         (not (string-match "PCRE2 is not available"
                            (shell-command-to-string "rg --pcre2-version"))))
    (let* ((titles (cons (org-roam-node-title node)
                         (org-roam-node-aliases node)))
           (rg-command (org-roam-cjk-unlinked-references--rg-command titles))
           (results (split-string (shell-command-to-string rg-command) "\n"))
           (match_count 0)
           f f-prev row row-prev col col-prev matched-text)
      (magit-insert-section
          (unlinked-references)
        (magit-insert-heading "Unlinked References:")
        (catch 'limit-result
          (dolist (line results)
            (save-match-data
              (when (string-match org-roam-unlinked-references-result-re line)
                (setq f (match-string 1 line)
                      row (string-to-number (match-string 2 line))
                      col (string-to-number (match-string 3 line))
                      matched-text (match-string 4 line))
                (when (and matched-text
                           (org-roam-cjk-unlinked-references--result-filter-p
                            matched-text f row col titles node))
                  (magit-insert-section
                      section (org-roam-grep-section)
                      (oset section file f)
                      (oset section row row)
                      (oset section col col)
                      (insert (propertize
                               (format "%s:%s:%s"
                                       (truncate-string-to-width (file-name-base f)
                                                                 15 nil nil t)
                                       row col)
                               'font-lock-face 'org-roam-dim)
                              " "
                              (org-roam-fontify-like-in-org-mode
                               (org-roam-cjk-unlinked-references--preview-line
                                f row col f-prev row-prev col-prev))
                              "\n")
                      (setq f-prev f
                            row-prev row
                            col-prev col
                            match_count (+ match_count 1))
                      (if (= match_count org-roam-cjk-unlinked-references-max-results-count)
                          (insert (format "WARNING: Results truncated to %d items (%d potential matches)"
                                          match_count (length results))))))))
            (if (= match_count org-roam-cjk-unlinked-references-max-results-count)
                ;; Throw outside of magit-insert-section to render correct item count.
                (throw 'limit-result match_count))))
        (insert ?\n)))))

(defun org-roam-cjk--mode-hook ()
  "Format `org-roam' buffer for ease of viewing multi-line items."
  (when (featurep 'adaptive-wrap)
    (turn-on-visual-line-mode)
    (setq-local adaptive-wrap-extra-indent 4)
    (adaptive-wrap-prefix-mode 1)))

(add-hook 'org-roam-mode-hook #'org-roam-cjk--mode-hook)

(provide 'org-roam-cjk)
;;; org-roam-cjk.el ends here
