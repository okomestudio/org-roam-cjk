;;; org-roam-cjk.el --- Org Roam Japanese Extensions  -*- lexical-binding: t -*-
;;
;; Copyright (C) 2025 Taro Sato
;;
;; Author: Taro Sato <okomestudio@gmail.com>
;; URL: https://github.com/okomestudio/org-roam-cjk
;; Version: 0.2.1
;; Keywords: org-mode, org-roam, plug-in
;; Package-Requires: ((emacs "30.1") (org "9.7") (org-roam "2.3.0") (s "1.13.1"))
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
;; This library provides the Japanese extension using `org-roam-cjk'.
;;
;;; Code:

(require 'org-roam-cjk)
(require 's)

(defcustom org-roam-cjk-ja-zenkaku-parens '("（「『【［〈《〔｛"
                                            "）」』】］〉》〕｝")
  "Zenkaku parentheses, left-side and right-side.")

(defcustom org-roam-cjk-ja-zenkaku-puncts "。、，・／；："
  "Zenkaku punctuations.")

(defcustom org-roam-cjk-ja-joshi
  '(("かい" "かり" "けど" "けれど" "けれども"
     "し" "ぜ" "ぞ"
     "たり" "つつ" "ってば" "て" "ても" "ところで" "とも"
     "な" "ので" "のに"
     "ば"
     "まま" "ものか" "ものの" "もん"
     "わ")
    ("か" "が" "かしら" "がてら" "から" "きり" "くらい" "ぐらい" "こそ"
     "さ" "さえ" "しか" "ずつ"
     "だけ" "だの" "で" "では" "でも" "と" "とは" "とも"
     "ながら" "なぞ" "など" "なり" "なんぞ" "に" "ね" "の" "のみ"
     "は" "ばかり" "へ" "ほど"
     "まで" "も"
     "や" "やら" "よ" "より"
     "を"))
  "Joshi (助詞) terms.
Each can come on the left side (first element) or the right side (second
element) of a target term.")

(defcustom org-roam-cjk-ja-misc
  '("家"                      ; （漫画）家山田花子
    "スト"                    ; （ピアニ）スト山田花子
    "ター"                    ; （イラストレー）ター山田花子
    )
  "Miscellaneous terms that can come before a target term (proper noun).")

(setopt
 org-roam-cjk-unlinked-references-word-boundary-re
 (let* (;; Strict version:
        ;;
        ;; Unlike English, Japanese words are not delimited by
        ;; character word boundary. In this stricter version, we use
        ;; "joshi" particles (助詞) as boundary indicator. This
        ;; implicitly assumes titles (words to be delimited by the
        ;; word boundary) to be "meishi" (名詞/noun).
        (l (append (cadr org-roam-cjk-ja-joshi)
                   (car org-roam-cjk-ja-joshi)
                   org-roam-cjk-ja-misc
                   (string-split (nth 0 org-roam-cjk-ja-zenkaku-parens) "" t)
                   (string-split (nth 1 org-roam-cjk-ja-zenkaku-parens) "" t)
                   (string-split org-roam-cjk-ja-zenkaku-puncts "" t)))
        (r (append (cadr org-roam-cjk-ja-joshi)
                   (string-split (nth 1 org-roam-cjk-ja-zenkaku-parens) "" t)
                   (string-split (nth 0 org-roam-cjk-ja-zenkaku-parens) "" t)
                   (string-split org-roam-cjk-ja-zenkaku-puncts "" t)))

        (word-boundary-re-strict
         (concat "|(\\b%1$s\\b"
                 "|(?<="
                 (s-join "|" (mapcar (lambda (it) (regexp-quote it)) l))
                 ")%1$s(?="
                 (s-join "|" (mapcar (lambda (it) (regexp-quote it)) r))
                 "))"))

        ;; Lenient version (Use any Japanese characters as word
        ;; boundary):
        (word-boundary-re-lenient
         (concat "|(\\b%1$s\\b"
                 "|(\\b%1$s\\b"
                 "|(?<=[^\x20-\x7e\xff61-\xff9f])"
                 "%1$s"
                 "(?=[^\x20-\x7e\xff61-\xff9f]))")))
   word-boundary-re-strict))

(provide 'org-roam-cjk-ja)
;;; org-roam-cjk-ja.el ends here
