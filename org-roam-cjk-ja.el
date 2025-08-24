;;; org-roam-cjk-ja.el --- Japanese customization  -*- lexical-binding: t -*-
;;
;; Copyright (C) 2025 Taro Sato
;;
;;; License:
;;
;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option) any later
;; version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License along with
;; this program. If not, see <https://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; This library provides the Japanese customization of `org-roam'.
;;
;;; Code:

(require 'org-roam-cjk)

(defcustom org-roam-cjk-ja-zenkaku-parens '("（「『【［〈《〔｛"
                                            "）」』】］〉》〕｝")
  "Zenkaku (全角) parentheses, left-side and right-side.")

(defcustom org-roam-cjk-ja-zenkaku-puncts "。、，・／；："
  "Zenkaku (全角) punctuations.")

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
  '(
    "スト"                    ; （ピアニ）スト山田花子
    "ター"                    ; （イラストレー）ター山田花子
    "家"                      ; （漫画）家山田花子
    "者"                      ; （哲学）者ソクラテス
    )
  "Miscellaneous terms that can come before a target term (proper noun).")

(setopt
 org-roam-cjk-rg-word-boundary-re
 (let* (;; Strict version:
        ;;
        ;; Unlike English, Japanese words are not delimited by character word
        ;; boundary. In this stricter version, we use "joshi" particles (助詞)
        ;; as boundary indicator. This implicitly assumes titles (words to be
        ;; delimited by the word boundary) to be "meishi" (名詞/noun).
        (l (mapconcat
            (lambda (it) (regexp-quote it))
            (append (cadr org-roam-cjk-ja-joshi)
                    (car org-roam-cjk-ja-joshi)
                    org-roam-cjk-ja-misc
                    (string-split (nth 0 org-roam-cjk-ja-zenkaku-parens) "" t)
                    (string-split (nth 1 org-roam-cjk-ja-zenkaku-parens) "" t)
                    (string-split org-roam-cjk-ja-zenkaku-puncts "" t))
            "|"))
        (r (mapconcat
            (lambda (it) (regexp-quote it))
            (append (cadr org-roam-cjk-ja-joshi)
                    (string-split (nth 1 org-roam-cjk-ja-zenkaku-parens) "" t)
                    (string-split (nth 0 org-roam-cjk-ja-zenkaku-parens) "" t)
                    (string-split org-roam-cjk-ja-zenkaku-puncts "" t))
            "|"))

        (word-boundary-re-strict
         (concat "|(\\b%1$s\\b"
                 "|(\\b|(?<=" l "))%1$s((?=" r ")|\\b))"))

        ;; Lenient version (Use any Japanese characters as word boundary):
        (j "[^\x20-\x7e\xff61-\xff9f]")
        (word-boundary-re-lenient
         (concat "|(\\b%1$s\\b"
                 "|(\\b|(?<=" j "))%1$s((?=" j ")|\\b))")))
   `((strict . ,word-boundary-re-strict)
     (lenient . ,word-boundary-re-lenient)
     (default . "|(\\b%1$s\\b)"))))

(provide 'org-roam-cjk-ja)
;;; org-roam-cjk-ja.el ends here
