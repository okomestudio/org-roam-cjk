;;; org-roam-cjk.el --- Org Roam Japanese Extensions  -*- lexical-binding: t -*-
;;
;; Copyright (C) 2025 Taro Sato
;;
;; Author: Taro Sato <okomestudio@gmail.com>
;; URL: https://github.com/okomestudio/org-roam-cjk
;; Version: 0.1.1
;; Keywords: org-mode, org-roam, plug-in
;; Package-Requires: ((emacs "30.1") (org "9.7") (org-roam "2.3.0"))
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

(setopt
 org-roam-cjk-unlinked-references-word-boundary-re
 (let* (;; Strict version:
        ;;
        ;; Unlike English, Japanese words are not delimited by character
        ;; word boundary. In this stricter version, we rely on "joshi"
        ;; particles (助詞; ja.wikipedia.org/wiki/%E5%8A%A9%E8%A9%9E) as
        ;; boundary indicator. This implicitly assumes titles (words to be
        ;; delimited by the word boundary) to be "meishi" (名詞).

        ;; The collection of joshi that comes on the right side of word:
        (joshi_r '("か" "が" "かしら" "がてら" "から" "きり" "くらい" "ぐらい" "こそ"
                   "さ" "さえ" "しか" "ずつ"
                   "だけ" "だの" "で" "では" "でも" "と" "とは" "とも"
                   "ながら" "なぞ" "など" "なり" "なんぞ" "に" "ね" "の" "のみ"
                   "は" "ばかり" "へ" "ほど"
                   "まで" "も"
                   "や" "やら" "よ" "より"
                   "を"))
        ;; The collection of joshi that comes on the left side of word:
        (joshi_l (append joshi_r
                         '("かい" "かり" "けど" "けれど" "けれども"
                           "し" "ぜ" "ぞ"
                           "たり" "つつ" "ってば" "て" "ても" "ところで" "とも"
                           "な" "ので" "のに"
                           "ば"
                           "まま" "ものか" "ものの" "もん"
                           "わ")))
        ;; These are not joshi, but nouns that can come before
        ;; (proper) nouns for descriptions, e.g., "政治家田中角栄":
        (joshi_l (append joshi_l
                         '("家" "スト")))

        (word-boundary-re-strict
         (concat "|(\\b%1$s\\b"
                 "|(?<="
                 (s-join "|" joshi_l) ")%1$s(?=" (s-join "|" joshi_r) "))"))

        ;; Lenient version:
        ;;
        ;; Use any Japanese characters as word boundary:
        (word-boundary-re-lenient
         (concat "|(\\b%1$s\\b"
                 "|(\\b%1$s\\b"
                 "|(?<=[^\x20-\x7e\xff61-\xff9f])%1$s(?=[^\x20-\x7e\xff61-\xff9f]))")))
   word-boundary-re-strict))

(provide 'org-roam-cjk-ja)
;;; org-roam-cjk-ja.el ends here
