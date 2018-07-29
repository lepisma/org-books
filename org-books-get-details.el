;;; org-books-get-details.el --- Info fetching module for org-books -*- lexical-binding: t; -*-

;; Copyright (c) 2018 Abhinav Tushar

;; Author: Abhinav Tushar <lepisma@fastmail.com>

;;; Commentary:

;; Info fetching module for org-books
;; This file is not a part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'enlive)
(require 's)
(require 'url-parse)
(require 'cl-lib)

(defcustom org-books-url-patterns
  '((amazon . "^\\(www\\.\\)?amazon\\.")
    (goodreads . "^\\(www\\.\\)?goodreads\\.com"))
  "Patterns for detecting url types")

(defun org-books--clean-str (text)
  (s-trim (s-collapse-whitespace text)))

(defun org-books-get-url-type (url pattern-alist)
  "Return type of url using the regex pattern"
  (unless (null pattern-alist)
    (let ((pattern (cdr (car pattern-alist))))
      (if (s-matches? pattern (url-host (url-generic-parse-url url)))
          (caar pattern-alist)
        (org-books-get-url-type url (cdr pattern-alist))))))

(defun org-books-get-details (url url-type)
  (cl-case url-type
    (amazon (org-books-get-details-amazon url))
    (goodreads (org-books-get-details-goodreads url))))

(defun org-books-get-details-amazon (url)
  "Get book details from amazon page"
  (let* ((page-node (enlive-fetch url))
         (title (org-books--clean-str (enlive-text (enlive-get-element-by-id page-node "productTitle"))))
         (author (org-books--clean-str (s-join ", " (mapcar #'enlive-text (enlive-query-all page-node [.a-section .author > a]))))))
    (if (not (string-equal title ""))
        (list title author `(("AMAZON" . ,url))))))

(defun org-books-get-details-goodreads (url)
  "Get book details from goodreads page"
  (let* ((page-node (enlive-fetch url))
         (title (org-books--clean-str (enlive-text (enlive-get-element-by-id page-node "bookTitle"))))
         (author (org-books--clean-str (s-join ", " (mapcar #'enlive-text (enlive-query-all page-node [.authorName > span]))))))
    (if (not (string-equal title ""))
        (list title author `(("GOODREADS" . ,url))))))

(provide 'org-books-get-details)

;;; org-books-get-details.el ends here
