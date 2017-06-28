;;; org-books.el --- Reading list management with Org mode   -*- lexical-binding: t -*-

;; Copyright (C) 2017 Abhinav Tushar

;; Author: Abhinav Tushar <abhinav.tushar.vs@gmail.com>
;; Version: 0.1.3
;; Package-Requires: ((emojify "0.4") (enlive "0.0.1") (s "1.11.0"))
;; URL: https://github.com/lepisma/org-books

;;; Commentary:

;; org-books.el is a tool for managing reading list in an Org mode file.
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
(require 'org)
(require 's)


(defgroup org-books nil
  "Org reading list management"
  :group 'org)

(defcustom org-books-file nil
  "File for keeping reading list"
  :type 'string
  :group 'org-books)

(defun org-books-create-file (file-path)
  "Write initialization stuff in a new file"
  (interactive "FFile: ")
  (if (file-exists-p file-path)
      (message "There is already a file present, skipping.")
    (with-temp-file file-path
      (insert "#+TITLE: Reading List\n"
              "#+AUTHOR: " (replace-regexp-in-string "" " " user-full-name) "\n\n"
              "#+TODO: READING NEXT | READ\n\n"))))

;;;###autoload
(defun org-books-add-url (url)
  "Add book from web url"
  (interactive "sUrl: ")
  (let ((details (cond ((s-contains? "amazon.com" url) (org-books-get-details-amazon url))
                       ((s-contains? "goodreads.com" url) (org-books-get-details-goodreads url))
                       (t (message "Url not recognized")))))
    (if details
        (apply #'org-books-add-book details))))

(defun org-books-get-details-amazon (url)
  "Get book details from amazon page"
  (let ((page-node (enlive-fetch url)))
    (list (enlive-text (enlive-get-element-by-id page-node "productTitle"))
          (s-join ", " (mapcar #'enlive-text (enlive-get-elements-by-class-name page-node "contributorNameID")))
          `(("AMAZON" . ,url)))))

(defun org-books-get-details-goodreads (url)
  "Get book details from goodreads page"
  (let ((page-node (enlive-fetch url)))
    (list (s-trim (enlive-text (enlive-get-element-by-id page-node "bookTitle")))
          (s-join ", " (mapcar #'enlive-text (enlive-get-elements-by-class-name page-node "authorName")))
          `(("GOODREADS" . ,url)))))

;;;###autoload
(defun org-books-add-book (title author &optional props)
  "Add a book to the org-books-file. Optional add props"
  (interactive "sBook Title: \nsAuthor: ")
  (if org-books-file
      (with-temp-buffer
        (org-mode)
        (org-insert-heading)
        (insert title "\n")
        (org-set-property "AUTHOR" author)
        (org-set-property "ADDED" (time-stamp-string "<%:y-%02m-%02d>"))
        (mapc (lambda (p) (org-set-property (car p) (cdr p))) props)
        (insert "\n")
        (append-to-file (point-min) (point-max) org-books-file))
    (message "org-books-file not set")))

;;;###autoload
(defun org-books-finish-book (rating)
  "Finish book at point."
  (interactive "nRating (stars 1-5, 0 -> NA): ")
  (org-todo "READ")
  (if (> rating 0)
      (org-set-property "RATING" (s-join "" (loop for i to (- rating 1) collect ":star:")))))

(provide 'org-books)
;;; org-books.el ends here
