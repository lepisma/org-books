;;; org-books.el --- Reading list management with Org mode   -*- lexical-binding: t -*-

;; Copyright (C) 2017 Abhinav Tushar

;; Author: Abhinav Tushar <abhinav.tushar.vs@gmail.com>
;; Version: 0.2.10
;; Package-Requires: ((enlive "0.0.1") (s "1.11.0") (helm "2.9.2") (dash "2.14.1") (emacs "25"))
;; URL: https://github.com/lepisma/org-books
;; Keywords: outlines

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

(require 'org)
(require 's)
(require 'helm)
(require 'helm-org)
(require 'dash)
(require 'org-books-get-details)


(defgroup org-books nil
  "Org reading list management."
  :group 'org)

(defcustom org-books-file nil
  "File for keeping reading list."
  :type 'file
  :group 'org-books)

(defcustom org-books-add-to-top t
  "Should add new books as the first item under a heading?"
  :type 'boolean
  :group 'org-books)

(defcustom org-books-file-depth 2
  "The max depth for adding book under headings."
  :type 'integer
  :group 'org-books)

(defun org-books-create-file (file-path)
  "Write initialization stuff in a new file at FILE-PATH."
  (interactive "FFile: ")
  (if (file-exists-p file-path)
      (message "There is already a file present, skipping.")
    (with-temp-file file-path
      (insert "#+TITLE: Reading List\n"
              "#+AUTHOR: " (replace-regexp-in-string "" " " user-full-name) "\n\n"
              "#+TODO: READING NEXT | READ\n\n"))))

(defun org-books-all-authors ()
  "Return a list of authors in the org-books-file."
  (with-current-buffer (find-file-noselect org-books-file)
    (->> (org-property-values "AUTHOR")
       (-reduce-from (lambda (acc line) (append acc (s-split "," line))) nil)
       (-map #'s-trim)
       (-distinct)
       (-sort #'s-less-p))))

;;;###autoload
(defun org-books-cliplink ()
  "Clip link from clipboard."
  (interactive)
  (let ((url (substring-no-properties (current-kill 0))))
    (org-books-add-url url)))

;;;###autoload
(defun org-books-add-url (url)
  "Add book from web URL."
  (interactive "sUrl: ")
  (let ((url-type (org-books-get-url-type url org-books-url-patterns)))
    (if (null url-type)
        (message "Url not recognized")
      (let ((details (org-books-get-details url url-type)))
        (if (null details)
            (message "Error in fetching url. Please retry.")
          (apply #'org-books-add-book details))))))

;;;###autoload
(defun org-books-add-isbn (isbn)
  "Add book from ISBN."
  (interactive "sISBN: ")
  (org-books-add-url (org-books-get-url-from-isbn isbn)))

(defun org-books-format (level title author &optional props)
  "Format the book details."
  (with-temp-buffer
    (org-mode)
    (insert (make-string level ?*) " " title "\n")
    (org-set-property "AUTHOR" author)
    (org-set-property "ADDED" (format-time-string "[%Y-%02m-%02d]"))
    (dolist (prop props)
      (org-set-property (car prop) (cdr prop)))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun org-books--insert (level title author &optional props)
  "Insert book template (specified by TITLE and AUTHOR) at current position at LEVEL heading.
Also set all the PROPS for that org entry."
  (insert (org-books-format level title author props)))

(defun org-books-goto-place ()
  "Move to the position where insertion should happen."
  (if org-books-add-to-top
      (let ((level (or (org-current-level) 0))
            (bound (save-excursion (org-get-next-sibling))))
        (if (re-search-forward (format "^\\*\\{%s\\}" (+ level 1)) bound t)
            (previous-line)))
    (if (org-get-next-sibling)
        (previous-line)))
  (goto-char (line-end-position)))

;;;###autoload
(defun org-books-add-book (title author &optional props)
  "Add a book (specified by TITLE and AUTHOR) to the ‘org-books-file’.
Optionally apply PROPS."
  (interactive
   (let ((completion-ignore-case t))
     (list
      (read-string "Book Title: ")
      (s-join ", " (completing-read-multiple "Author(s): " (org-books-all-authors))))))
  (if org-books-file
      (save-excursion
        (with-current-buffer (find-file-noselect org-books-file)
          (let* ((helm-org-headings-max-depth org-books-file-depth)
                 (headers (helm-org-get-candidates (list (current-buffer)))))
            (if (null headers)
                (progn
                  (goto-char (point-max))
                  (org-books--insert 1 title author props)
                  (save-buffer))
              (helm :sources (helm-build-sync-source "org-book categories"
                               :candidates (-map (lambda (h) (cons (car h) (marker-position (cdr h)))) headers)
                               :action (lambda (pos)
                                         (org-content)
                                         (goto-char pos)
                                         (let ((level (or (org-current-level) 0)))
                                           (org-books-goto-place)
                                           (insert "\n")
                                           (org-books--insert (+ level 1) title author props)
                                           (save-buffer))))
                    :buffer "*helm org-books add*")))))
    (message "org-books-file not set")))

;;;###autoload
(defun org-books-rate-book (position rating)
  "Apply RATING to book at given POSITION."
  (interactive "d\nnRating (stars 1-5): ")
  (if (> rating 0)
      (org-set-property "RATING" (s-repeat rating ":star:"))))

(provide 'org-books)
;;; org-books.el ends here
