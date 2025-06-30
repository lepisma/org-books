;; Tests

(load-file "org-books.el")

(require 'f)
(require 's)

(defun files-equal (file-a file-b)
  (string-equal (f-read-text file-a 'utf-8)
                (f-read-text file-b 'utf-8)))

(ert-deftest test-goodreads ()
  (let* ((url "https://www.goodreads.com/book/show/23754.Preludes_Nocturnes")
         (res (org-books-get-details url)))
    (should (string-equal (first res) "The Sandman, Vol. 1: Preludes & Nocturnes"))
    (should (string-equal (second res) "Neil Gaiman, Sam Kieth, Mike Dringenberg, Malcolm Jones III, Todd Klein, Karen Berger"))))

(ert-deftest test-amazon ()
  (let* ((url "https://www.amazon.com/Organization-Man-William-H-Whyte/dp/0812218191")
         (res (org-books-get-details url)))
    (should (string-equal (first res) "The Organization Man"))
    (should (string-equal (second res) "William H. Whyte, Joseph Nocera"))))

(ert-deftest test-amazon-with-author-page ()
  (let* ((url "https://www.amazon.com/Elements-Programming-Style-2nd/dp/0070342075")
         (res (org-books-get-details url)))
    (should (string-equal (first res) "The Elements of Programming Style, 2nd Edition"))
    (should (string-equal (second res) "Brian W. Kernighan, P. J. Plauger"))))

(ert-deftest test-isbn ()
  (let* ((isbn "0517149257")
	       (res (org-books-get-details (org-books-get-url-from-isbn isbn))))
    (should (string-equal (first res) "The Ultimate Hitchhiker's Guide"))
    (should (string-equal (second res) "Douglas Adams"))))

(ert-deftest test-basic-insertion ()
  (let* ((pre-file "./test/files/insert-test-pre.org")
         (post-file "./test/files/insert-test-post.org")
         (org-books-file (make-temp-file "org-books-test" nil ".org" (f-read-text pre-file 'utf-8))))
    (with-current-buffer (find-file-noselect org-books-file)
      (org-books--insert-at-pos (point-max) "Book Title" "Book Author" '(("ADDED" . "[2020-05-10]"))))
    (should (files-equal org-books-file post-file))
    (f-delete org-books-file)))
