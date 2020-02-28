;; Tests

(load-file "../org-books-get-details.el")

(ert-deftest test-goodreads-url ()
  (let ((urls (list "https://www.goodreads.com"
                    "http://goodreads.com"
                    "http://www.goodreads.com/")))
    (dolist (url urls)
      (should (eq (org-books-get-url-type url org-books-url-patterns) 'goodreads)))))

(ert-deftest test-amazon-url ()
  (let ((urls (list "https://www.amazon.com"
                    "http://amazon.in"
                    "http://www.amazon.de")))
    (dolist (url urls)
      (should (eq (org-books-get-url-type url org-books-url-patterns) 'amazon)))))

(ert-deftest test-goodreads ()
  (let* ((url "https://www.goodreads.com/book/show/23754.Preludes_Nocturnes")
         (res (org-books-get-details-goodreads url)))
    (should (string-equal (first res) "Preludes & Nocturnes"))
    (should (string-equal (second res) "Neil Gaiman, Sam Kieth, Mike Dringenberg, Malcolm Jones III, Todd Klein, Karen Berger"))))

(ert-deftest test-amazon ()
  (let* ((url "https://www.amazon.com/Organization-Man-William-H-Whyte/dp/0812218191")
         (res (org-books-get-details-amazon url)))
    (should (string-equal (first res) "The Organization Man"))
    (should (string-equal (second res) "William H. Whyte, Joseph Nocera"))))

(ert-deftest test-amazon-with-author-page ()
  (let* ((url "https://www.amazon.com/Elements-Programming-Style-2nd/dp/0070342075")
         (res (org-books-get-details-amazon url)))
    (should (string-equal (first res) "The Elements of Programming Style, 2nd Edition"))
    (should (string-equal (second res) "Brian W. Kernighan, P. J. Plauger"))))

(ert-deftest test-isbn ()
  (let* ((isbn "0517149257")
	       (res (org-books-get-details-isbn (org-books-get-url-from-isbn isbn))))
    (should (string-equal (first res) "The Ultimate Hitchhiker's Guide"))
    (should (string-equal (second res) "Douglas Adams"))))
