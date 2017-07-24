;; Tests

(load-file "org-books.el")

(ert-deftest test-goodreads ()
  (let* ((url "https://www.goodreads.com/book/show/23754.Preludes_Nocturnes")
         (res (org-books-get-details-goodreads url)))
    (should (string-equal (first res) "Preludes & Nocturnes (The Sandman #1)"))
    (should (string-equal (second res) "Neil Gaiman, Sam Kieth, Mike Dringenberg, Malcolm Jones III, Todd Klein, Karen Berger"))))

(ert-deftest test-amazon ()
  (let* ((url "https://www.amazon.com/Organization-Man-William-H-Whyte/dp/0812218191")
         (res (org-books-get-details-amazon url)))
    (should (string-equal (first res) "The Organization Man"))
    (should (string-equal (second res) "William H. Whyte, Joseph Nocera"))))
