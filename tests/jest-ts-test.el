(load-file (expand-file-name "../jest-ts.el"))

(ert-deftest jest-ts--unquote-test ()
  (should (string-equal (jest-ts--unquote "'asd'") "asd" ))
  (should (string-equal (jest-ts--unquote "asd\"") "asd" ))
  (should (string-equal (jest-ts--unquote "`asd") "asd" ))
  (should (string-equal (jest-ts--unquote "asd") "asd" )))

(ert-deftest jest-ts-tests-test ()
  (with-temp-buffer
    (insert-file-contents "./example.test.ts")
    (should (= (length (jest-ts-tests)) 8))))

(ert-deftest jest-ts-tests-at-point-test-outside-tests ()
  (with-temp-buffer
    (insert-file-contents "./example.test.ts")
    (goto-char 72)
    (should (= (length (jest-ts-tests-at-point)) 0))))

(ert-deftest jest-ts-tests-at-point-test-outer ()
  (with-temp-buffer
    (insert-file-contents "./example.test.ts")
    (goto-char 85)
    (should (= (length (jest-ts-tests-at-point)) 1))
    (should (string-equal (cadar (jest-ts-tests-at-point)) "basket"))))

(ert-deftest jest-ts-test--string-at-point-test ()
  (with-temp-buffer
    (insert-file-contents "./example.test.ts")
    (goto-char 247)
    (should (string-equal (jest-ts--test-string-at-point) "basket Test A errors"))))

(ert-deftest jest-ts-test--string-at-point-test2 ()
  (with-temp-buffer
    (insert-file-contents "./example.test.ts")
    (goto-char 475)
    (should (string-equal (jest-ts--test-string-at-point) "basket Test B template string nests"))))
