(require 'tree-sitter)
(require 'tree-sitter-langs)
(require 'term)

(defun jest-ts--unquote (string)
  (replace-regexp-in-string "[\"`']?\\(.*\\)[\"`']$" "\\1" string))

(defun jest-ts--get-typescript-parser ()
  (let ((parser (tsc-make-parser)))
    (tsc-set-language parser (tree-sitter-require 'typescript))
    parser))

(defcustom jest-ts-query-sexp
  '((call_expression [(identifier) (member_expression)]
                     @identifier (.match? @identifier "^(describe|^it|^test)")
                     (arguments [(string) (template_string)] @test-name
                                [(arrow_function) (function)])) @contents)
  "The tree-sitter query to use for finding jest tests"
  :type 'sexp
  :group 'jest-ts)

(defun jest-ts-tests ()
  "Get the jest tests defined in the current buffer"
  (let* ((buffer-text (buffer-substring-no-properties (point-min) (point-max)))
         (parser (jest-ts--get-typescript-parser))
         (tree (tsc-parse-string parser buffer-text))
         (root-node (tsc-root-node tree))
         (query (tsc-make-query (tree-sitter-require 'typescript)
                                jest-ts-query-sexp))
         (query-result (tsc-query-captures query root-node #'ts--buffer-substring-no-properties))
         (match-tuples (seq-partition (append query-result nil) 3)))
    (mapcar (lambda (test)
              (list (tsc-node-position-range (alist-get 'contents test))
                    (jest-ts--unquote (tsc-node-text (alist-get 'test-name test)))))
            match-tuples)))

(defun jest-ts-tests-at-point ()
  "Get tests surrounding current point"
  (seq-filter (lambda (test) (and (>= (point) (caar test))
                                  (<= (point) (cdar test))))
              (jest-ts-tests)))

(defun jest-ts--test-string-at-point ()
  "Get concatenated test name for tests surrounding current point"
  (mapconcat #'cadr (jest-ts-tests-at-point) " "))

(defun jest-ts-run-test-at-point (arg)
  "Run the test at point"
  (interactive "P")
  (let* (
         (default-directory
           (file-name-as-directory
            (locate-dominating-file default-directory "package.json")))
         (buffer (get-buffer-create "jest"))
         (file-name (buffer-file-name))
         (test-string (jest-ts--test-string-at-point)))

     (with-current-buffer buffer
       (term-mode)
       (term-exec buffer "jest" "yarn" nil
                  `("jest"
                    "--color"
                    ,@(when arg '("--watch"))
                    "--testNamePattern"
                    ,test-string
                    ,file-name))
       (term-char-mode))
    (display-buffer buffer)))

(provide 'jest-ts)
;;; jest-ts.el ends here
