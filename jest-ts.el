;;; jest-ts.el --- Find and run jest tests -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Tom Klaver
;;
;; Author: Tom Klaver <https://github.com/tommos0>
;; Maintainer: Tom Klaver <tomklav@gmail.com>
;; Created: februari 15, 2022
;; Modified: februari 15, 2022
;; Version: 0.0.1
;; Keywords: tools languages jest test typescript javascript
;; Homepage: https://github.com/tommos0/jest-ts.el
;; Package-Requires: ((emacs "25.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:
(require 'tree-sitter)
(require 'tree-sitter-langs)
(require 'term)

(defcustom jest-ts-query-sexp
  '((call_expression [(identifier) (member_expression)]
                     @identifier (.match? @identifier "^(describe|^it|^test)")
                     (arguments [(string) (template_string)] @test-name
                                [(arrow_function) (function)])) @contents)
  "The tree-sitter query to use for finding jest tests"
  :type 'sexp
  :group 'jest-ts)

(defun jest-ts--create-parser (language)
  "Create a new parser and set language to language"
  (let ((parser (tsc-make-parser)))
    (tsc-set-language parser (tree-sitter-require language))
    parser))

(defun jest-ts--parse-string (string query-sexp language)
  "Parse string using query-sexp in language"
  (let* ((parser (jest-ts--create-parser language))
         (tree (tsc-parse-string parser string))
         (root-node (tsc-root-node tree))
         (query (tsc-make-query (tree-sitter-require language)
                                query-sexp)))
    (tsc-query-captures query root-node #'tsc--buffer-substring-no-properties)))

(defun jest-ts--unquote (string)
  "Removes quotes from beginning and end of string"
  (replace-regexp-in-string "[\"`']?\\(.*?\\)[\"`']?$" "\\1" string))

(defun jest-ts-tests ()
  "Get the jest tests defined in the current buffer"
  (let* ((buffer-text (buffer-substring-no-properties (point-min) (point-max)))  
         (query-result (jest-ts--parse-string buffer-text jest-ts-query-sexp 'typescript))
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
