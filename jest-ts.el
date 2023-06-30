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
;(require 'tree-sitter)
;(require 'tree-sitter-langs)
(require 'term)

(defcustom jest-ts-query-sexp
  '((call_expression
     function: [(identifier) (member_expression)]
     @function-name (:match "^\\(describe\\|it\\).*" @function-name)
     arguments: (arguments ([(string (string_fragment) @test-name)
			     (template_string) @test-name]))) @outer)

  "The tree-sitter query to use for finding jest tests."
  :type 'sexp
  :group 'jest-ts)

(defun jest-ts--unquote (string)
  "Removes quotes from beginning and end of string"
  (replace-regexp-in-string "[\"`']?\\(.*?\\)[\"`']?$" "\\1" string))

(defun jest-ts--tests ()
  "Get the jest tests defined in the current buffer"
  (let* ((captures (treesit-query-capture (treesit-buffer-root-node) jest-ts-query-sexp)))
    (mapcar (lambda (group)
	  (list (treesit-node-start (alist-get 'outer group))
		(treesit-node-end (alist-get 'outer group))
		(jest-ts--unquote (substring-no-properties
				   (treesit-node-text (alist-get 'test-name group))))))
        (seq-partition captures 3))))

(defun jest-ts-tests-at-point ()
  "Get tests surrounding current point"
  (seq-filter (lambda (test) (and (>= (point) (car test))
                             (<= (point) (cadr test))))
              (jest-ts--tests)))

(defun jest-ts--test-string-at-point ()
  "Get concatenated test name for tests surrounding current point"
  (mapconcat #'caddr (jest-ts-tests-at-point) " "))

(defun jest-ts--jest-path ()
  "Get the current jest path by running yarn which jest"
  (replace-regexp-in-string "\n$" "" (shell-command-to-string "yarn bin jest")))

(defcustom jest-ts-extra-node-args '()
  "Extra node arguments to pass to jest"
  :type 'list
  :group 'jest-ts)

(defun jest-ts-toggle-debug ()
  (interactive)
  (setq jest-ts-extra-node-args (if jest-ts-extra-node-args
                                    nil
                                  '("--inspect-brk"))))

;;;###autoload
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
      (setq buffer-read-only nil)
      (erase-buffer)
      (term-mode)
      (term-exec buffer "jest" "node" nil
                 `(,@jest-ts-extra-node-args
                   ,(jest-ts--jest-path)
                   "--color"
                   ,@(when arg '("--watch"))
                   "--testNamePattern"
                   ,test-string
                   ,file-name))
      (term-char-mode))
    (display-buffer buffer)))

(provide 'jest-ts)
;;; jest-ts.el ends here
