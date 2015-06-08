;; -*- lexical-binding: t -*-
;;
;;; Tests for tokenizer.el.

(require 'tokenizer)

;;; Tests
;;; -----

(defconst tokenizer-test-operator-str-list
  '("-" "+" "/" "*" "**" "%" "==" "="
    "!=" "<>" ":" "," ";" "<" ">" "<=" ">="))
(defconst tokenizer-test-operator-token-list
  '(MINUS PLUS SLASH STAR DOUBLESTAR PERCENT EQEQUAL EQUAL
          NOTEQUAL NOTEQUAL COLON COMMA SEMI LESS GREATER LESSEQUAL GREATEREQUAL))
(ert-deftest tokenizer-test-operators ()
  (loop for operator-str in tokenizer-test-operator-str-list
        for operator-token in tokenizer-test-operator-token-list
        do
        (tokenizer-test-with-tokens operator-str
          (should (equal 3 (length tokens)))
          (let ((token (first tokens)))
            (should (eq operator-token (first token)))
            (should (equal operator-str (second token)))))))

;;; Utils
;;; -----

(defmacro tokenizer-test-with-tokens (code &rest body)
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,code)
     (let* ((tokens (reverse (tokenizer-tokenize-region))))
       ,@body)))
