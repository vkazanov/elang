;; -*- lexical-binding: t -*-
;;
;;; Tests for tokenizer.el.
;;
;; NOTE: tests are incomplete

(require 'tokenizer)
(require 'cl)

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
        (evaluator-with-tokenized operator-str
          (let ((token (first tokens)))
            (should (eq operator-token (first token)))
            (should (equal operator-str (second token)))))))

(ert-deftest tokenizer-test-strings ()
  (evaluator-with-tokenized "\"a simple string\""
    (let ((token (first tokens)))
      (should (eq 'STRING (first token)))
      (should (equal "\"a simple string\"" (second token)))))
  (evaluator-with-tokenized "\"\"\"triple string\"\"\""
    (let ((token (first tokens)))
      (should (eq 'STRING (first token)))
      (should (equal "\"\"\"triple string\"\"\"" (second token)))))
  (evaluator-with-tokenized "'single'"
    (let ((token (first tokens)))
      (should (eq 'STRING (first token)))
      (should (equal "'single'" (second token))))))
