;; -*- lexical-binding: t -*-
;;
;;; Tests for parser.el.

(require 'parser)
(require 'tokenizer)

;;; Tests
;;; -----

(ert-deftest parser-test-comp-op ()
  (dolist (op (list "<" ">" "==" ">=" "<=" "<>" "!="))
    (parser-test-with-tokenized op
      (let ((token (first tokens)))
        (should (eq (first token) 'OP))
        (should (equal (second token) op))
        (should (eq (parser-parse-comp-op)
                    (intern op)))))))

(ert-deftest parser-test-atom ()
  (loop for text in (list "name" "123" "\"a string\"")
        for token-type in (list 'NAME 'NUMBER 'STRING)
        for parse-res in (list 'name 123 "a string")
        do (parser-test-with-tokenized text
             (let ((token (first tokens)))
               (should (eq (first token)
                           token-type))
               (should (equal (parser-parse-atom)
                              parse-res))))))

;; (ert-deftest parser-test-expr ()
;;   (parser-test-with-tokenized "(a + b) * 2"
;;     (should (equal '(* (+ a b) 2)
;;                    (parser-parse-expr)))))

;;; Utils
;;; -----

(defmacro parser-test-with-tokenized (code &rest body)
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,code)
     (let* ((tokens (reverse (tokenizer-tokenize-region))))
       (setq-local parser-token-stream tokens)
       ,@body)))
