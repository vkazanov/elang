;; -*- lexical-binding: t -*-
;;
;;; Tests for parser.el.

(require 'parser)
(require 'tokenizer)

;;; Tests
;;; -----

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

(ert-deftest parser-test-power ()
  (parser-test-with-tokenized "123"
    (let ((token (first tokens)))
      (should (eq (first token)
                  'NUMBER))
      (should (equal "123"
                     (second token)))
      (should (equal 123
                     (parser-parse-atom)))))
  (parser-test-with-tokenized "123**2"
    (should (equal (parser-parse-power)
                   '(** 123 2))))
  (parser-test-with-tokenized "123**2**3"
    (should (equal (parser-parse-power)
                   '(** 123 (** 2 3))))))

(ert-deftest parser-test-factor ()
  (parser-test-with-tokenized "123"
    (should (equal 123
                   (parser-parse-factor))))
  (parser-test-with-tokenized "-123"
    (should (equal '(- 123)
                   (parser-parse-factor))))
  (parser-test-with-tokenized "+123"
    (should (equal '(+ 123)
                   (parser-parse-factor)))))

(ert-deftest parser-test-term ()
  (parser-test-with-tokenized "123"
    (should (equal 123
                   (parser-parse-term))))
  (parser-test-with-tokenized "123 * 456"
    (should (equal '(* 123 456)
                   (parser-parse-term))))
  (parser-test-with-tokenized "123 / 456"
    (should (equal '(/ 123 456)
                   (parser-parse-term))))
  (parser-test-with-tokenized "123 % 456"
    (should (equal '(% 123 456)
                   (parser-parse-term)))))

(ert-deftest parser-test-expr ()
  (parser-test-with-tokenized "123"
    (should (equal 123
                   (parser-parse-expr))))
  (parser-test-with-tokenized "123 + 456"
    (should (equal '(+ 123 456)
                   (parser-parse-expr))))
  (parser-test-with-tokenized "123 - 456"
    (should (equal '(- 123 456)
                   (parser-parse-expr)))))

(ert-deftest parser-test-comp-op ()
  (dolist (op parser-comp-ops)
    (parser-test-with-tokenized op
      (let ((token (first tokens)))
        (should (eq (first token) 'OP))
        (should (equal (second token) op))
        (should (eq (parser-parse-comp-op)
                    (intern op)))))))

(ert-deftest parser-test-comparison ()
  (parser-test-with-tokenized "1"
    (should (equal '1
                   (parser-parse-comparison))))
  (parser-test-with-tokenized "1 < 2"
    (should (equal '(< 1 2)
                   (parser-parse-comparison))))
  (parser-test-with-tokenized "1 < 2 < 3"
    (should (equal '(< (< 1 2) 3)
                   (parser-parse-comparison)))))

(ert-deftest parser-test-not-test ()
  (parser-test-with-tokenized "2"
    (should (equal 2
                   (parser-parse-not-test))))
  (parser-test-with-tokenized "not 2"
    (should (equal '(not 2)
                   (parser-parse-not-test)))))

(ert-deftest parser-test-and-test ()
  (parser-test-with-tokenized "2"
    (should (equal 2
                   (parser-parse-and-test))))
  (parser-test-with-tokenized "2 and 3"
    (should (equal '(and 2 3)
                   (parser-parse-and-test)))))

(ert-deftest parser-test-test ()
  (parser-test-with-tokenized "2"
    (should (equal 2
                   (parser-parse-and-test))))
  (parser-test-with-tokenized "2 or 3"
    (should (equal '(or 2 3)
                   (parser-parse-test)))))

;;; Utils
;;; -----

(defmacro parser-test-with-tokenized (code &rest body)
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,code)
     (let* ((tokens (reverse (tokenizer-tokenize-region))))
       (setq-local parser-token-stream tokens)
       ,@body)))
