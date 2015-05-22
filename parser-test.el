;; -*- lexical-binding: t -*-
;;
;;; Tests for parser.el.

(require 'parser)
(require 'tokenizer)

;;; Tests
;;; -----

(ert-deftest parser-test-expr ()
  "Check basic expression parsing"
  (parser-test-with-tokenized "(a + b) * 2"
    (setq-local parser-token-stream tokens)
    (should (equal '(* (+ a b) 2)
                   (parser-parse-expr)))))

;;; Utils
;;; ----------

(defmacro parser-test-with-tokenized (code &rest body)
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,code)
     (let ((tokens (tokenizer-tokenize-region)))
       ,@body)))
