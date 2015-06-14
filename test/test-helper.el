;; -*- lexical-binding: t -*-
;;
;;; Test utility macros and functions

(require 'tokenizer)
(require 'parser)

(defmacro with-tokenized (str &rest body)
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,str)
     (let* ((tokens (reverse (tokenizer-tokenize-region))))
       (setq-local parser-token-stream tokens)
       ,@body)))

(defmacro with-parsed (str &rest body)
  (declare (indent 1))
  `(let (parse-tree)
     (with-tokenized ,str
       (setq parse-tree (parser-parse-single-input)))
     ,@body))
