;; -*- lexical-binding: t -*-
;;
;; A simple RD parser for a Python-like language. Directly outputs an AST.

(require 'tokenizer)

(eval-when-compile (require 'names))

(define-namespace parser-

;;; Parser
;;; ------

(defvar-local token-stream
  nil)

(defun parse (tokens)
  (setq token-stream tokens))

(defun parse-stmt ()
  )

(defun parse-simple-stmt ()
  )

(defun parse-compound-stmt ()
  )

(defun parse-test ()
  )

(defun parse-end-test ()
  )

(defun parse-not-test ()
  )

(defun parse-comparison ()
  )

(defun parse-comp-op ()
  (print token-stream))

(defun parse-expr ()
  )

(defun parse-arith-expr ()
  )

(defun parse-term ()
  )

(defun parse-factor ()
  )

(defun parse-power ()
  )

(defun parse-atom ()
  )

(defun parse-trailer ()
  )

(defun parse-exprlist ()
  )

(defun parse-testlist ()
  )

(defun parse-arglist ()
  )

(defun parse-argument ()
  )

)
