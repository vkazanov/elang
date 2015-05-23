;; -*- lexical-binding: t -*-
;;
;; A simple RD parser for a Python-like language. Directly outputs an AST.

(require 'tokenizer)

(eval-when-compile (require 'names))

(define-namespace parser-

;;; Parser
;;; ------

(defvar token-stream
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

(defconst comp-ops (list "<" ">" "==" ">=" "<=" "<>" "!="))
(defun parse-comp-op ()
  (destructuring-bind
      (type value start end line) (pop token-stream)
    (when  (or (not (eq type 'OP))
               (not (member value comp-ops)))
      (throw 'parser-error "Unexpected token"))
    (intern value)))

(defun parse-expr ()
  )

(defun parse-arith-expr ()
  )

(defun parse-term ()
  (destructuring-bind
      (type value start end line) (pop token-stream)
    (when  (or (not (eq type 'OP))
               (not (member value comp-ops)))
      (throw 'parser-error "Unexpected token"))
    (intern value)))

(defun parse-atom ()
  (destructuring-bind
      (type value start end line) (pop token-stream)
    (cond
     ((eq type 'NAME)
      (message "NAME: %s" value)
      (intern value))
     ((eq type 'NUMBER)
      (message "NUMBER: %s" value)
      (string-to-number value))
     ((eq type 'STRING)
      (message "STRING: %s" value)
      (read value))
     (t (throw 'parser-error "Unexpected token")))))

(defun parse-power ()
  )

(defun parse-factor ()
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

) ;; end parser- namespace

(provide 'parser)
