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

(defun parse-arith-expr ()
  )

(defun parse-term ()
  (dbind (type value start end line) (pop token-stream)
    (when  (or (not (eq type 'OP))
               (not (member value comp-ops)))
      (throw 'parser-error "Unexpected token"))
    (intern value)))

(defun parse-atom ()
  (dbind (type value start end line) (pop token-stream)
    (cond
     ((eq type 'NAME)
      (intern value))
     ((eq type 'NUMBER)
      (string-to-number value))
     ((eq type 'STRING)
      (read value))
     (t (throw 'parser-error "Unexpected token")))))

(defun parse-power ()
  (let ((atom (parse-atom)))
    (dbind (type value start end line) (car token-stream)
      (cond
       ((check-current-token 'OP '("**"))
        (pop token-stream)
        (list (intern "**") atom (parse-power)))
       (t atom)))))

(defun parse-factor ()
  (dbind (type value start end line) (car token-stream)
    (cond
     ((check-current-token 'OP '("+" "-"))
      (pop token-stream)
      (list (intern value) (parse-factor)))
     (t
      (parse-power)))))

(defun parse-term ()
  (let ((factor (parse-factor)))
    (dbind (type value start end line) (car token-stream)
      (cond
       ((check-current-token 'OP '("*" "/" "%"))
        (pop token-stream)
        (list (intern value) factor (parse-factor)))
       (t
        factor)))))

(defun parse-expr ()
  (let ((term (parse-term)))
    (dbind (type value start end line) (car token-stream)
      (cond
       ((check-current-token 'OP '("-" "+"))
        (pop token-stream)
        (list (intern value) term (parse-term)))
       (t
        term)))))

(defconst comp-ops (list "<" ">" "==" ">=" "<=" "<>" "!="))
(defun parse-comp-op ()
  (when (not (check-current-token 'OP comp-ops))
    (throw 'parser-error "Unexpected token"))
  (dbind (type value start end line) (pop token-stream)
    (intern value)))

(defun parse-comparison ()
  (let ((expr (parse-expr)))
    (while (check-current-token 'OP comp-ops)
      (setq expr (list (parse-comp-op) expr (parse-expr))))
    expr))

(defun parse-not-test ()
  (cond ((check-current-token 'NAME '("not"))
         (pop token-stream)
         (list 'not (parse-not-test)))
        (t (parse-comparison))))

(defun parse-and-test ()
  (let ((not-test (parse-not-test)))
    (while (check-current-token 'NAME '("and"))
      (pop token-stream)
      (setq not-test (list 'and not-test (parse-not-test))))
    not-test))

(defun parse-test ()
  (let ((test (parse-and-test)))
    (while (check-current-token 'NAME '("or"))
      (pop token-stream)
      (setq test (list 'or test (parse-and-test))))
    test))

(defun parse-exprlist ()
  (let ((exprlist (list (parse-expr))))
    (while (check-current-token 'OP '(","))
      (pop token-stream)
      (push (parse-expr) exprlist))
    (reverse exprlist)))

(defun parse-testlist ()
  (let ((testlist (list (parse-test))))
    (while (check-current-token 'OP '(","))
      (pop token-stream)
      (push (parse-test) testlist))
    (reverse testlist)))

(defun parse-arglist ()
  )

(defun parse-argument ()
  )

;;; Utils
;;; -----

(defun check-current-token (check-type &optional check-values)
  (dbind (type value start end line) (car token-stream)
    (and (eq type check-type)
         (if check-values
             (member value check-values)
           t))))

(defalias 'dbind 'destructuring-bind)

) ;; end parser- namespace

(provide 'parser)
