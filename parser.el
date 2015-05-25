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

(defun parse-atom ()
  (dbind (type value start end line) (pop token-stream)
    (cond
     ((and (eq type 'OP)
           (equal value "("))
      (let ((testlist (parse-testlist)))
        (pop-current-token 'OP '(")"))
        testlist))
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
       ((current-token-p 'OP '("**"))
        (pop token-stream)
        (list (intern "**") atom (parse-power)))
       (t atom)))))

(defun parse-factor ()
  (dbind (type value start end line) (car token-stream)
    (cond
     ((current-token-p 'OP '("+" "-"))
      (pop token-stream)
      (list (intern value) (parse-factor)))
     (t
      (parse-power)))))

(defun parse-term ()
  (let ((factor (parse-factor)))
    (dbind (type value start end line) (car token-stream)
      (cond
       ((current-token-p 'OP '("*" "/" "%"))
        (pop token-stream)
        (list (intern value) factor (parse-factor)))
       (t
        factor)))))

(defun parse-expr ()
  (let ((term (parse-term)))
    (dbind (type value start end line) (car token-stream)
      (cond
       ((current-token-p 'OP '("-" "+"))
        (pop token-stream)
        (list (intern value) term (parse-term)))
       (t
        term)))))

(defconst comp-ops (list "<" ">" "==" ">=" "<=" "<>" "!="))
(defun parse-comp-op ()
  (when (not (current-token-p 'OP comp-ops))
    (throw 'parser-error "Unexpected token"))
  (dbind (type value start end line) (pop token-stream)
    (intern value)))

(defun parse-comparison ()
  (let ((expr (parse-expr)))
    (while (current-token-p 'OP comp-ops)
      (setq expr (list (parse-comp-op) expr (parse-expr))))
    expr))

(defun parse-not-test ()
  (cond ((current-token-p 'NAME '("not"))
         (pop token-stream)
         (list 'not (parse-not-test)))
        (t (parse-comparison))))

(defun parse-and-test ()
  (let ((not-test (parse-not-test)))
    (while (current-token-p 'NAME '("and"))
      (pop token-stream)
      (setq not-test (list 'and not-test (parse-not-test))))
    not-test))

(defun parse-test ()
  (let ((test (parse-and-test)))
    (while (current-token-p 'NAME '("or"))
      (pop token-stream)
      (setq test (list 'or test (parse-and-test))))
    test))

(defun parse-exprlist ()
  (let ((exprlist (list (parse-expr))))
    (while (current-token-p 'OP '(","))
      (pop token-stream)
      (push (parse-expr) exprlist))
    (reverse exprlist)))

(defconst testlist-type-firstset '(STRING NUMBER NAME))
(defconst testlist-val-firstset '("(" "not" "-" "+"))
(defun parse-testlist ()
  (let ((testlist (list (parse-test))))
    (while (current-token-p 'OP '(","))
      (pop token-stream)
      (push (parse-test) testlist))
    (if (length1-p testlist)
        (car testlist)
      (reverse testlist))))

(defun parse-expr-stmt ()
  (let ((testlist-left (parse-testlist)))
    (cond ((current-token-p 'OP '("="))
           (pop token-stream)
           (list 'assign testlist-left (parse-testlist)))
          (t testlist-left))))

(defconst flow-firstset '("break" "continue"))
(defun parse-flow-stmt ()
  (let* ((current-token (car token-stream))
         (token-value (cadr current-token)))
    (cond
     ((current-token-p 'NAME flow-firstset)
      (intern token-value))
     ((current-token-p 'NAME '("return"))
      (parse-return-stmt))
     (t (throw 'parser-error "Unexpected token")))))

(defun parse-return-stmt ()
  (pop-current-token 'NAME '("return"))
  'return
  (if (current-token-in-firstset-p testlist-type-firstset
                                   testlist-val-firstset)
      (list 'return (parse-testlist))
    'return))

(defun parse-assert-stmt ()
  (pop-current-token 'NAME '("assert"))
  (list 'assert (parse-test)))

;;; Utils
;;; -----

(defun current-token-p (check-type &optional check-values)
  (dbind (type value start end line) (car token-stream)
    (and (eq type check-type)
         (if check-values
             (member value check-values)
           t))))

(defun pop-current-token (check-type &optional check-values)
  (if (current-token-p check-type check-values)
      (pop token-stream)
    (throw 'parser-error "Unexpected token")))

(defun current-token-in-firstset-p (type-set val-set)
  (let* ((token (car token-stream))
         (type (car token))
         (value (cadr token)))
    (when (or (member value val-set)
              (memq type type-set))
      t)))

(defun length1-p (l)
  (= (length l) 1))

(defalias 'dbind 'destructuring-bind)

) ;; end parser- namespace

(provide 'parser)
