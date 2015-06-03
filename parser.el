;; -*- lexical-binding: t -*-
;;
;; A simple RD parser for small Python subset.

(require 'tokenizer)

(eval-when-compile (require 'names))

(define-namespace parser-

(defvar token-stream
  nil)

;;; Parser
;;; ------

(defun parse-atom ()
  (dbind (type value start end line) (car token-stream)
    (cond
     ((token-is-p 'LPAR)
      (token-pop)
      (let ((testlist (parse-testlist)))
        (token-pop-or-fail 'RPAR)
        testlist))
     ((eq type 'NAME)
      (token-pop)
      (intern value))
     ((eq type 'NUMBER)
      (token-pop)
      (string-to-number value))
     ((eq type 'STRING)
      (token-pop)
      (read value))
     (t (throw 'parser-error (format "Unexpected token: %s instead of atom" type))))))

(defun parse-power ()
  (let ((atom (parse-atom)))
    (dbind (type value start end line) (car token-stream)
      (cond
       ((token-is-p 'DOUBLESTAR)
        (token-pop)
        (list (intern "**") atom (parse-power)))
       (t atom)))))

(defun parse-factor ()
  (dbind (type value start end line) (car token-stream)
    (cond
     ((token-one-of-p '(MINUS PLUS))
      (token-pop)
      (list (intern value) (parse-factor)))
     (t
      (parse-power)))))

(defun parse-term ()
  (let ((factor (parse-factor)))
    (dbind (type value start end line) (car token-stream)
      (cond
       ((token-one-of-p '(STAR SLASH PERCENT))
        (token-pop)
        (list (intern value) factor (parse-factor)))
       (t
        factor)))))

(defun parse-expr ()
  (let ((term (parse-term)))
    (dbind (type value start end line) (car token-stream)
      (cond
       ((token-one-of-p '(PLUS MINUS))
        (token-pop)
        (list (intern value) term (parse-term)))
       (t
        term)))))

(defconst comp-ops '(LESS GREATER EQEQUAL GREATEREQUAL LESSEQUAL NOTEQUAL))
(defun parse-comp-op ()
  (when (not (token-one-of-p comp-ops))
    (throw 'parser-error (format "Unexpected token: %s" (car token-stream))))
  (dbind (type value start end line) (token-pop)
    (intern value)))

(defun parse-comparison ()
  (let ((expr (parse-expr)))
    (while (token-one-of-p comp-ops)
      (setq expr (list (parse-comp-op) expr (parse-expr))))
    expr))

(defun parse-not-test ()
  (cond ((token-is-keyword-p '("not"))
         (token-pop)
         (list 'not (parse-not-test)))
        (t (parse-comparison))))

(defun parse-and-test ()
  (let ((not-test (parse-not-test)))
    (while (token-is-keyword-p '("and"))
      (token-pop)
      (setq not-test (list 'and not-test (parse-not-test))))
    not-test))

(defun parse-test ()
  (let ((test (parse-and-test)))
    (while (token-is-keyword-p '("or"))
      (token-pop)
      (setq test (list 'or test (parse-and-test))))
    test))

(defun parse-exprlist ()
  (let ((exprlist (list (parse-expr))))
    (while (token-is-p 'COMMA)
      (token-pop)
      (push (parse-expr) exprlist))
    (reverse exprlist)))

(defconst testlist-type-firstset '(LPAR MINUS PLUS STRING NUMBER NAME))
(defconst testlist-val-firstset '("not"))
(defun parse-testlist ()
  (let ((testlist (list (parse-test))))
    (while (token-is-p 'COMMA)
      (token-pop)
      (push (parse-test) testlist))
    (if (length1-p testlist)
        (car testlist)
      (reverse testlist))))

(defconst expr-type-firstset '(STRING NUMBER NAME MINUS PLUS RPAR))
(defconst expr-val-firstset '("not"))
(defun parse-expr-stmt ()
  (let ((testlist-left (parse-testlist)))
    (cond ((token-is-p 'EQUAL)
           (token-pop)
           (list 'assign testlist-left (parse-testlist)))
          (t testlist-left))))

(defconst flow-val-firstset '("break" "continue" "return"))
(defun parse-flow-stmt ()
  (let* ((current-token (first token-stream))
         (token-value (second current-token)))
    (cond
     ((token-is-keyword-p '("break" "continue"))
      (intern token-value))
     ((token-is-keyword-p '("return"))
      (parse-return-stmt))
     (t (throw 'parser-error "Unexpected token")))))

(defun parse-return-stmt ()
  (token-pop-or-fail 'KEYWORD '("return"))
  (if (or (token-one-of-p testlist-type-firstset)
          (token-is-keyword-p testlist-val-firstset))
      (list 'return (parse-testlist))
    'return))

(defun parse-assert-stmt ()
  (token-pop-or-fail 'KEYWORD '("assert"))
  (list 'assert (parse-test)))

(defconst small-type-firstset '(STRING NUMBER NAME MINUS LPAR PLUS))
(defconst small-val-firstset '("return" "assert" "not" "pass" "break" "continue"))
(defun parse-small-stmt ()
  (cond
   ((token-is-keyword-p flow-val-firstset)
    (parse-flow-stmt))
   ((token-is-keyword-p '("assert"))
    (parse-assert-stmt))
   ((token-is-keyword-p '("pass"))
    (token-pop)
    nil)
   ((or (token-one-of-p expr-type-firstset)
        (token-is-keyword-p expr-val-firstset))
    (parse-expr-stmt))
   (t (throw 'parser-error "Unexpected token"))))

(defconst simple-type-firstset '(STRING NUMBER NAME MINUS LPAR PLUS))
(defconst simple-val-firstset '("return" "assert" "not" "pass" "break" "continue"))
(defun parse-simple-stmt ()
  (let ((small-stmts (list (parse-small-stmt))))
    (when (token-is-p 'SEMI)
      (token-pop))
    (while (or (token-one-of-p small-type-firstset)
               (token-is-keyword-p small-val-firstset))
      (push (parse-small-stmt) small-stmts)
      (when (token-is-p 'SEMI)
        (token-pop)))
    (token-pop-or-fail 'NEWLINE)
    (if (length1-p small-stmts)
        (car small-stmts)
      `(progn ,@(reverse small-stmts)))))

(defun parse-suite ()
  (cond
   ((token-is-p 'NEWLINE)
    (token-pop)
    (token-pop-or-fail 'INDENT)
    (let ((stmtlist (list (parse-simple-stmt))))
      (while (or (token-one-of-p stmt-type-firstset)
                 (token-is-keyword-p stmt-val-firstset))
        (push (parse-stmt) stmtlist))
      (token-pop-or-fail 'DEDENT)
      (if (length1-p stmtlist)
          (car stmtlist)
        `(progn ,@(reverse stmtlist)))))
   (t (parse-simple-stmt))))

(defun parse-while ()
  (token-pop-or-fail 'KEYWORD '("while"))
  (let ((test (parse-test))
        suite)
    (token-pop-or-fail 'COLON)
    (list 'while test (parse-suite))))

;; TODO: simplify
(defun parse-if ()
  (token-pop-or-fail 'KEYWORD '("if"))
  (let ((iftest (parse-test))
        ifsuite
        eliftestsuites
        elsesuite)
    (token-pop-or-fail 'COLON)
    (setq ifsuite (parse-suite))
    (while (token-is-keyword-p '("elif"))
      (token-pop)
      (let ((test (parse-test))
            suite)
        (token-pop-or-fail 'COLON)
        (setq suite (parse-suite))
        (push (cons test suite) eliftestsuites )))
    (when (token-is-keyword-p '("else"))
      (token-pop)
      (token-pop-or-fail 'COLON)
      (setq elsesuite (parse-suite)))
    (list 'cond iftest ifsuite eliftestsuites elsesuite)))

(defun parse-varargslist ()
  (let ((args (list (intern (second (token-pop-or-fail 'NAME))))))
    (while (token-is-p 'COMMA)
      (token-pop)
      (push (intern (second (token-pop-or-fail 'NAME))) args))
    (reverse args)))

(defun parse-parameters ()
  (token-pop-or-fail 'LPAR)
  (let (varargslist)
    (when (token-is-p 'NAME)
      (setq varargslist (parse-varargslist)))
    (token-pop-or-fail 'RPAR)
    varargslist))

(defun parse-funcdef ()
  (token-pop-or-fail 'KEYWORD '("def"))
  (let ((name (second (token-pop-or-fail 'NAME)))
        (params (parse-parameters))
        body)
    (token-pop-or-fail 'COLON)
    (setq body (parse-suite))
    (list 'defun name params body)))

(defconst compound-stmt-val-firstset '("while" "def" "if"))
(defun parse-compound-stmt ()
  (cond
   ((token-is-keyword-p '("while"))
    (parse-while))
   ((token-is-keyword-p '("def"))
    (parse-funcdef))
   ((token-is-keyword-p '("if"))
    (parse-if))
   (t (throw 'parser-error "Unexpected token"))))

(defconst stmt-type-firstset '(STRING NUMBER MINUS LPAR PLUS))
(defconst stmt-val-firstset
  '("return" "assert" "if" "not" "pass" "def" "break" "continue" "while"))
(defun parse-stmt ()
  (cond
   ((token-is-keyword-p compound-stmt-val-firstset)
    (parse-compound-stmt))
   ((or (token-is-keyword-p simple-val-firstset )
        (token-one-of-p simple-type-firstset))
    (parse-simple-stmt))
   (t (throw 'parser-error "Unexpected token"))))

(defun parse-single-input ()
  (cond
   ((token-is-p 'NEWLINE)
    (token-pop)
    nil)
   ((token-is-keyword-p compound-stmt-val-firstset)
    (parse-compound-stmt))
   ((or (token-is-keyword-p simple-val-firstset)
        (token-one-of-p simple-type-firstset))
    (parse-simple-stmt))
   (t (throw 'parser-error "Unexpected token"))))

(defun parse-file-input ()
  (let (res)
    (while (not (token-is-p 'ENDMARKER))
      (cond
       ((token-one-of-p '(NEWLINE NL))
        (token-pop))
       (t
        (push (parse-stmt) res))))
    (token-pop-or-fail 'ENDMARKER)
    (reverse res)))

;;; Utils
;;; -----

(defun token-is-p (token-type &optional token-values)
  (let* ((token (first token-stream))
         (type (first token))
         (value (second token)))
    (and (eq type token-type)
         (if token-values
             (member value token-values)
           t))))

(defun token-is-keyword-p (token-values)
  (token-is-p 'KEYWORD token-values))

(defun token-one-of-p (token-types)
  (let* ((token (first token-stream))
         (type (first token)))
    (memq type token-types)))

(defun token-pop ()
  (pop token-stream))

(defun token-pop-or-fail (token-type &optional token-values)
  (if (token-is-p token-type token-values)
      (token-pop)
    (throw 'parser-error (format "Unexpected token: %s instead of %s"
                                 (caar token-stream) token-type))))

(defun length1-p (l)
  (= (length l) 1))

(defalias 'dbind 'destructuring-bind)

) ;; end parser- namespace

(provide 'parser)
