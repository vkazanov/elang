;;; elang-parser.el --- a simple RD parser for a Python subset.  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Vladimir Kazanov

;; Author: Vladimir Kazanov <vkazanov@inbox.ru>
;; Keywords: languages
;; URL: https://github.com/vkazanov/elang
;; Version: 0.0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:



(require 'names)

(define-namespace elang-

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
        (to-plain-list-if-single testlist)))
     ((eq type 'NAME)
      (token-pop)
      (intern value))
     ((eq type 'NUMBER)
      (token-pop)
      (string-to-number value))
     ((eq type 'STRING)
      (token-pop)
      (parse-string value))
     (t (throw 'parser-error (format "Unexpected token: %s instead of atom" type))))))

(defun parse-string (value)
  (cond
   ((string-prefix-p "\"\"\"" value)
    (substring-no-properties value 3 -3))
   ((string-prefix-p "\"" value)
    (substring-no-properties value 1 -1))
   ((string-prefix-p "'''" value)
    (substring-no-properties value 3 -3))
   ((string-prefix-p "'" value)
    (substring-no-properties value 1 -1))
   (t (throw 'parser-error (format "Unknown string type: %s" value)))))

(defun parse-power ()
  (let* ((atom (parse-atom))
         (res atom))
    (while (token-is-p 'LPAR)
      (let* ((trailer (parse-trailer))
             (trailer (if (listp trailer) trailer (list trailer))))
        (setq res `(call ,res ,@trailer))))
    (while (token-is-p 'DOUBLESTAR)
      (token-pop)
      (setq res (list 'call (intern "**") res (parse-factor))))
    res))

(defun parse-trailer ()
  (token-pop-or-fail 'LPAR)
  (let (res)
    (when (or (token-one-of-p testlist-type-firstset)
              (token-is-keyword-p testlist-val-firstset))
      (setq res (parse-testlist)))
    (token-pop-or-fail 'RPAR)
    res))

(defun parse-factor ()
  (let ((token-value (second (first token-stream))))
    (cond
     ((token-one-of-p '(MINUS PLUS))
      (token-pop)
      (list 'call (intern token-value) (parse-factor)))
     (t
      (parse-power)))))

(defun parse-term ()
  (let* ((factor (parse-factor))
         (res factor))
    (while (token-one-of-p '(STAR SLASH PERCENT))
      (let ((token-value (second (first token-stream))))
        (token-pop)
        (setq res (list 'call (intern token-value) res (parse-factor)))))
    res))

(defun parse-expr ()
  (let* ((term (parse-term))
         (res term))
    (while (token-one-of-p '(PLUS MINUS))
      (let ((token-value (second (first token-stream))))
        (token-pop)
        (setq res (list 'call (intern token-value) res (parse-term)))))
    res))

(defconst comp-ops '(LESS GREATER EQEQUAL GREATEREQUAL LESSEQUAL NOTEQUAL))
(defun parse-comp-op ()
  (when (not (token-one-of-p comp-ops))
    (throw 'parser-error (format "Unexpected token: %s" (car token-stream))))
  (dbind (type value start end line) (token-pop)
    (intern value)))

(defun parse-comparison ()
  (let ((expr (parse-expr)))
    (while (token-one-of-p comp-ops)
      (setq expr (list 'call (parse-comp-op) expr (parse-expr))))
    expr))

(defun parse-not-test ()
  (cond ((token-is-keyword-p '("not"))
         (token-pop)
         (list 'call 'not (parse-not-test)))
        (t (parse-comparison))))

(defun parse-and-test ()
  (let* ((not-test (parse-not-test))
         (res not-test))
    (while (token-is-keyword-p '("and"))
      (token-pop)
      (setq res (list 'and res (parse-not-test))))
    res))

(defun parse-test ()
  (let* ((test (parse-and-test))
         (res test))
    (while (token-is-keyword-p '("or"))
      (token-pop)
      (setq res (list 'or res (parse-and-test))))
    res))

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
    (reverse testlist)))

(defconst expr-type-firstset '(STRING NUMBER NAME MINUS PLUS RPAR))
(defconst expr-val-firstset '("not"))
(defun parse-expr-stmt ()
  (let ((testlist-left (parse-testlist)))
    (cond ((token-is-p 'EQUAL)
           (token-pop)
           (list 'assign
                 (to-plain-list-if-single testlist-left)
                 (to-plain-list-if-single (parse-testlist))))
          (t (to-plain-list-if-single testlist-left)))))

(defconst flow-val-firstset '("break" "continue" "return"))
(defun parse-flow-stmt ()
  (let* ((current-token (first token-stream))
         (token-value (second current-token)))
    (cond
     ((token-is-keyword-p '("continue"))
      (parse-continue-stmt))
     ((token-is-keyword-p '("break"))
      (parse-break-stmt))
     ((token-is-keyword-p '("return"))
      (parse-return-stmt))
     (t (throw  'parser-error (format "Unexpected token: %s" (car token-stream)))))))

(defun parse-break-stmt ()
  (token-pop-or-fail 'KEYWORD '("break"))
  (list 'break))

(defun parse-continue-stmt ()
  (token-pop-or-fail 'KEYWORD '("continue"))
  (list 'continue))

(defun parse-return-stmt ()
  (token-pop-or-fail 'KEYWORD '("return"))
  (if (or (token-one-of-p testlist-type-firstset)
          (token-is-keyword-p testlist-val-firstset))
      (list 'return (to-plain-list-if-single (parse-testlist)))
    (list 'return nil)))

(defun parse-assert-stmt ()
  (token-pop-or-fail 'KEYWORD '("assert"))
  (list 'assert (parse-test)))

(defun parse-global-stmt ()
  (token-pop-or-fail 'KEYWORD '("global"))
  (let (res)
    (push (parse-test) res)
    (while (token-is-p 'COMMA)
      (token-pop)
      (push (parse-test) res))
    (list 'global (reverse res))))

(defconst small-type-firstset '(STRING NUMBER NAME MINUS LPAR PLUS))
(defconst small-val-firstset '("return" "assert" "not" "pass" "break" "continue" "global"))
(defun parse-small-stmt ()
  (cond
   ((token-is-keyword-p flow-val-firstset)
    (parse-flow-stmt))
   ((token-is-keyword-p '("assert"))
    (parse-assert-stmt))
   ((token-is-keyword-p '("global"))
    (parse-global-stmt))
   ((token-is-keyword-p '("pass"))
    (token-pop)
    nil)
   ((or (token-one-of-p expr-type-firstset)
        (token-is-keyword-p expr-val-firstset))
    (parse-expr-stmt))
   (t (throw 'parser-error (format "Unexpected token: %s" (car token-stream))))))

(defconst simple-type-firstset '(STRING NUMBER NAME MINUS LPAR PLUS))
(defconst simple-val-firstset '("return" "assert" "not" "pass" "break" "continue" "global"))
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
    (let ((stmtlist (list (parse-stmt))))
      (while (or (token-one-of-p stmt-type-firstset)
                 (token-is-keyword-p stmt-val-firstset))
        (push (parse-stmt) stmtlist))
      (token-pop-or-fail 'DEDENT)
      `(progn ,@(reverse stmtlist))))
   (t `(progn ,(parse-simple-stmt)))))

(defun parse-while ()
  (token-pop-or-fail 'KEYWORD '("while"))
  (let ((test (parse-test))
        suite)
    (token-pop-or-fail 'COLON)
    (list 'while test (parse-suite))))

(defun parse-for ()
  (token-pop-or-fail 'KEYWORD '("for"))
  (let ((varname (second (token-pop-or-fail 'NAME)))
        testlist
        suite)
    (token-pop-or-fail 'KEYWORD '("in"))
    (setq testlist (parse-testlist))
    (token-pop-or-fail 'COLON)
    (setq suite (parse-suite))
    (list 'for varname testlist suite)))

(defun parse-if ()
  (token-pop-or-fail 'KEYWORD '("if"))
  (let ((iftest (parse-test))
        thensuite
        eliftestsuites
        elsesuite)
    (token-pop-or-fail 'COLON)
    (setq thensuite (parse-suite))
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
    (list 'if iftest thensuite
          (parse-if-build-else (reverse eliftestsuites) elsesuite))))

(defun parse-if-build-else (elifs else)
  (if elifs (let* ((iftest (caar elifs))
                   (thensuite (cdar elifs)))
              (list 'if iftest thensuite
                    (parse-if-build-else (cdr elifs) else)))
    else))

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

(defconst compound-stmt-val-firstset '("while" "def" "if" "for"))
(defun parse-compound-stmt ()
  (cond
   ((token-is-keyword-p '("while"))
    (parse-while))
   ((token-is-keyword-p '("def"))
    (parse-funcdef))
   ((token-is-keyword-p '("if"))
    (parse-if))
   ((token-is-keyword-p '("for"))
    (parse-for))
   (t (throw 'parser-error (format "Unexpected token: %s" (car (car token-stream)))))))

(defconst stmt-type-firstset '(NAME STRING NUMBER MINUS LPAR PLUS))
(defconst stmt-val-firstset
  '("return" "assert" "global" "if" "not" "pass" "def" "break" "continue" "while" "for"))
(defun parse-stmt ()
  (cond
   ((token-is-keyword-p compound-stmt-val-firstset)
    (parse-compound-stmt))
   ((or (token-is-keyword-p simple-val-firstset )
        (token-one-of-p simple-type-firstset))
    (parse-simple-stmt))
   (t (throw 'parser-error (format "Unexpected token: %s" (car token-stream))))))

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
   (t (throw 'parser-error (format "Unexpected token: %s" (car token-stream))))))

(defun parse-file-input ()
  (let (res)
    (while (not (token-is-p 'ENDMARKER))
      (cond
       ((token-one-of-p '(NEWLINE NL))
        (token-pop))
       (t
        (push (parse-stmt) res))))
    (token-pop-or-fail 'ENDMARKER)
    `(progn ,@(reverse res))))

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
  (let ((res (pop token-stream)))
    (while (token-is-p 'NL)
      (pop token-stream))
    res))

(defun token-pop-or-fail (token-type &optional token-values)
  (if (token-is-p token-type token-values)
      (token-pop)
    (throw 'parser-error (format "Unexpected token: %s instead of %s"
                                 (caar token-stream) token-type))))

(defun length1-p (l)
  (= (length l) 1))

(defun to-plain-list-if-single (l)
  (if (length1-p l) (first l)
    l))

(defalias 'dbind 'destructuring-bind)

) ;; end elang- namespace

(provide 'elang-parser)

;;; elang-parser.el ends here
