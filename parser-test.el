;; -*- lexical-binding: t -*-
;;
;;; Tests for parser.el.

(require 'parser)
(require 'tokenizer)

;;; Tests
;;; -----

(ert-deftest parser-test-atom ()
  (loop for text in
        (list "name" "123" "\"a string\"" "1" "(1 + 2)" "(1 + (1 + 2))")
        for atom in
        (list 'name 123 "a string" 1 '(+ 1 2) '(+ 1 (+ 1 2)))
        do
        (parser-test-with-tokenized text
          (should (equal (parser-parse-atom)
                         atom)))))

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
                   (parser-parse-term))))
  (parser-test-with-tokenized "123 % 456 / 5"
    (should (equal '(/ (% 123 456) 5)
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
  (dolist (op-text '(">" "<" "==" "!=" "<>" ">=" "<="))
    (parser-test-with-tokenized op-text
      (let ((token (first tokens)))
        (should (memq (first token) parser-comp-ops))))))

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
                   (parser-parse-test))))
  (parser-test-with-tokenized "2 and 3"
    (should (equal '(and 2 3)
                   (parser-parse-test)))))

(ert-deftest parser-test-exprlist ()
  (parser-test-with-tokenized "1,2,3"
    (should (equal '(1 2 3)
                   (parser-parse-exprlist)))))

(ert-deftest parser-test-testlist ()
  (parser-test-with-tokenized "v + 2"
    (should (equal '(+ v 2)
                   (parser-parse-testlist))))
  (parser-test-with-tokenized "1,2,3"
    (should (equal '(1 2 3)
                   (parser-parse-testlist)))))

(ert-deftest parser-test-expr-stmt ()
  (parser-test-with-tokenized "v + 2"
    (should (equal '(+ v 2)
                   (parser-parse-expr-stmt))))
  (parser-test-with-tokenized "v + 2 = a + 3"
    (should (equal '(assign (+ v 2) (+ a 3))
                   (parser-parse-expr-stmt)))))

(ert-deftest parser-test-flow-stmt ()
  (parser-test-with-tokenized "break"
    (should (equal 'break
                   (parser-parse-flow-stmt))))
  (parser-test-with-tokenized "continue"
    (should (equal 'continue
                   (parser-parse-flow-stmt))))
  (parser-test-with-tokenized "return"
    (should (equal 'return
                   (parser-parse-flow-stmt)))))

(ert-deftest parser-test-return-stmt ()
  (parser-test-with-tokenized "return"
    (should (equal 'return
                   (parser-parse-return-stmt))))
  (parser-test-with-tokenized "return 1"
    (should (equal '(return 1)
                   (parser-parse-return-stmt))))
  (parser-test-with-tokenized "return 1 + 2"
    (should (equal '(return (+ 1 2))
                   (parser-parse-return-stmt)))))

(ert-deftest parser-test-assert-stmt ()
  (parser-test-with-tokenized "assert 12"
    (should (equal '(assert 12)
                   (parser-parse-assert-stmt)))))

(ert-deftest parser-test-small-stmt ()
  (parser-test-with-tokenized "assert 12"
    (should (equal '(assert 12)
                   (parser-parse-small-stmt))))
  (parser-test-with-tokenized "pass"
    (should (equal nil
                   (parser-parse-small-stmt))))
  (parser-test-with-tokenized "continue"
    (should (equal 'continue
                   (parser-parse-small-stmt))))
  (parser-test-with-tokenized "a + b"
    (should (equal '(+ a b)
                   (parser-parse-small-stmt)))))

(ert-deftest parser-test-simple-stmt ()
  (parser-test-with-tokenized "1;2;3"
    (should (equal '(progn 1 2 3)
                   (parser-parse-simple-stmt))))
  (parser-test-with-tokenized "a + b"
    (should (equal '(+ a b)
                   (parser-parse-simple-stmt)))))

(ert-deftest parser-test-suite ()
  (parser-test-with-tokenized "1"
    (should (equal 1
                   (parser-parse-suite))))
  (parser-test-with-tokenized "stub\n  1\n  2\n"
    (pop parser-token-stream)
    (should (equal '(progn 1 2)
                   (parser-parse-suite)))))

(ert-deftest parser-test-while ()
  (parser-test-with-tokenized "while True: 1"
    (should (equal '(while True
                      1)
                   (parser-parse-while))))
  (parser-test-with-tokenized "while True: \n   a = 1 + 2\n"
    (should (equal '(while True
                      (assign a (+ 1 2)))
                   (parser-parse-while)))))

(ert-deftest parser-test-if ()
  (parser-test-with-tokenized "if True: 1"
    (should (equal '(cond True
                          1 nil nil)
                   (parser-parse-if))))
  (parser-test-with-tokenized "if True: 1\nelif False: \n 2\nelse: 3"
    (should (equal '(cond True
                          1 ((False . 2)) 3)
                   (parser-parse-if)))))

(ert-deftest parser-test-varargslist ()
  (parser-test-with-tokenized "a"
    (should (equal '(a)
                   (parser-parse-varargslist))))
  (parser-test-with-tokenized "a, b, c"
    (should (equal '(a b c)
                   (parser-parse-varargslist)))))

(ert-deftest parser-test-parameters ()
  (parser-test-with-tokenized "(a)"
    (should (equal '(a)
                   (parser-parse-parameters))))
  (parser-test-with-tokenized "(a, b, c)"
    (should (equal '(a b c)
                   (parser-parse-parameters))))
  (parser-test-with-tokenized "()"
    (should (equal nil
                   (parser-parse-parameters)))))

(ert-deftest parser-test-funcdef ()
  (parser-test-with-tokenized "def vova(a, b, c): return"
    (should (equal '(defun "vova" (a b c) return)
                   (parser-parse-funcdef))))
  (parser-test-with-tokenized "def vova(a, b, c):\n return"
    (should (equal '(defun "vova" (a b c) return)
                   (parser-parse-funcdef))))
  (parser-test-with-tokenized "def vova(a, b\n , c):\n return"
    (should (equal '(defun "vova" (a b c) return)
                   (parser-parse-funcdef)))))

(ert-deftest parser-test-compound-stmt ()
  (parser-test-with-tokenized "while True: return"
    (should (equal '(while True return)
                   (parser-parse-compound-stmt))))
  (parser-test-with-tokenized "def vova(): pass"
    (should (equal '(defun "vova" nil nil)
                   (parser-parse-compound-stmt))))
  (parser-test-with-tokenized "if True: pass"
    (should (equal '(cond True nil nil nil)
                   (parser-parse-compound-stmt)))))

(ert-deftest parser-test-stmt ()
  (parser-test-with-tokenized "while True: return"
    (should (equal '(while True return)
                   (parser-parse-stmt))))
  (parser-test-with-tokenized "1;2;3"
    (should (equal '(progn 1 2 3)
                   (parser-parse-stmt)))))

(ert-deftest parser-test-single-input ()
  (parser-test-with-tokenized "while True: return\n\n"
    (should (equal '(while True return)
                   (parser-parse-single-input))))
  (parser-test-with-tokenized "1;2;3"
    (should (equal '(progn 1 2 3)
                   (parser-parse-single-input)))))

(ert-deftest parser-test-file-input ()
  (parser-test-with-tokenized "\n\ns1\n\n"
    (should (equal '(s1)
                   (parser-parse-file-input))))
  (parser-test-with-tokenized "\n\ns1\ns2\n\n"
    (should (equal '(s1 s2)
                   (parser-parse-file-input)))))

;;; Utils
;;; -----

(defmacro parser-test-with-tokenized (code &rest body)
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,code)
     (let* ((tokens (reverse (tokenizer-tokenize-region))))
       (setq-local parser-token-stream tokens)
       ,@body)))
