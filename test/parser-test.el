;; -*- lexical-binding: t -*-
;;
;;; Tests for parser.el.

(require 'parser)
(require 'tokenizer)

(ert-deftest parser-test-atom ()
  (loop for text in
        (list "name" "123" "\"a string\"" "1" "(1 + 2)" "(1 + (1 + 2))")
        for atom in
        (list 'name 123 "a string" 1 '(+ 1 2) '(+ 1 (+ 1 2)))
        do
        (with-tokenized text
          (should (equal (parser-parse-atom)
                         atom)))))

(ert-deftest parser-test-power ()
  (with-tokenized "123"
    (let ((token (first tokens)))
      (should (eq (first token)
                  'NUMBER))
      (should (equal "123"
                     (second token)))
      (should (equal 123
                     (parser-parse-atom)))))
  (with-tokenized "123**2"
    (should (equal (parser-parse-power)
                   '(** 123 2))))
  (with-tokenized "123**2**3"
    (should (equal (parser-parse-power)
                   '(** 123 (** 2 3))))))

(ert-deftest parser-test-factor ()
  (with-tokenized "123"
    (should (equal 123
                   (parser-parse-factor))))
  (with-tokenized "-123"
    (should (equal '(- 123)
                   (parser-parse-factor))))
  (with-tokenized "+123"
    (should (equal '(+ 123)
                   (parser-parse-factor)))))

(ert-deftest parser-test-term ()
  (with-tokenized "123"
    (should (equal 123
                   (parser-parse-term))))
  (with-tokenized "123 * 456"
    (should (equal '(* 123 456)
                   (parser-parse-term))))
  (with-tokenized "123 / 456"
    (should (equal '(/ 123 456)
                   (parser-parse-term))))
  (with-tokenized "123 % 456"
    (should (equal '(% 123 456)
                   (parser-parse-term))))
  (with-tokenized "123 % 456 / 5"
    (should (equal '(/ (% 123 456) 5)
                   (parser-parse-term)))))

(ert-deftest parser-test-expr ()
  (with-tokenized "123"
    (should (equal 123
                   (parser-parse-expr))))
  (with-tokenized "123 + 456"
    (should (equal '(+ 123 456)
                   (parser-parse-expr))))
  (with-tokenized "123 - 456"
    (should (equal '(- 123 456)
                   (parser-parse-expr)))))

(ert-deftest parser-test-comp-op ()
  (dolist (op-text '(">" "<" "==" "!=" "<>" ">=" "<="))
    (with-tokenized op-text
      (let ((token (first tokens)))
        (should (memq (first token) parser-comp-ops))))))

(ert-deftest parser-test-comparison ()
  (with-tokenized "1"
    (should (equal '1
                   (parser-parse-comparison))))
  (with-tokenized "1 < 2"
    (should (equal '(< 1 2)
                   (parser-parse-comparison))))
  (with-tokenized "1 < 2 < 3"
    (should (equal '(< (< 1 2) 3)
                   (parser-parse-comparison)))))

(ert-deftest parser-test-not-test ()
  (with-tokenized "2"
    (should (equal 2
                   (parser-parse-not-test))))
  (with-tokenized "not 2"
    (should (equal '(not 2)
                   (parser-parse-not-test)))))

(ert-deftest parser-test-and-test ()
  (with-tokenized "2"
    (should (equal 2
                   (parser-parse-and-test))))
  (with-tokenized "2 and 3"
    (should (equal '(and 2 3)
                   (parser-parse-and-test)))))

(ert-deftest parser-test-test ()
  (with-tokenized "2"
    (should (equal 2
                   (parser-parse-and-test))))
  (with-tokenized "2 or 3"
    (should (equal '(or 2 3)
                   (parser-parse-test))))
  (with-tokenized "2 and 3"
    (should (equal '(and 2 3)
                   (parser-parse-test)))))

(ert-deftest parser-test-exprlist ()
  (with-tokenized "1,2,3"
    (should (equal '(1 2 3)
                   (parser-parse-exprlist)))))

(ert-deftest parser-test-testlist ()
  (with-tokenized "v + 2"
    (should (equal '(+ v 2)
                   (parser-parse-testlist))))
  (with-tokenized "1,2,3"
    (should (equal '(1 2 3)
                   (parser-parse-testlist)))))

(ert-deftest parser-test-expr-stmt ()
  (with-tokenized "v + 2"
    (should (equal '(+ v 2)
                   (parser-parse-expr-stmt))))
  (with-tokenized "v + 2 = a + 3"
    (should (equal '(assign (+ v 2) (+ a 3))
                   (parser-parse-expr-stmt)))))

(ert-deftest parser-test-flow-stmt ()
  (with-tokenized "break"
    (should (equal 'break
                   (parser-parse-flow-stmt))))
  (with-tokenized "continue"
    (should (equal 'continue
                   (parser-parse-flow-stmt))))
  (with-tokenized "return"
    (should (equal '(return nil)
                   (parser-parse-flow-stmt)))))

(ert-deftest parser-test-return-stmt ()
  (with-tokenized "return"
    (should (equal '(return nil)
                   (parser-parse-return-stmt))))
  (with-tokenized "return 1"
    (should (equal '(return 1)
                   (parser-parse-return-stmt))))
  (with-tokenized "return 1 + 2"
    (should (equal '(return (+ 1 2))
                   (parser-parse-return-stmt)))))

(ert-deftest parser-test-assert-stmt ()
  (with-tokenized "assert 12"
    (should (equal '(assert 12)
                   (parser-parse-assert-stmt)))))

(ert-deftest parser-test-small-stmt ()
  (with-tokenized "assert 12"
    (should (equal '(assert 12)
                   (parser-parse-small-stmt))))
  (with-tokenized "pass"
    (should (equal nil
                   (parser-parse-small-stmt))))
  (with-tokenized "continue"
    (should (equal 'continue
                   (parser-parse-small-stmt))))
  (with-tokenized "a + b"
    (should (equal '(+ a b)
                   (parser-parse-small-stmt)))))

(ert-deftest parser-test-simple-stmt ()
  (with-tokenized "1;2;3"
    (should (equal '(progn 1 2 3)
                   (parser-parse-simple-stmt))))
  (with-tokenized "a + b"
    (should (equal '(+ a b)
                   (parser-parse-simple-stmt)))))

(ert-deftest parser-test-suite ()
  (with-tokenized "stub\n  1\n"
    (pop parser-token-stream)
    (should (equal '(progn 1)
                   (parser-parse-suite))))
  (with-tokenized "stub\n  1\n  2\n"
    (pop parser-token-stream)
    (should (equal '(progn 1 2)
                   (parser-parse-suite)))))

(ert-deftest parser-test-while ()
  (with-tokenized "while True: 1"
    (should (equal '(while True
                      (progn 1))
                   (parser-parse-while))))
  (with-tokenized "while True: \n   a = 1 + 2\n"
    (should (equal '(while True
                      (progn (assign a (+ 1 2))))
                   (parser-parse-while))))
  (with-tokenized "while True: \n a = 1 + 2\n b = 3"
    (should (equal '(while True
                      (progn
                        (assign a (+ 1 2))
                        (assign b 3)))
                   (parser-parse-while)))))

(ert-deftest parser-test-if ()
  (with-tokenized "if True: 1"
    (should (equal '(if True (progn 1) nil)
                   (parser-parse-if))))
  (with-tokenized "if True: 1\nelif False: \n 2\nelse: 3"
    (should (equal '(if True (progn 1)
                      (if False (progn 2)
                        (progn 3)))
                   (parser-parse-if))))
  (with-tokenized "if True: 1\nelif False: \n 2\nelif None:\n 3\nelse: 4"
    (should (equal '(if True (progn 1)
                      (if False (progn 2)
                        (if None (progn 3)
                          (progn 4))))
                   (parser-parse-if)))))

(ert-deftest parser-test-varargslist ()
  (with-tokenized "a"
    (should (equal '(a)
                   (parser-parse-varargslist))))
  (with-tokenized "a, b, c"
    (should (equal '(a b c)
                   (parser-parse-varargslist)))))

(ert-deftest parser-test-parameters ()
  (with-tokenized "(a)"
    (should (equal '(a)
                   (parser-parse-parameters))))
  (with-tokenized "(a, b, c)"
    (should (equal '(a b c)
                   (parser-parse-parameters))))
  (with-tokenized "()"
    (should (equal nil
                   (parser-parse-parameters)))))

(ert-deftest parser-test-funcdef ()
  (with-tokenized "def vova(a, b, c): return"
    (should (equal '(defun "vova" (a b c) (progn (return nil)))
                   (parser-parse-funcdef))))
  (with-tokenized "def vova(a, b, c):\n return"
    (should (equal '(defun "vova" (a b c) (progn (return nil)))
                   (parser-parse-funcdef))))
  (with-tokenized "def vova(a, b\n , c):\n return"
    (should (equal '(defun "vova" (a b c) (progn (return nil)))
                   (parser-parse-funcdef)))))

(ert-deftest parser-test-compound-stmt ()
  (with-tokenized "while True: return"
    (should (equal '(while True
                      (progn (return nil)))
                   (parser-parse-compound-stmt))))
  (with-tokenized "def vova(): pass"
    (should (equal '(defun "vova" nil (progn nil))
                   (parser-parse-compound-stmt))))
  (with-tokenized "if True: pass"
    (should (equal '(if True (progn nil) nil)
                   (parser-parse-compound-stmt)))))

(ert-deftest parser-test-stmt ()
  (with-tokenized "while True: return"
    (should (equal '(while True (progn (return nil)))
                   (parser-parse-stmt))))
  (with-tokenized "1;2;3"
    (should (equal '(progn 1 2 3)
                   (parser-parse-stmt)))))

(ert-deftest parser-test-single-input ()
  (with-tokenized "while True: return\n\n"
    (should (equal '(while True (progn (return nil)))
                   (parser-parse-single-input))))
  (with-tokenized "1;2;3"
    (should (equal '(progn 1 2 3)
                   (parser-parse-single-input)))))

(ert-deftest parser-test-file-input ()
  (with-tokenized "\n\ns1\n\n"
    (should (equal '(progn s1)
                   (parser-parse-file-input))))
  (with-tokenized "\n\ns1\ns2\n\n"
    (should (equal '(progn s1 s2)
                   (parser-parse-file-input)))))
