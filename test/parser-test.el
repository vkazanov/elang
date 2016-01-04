;; -*- lexical-binding: t -*-
;;
;;; Tests for elang-parser.el.

(ert-deftest parser-test-atom ()
  (loop for text in
        (list "name" "123" "\"a string\"" "1" "(1 + 2)" "(1 + (1 + 2))")
        for atom in
        (list 'name 123 "a string" 1 '(call + 1 2) '(call + 1 (call + 1 2)))
        do
        (elang-with-tokenized text
          (should (equal (elang-parse-atom)
                         atom)))))

(ert-deftest parser-test-string ()
  (loop for text in
        (list  "\"string\"" "'string'" "\"\"\"string\"\"\"" "'''string'''")
        do
        (elang-with-tokenized text
          (should (equal (elang-parse-atom)
                         "string")))))

(ert-deftest parser-test-power ()
  (elang-with-tokenized "123"
    (let ((token (first tokens)))
      (should (eq (first token)
                  'NUMBER))
      (should (equal "123"
                     (second token)))
      (should (equal 123
                     (elang-parse-atom)))))
  (elang-with-tokenized "123**2"
    (should (equal (elang-parse-power)
                   '(call ** 123 2))))
  (elang-with-tokenized "123**2**3"
    (should (equal (elang-parse-power)
                   '(call ** 123 (call ** 2 3)))))
  (elang-with-tokenized "fun()"
    (should (equal '(call fun)
                   (elang-parse-power))))
  (elang-with-tokenized "fun(fun2(1))"
    (should (equal '(call fun (call fun2 1))
                   (elang-parse-power))))
  (elang-with-tokenized "fun(1,2)"
    (should (equal '(call fun 1 2)
                   (elang-parse-power))))
  (elang-with-tokenized "fun(fun2(1, 2),2)"
    (should (equal '(call fun (call fun2 1 2) 2)
                   (elang-parse-power)))))

(ert-deftest parser-test-trailer ()
  (elang-with-tokenized "()"
    (should (equal (elang-parse-trailer)
                   '())))
  (elang-with-tokenized "(1)"
    (should (equal (elang-parse-trailer)
                   '(1))))
  (elang-with-tokenized "(1, 2, 3)"
    (should (equal (elang-parse-trailer)
                   '(1 2 3)))))

(ert-deftest parser-test-factor ()
  (elang-with-tokenized "123"
    (should (equal 123
                   (elang-parse-factor))))
  (elang-with-tokenized "-123"
    (should (equal '(call - 123)
                   (elang-parse-factor))))
  (elang-with-tokenized "+123"
    (should (equal '(call + 123)
                   (elang-parse-factor)))))

(ert-deftest parser-test-term ()
  (elang-with-tokenized "123"
    (should (equal 123
                   (elang-parse-term))))
  (elang-with-tokenized "123 * 456"
    (should (equal '(call * 123 456)
                   (elang-parse-term))))
  (elang-with-tokenized "123 / 456"
    (should (equal '(call / 123 456)
                   (elang-parse-term))))
  (elang-with-tokenized "123 % 456"
    (should (equal '(call % 123 456)
                   (elang-parse-term))))
  (elang-with-tokenized "123 % 456 / 5"
    (should (equal '(call / (call % 123 456) 5)
                   (elang-parse-term)))))

(ert-deftest parser-test-expr ()
  (elang-with-tokenized "123"
    (should (equal 123
                   (elang-parse-expr))))
  (elang-with-tokenized "123 + 456"
    (should (equal '(call + 123 456)
                   (elang-parse-expr))))
  (elang-with-tokenized "123 - 456"
    (should (equal '(call - 123 456)
                   (elang-parse-expr)))))

(ert-deftest parser-test-comp-op ()
  (dolist (op-text '(">" "<" "==" "!=" "<>" ">=" "<="))
    (elang-with-tokenized op-text
      (let ((token (first tokens)))
        (should (memq (first token) elang-comp-ops))))))

(ert-deftest parser-test-comparison ()
  (elang-with-tokenized "1"
    (should (equal '1
                   (elang-parse-comparison))))
  (elang-with-tokenized "1 < 2"
    (should (equal '(call < 1 2)
                   (elang-parse-comparison))))
  (elang-with-tokenized "1 < 2 < 3"
    (should (equal '(call < (call < 1 2) 3)
                   (elang-parse-comparison)))))

(ert-deftest parser-test-not-test ()
  (elang-with-tokenized "2"
    (should (equal 2
                   (elang-parse-not-test))))
  (elang-with-tokenized "not 2"
    (should (equal '(call not 2)
                   (elang-parse-not-test)))))

(ert-deftest parser-test-and-test ()
  (elang-with-tokenized "2"
    (should (equal 2
                   (elang-parse-and-test))))
  (elang-with-tokenized "2 and 3"
    (should (equal '(and 2 3)
                   (elang-parse-and-test)))))

(ert-deftest parser-test-test ()
  (elang-with-tokenized "2"
    (should (equal 2
                   (elang-parse-and-test))))
  (elang-with-tokenized "2 or 3"
    (should (equal '(or 2 3)
                   (elang-parse-test))))
  (elang-with-tokenized "2 and 3"
    (should (equal '(and 2 3)
                   (elang-parse-test)))))

(ert-deftest parser-test-exprlist ()
  (elang-with-tokenized "1,2,3"
    (should (equal '(1 2 3)
                   (elang-parse-exprlist)))))

(ert-deftest parser-test-testlist ()
  (elang-with-tokenized "v + 2"
    (should (equal '((call + v 2))
                   (elang-parse-testlist))))
  (elang-with-tokenized "1,2,3"
    (should (equal '(1 2 3)
                   (elang-parse-testlist)))))

(ert-deftest parser-test-expr-stmt ()
  (elang-with-tokenized "v + 2"
    (should (equal '(call + v 2)
                   (elang-parse-expr-stmt))))
  (elang-with-tokenized "v + 2 = a + 3"
    (should (equal '(assign (call + v 2) (call + a 3))
                   (elang-parse-expr-stmt)))))

(ert-deftest parser-test-flow-stmt ()
  (elang-with-tokenized "break"
    (should (equal '(break)
                   (elang-parse-flow-stmt))))
  (elang-with-tokenized "continue"
    (should (equal '(continue)
                   (elang-parse-flow-stmt))))
  (elang-with-tokenized "return"
    (should (equal '(return nil)
                   (elang-parse-flow-stmt)))))

(ert-deftest parser-test-return-stmt ()
  (elang-with-tokenized "return"
    (should (equal '(return nil)
                   (elang-parse-return-stmt))))
  (elang-with-tokenized "return 1"
    (should (equal '(return 1)
                   (elang-parse-return-stmt))))
  (elang-with-tokenized "return 1 + 2"
    (should (equal '(return (call + 1 2))
                   (elang-parse-return-stmt)))))

(ert-deftest parser-test-assert-stmt ()
  (elang-with-tokenized "assert 12"
    (should (equal '(assert 12)
                   (elang-parse-assert-stmt)))))

(ert-deftest parser-test-global-stmt ()
  (elang-with-tokenized "global x"
    (should (equal '(global (x))
                   (elang-parse-global-stmt))))
  (elang-with-tokenized "global x, y"
    (should (equal '(global (x y))
                   (elang-parse-global-stmt)))))

(ert-deftest parser-test-small-stmt ()
  (elang-with-tokenized "assert 12"
    (should (equal '(assert 12)
                   (elang-parse-small-stmt))))
  (elang-with-tokenized "global x"
    (should (equal '(global (x))
                   (elang-parse-small-stmt))))
  (elang-with-tokenized "pass"
    (should (equal nil
                   (elang-parse-small-stmt))))
  (elang-with-tokenized "continue"
    (should (equal '(continue)
                   (elang-parse-small-stmt))))
  (elang-with-tokenized "a + b"
    (should (equal '(call + a b)
                   (elang-parse-small-stmt)))))

(ert-deftest parser-test-simple-stmt ()
  (elang-with-tokenized "global x"
    (should (equal '(global (x))
                   (elang-parse-small-stmt))))
  (elang-with-tokenized "1;2;3"
    (should (equal '(progn 1 2 3)
                   (elang-parse-simple-stmt))))
  (elang-with-tokenized "a + b"
    (should (equal '(call + a b)
                   (elang-parse-simple-stmt)))))

(ert-deftest parser-test-suite ()
  (elang-with-tokenized "stub\n  1\n"
    (pop elang-token-stream)
    (should (equal '(progn 1)
                   (elang-parse-suite))))
  (elang-with-tokenized "stub\n  1\n  2\n"
    (pop elang-token-stream)
    (should (equal '(progn 1 2)
                   (elang-parse-suite)))))

(ert-deftest parser-test-while ()
  (elang-with-tokenized "while True: 1"
    (should (equal '(while True
                      (progn 1))
                   (elang-parse-while))))
  (elang-with-tokenized "while True: \n   a = 1 + 2\n"
    (should (equal '(while True
                      (progn (assign a (call + 1 2))))
                   (elang-parse-while))))
  (elang-with-tokenized "while True: \n a = 1 + 2\n b = 3"
    (should (equal '(while True
                      (progn
                        (assign a (call + 1 2))
                        (assign b 3)))
                   (elang-parse-while)))))

(ert-deftest parser-test-for ()
  (elang-with-tokenized "for a in things: 1"
    (should (equal '(for "a" (things) (progn 1))
                   (elang-parse-for)))))

(ert-deftest parser-test-while-break ()
  (elang-with-tokenized "while True: break"
    (should (equal '(while True
                      (progn
                        (break)))
                   (elang-parse-while))))
  (elang-with-tokenized "while True: \n break\n"
    (should (equal '(while True
                      (progn
                        (break)))
                   (elang-parse-while)))))

(ert-deftest parser-test-if ()
  (elang-with-tokenized "if True: 1"
    (should (equal '(if True (progn 1) nil)
                   (elang-parse-if))))
  (elang-with-tokenized "if True: 1\nelif False: \n 2\nelse: 3"
    (should (equal '(if True (progn 1)
                      (if False (progn 2)
                        (progn 3)))
                   (elang-parse-if))))
  (elang-with-tokenized "if True: 1\nelif False: \n 2\nelif None:\n 3\nelse: 4"
    (should (equal '(if True (progn 1)
                      (if False (progn 2)
                        (if None (progn 3)
                          (progn 4))))
                   (elang-parse-if)))))

(ert-deftest parser-test-varargslist ()
  (elang-with-tokenized "a"
    (should (equal '(a)
                   (elang-parse-varargslist))))
  (elang-with-tokenized "a, b, c"
    (should (equal '(a b c)
                   (elang-parse-varargslist)))))

(ert-deftest parser-test-parameters ()
  (elang-with-tokenized "(a)"
    (should (equal '(a)
                   (elang-parse-parameters))))
  (elang-with-tokenized "(a, b, c)"
    (should (equal '(a b c)
                   (elang-parse-parameters))))
  (elang-with-tokenized "()"
    (should (equal nil
                   (elang-parse-parameters)))))

(ert-deftest parser-test-funcdef ()
  (elang-with-tokenized "def vova(a, b, c): return"
    (should (equal '(defun "vova" (a b c) (progn (return nil)))
                   (elang-parse-funcdef))))
  (elang-with-tokenized "def vova(a, b, c):\n return"
    (should (equal '(defun "vova" (a b c) (progn (return nil)))
                   (elang-parse-funcdef))))
  (elang-with-tokenized "def vova(a, b\n , c):\n return"
    (should (equal '(defun "vova" (a b c) (progn (return nil)))
                   (elang-parse-funcdef)))))

(ert-deftest parser-test-compound-stmt ()
  (elang-with-tokenized "while True: return"
    (should (equal '(while True
                      (progn (return nil)))
                   (elang-parse-compound-stmt))))
  (elang-with-tokenized "def vova(): pass"
    (should (equal '(defun "vova" nil (progn nil))
                   (elang-parse-compound-stmt))))
  (elang-with-tokenized "if True: pass"
    (should (equal '(if True (progn nil) nil)
                   (elang-parse-compound-stmt))))
  (elang-with-tokenized "if True: pass"
    (should (equal '(if True (progn nil) nil)
                   (elang-parse-compound-stmt))))
  (elang-with-tokenized "for a in things: 1"
    (should (equal '(for "a" (things) (progn 1))
                   (elang-parse-compound-stmt)))))

(ert-deftest parser-test-stmt ()
  (elang-with-tokenized "while True: return"
    (should (equal '(while True (progn (return nil)))
                   (elang-parse-stmt))))
  (elang-with-tokenized "1;2;3"
    (should (equal '(progn 1 2 3)
                   (elang-parse-stmt)))))

(ert-deftest parser-test-single-input ()
  (elang-with-tokenized "while True: return\n\n"
    (should (equal '(while True (progn (return nil)))
                   (elang-parse-single-input))))
  (elang-with-tokenized "1;2;3"
    (should (equal '(progn 1 2 3)
                   (elang-parse-single-input)))))

(ert-deftest parser-test-file-input ()
  (elang-with-tokenized "\n\ns1\n\n"
    (should (equal '(progn s1)
                   (elang-parse-file-input))))
  (elang-with-tokenized "\n\ns1\ns2\n\n"
    (should (equal '(progn s1 s2)
                   (elang-parse-file-input)))))
