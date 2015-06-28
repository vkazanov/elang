;; -*- lexical-binding: t -*-
;;
;;; Tests for compiler.el

(require 'compiler)
(require 'parser)
(require 'tokenizer)

(ert-deftest compiler-test-const-expr-to-lap ()
  (with-compiled "1"
    (should (equal '((byte-constant . 0)
                     (byte-return))
                   codes))
    (should (equal [1]
                   constants)))

  (with-compiled "1 + 2"
    (should (equal '((byte-constant . 0)
                     (byte-constant . 1)
                     (byte-constant . 2)
                     (byte-call . 2)
                     (byte-return))
                   codes))
    (should (equal [+ 1 2]
                   constants)))
  (with-compiled "1 + 2 + 3"
    (should (equal '((byte-constant . 0)
                     (byte-constant . 1)
                     (byte-constant . 2)
                     (byte-constant . 3)
                     (byte-call . 2)
                     (byte-constant . 4)
                     (byte-call . 2)
                     (byte-return))
                   codes))
    (should (equal [+ + 1 2 3]
                   constants))))

(ert-deftest compiler-test-varref-expr-to-lap ()
  (with-compiled "a"
    (should (equal '((byte-varref . 0)
                     (byte-return))
                   codes))
    (should (equal [a]
                   constants)))

  (with-compiled "a + 2"
    (should (equal '((byte-constant . 0)
                     (byte-varref . 1)
                     (byte-constant . 2)
                     (byte-call . 2)
                     (byte-return))
                   codes))
    (should (equal [+ a 2]
                   constants))))

(ert-deftest compiler-test-if-to-lap ()
  (with-compiled "if a: 1"
    (should (equal '((byte-varref . 0)
                     (byte-goto-if-nil-else-pop . (0 5))
                     (byte-constant . 1)
                     (byte-return))
                   codes))
    (should (equal [a 1]
                   constants))))

(ert-deftest compiler-test-if-then-to-lap ()
  (with-compiled "if a: 1\nelse: 2"
    (should (equal '((byte-varref . 0)
                     (byte-goto-if-nil . (0 8))
                     (byte-constant . 1)
                     (byte-goto . (0 9))
                     (byte-constant . 2)
                     (byte-return))
                   codes))
    (should (equal [a 1 2]
                   constants))))

(ert-deftest compiler-test-if-elif-to-lap ()
  (with-compiled "if a: 1\nelif b: 2"
    (should (equal '((byte-varref . 0)
                     (byte-goto-if-nil 0 8)
                     (byte-constant . 1)
                     (byte-goto 0 13)
                     (byte-varref . 2)
                     (byte-goto-if-nil-else-pop 0 13)
                     (byte-constant . 3)
                     (byte-return))
                   codes))
    (should (equal [a 1 b 2]
                   constants))))

(ert-deftest compiler-test-assign-to-lap ()
  (with-compiled "x = 1"
    (should (equal '((byte-constant . 0)
                     (byte-varbind . 1)
                     (byte-unbind . 1)
                     (byte-return))
                   codes))
    (should (equal [1 x]
                   constants)))

  (with-compiled "x = 1;x = 2"
    (should (equal '((byte-constant . 0)
                     (byte-varbind . 1)
                     (byte-constant . 2)
                     (byte-varset . 1)
                     (byte-unbind . 1)
                     (byte-return))
                   codes))
    (should (equal [1 x 2]
                   constants)))

  (with-compiled "x = 1;y = 2"
    (should (equal '((byte-constant . 0)
                     (byte-varbind . 1)
                     (byte-constant . 2)
                     (byte-varbind . 3)
                     (byte-unbind . 3)
                     (byte-unbind . 1)
                     (byte-return))
                   codes))
    (should (equal [1 x 2 y]
                   constants)))
  ;; TODO: 2 assignments as x, y = 1, 2
  )


(ert-deftest compiler-test-while-to-lap ()
  (with-compiled "while a: a"
    (should (equal '((byte-varref . 0)
                     (byte-goto-if-nil-else-pop 0 9)
                     (byte-varref . 1)
                     (byte-discard)
                     (byte-goto 0 0)
                     (byte-return))
                   codes))
    (should (equal [a a]
                   constants))))

;; (ert-deftest compiler-test-return-to-lap ()
;;   ;; plain return should just return nil
;;   (with-compiled "return"
;;     (should (equal '((byte-constant . 0)
;;                      (byte-return))
;;                    codes))
;;     (should (equal [nil]
;;                    constants)))
;;   ;; return a form evaluation result
;;   (with-compiled "return a"
;;     (should (equal '((byte-varref . 0)
;;                      (byte-return))
;;                    codes))
;;     (should (equal [a]
;;                    constants)))
;;   ;; discard prev expr value, return nil
;;   (with-compiled "a\nreturn"
;;     (should (equal '((byte-varref . 0)
;;                      (byte-discard)
;;                      (byte-constant . 1)
;;                      (byte-return))
;;                    codes))
;;     (should (equal [a nil]
;;                    constants))))
