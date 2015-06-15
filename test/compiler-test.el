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

;; (ert-deftest compiler-test-if-to-lap ()
;;   (with-compiled "if a: 1"
;;     (should (equal '((byte-varref . 0)
;;                      (byte-goto-if-nil-else-pop . (0  5))
;;                      (byte-constant . 1)
;;                      (byte-return))
;;                    codes))
;;     (should (equal [a 1]
;;                    constants))))
