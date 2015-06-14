;; -*- lexical-binding: t -*-
;;
;;; Tests for compiler.el

(require 'compiler)
(require 'parser)
(require 'tokenizer)

(ert-deftest compiler-test-expr ()
  (with-parsed "1"
    (dbind (codes constants) (compiler-compile-to-lapcode parse-tree)
      (should (equal '((byte-constant . 0)
                       (byte-return))
                     codes))
      (should (equal [1]
                     constants))))

  (with-parsed "1 + 2"
    (dbind (codes constants) (compiler-compile-to-lapcode parse-tree)
      (should (equal '((byte-constant . 0)
                       (byte-constant . 1)
                       (byte-constant . 2)
                       (byte-call . 2)
                       (byte-return))
                     codes))
      (should (equal [+ 1 2]
                     constants))))

  (with-parsed "1 + 2 + 3"
    (dbind (codes constants) (compiler-compile-to-lapcode parse-tree)
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
                     constants)))))
