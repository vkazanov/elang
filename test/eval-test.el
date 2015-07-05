;; -*- lexical-binding: t -*-
;;
;;; Tests for compiled lapcode evaluation


(require 'compiler)
(require 'parser)
(require 'tokenizer)

;; (ert-deftest eval-test-expr ()
;;   (should (equal (compile-and-run "return")
;;                  nil))
;;   (should (equal (compile-and-run "return 1")
;;                  1))
;;   (should (equal (compile-and-run "return 2 + 3")
;;                  5))
;;   (should (equal (compile-and-run "return 2*(2 + 3)")
;;                  10)))

;; (ert-deftest eval-test-assign ()
;;   (should (equal (compile-and-run "x = 1\nreturn x")
;;                  1))
;;   (should (equal (compile-and-run "x = 1\nx = 2\nreturn x")
;;                  2))
;;   (should (equal (compile-and-run "x = 1\nx = 2 + x\nreturn x")
;;                  3))
;;   (should (equal (compile-and-run "x = 1\nx = x - 1\nreturn x")
;;                  0))
;;   (should (equal (compile-and-run "x = 1\ny = 2\nreturn x + y")
;;                  3)))

(ert-deftest eval-test-while ()
  ;; (should (equal (compile-and-run "a = 10\nwhile a > 1: a = 1\nreturn a")
  ;;                1))
  ;; (should (equal (compile-and-run "a = 10\nwhile a > 1:\n a = 1\n b = 2\nreturn b")
  ;;                2))
  (should (equal (compile-and-run "a = 10\nwhile a > 1: a = a - 1\nreturn a")
                 1))
  )
