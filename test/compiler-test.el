;; -*- lexical-binding: t -*-
;;
;;; Tests for elang-compiler.el

(ert-deftest compiler-test-compile-funcall-synonym ()
  (with-compiled-single "1 == 2"
    (should (equal '((byte-constant . 0)
                     (byte-constant . 1)
                     (byte-constant . 2)
                     (byte-call . 2))
                   codes))
    (should (equal [elang-synonym-== 1 2]
                   constants))
    (should (equal 3 depth)))
  (with-compiled-single "1 != 2"
    (should (equal '((byte-constant . 0)
                     (byte-constant . 1)
                     (byte-constant . 2)
                     (byte-call . 2))
                   codes))
    (should (equal [elang-synonym-!= 1 2]
                   constants))
    (should (equal 3 depth))))

(ert-deftest compiler-test-const-expr-to-lap ()
  (with-compiled-single "1"
    (should (equal '((byte-constant . 0))
                   codes))
    (should (equal [1]
                   constants))
    (should (equal 1 depth)))

  (with-compiled-single "1 + 2"
    (should (equal '((byte-constant . 0)
                     (byte-constant . 1)
                     (byte-constant . 2)
                     (byte-call . 2))
                   codes))
    (should (equal [+ 1 2]
                   constants))
    (should (equal 3 depth)))
  (with-compiled-single "1 + 2 + 3"
    (should (equal '((byte-constant . 0)
                     (byte-constant . 1)
                     (byte-constant . 2)
                     (byte-constant . 3)
                     (byte-call . 2)
                     (byte-constant . 4)
                     (byte-call . 2))
                   codes))
    (should (equal [+ + 1 2 3]
                   constants))
    (should (equal 4 depth))))

(ert-deftest compiler-test-varref-expr-to-lap ()
  (with-compiled-single "a"
    (should (equal '((byte-varref . 0))
                   codes))
    (should (equal [a]
                   constants))
    (should (equal 1 depth)))

  (with-compiled-single "a + 2"
    (should (equal '((byte-constant . 0)
                     (byte-varref . 1)
                     (byte-constant . 2)
                     (byte-call . 2))
                   codes))
    (should (equal [+ a 2]
                   constants))
    (should (equal 3 depth))))

(ert-deftest compiler-test-if-to-lap ()
  (with-compiled-single "if a: 1"
    (should (equal '((byte-varref . 0)
                     (byte-goto-if-nil-else-pop . (TAG 1))
                     (byte-constant . 1)
                     (byte-discard)
                     (TAG 1)
                     (TAG 2))
                   codes))
    (should (equal [a 1]
                   constants))
    (should (equal 1 depth))))

(ert-deftest compiler-test-if-then-to-lap ()
  (with-compiled-single "if a: 1\nelse: 2"
    (should (equal '((byte-varref . 0)
                     (byte-goto-if-nil . (TAG 1))
                     (byte-constant . 1)
                     (byte-discard)
                     (byte-goto . (TAG 2))
                     (TAG 1)
                     (byte-constant . 2)
                     (byte-discard)
                     (TAG 2))
                   codes))
    (should (equal [a 1 2]
                   constants))))

(ert-deftest compiler-test-if-elif-to-lap ()
  (with-compiled-single "if a: 1\nelif b: 2"
    (should (equal '((byte-varref . 0)
                     (byte-goto-if-nil . (TAG 1))
                     (byte-constant . 1)
                     (byte-discard)
                     (byte-goto . (TAG 2))
                     (TAG 1)
                     (byte-varref . 2)
                     (byte-goto-if-nil-else-pop . (TAG 3))
                     (byte-constant . 3)
                     (byte-discard)
                     (TAG 3)
                     (TAG 4)
                     (TAG 2))
                   codes))
    (should (equal [a 1 b 2]
                   constants))
    (should (equal 1 depth))))

(ert-deftest compiler-test-assign-to-lap ()
  (with-compiled-single "x = 1"
    (should (equal '((byte-constant . 0)
                     (byte-varbind . 1))
                   codes))
    (should (equal [1 x]
                   constants))
    (should (equal 1 depth)))

  (with-compiled-single "x = 1;x = 2"
    (should (equal '((byte-constant . 0)
                     (byte-varbind . 1)
                     (byte-constant . 2)
                     (byte-varset . 1))
                   codes))
    (should (equal [1 x 2]
                   constants))
    (should (equal 1 depth)))

  (with-compiled-single "x = 1;y = 2"
    (should (equal '((byte-constant . 0)
                     (byte-varbind . 1)
                     (byte-constant . 2)
                     (byte-varbind . 3))
                   codes))
    (should (equal [1 x 2 y]
                   constants))
    (should (equal 1 depth)))
  (with-compiled-single "global z; z = 1;y = 2"
    (should (equal '((byte-constant . 1)
                     (byte-varset . 0)
                     (byte-constant . 2)
                     (byte-varbind . 3))
                   codes))
    (should (equal [z 1 2 y]
                   constants))
    (should (equal 1 depth)))
  ;; TODO: 2 assignments as x, y = 1, 2
  )


(ert-deftest compiler-test-while-to-lap ()
  (with-compiled-single "while a: a"
    (should (equal '((TAG 1)
                     (byte-varref . 0)
                     (byte-goto-if-nil-else-pop . (TAG 2))
                     (byte-varref . 1)
                     (byte-discard)
                     (byte-goto . (TAG 1))
                     (TAG 2))
                   codes))
    (should (equal [a a]
                   constants))
    (should (equal 1 depth))))

(ert-deftest compiler-test-while-break-to-lap ()
  (with-compiled-single "while a: break"
    (should (equal '((TAG 1)
                     (byte-varref . 0)
                     (byte-goto-if-nil-else-pop . (TAG 2))
                     (byte-goto . (TAG 2))
                     (byte-goto . (TAG 1))
                     (TAG 2))
                   codes))
    (should (equal [a]
                   constants))
    (should (equal 1 depth))))

(ert-deftest compiler-test-while-continue-to-lap ()
  (with-compiled-single "while a: continue"
    (should (equal '((TAG 1)
                     (byte-varref . 0)
                     (byte-goto-if-nil-else-pop . (TAG 2))
                     (byte-goto . (TAG 1))
                     (byte-goto . (TAG 1))
                     (TAG 2))
                   codes))
    (should (equal [a]
                   constants))
    (should (equal 1 depth))))

(ert-deftest compiler-test-or-to-lap ()
  (with-compiled-single "a or b"
    (should (equal '((byte-varref . 0)
                     (byte-goto-if-not-nil . (TAG 1))
                     (byte-varref . 1)
                     (byte-goto-if-not-nil . (TAG 1))
                     (byte-constant . 2) ;; fail
                     (byte-goto . (TAG 2))
                     (TAG 1) ;; success
                     (byte-constant . 3)
                     (TAG 2) ;; done
                     )
                   codes))
    (should (equal [a b nil t]
                   constants))
    (should (equal 2 depth))))

(ert-deftest compiler-test-and-to-lap ()
  (with-compiled-single "a and b"
    (should (equal '((byte-varref . 0)
                     (byte-goto-if-nil . (TAG 1))
                     (byte-varref . 1)
                     (byte-goto-if-nil . (TAG 1))
                     (byte-constant . 2) ;; sucess
                     (byte-goto . (TAG 2))
                     (TAG 1) ;; fail
                     (byte-constant . 3)
                     (TAG 2) ;; done
                     )
                   codes))
    (should (equal [a b t nil]
                   constants))
    (should (equal 2 depth))))

(ert-deftest compiler-test-return-to-lap ()
  ;; plain return should just return nil
  (with-compiled-single "return"
    (should (equal '((byte-constant . 0)
                     (byte-return))
                   codes))
    (should (equal [nil]
                   constants))
    (should (equal 1 depth)))
  ;; return a form evaluation result
  (with-compiled-single "return a"
    (should (equal '((byte-varref . 0)
                     (byte-return))
                   codes))
    (should (equal [a]
                   constants))
    (should (equal 1 depth)))
  ;; discard prev expr value, return nil
  (with-compiled-file "a\nreturn\n"
    (should (equal '((byte-varref . 0)
                     (byte-discard)
                     (byte-constant . 1)
                     (byte-return))
                   codes))
    (should (equal [a nil]
                   constants))
    (should (equal 1 depth)))
  ;; lack of return means the same as above
  (with-compiled-file "a\n"
    (should (equal '((byte-varref . 0)
                     (byte-discard)
                     (byte-constant . 1)
                     (byte-return))
                   codes))
    (should (equal [a nil]
                   constants))
    (should (equal 1 depth))))
