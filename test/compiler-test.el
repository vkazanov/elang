;; -*- lexical-binding: t -*-
;;
;;; Tests for compiler.el compiling to Emacs Lisp.

(require 'compiler)
(require 'parser)
(require 'tokenizer)

(ert-deftest compiler-test-expr ()
  (with-parsed "1 + 2"
    (message "PARSE TREE: %s" parse-tree)))
