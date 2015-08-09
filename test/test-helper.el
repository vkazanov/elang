;; -*- lexical-binding: t -*-
;;
;;; Test utility macros and functions

(require 'elang-tokenizer)
(require 'elang-parser)
(require 'elang-compiler)
(require 'elang)

(defmacro with-compiled-single (str &rest body)
  (declare (indent 1))
  `(let (parse-tree)
     (elang-with-tokenized ,str
       (setq parse-tree (elang-parse-single-input)))
     (dbind (codes constants depth) (elang-compile-to-lapcode parse-tree)
       ,@body)))

(defmacro with-compiled-file (str &rest body)
  (declare (indent 1))
  `(let (parse-tree)
     (elang-with-tokenized ,str
       (setq parse-tree (elang-parse-file-input)))
     (dbind (codes constants depth) (elang-compile-to-lapcode parse-tree t)
       ,@body)))

(defun compile-to-function (bodystr arglist)
  (elang-with-tokenized bodystr
    (let ((parse-tree (elang-parse-file-input)))
      (dbind (lapcode constants depth) (elang-compile-to-lapcode parse-tree)
        (make-byte-code
         arglist
         (byte-compile-lapcode lapcode)
         constants
         depth)))))

(defun compile-and-run (bodystr &optional arglist args)
  (let ((fun (compile-to-function bodystr arglist)))
    (if (not arglist) (funcall fun)
      (funcall fun args))))
