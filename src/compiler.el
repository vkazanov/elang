;; -*- lexical-binding: t -*-
;;
;;; Parser IL to lap-code compiler

(eval-when-compile (require 'names))

(require 'parser)

(define-namespace compiler-

(defun compile-to-lapcode (parse-tree)
  (let (codes
        constants)
    (cl-labels
        ((emit-code
          (code &optional arg)
          (push `(,code . ,arg) codes))
         (add-constant
          (constant)
          (push constant constants))
         (compile
          (tree)
          (cond
           ((symbolp tree)
            (emit-code 'byte-varref (length constants))
            (add-constant tree))
           ((numberp tree)
            (emit-code 'byte-constant (length constants))
            (add-constant tree))
           ((listp tree)
            (emit-code 'byte-constant (length constants))
            (add-constant (first tree))
            (mapc #'compile (rest tree))
            (emit-code 'byte-call (1- (length tree))))
           (t (error "Cannot compile") ))))
      (compile parse-tree)
      (emit-code 'byte-return)
      (values (reverse codes) (vconcat (reverse constants))))))

) ;;; end of compiler- namespace

(provide 'compiler)
