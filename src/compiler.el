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
         (compile
          (tree)
          (cond
           ((symbolp tree)
            (emit-code 'byte-varref (length constants))
            (push tree constants))
           ((numberp tree)
            (emit-code 'byte-constant (length constants))
            (push tree constants))
           ((listp tree)
            (emit-code 'byte-constant (length constants))
            (push (first tree) constants)
            (mapc #'compile (rest tree))
            (emit-code 'byte-call (1- (length tree))))
           (t (error "Cannot compile") ))))
      (compile parse-tree)
      (emit-code 'byte-return)
      (values (reverse codes) (vconcat (reverse constants))))))

) ;;; end of compiler- namespace

(provide 'compiler)
