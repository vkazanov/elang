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
        ((compile (tree)
                  (cond
                   ((or (symbolp tree)
                        (numberp tree))
                    (push `(byte-constant . ,(length constants)) codes)
                    (push tree constants))
                   ((listp tree)
                    (mapc #'compile tree)
                    (push `(byte-call . ,(1- (length tree))) codes))
                   (t (error "not implemented") ))))
      (compile parse-tree)
      (push '(byte-return) codes)
      (values (reverse codes) (vconcat (reverse constants))))))

) ;;; end of compiler- namespace

(provide 'compiler)
