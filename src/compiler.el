;; -*- lexical-binding: t -*-
;;
;;; IR to lapcode compiler

(eval-when-compile (require 'names))

(require 'parser)

(define-namespace compiler-

(defun compile-to-lapcode (parse-tree)
  (let (codes                           ; codes emitted
        constants                       ; constants vector
        (pc 0))                         ; program counter
    (cl-labels
        ((emit-code (code &optional arg pc-incr)
                    (push `(,code . ,arg) codes)
                    (setq pc (if pc-incr
                                 (+ pc pc-incr)
                               (1+ pc))))
         (add-constant (constant)
                       (push constant constants))
         (compile-funcall (tree)
                          (emit-code 'byte-constant (length constants))
                          (add-constant (first tree))
                          (mapc #'compile-expr (rest tree))
                          (emit-code 'byte-call (1- (length tree))))
         (compile-if (tree)
                     (let ((testexpr (second tree))
                           (thenexpr (third tree)))
                       (compile-expr testexpr)
                       ;; correct target pc can only be set after compiling
                       ;; thenexpr
                       (let ((gotoaddr (list 0 0)))
                         (emit-code 'byte-goto-if-nil-else-pop gotoaddr 3)
                         (compile-expr thenexpr)
                         (setf (second gotoaddr) pc))))
         (compile-expr (tree)
                       (cond
                        ((symbolp tree)
                         (emit-code 'byte-varref (length constants))
                         (add-constant tree))
                        ((numberp tree)
                         (emit-code 'byte-constant (length constants))
                         (add-constant tree))
                        ((listp tree)
                         (cond
                          ((eq (first tree) 'if)
                           (compile-if tree))
                          (t
                           (compile-funcall tree))))
                        (t (error "Cannot compile") ))))
      (compile-expr parse-tree)
      (emit-code 'byte-return)
      (values (reverse codes) (vconcat (reverse constants))))))

) ;;; end of compiler- namespace

(provide 'compiler)
