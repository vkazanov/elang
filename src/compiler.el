;; -*- lexical-binding: t -*-
;;
;;; IR to lapcode compiler

(eval-when-compile (require 'names))

(require 'parser)

(define-namespace compiler-

(defun compile-to-lapcode (parse-tree &optional file-input)
  (let (codes                           ; codes emitted
        constants                       ; constants vector
        (pc 0)                          ; program counter
        binds)                          ; bound var alist
    (cl-labels
        ( ;; Save a lapcode
         (emit-code (code &optional arg pc-incr)
                    (push `(,code . ,arg) codes)
                    (setq pc (if pc-incr
                                 (+ pc pc-incr)
                               (1+ pc))))
         ;; Push a constant into the constants vector
         (add-constant (constant)
                       (push constant constants))
         (add-bind (bindname constidx)
                   (push (cons bindname constidx) binds))
         ;; Compile an expression (main compilation entry point)
         (compile-expr (tree)
                       (cond
                        ((symbolp tree)
                         (emit-code 'byte-varref (length constants))
                         (add-constant tree))
                        ((numberp tree)
                         (emit-code 'byte-constant (length constants))
                         (add-constant tree))
                        ((listp tree)
                         (cl-case (first tree)
                           ('if (compile-if tree))
                           ('assign (compile-assign tree))
                           ('progn (compile-progn tree))
                           ('while (compile-while tree))
                           ('return (compile-return tree))
                           (t (compile-funcall tree))))
                        (t (error "Cannot compile") )))
         ;; Compile a usual function call
         (compile-funcall (tree)
                          (emit-code 'byte-constant (length constants))
                          (add-constant (first tree))
                          (mapc #'compile-expr (rest tree))
                          (emit-code 'byte-call (1- (length tree))))
         ;; Compile the if/then form
         (compile-if (tree)
                     (let ((testexpr (second tree))
                           (thenexpr (third tree))
                           (elseexpr (fourth tree))
                           lapcode)
                       (setq lapcode (if (not elseexpr)
                                         'byte-goto-if-nil-else-pop
                                       'byte-goto-if-nil))
                       (compile-expr testexpr)
                       ;; correct target pc can only be set after compiling
                       ;; thenexpr and elseexpr
                       (let ((after-then-pc (list 0 0))
                             (after-else-pc (list 0 0)))
                         (emit-code lapcode after-then-pc 3)
                         (compile-expr thenexpr)
                         (when elseexpr
                           (emit-code 'byte-goto after-else-pc 3))
                         (setf (second after-then-pc) pc)
                         (when elseexpr
                           (compile-expr elseexpr)
                           (setf (second after-else-pc) pc)))))
         ;; Compile a list of exprs
         (compile-progn (tree)
                        (let ((forms (rest tree)))
                          (while forms
                            (compile-expr (pop forms))
                            (unless (eq (caar codes) 'byte-return)
                              (emit-code 'byte-discard)))))
         ;; Compile an assignment
         (compile-assign (tree)
                         (let ((lvalue (second tree))
                               (rvalue (third tree)))
                           (compile-expr rvalue)
                           (let ((bind (assq lvalue binds))
                                 (constidx (length constants)))
                             (cond ((not bind)
                                    (add-constant lvalue)
                                    (emit-code 'byte-varbind constidx)
                                    (add-bind lvalue constidx))
                                   (t
                                    (emit-code 'byte-varset (cdr bind)))))))
         ;; Compile a while loop
         (compile-while (tree)
                        (let ((testexpr (second tree))
                              (bodyexpr (third tree)))
                          ;; correct
                          (let ((before-while-pc (list 0 pc))
                                (after-loop-pc (list 0 0)))
                            (compile-expr testexpr)
                            (emit-code 'byte-goto-if-nil-else-pop after-loop-pc 3)
                            (compile-expr bodyexpr)
                            (emit-code 'byte-discard)
                            (emit-code 'byte-goto before-while-pc 3)
                            (setf (second after-loop-pc) pc))))
         ;; Compile a return statement
         (compile-return (tree)
                         (let ((retexpr (second tree)))
                           (if retexpr
                               (compile-expr retexpr)
                             (add-constant nil)
                             (emit-code 'byte-constant (1- (length constants))))
                           (emit-code 'byte-return))))
      (compile-expr parse-tree)
      ;; unbind everything
      (dolist (bind binds)
        (emit-code 'byte-unbind (cdr bind)))
      ;; Check if the return is implicit (i.e., when the final bytecode is not a return).
      ;; This only works for file-input
      (when (and file-input
                 (not (eq (caar codes) 'byte-return)))
        (add-constant nil)
        (emit-code 'byte-constant (1- (length constants)))
        (emit-code 'byte-return))
      (values (reverse codes) (vconcat (reverse constants))))))

) ;;; end of compiler- namespace

(provide 'compiler)
