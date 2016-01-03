;;; elang-compiler.el --- IR to lapcode compiler -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Vladimir Kazanov

;; Author: Vladimir Kazanov <vkazanov@inbox.ru>
;; Keywords: languages
;; URL: https://github.com/vkazanov/elang
;; Version: 0.0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:



(require 'names)

(define-namespace elang-

(defconst varname-synonyms '((True . t)
                             (False . nil)))

(defconst funcall-synonyms '((== . elang-synonym-==)
                             (!= . elang-synonym-!=)
                             (** . elang-synonym-**)))

(defun synonym-== (left right)
  (equal left right))

(defun synonym-!= (left right)
  (not (equal left right)))

(defun synonym-** (left right)
  (expt left right))

(defun name-translate (symbol)
  "Translate a Python-style symbol name into an elisp one"
  (let ((sname (symbol-name symbol)))
    (with-temp-buffer
      (insert sname)
      (goto-char (point-min))
      (while (search-forward "_" nil t)
        (replace-match "-" nil t))
      (intern (buffer-string)))))

(defun compile-to-lapcode (parse-tree &optional file-input)
  (let (codes                           ; codes emitted
        constants                       ; constants vector
        (tag-counter 0)                 ; current tag number
        binds                           ; bound var alist
        globals                         ; global vars alist
        loop-exit-tags                  ; current loop break target stack
        loop-continue-tags              ; current loop continue target stack
        (depth 0)                       ; current stack depth
        (maxdepth 0)                    ; max stack depth
        )
    (cl-labels
        ( ;; Save a lapcode
         (emit-code (code &optional arg)
                    (push `(,code . ,arg) codes)
                    (setq depth (+ depth (byte-compile-stack-adjustment code arg)))
                    (setq maxdepth (max depth maxdepth)))
         ;; Save a tag
         (emit-tag (tag)
                   (push tag codes))
         ;; Make a tag
         (make-tag ()
                   (list 'TAG (setq tag-counter (1+ tag-counter))))
         ;; Push a constant into the constants vector
         (add-constant (constant)
                       (push constant constants))
         (add-bind (bindname constidx)
                   (push (cons bindname constidx) binds))
         (unbind-all ()
                     (when binds
                       (emit-code 'byte-unbind (length binds))))
         ;; Compile an expression (main compilation entry point)
         (compile-expr (tree)
                       (cond
                        ((symbolp tree)
                         (compile-name tree))
                        ((numberp tree)
                         (compile-number tree))
                        ((stringp tree)
                         (compile-string tree))
                        ((listp tree)
                         (cl-case (first tree)
                           ('if (compile-if tree))
                           ('assign (compile-assign tree))
                           ('progn (compile-progn tree))
                           ('while (compile-while tree))
                           ('break (compile-break tree))
                           ('continue (compile-continue tree))
                           ('return (compile-return tree))
                           ('call (compile-funcall tree))
                           ('or (compile-or tree))
                           ('and (compile-and tree))
                           ('global (compile-global tree))))
                        (t (error "Cannot compile") )))
         ;; Compile variable names
         (compile-name (tree)
                       (emit-code 'byte-varref (length constants))
                       (let ((synonym (assq tree varname-synonyms)))
                         (add-constant (if synonym (cdr synonym)
                                         (name-translate tree)))))
         ;; Compile a number
         (compile-number (tree)
                         (emit-code 'byte-constant (length constants))
                         (add-constant tree))
         ;; Compile a string
         (compile-string (tree)
                         (emit-code 'byte-constant (length constants))
                         (add-constant tree))
         ;; Compile a usual function call
         (compile-funcall (tree)
                          (let* ((tree (rest tree))
                                 (sym (first tree))
                                 (args (rest tree))
                                 (synonym (assq sym funcall-synonyms)))
                            (emit-code 'byte-constant (length constants))
                            (add-constant (if synonym (cdr synonym)
                                            (name-translate sym)))
                            (mapc #'compile-expr args)
                            (emit-code 'byte-call (length args))))
         ;; Compile the or form
         (compile-or (tree)
                     (let ((successtag (make-tag))
                           (donetag (make-tag)))
                       (dolist (test (rest tree))
                         (compile-expr test)
                         (emit-code 'byte-goto-if-not-nil successtag))
                       (emit-code 'byte-constant (length constants))
                       (add-constant nil)
                       (emit-code 'byte-goto donetag)
                       (emit-tag successtag)
                       (emit-code 'byte-constant (length constants))
                       (add-constant t)
                       (emit-tag donetag)))
         ;; Compile the and form
         (compile-and (tree)
                      (let ((failtag (make-tag))
                            (donetag (make-tag)))
                        (dolist (test (rest tree))
                          (compile-expr test)
                          (emit-code 'byte-goto-if-nil failtag))
                        (emit-code 'byte-constant (length constants))
                        (add-constant t)
                        (emit-code 'byte-goto donetag)
                        (emit-tag failtag)
                        (emit-code 'byte-constant (length constants))
                        (add-constant nil)
                        (emit-tag donetag)))
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
                       (let ((after-then-tag (make-tag))
                             (after-else-tag (make-tag)))
                         (emit-code lapcode after-then-tag)
                         (compile-expr thenexpr)
                         (when elseexpr
                           (emit-code 'byte-goto after-else-tag))
                         (emit-tag after-then-tag)
                         (when elseexpr
                           (compile-expr elseexpr))
                         (emit-tag after-else-tag))))
         ;; Compile a list of exprs
         (compile-progn (tree)
                        (let ((forms (rest tree)))
                          (while forms
                            (let ((form (pop forms)))
                              (compile-expr form)
                              ;; TODO: dirty, move into compile expr as exprs
                              ;; know, whether it is needed to discard the value
                              (unless (or (memq (caar codes) '(byte-return
                                                               byte-varbind
                                                               byte-varset))
                                          (and (listp form)
                                               (memq (first form) '(global
                                                                    break
                                                                    continue))))
                                (emit-code 'byte-discard))))))
         ;; Compile an assignment
         (compile-assign (tree)
                         (let ((lvalue (second tree))
                               (rvalue (third tree)))
                           (compile-expr rvalue)
                           (let ((bind (assq lvalue binds))
                                 (global (assq lvalue globals))
                                 (constidx (length constants)))
                             (cond (global
                                    (emit-code 'byte-varset (cdr global)))
                                   (bind
                                    (emit-code 'byte-varset (cdr bind)))
                                   (t
                                    (add-constant lvalue)
                                    (emit-code 'byte-varbind constidx)
                                    (add-bind lvalue constidx))))))
         ;; Compile a while loop
         (compile-while (tree)
                        (let ((testexpr (second tree))
                              (bodyexpr (third tree)))
                          ;; correct
                          (let ((before-loop-tag (make-tag))
                                (after-loop-tag (make-tag)))
                            (push after-loop-tag loop-exit-tags)
                            (push before-loop-tag loop-continue-tags)
                            (emit-tag before-loop-tag)
                            (compile-expr testexpr)
                            (emit-code 'byte-goto-if-nil-else-pop after-loop-tag)
                            (compile-expr bodyexpr)
                            (emit-code 'byte-goto before-loop-tag)
                            (emit-tag after-loop-tag)
                            (pop loop-exit-tags)
                            (pop loop-continue-tags))))
         ;; Compile a break (which can only be within a while loop
         (compile-break (tree)
                        (unless loop-exit-tags
                          (throw 'compiler-error "A break stmt without an outer loop"))
                        (emit-code 'byte-goto (first loop-exit-tags)))
         ;; Compile a continue (which can only be within a while loop
         (compile-continue (tree)
                           (unless loop-exit-tags
                             (throw 'compiler-error "A continue stmt without an outer loop"))
                           (emit-code 'byte-goto (first loop-continue-tags)))
         ;; Compile a return statement
         (compile-return (tree)
                         (let ((retexpr (second tree)))
                           (if retexpr
                               (compile-expr retexpr)
                             (add-constant nil)
                             (emit-code 'byte-constant (1- (length constants))))
                           (unbind-all)
                           (emit-code 'byte-return)))
         ;; Compile a global declaration
         (compile-global (tree)
                         (dolist (global (second tree))
                           (let ((constidx (length constants)))
                             (add-constant global)
                             (push (cons global constidx) globals))))
         ;; Compile a *local* function definition
         (compile-defun (tree)
                        (throw 'compiler-error "Local functions are not implemented yet"))
         )
      (compile-expr parse-tree)
      ;; Check if the return is implicit (i.e., when the final bytecode is not a return).
      ;; This only works for file-input
      (when (and file-input
                 (not (eq (caar codes) 'byte-return)))
        (add-constant nil)
        (emit-code 'byte-constant (1- (length constants)))
        (unbind-all)
        (emit-code 'byte-return))
      (values (reverse codes) (vconcat (reverse constants)) maxdepth))))


) ;;; end of elang-compiler- namespace

(provide 'elang-compiler)

;;; elang-compiler.el ends here
