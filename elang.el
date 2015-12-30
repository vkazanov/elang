;; -*- lexical-binding: t -*-
;;
;;; Language evaluation and loading

(require 'names)

(require 'elang-tokenizer)
(require 'elang-parser)
(require 'elang-compiler)
(require 'python)

;;;###autoload
(define-namespace elang-

:autoload
(defun eval-current-defun ()
  (interactive)
  (save-excursion
    (let (min max defunstr)
      (end-of-line 1)
      (while (and (or (python-nav-beginning-of-defun)
                      (beginning-of-line 1))
                  (> (current-indentation) 0)))
      (setq min (point))
      (or (python-nav-end-of-defun)
          (end-of-line 1))
      (setq max (point))
      (eval-region min max))))


:autoload
(defun eval-buffer ()
  (interactive)
  (eval-region (point-min) (point-max)))

:autoload
(defun eval-region (min max)
  (interactive "r")
  (let ((regionstr (buffer-substring-no-properties min max)))
    (with-parsed regionstr
                 (dolist (form (rest parse-tree))
                   (elang-eval-toplevel form)))))

(defmacro with-tokenized (str &rest body)
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,str)
     (let* ((tokens (reverse (elang-tokenize-region))))
       (setq-local elang-token-stream tokens)
       ,@body)))

(defmacro with-parsed (str &rest body)
  (declare (indent 1))
  `(let (parse-tree)
     (elang-with-tokenized ,str
       (setq parse-tree (elang-parse-file-input)))
     ,@body))

(defun eval-toplevel (form)
  (pcase form
    (`(defun ,name ,arglist ,body)
     (fset (elang-name-translate (intern name)) (elang-make-function body arglist)))
    (`(assign ,testlist-left ,testlist-right)
     (set (elang-name-translate testlist-left) testlist-right))
    (_
     (throw 'evaluator-error "Unknown toplevel form"))))

(defun make-function (parse-tree arglist)
  (destructuring-bind (lapcode constants depth) (elang-compile-to-lapcode parse-tree t)
    (make-byte-code
     arglist
     (byte-compile-lapcode lapcode)
     constants
     depth)))

)   ;;; end of elang- namespace

(provide 'elang)
