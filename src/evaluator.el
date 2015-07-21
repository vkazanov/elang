;; -*- lexical-binding: t -*-
;;
;;; Language evaluation and loading

(eval-when-compile (require 'names))

(require 'tokenizer)
(require 'parser)
(require 'compiler)
(require 'python)

(define-namespace evaluator-

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
      (setq defunstr (buffer-substring-no-properties min max))
      (message defunstr)
      ;; TODO: use defun parser
      )))

(defun eval-region (min max)
  (interactive "r")
  (let ((regionstr (buffer-substring-no-properties min max)))
    ;; TODO: parse and walk
    (message regionstr)))

(defun eval-buffer ()
  (interactive)
  (eval-region (point-min) (point-max)))

) ;;; end of evaluator- namespace

(provide 'evaluator)
