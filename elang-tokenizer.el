;;; elang-tokenizer.el --- a straightforward port of CPython's Lib/tokenize.py. -*- lexical-binding: t -*-

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
(require 'cl)

(define-namespace elang-

;;; Tokenizer entry point
;;; ---------------------

(defun tokenize-region (&optional beg end)
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list (point-min) (point-max))))
  (unless (and beg end)
    (setq beg (point-min)
          end (point-max)))
  (let ((text (buffer-substring-no-properties beg end))
        tokens)
    (cl-flet ((yield (elem) (push elem tokens)))
      (with-temp-buffer
        (insert text)
        (goto-char (point-min))
        (generate-tokens #'yield)))))

;;; Token regexes and regex-related helper functions
;;; ------------------------------------------------

(defun flatten (list-to-flatten)
  (cond
   ((null list-to-flatten) nil)
   ((atom list-to-flatten) (list list-to-flatten))
   (t
    (append (flatten (car list-to-flatten)) (flatten (cdr list-to-flatten))))))

;; Regex helper functions
;; TODO: should probably use RX? It should have something similar
(defun group (&rest choices)
  (concat "\\(" (mapconcat 'identity (flatten choices) "\\|") "\\)"))
(defun any (&rest choices)
  (concat (group (flatten choices)) "*"))
(defun maybe (&rest choices)
  (concat (group (flatten choices)) "?"))

;; TODO: spends extra time checking the rest of the string, should only check
;; from the pos
;;
;; SOLUTION:
;;(let ((re (format ".\\{%d\\}vova" 4)))
;; (message re)
;; (string-match re "  vovova"))
(defun looking-at-string (regex string start)
  "Match the string using the regex. Look for the match in the
string beginning only. Return the match or nil."
  (let* ((matched (string-match regex string start)))
    (when (and matched (= matched start))
      (match-string 0 string))))

;; Group characters
(defconst ascii-lowercase (append "abcdefghijklmnopqrstuvwxyz" nil))
(defconst ascii-uppercase (append "ABCDEFGHIJKLMNOPQRSTUVWXYZ" nil))
(defconst ascii-letters (append ascii-lowercase ascii-uppercase))
(defconst namechars (append ascii-letters '(?_)))
(defconst numchars (append "0123456789" nil))
(defconst triple-quoted '("'''" "\"\"\""
                          "r'''" "r\"\"\"" "R'''" "R\"\"\""
                          "u'''" "u\"\"\"" "U'''""U\"\"\""
                          "ur'''" "ur\"\"\"" "Ur'''" "Ur\"\"\""
                          "uR'''" "uR\"\"\"" "UR'''" "UR\"\"\""
                          "b'''" "b\"\"\"" "B'''" "B\"\"\""
                          "br'''" "br\"\"\"" "Br'''" "Br\"\"\""
                          "bR'''" "bR\"\"\"" "BR'''" "BR\"\"\""))
(defconst single-quoted '(?'  ?\"
                              "r'" "r\""  "R'"  "R\""
                              "u'" "u\"" "U'" "U\""
                              "ur'" "ur\"" "Ur'" "Ur\""
                              "uR'" "uR\"" "UR'" "UR\""
                              "b'" "b\"" "B'" "B\""
                              "br'" "br\"" "Br'" "Br\""
                              "bR'" "bR\"" "BR'" "BR\"" ))

;; Quotes
(defconst regex-single "[^'\\]*\\(?:\\.[^'\\]*\\)*'")
(defconst regex-double "[^\"\\]*\\(?:\\.[^\"\\]*\\)*\"")
(defconst regex-single3 "[^'\\]*\\(?:\\(?:\\\\.\\|'\\(?:[^']\\|'[^']\\)\\)[^'\\]*\\)*'''")
(defconst regex-double3 "[^\"\\]*\\(?:\\(?:\\.\\|\"\\(?:[^\"]\\|\"[^\"]\\)\\)[^\"\\]*\\)*\"\"\"")
(defconst regex-triple (group "[uUbB]?[rR]?'''" "[uUbB]?[rR]?\"\"\""))
(defconst regex-string (group "[uUbB]?[rR]?'[^\n'\\]*\\(?:\\.[^\n'\\]*\\)*'"
                              "[uUbB]?[rR]?\"[^\n\"\\\\]*\\(?:\\\\.[^\n\"\\\\]*\\)*\""))

(defconst regex-endprogs (list (cons ?' regex-single) (cons ?\" regex-double)
                               (cons "'''" regex-single3) (cons "\"\"\"" regex-double3)
                               (cons "r'''" regex-single3) (cons "r\"\"\"" regex-double3)
                               (cons "u'''" regex-single3) (cons "u\"\"\"" regex-double3)
                               (cons "ur'''" regex-single3) (cons "ur\"\"\"" regex-double3)
                               (cons "R'''" regex-single3) (cons "R\"\"\"" regex-double3)
                               (cons "U'''" regex-single3) (cons "U\"\"\"" regex-double3)
                               (cons "uR'''" regex-single3) (cons "uR\"\"\"" regex-double3)
                               (cons "Ur'''" regex-single3) (cons "Ur\"\"\"" regex-double3)
                               (cons "UR'''" regex-single3) (cons "UR\"\"\"" regex-double3)
                               (cons "b'''" regex-single3) (cons "b\"\"\"" regex-double3)
                               (cons "br'''" regex-single3) (cons "br\"\"\"" regex-double3)
                               (cons "B'''" regex-single3) (cons "B\"\"\"" regex-double3)
                               (cons "bR'''" regex-single3) (cons "bR\"\"\"" regex-double3)
                               (cons "Br'''" regex-single3) (cons "Br\"\"\"" regex-double3)
                               (cons "BR'''" regex-single3) (cons "BR\"\"\"" regex-double3)
                               (cons "r" nil) (cons "R" nil) (cons "u" nil) (cons "U" nil)
                               (cons "b" nil) (cons "B" nil)))
(defun regex-get-endprog (endprogs token)
  (cdr (assoc token endprogs)))

;; Operators, brackets, names, whitespaces
(defconst regex-whitespace "[ \f\t]*")
(defconst regex-name "[a-zA-Z_][a-zA-Z0-9_]*")
(defconst regex-special (group "\r?\n" "[:;.,`@]"))
(defconst regex-bracket "[][(){}]")
(defconst regex-operator (group "\\*\\*" "<>" "!=" "[<>=]=?"
                                "//"
                                "[-+\\*/%=<>]"))
(defconst regex-funny (group regex-operator regex-bracket regex-special))
;; First (or only) line of ' or " string.
(defconst regex-contstr (group (concat "[uUbB]?[rR]?'[^\n'\\\\]*\\(?:\\\\.[^\n'\\\\]*\\)*"
                                       (group "'" "\\\\\r?\n"))
                               (concat "[uUbB]?[rR]?\"[^\n\"\\\\]*\\(?:\\\\.[^\n\"\\\\]*\\)*"
                                       (group "\"" "\\\\\r?\n"))))
(defconst regex-triple (group "[uUbB]?[rR]?'''" "[uUbB]?[rR]?\"\"\""))
(defconst regex-comment "#[^\r\n]*")

;; Numbers
(defconst regex-decnumber "[1-9][0-9]*[lL]?")
(defconst regex-binnumber "0[bB][01]+[lL]?")
(defconst regex-octnumber "\\(0[oO][0-7]+\\)\\|\\(0[0-7]*\\)[lL]?")
(defconst regex-hexnumber "0[xX][0-9a-fA-F]+[lL]?")
(defconst regex-exponent "[eE][-+]?[0-9]+")
(defconst regex-expfloat (concat "[0-9]+" regex-exponent))
(defconst regex-pointfloat (concat (group "[0-9]+\\.[0-9]*" "\\.[0-9]+") (maybe regex-exponent)))
(defconst regex-floatnumber (group regex-pointfloat regex-expfloat))
(defconst regex-imagnumber (group "[0-9]+[jJ]" (concat regex-floatnumber "[jJ]")))
(defconst regex-intnumber (group regex-hexnumber regex-binnumber regex-octnumber regex-decnumber))
(defconst regex-number (group regex-imagnumber regex-floatnumber regex-intnumber))

;; Higher-level regexes
(defconst regex-pseudoextras (group "\\\\\r?\n\\|\\'" regex-comment regex-triple))
(defconst regex-pseudotoken (concat regex-whitespace (group regex-pseudoextras regex-number
                                                            regex-funny regex-contstr regex-name)))


;;; The tokenizer itself
;;; --------------------

(defconst tabsize 8)

(defconst keyword-names
  '("return" "assert" "global" "if" "elif" "else" "not" "pass" "def" "break" "continue" "while" "for" "in" "or" "and"))

;; Main starting point
(defun generate-tokens (yield)
  "Calls YIELD with every token found in the current buffer.

YIELD is a function meant for building a list of tokens. Every
token is list of the following form:

\(TYPE VALUE START END CONTEXT-LINE)
"
  (let ((lnum 0)
        (parenlev 0)
        continued
        contstr
        needcont
        contline
        (indents (list 0))
        line
        pos
        max
        endprog
        strstart
        (do-loop-outer t)
        (do-loop-inner t))
    (catch 'break
      ;; loop over lines in stream
      (while do-loop-outer
        (catch 'continue
          (setq line (readline)
                lnum (1+ lnum)
                pos 0
                max (length line))
          (cond

           ;; continued string
           (contstr
            (unless line
              (throw 'token-error (cons "EOF in multi-line string" strstart)))
            (let ((matched (string-match endprog line pos))
                  end)
              (cond
               (matched
                (setq pos (match-end 0)
                      end pos)
                (funcall yield (list 'STRING
                                     (concat contstr (substring line 0 end))
                                     strstart (cons lnum end) (concat contline line)))
                (setq contstr nil
                      needcont nil
                      contline nil))
               ((and needcont
                     (not (equal (substring line (- (length line) 2))
                                 "\\\n") )
                     (not (equal (substring line (- (length line) 3))
                                 "\\\r\n") ))
                (funcall yield (list 'ERRORTOKEN
                                     (concat contstr line)
                                     strstart (cons lnum (length line))
                                     contline))
                (setq contstr nil
                      contline nil)
                (throw 'continue nil))
               (t
                (setq contstr (concat contstr line)
                      contline (concat contline line))
                (throw 'continue nil)))))

           ;; new statement
           ((and (= parenlev 0) (not continued))
            (unless line
              (setq do-loop-outer nil)
              (throw 'break nil))

            (let ((column 0))
              ;; measure leading whitespace
              (setq do-loop-inner t)
              (catch 'break
                (while (and do-loop-inner
                            (< pos max))
                  (cond
                   ((equal (elt line pos) ? )
                    (setq column (1+ column)))
                   ((equal (elt line pos) ?\t)
                    (setq column (* (1+ (/ column tabsize)) tabsize)))
                   ((equal (elt line pos) ?\f)
                    (setq column 0))
                   (t
                    (setq do-loop-inner nil)
                    (throw 'break nil)))
                  (setq pos (1+ pos))))
              (when (= pos max)
                (setq do-loop-outer nil)
                (throw 'break nil))

              ;; skip comments or blank lines
              (when (member (elt line pos) '(?# ?\r ?\n))
                (cond
                 ((equal (elt line pos) ?#)
                  (let* ((comment-token (replace-regexp-in-string "\r?\n$" "" (substring line pos)))
                         (nl-pos (+ pos (length comment-token))))
                    (funcall yield
                             (list 'COMMENT comment-token
                                   (cons lnum pos) (cons lnum (length comment-token)) line))
                    (funcall yield
                             (list 'NL (substring line nl-pos)
                                   (cons lnum nl-pos) (cons lnum (length line)) line))))
                 (t (funcall yield
                             (if (equal (elt line pos) ?#)
                                 (list 'COMMENT (substring line pos)
                                       (cons lnum pos) (cons lnum (length line)) line)
                               (list 'NL (substring line pos)
                                     (cons lnum pos) (cons lnum (length line)) line)))))
                (throw 'continue nil))

              ;; count indents or dedents
              (when (> column (car (last indents)))
                (setq indents (append indents (list column)))
                (funcall yield (list 'INDENT (substring line 0 pos)
                                     (cons lnum 0) (cons lnum pos) line)))
              (while (< column (car (last indents)))
                (unless (member column indents)
                  (throw 'indentation-error
                         (cons "unindent does not match any outer indentation level"
                               (list "<tokenize>" lnum pos line))))
                (setq indents (butlast indents))
                (funcall yield
                         (list 'DEDENT "" (cons lnum pos) (cons lnum pos) line)))))
           (t
            (unless line
              (throw 'token-error
                     (cons "EOF in multi-line statement" (cons lnum 0))))
            (setq continued nil)))

          (setq do-loop-inner t)
          ;; FIXME: break/continue?!
          (catch 'break
            (catch 'continue
              (while (and do-loop-inner
                          (< pos max))
                (let ((pseudomatch (looking-at-string regex-pseudotoken line pos)))
                  (cond
                   ;; scan for tokens
                   (pseudomatch
                    (let* ((start (match-beginning 1))
                           (end (match-end 1))
                           (spos (cons lnum start))
                           (epos (cons lnum end))
                           (token (substring line start end))
                           (initial (elt line start)))
                      (setq pos end)
                      (when (equal start end)
                        (throw 'continue nil))

                      (cond
                       ;; ordinary number
                       ((or (member initial numchars)
                            (and (equal initial ?.) (not (equal token "."))))
                        (funcall yield (list 'NUMBER
                                             token spos epos line)))
                       ;; newline
                       ((member initial '(?\r ?\n))
                        (funcall yield (list (if (> parenlev 0 ) 'NL 'NEWLINE)
                                             token spos epos line)))
                       ;; comment
                       ((equal initial ?#)
                        (funcall yield (list 'COMMENT
                                             token spos epos line)))

                       ;; triple quoted
                       ((member token triple-quoted)
                        (setq endprog (regex-get-endprog regex-endprogs token))
                        (let ((matched (string-match endprog line pos)))
                          (cond
                           ;; all on one line
                           (matched
                            (setq pos (match-end 0)
                                  token (substring line start pos))
                            (funcall yield (list 'STRING
                                                 token spos (cons lnum pos) line)))
                           ;; multiple line
                           (t
                            (setq strstart (cons lnum start)
                                  contstr (substring line start)
                                  contline line)
                            (setq do-loop-inner nil)
                            (throw 'break nil)))))

                       ;; single_quoted
                       ((or (member initial single-quoted)
                            (and (>= (length token) 2)
                                 (member (substring token 0 2) single-quoted))
                            (and (>= (length token) 3)
                                 (member (substring token 0 3) single-quoted)))
                        (cond
                         ((equal (elt token (1- (length token))) ?\n)
                          (setq strstart (cons lnum start)
                                endprog (or (regex-get-endprog regex-endprogs initial)
                                            (regex-get-endprog regex-endprogs (elt token 1))
                                            (regex-get-endprog regex-endprogs (elt token 2)))
                                contstr (substring line start)
                                needcont t
                                contline line)
                          (setq do-loop-inner nil)
                          (throw 'break nil))
                         (t
                          (funcall yield (list 'STRING token spos epos line)))))

                       ;; ordinary name
                       ((member initial namechars)
                        (funcall yield (list (if (member token keyword-names)
                                                 'KEYWORD
                                               'NAME)
                                             token spos epos line)))

                       ;; continued stmt
                       ((equal initial ?\\)
                        (setq continued t))

                       ;; parentheses
                       (t
                        (cond
                         ((member initial '(?\( ?\[ ?\{))
                          (setq parenlev (1+ parenlev)))
                         ((member initial '(?\) ?\] ?\}))
                          (setq parenlev (1- parenlev))))
                        (funcall yield (list (gethash token opmap)
                                             token spos epos line))))))
                   (t
                    (funcall yield (list 'ERRORTOKEN
                                         (elt line pos)
                                         (cons lnum pos)
                                         (cons lnum (1+ pos))
                                         line))
                    (setq pos (1+ pos)))))))))))
    (dolist (indent (cdr indents))
      (funcall yield (list 'DEDENT
                           ""
                           (cons lnum 0)
                           (cons lnum 0)
                           "")))
    (funcall yield (list 'ENDMARKER
                         ""
                         (cons lnum 0)
                         (cons lnum 0)
                         ""))))

;;; Utils
;;; -----

(defun readline ()
  "Read one more line in the current buffer, return as a plain
string with no properties, nil if EOF is reached"
  (unless (equal (point) (point-max))
    (let (p1 p2 line)
      (setq p1 (line-beginning-position) )
      (setq p2 (line-end-position) )
      (setq line (concat (buffer-substring-no-properties p1 p2) "\n"))
      (forward-line)
      line)))

(defun yield-to-buf (elem)
  (with-current-buffer "*tokens*"
    (insert (prin1-to-string elem))
    (insert "\n")))

(defun yield-print (elem)
  (print elem))

;;; Mapping OP values to operator tokens
;;; ------------------------------------

(defconst opmap-raw "
\( LPAR
\) RPAR
: COLON
, COMMA
; SEMI
+ PLUS
- MINUS
* STAR
/ SLASH
< LESS
> GREATER
= EQUAL
% PERCENT
== EQEQUAL
!= NOTEQUAL
<> NOTEQUAL
<= LESSEQUAL
>= GREATEREQUAL
** DOUBLESTAR
")
(defconst opmap
  (eval-when-compile
    (let ((map (make-hash-table :test 'equal)))
      (dolist (pair-str (split-string opmap-raw "[\\\n]" t) map)
        (let* ((pair (split-string pair-str))
               (op (car pair))
               (token-name (cadr pair))
               ;; should be defined in token.el already
               (token-value (intern token-name)))
          (puthash op token-value map))))))

) ;; elang- namespace end

(provide 'elang-tokenizer)

;;; elang-tokenizer.el ends here
