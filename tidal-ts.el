;; Author: trimmer@blap.space
;; Homepage: https://github.com/tidalcycles/Tidal

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>

(require 'haskell-ts-mode)

(defcustom tidal-ts-motion-search "variable"
  "Predicate for forward-sexp and backward-sexp motion"
  :type '(string) 
  :group 'tidal
  :version "29.4")

(defcustom tidal-ts-cc-label "xl1\\|xl2\\|xl3\\|xr1\\|xr2\\|xr3\\|xlh\\|xlm\\|xll\\|xrh\\|xrm\\|xrl\\|xlp\\|xlv\\|xrp\\|xrv"
  "Regexp matching cc label."
  :type '(string)
  :group 'tidal
  :version "29.4")

(defcustom tidal-ts-cc-label "xl1\\|xl2\\|xl3\\|xr1\\|xr2\\|xr3\\|xlh\\|xlm\\|xll\\|xrh\\|xrm\\|xrl\\|xlp\\|xlv\\|xrp\\|xrv"
  "Regexp matching cc label."
  :type '(string)
  :group 'tidal
  :version "29.4")


(defface tidal-hush-face  '((default :inherit 'font-lock-variable-name-face))
  "Tidal mode face for hush."
  :group 'tidal
  :version "29.4")

(defface tidal-runner-face '((default :inherit 'font-lock-variable-name-face))
  "Tidal mode face for runner (eg: d1)."
  :group 'tidal
  :version "29.4")

(defface tidal-function-call-face '((default :inherit 'font-lock-function-call-face))
  "Tidal mode face for function call."
  :group 'tidal
  :version "29.4")

(defface tidal-pattern-face '((default :inherit 'font-lock-string-face))
  "Tidal mode face for string pattern."
  :group 'tidal
  :version "29.4")

(defface tidal-cc-face '((default :inherit 'font-lock-warning-face))
  "Tidal mode face for cc labels."
  :group 'tidal
  :version "29.4")

(defvar haskell-ts-prettify-symbols-alist)

(defvar tidal-ts-prettify-symbols-alist
  (append haskell-ts-prettify-symbols-alist
	  '(("<~" . "↜")
	    ("~>" . "↝"))))

(defvar tidal-ts-font-lock
  (treesit-font-lock-rules
   :language 'haskell
   :feature 'tidal-hush
   `(((top_splice (variable) @tidal-hush-face) (:match "hush" @tidal-hush-face)))
   :language 'haskell
   :feature 'tidal-runner
   `((top_splice
      (infix left_operand: (_) @tidal-runner-face 
	     operator: (_))))
   :language 'haskell
   :feature 'tidal-function
   `((apply function: (variable) @tidal-function-call-face
	    argument: (_)))
   :language 'haskell
   :feature 'tidal-pattern
   `((literal (_)) @tidal-pattern-face)
   :language 'haskell
   :feature 'cc
   `((((variable) @var) (:match ,tidal-ts-cc-label  @var)) @tidal-cc-face)
   :language 'haskell
   :feature 'keyword
   `(["module" "import" "data" "let" "where" "case" "type"
      "if" "then" "else" "of" "do" "in" "instance" "class"]
     @font-lock-keyword-face)
   :language 'haskell
   :feature 'otherwise
   :override t
   `(((match (guards guard: (boolean (variable) @font-lock-keyword-face)))
      (:match "otherwise" @font-lock-keyword-face)))
   :language 'haskell
   :feature 'type-sig
   "(signature (binding_list (variable) @font-lock-doc-markup-face))
    (signature (variable) @font-lock-doc-markup-face)"
   :language 'haskell
   :feature 'args
   :override 'keep
   (concat
    "(function (infix left_operand: (_) @haskell-ts--fontify-arg))"
    "(function (infix right_operand: (_) @haskell-ts--fontify-arg))"
    "(generator . (_) @haskell-ts--fontify-arg)"
    "(bind (as (variable) . (_) @haskell-ts--fontify-arg))"
    "(patterns) @haskell-ts--fontify-arg")
   :language 'haskell
   :feature 'type
   `((type) @font-lock-type-face
     (constructor) @font-lock-type-face)
   :language 'haskell
   :override t
   :feature 'signature
   `((signature (function) @haskell-ts--fontify-type)
     (context (function) @haskell-ts--fontify-type))
   :language 'haskell
   :feature 'match
   `((match ("|" @font-lock-doc-face) ("=" @font-lock-doc-face))
     (list_comprehension ("|" @font-lock-doc-face
			  (qualifiers (generator "<-" @font-lock-doc-face))))
     (match ("->" @font-lock-doc-face)))
   :language 'haskell
   :feature 'comment
   `(((comment) @font-lock-comment-face)
     ((haddock) @font-lock-doc-face))
   :language 'haskell
   :feature 'pragma
   `((pragma) @font-lock-preprocessor-face
     (cpp) @font-lock-preprocessor-face)
   :language 'haskell
   :feature 'str
   :override t
   `(
     (quasiquote (quoter) @font-lock-type-face)
     (quasiquote (quasiquote_body) @font-lock-preprocessor-face))
   :language 'haskell
   :feature 'parens
   :override t
   `(["(" ")" "[" "]"] @font-lock-operator-face
     (infix operator: (_) @nano-salient))
   :language 'haskell
   :feature 'function
   :override t
   `((function name: (variable) @font-lock-function-name-face)
     (function (infix (operator)  @font-lock-function-name-face))
     (declarations (type_synomym (name) @font-lock-function-name-face))
     (bind (variable) @font-lock-function-name-face)
     (function (infix (infix_id (variable) @font-lock-function-name-face)))
     (bind (as (variable) @font-lock-function-name-face))))
  "A function that returns the treesit font lock lock settings for haskell.")

(defvar tidal-ts-font-lock-feature-list
  `((comment str pragma parens)
    (type definition function args)
    (match keyword)
    (otherwise signature type-sig tidal-runner tidal-hush tidal-functionè-call tidal-pattern)))


(defmacro tidal-ts-imenu-name-function (check-func)
  `(lambda (node)
     (if (funcall ,check-func node)
	 (treesit-node-text node)
       nil)))

(defvar tidal-ts-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; The defaults are mostly fine
    (mapc
     (lambda (ls)
       (mapc
	(lambda (char)
	  (modify-syntax-entry char (car ls) table))
	(cdr ls)))
     '(("_" ?! ?_)
       ("w" ?' ?| ?#)
       ;; Haskell has some goofy comment enders like C-q C-l
       (">" 13 10 12 11)
       ("_ 123" ?-)
       ("(}1nb" ?\{)
       ("){4nb" ?\})
       ;;("<" ?#)
       (">" ?\n)
       ;; Special operaters
       ("." ?\, ?\; )
       ("\"" ?\")
       ("$`"  ?\`)))
    table))

(defun tidal-ts-imenu-runner-node-p (node)
  (and (string-match-p "infix\\|variable" (treesit-node-type node))
       (string= (treesit-node-type (treesit-node-parent node)) "top_splice")))


(defun tidal-ts-forward-operator (point)
  (interactive "d")
  (goto-char (treesit-node-end (treesit-search-forward (treesit-node-at point) tidal-ts-motion-search))))

(defun tidal-ts-backward-operator (point)
  (interactive "d")
  (goto-char (treesit-node-end (treesit-search-forward (treesit-node-at point) tidal-ts-motion-search 'backward))))

(provide 'tidal-ts)
