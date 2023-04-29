;;; treesit-python-mode.el --- Tree-sitter Python Demo Mode  -*- lexical-binding: t; -*-

;; Author: Yuan Fu <casouri@gmail.com>

;;; This file is NOT part of GNU Emacs

;;; Commentary:

;;; Code:

(require 'treesit)
(require 'python)
(require 'cl-lib)

;;; Font-lock

(defvar treesit-python-builtins
  '("abs" "all" "any" "ascii" "bin" "bool" "breakpoint" "bytearray"
    "bytes" "callable" "chr" "classmethod" "compile" "complex"
    "delattr" "dict" "dir" "divmod" "enumerate" "eval" "exec"
    "filter" "float" "format" "frozenset" "getattr" "globals"
    "hasattr" "hash" "help" "hex" "id" "input" "int" "isinstance"
    "issubclass" "iter" "len" "list" "locals" "map" "max"
    "memoryview" "min" "next" "object" "oct" "open" "ord" "pow"
    "print" "property" "range" "repr" "reversed" "round" "set"
    "setattr" "slice" "sorted" "staticmethod" "str" "sum" "super"
    "tuple" "type" "vars" "zip" "__import__"))

(defvar treesit-python-operators
  '("-" "-=" "!=" "*" "**" "**=" "*=" "/" "//" "//=" "/=" "&" "%" "%="
    "^" "+" "+=" "<" "<<" "<=" "<>" "=" "==" ">" ">=" ">>" "|" "~"
    "and" "in" "is" "not" "or"))

(defvar treesit-python-keywords
  '("as" "assert" "async" "await" "break" "class" "continue" "def"
    "del" "elif" "else" "except" "exec" "finally" "for" "from"
    "global" "if" "import" "lambda" "nonlocal" "pass" "print"
    "raise" "return" "try" "while" "with" "yield"))

(defvar treesit-python-special-attributes
  '("__annotations__" "__closure__" "__code__"
    "__defaults__" "__dict__" "__doc__" "__globals__"
    "__kwdefaults__" "__name__" "__module__" "__package__"
    "__qualname__" "__all__"))

(defvar treesit-python-exceptions
  '(;; Python 2 and 3:
    "ArithmeticError" "AssertionError" "AttributeError" "BaseException"
    "BufferError" "BytesWarning" "DeprecationWarning" "EOFError"
    "EnvironmentError" "Exception" "FloatingPointError" "FutureWarning"
    "GeneratorExit" "IOError" "ImportError" "ImportWarning"
    "IndentationError" "IndexError" "KeyError" "KeyboardInterrupt"
    "LookupError" "MemoryError" "NameError" "NotImplementedError"
    "OSError" "OverflowError" "PendingDeprecationWarning"
    "ReferenceError" "RuntimeError" "RuntimeWarning" "StopIteration"
    "SyntaxError" "SyntaxWarning" "SystemError" "SystemExit" "TabError"
    "TypeError" "UnboundLocalError" "UnicodeDecodeError"
    "UnicodeEncodeError" "UnicodeError" "UnicodeTranslateError"
    "UnicodeWarning" "UserWarning" "ValueError" "Warning"
    "ZeroDivisionError"
    ;; Python 2:
    "StandardError"
    ;; Python 3:
    "BlockingIOError" "BrokenPipeError" "ChildProcessError"
    "ConnectionAbortedError" "ConnectionError" "ConnectionRefusedError"
    "ConnectionResetError" "FileExistsError" "FileNotFoundError"
    "InterruptedError" "IsADirectoryError" "NotADirectoryError"
    "PermissionError" "ProcessLookupError" "RecursionError"
    "ResourceWarning" "StopAsyncIteration" "TimeoutError"
    ;; OS specific
    "VMSError" "WindowsError"
    ))

(defun treesit-python-string-fontify (beg end _)
  "Do not fontify the initial f for f-strings."
  (let ((beg (if (eq (char-after beg) ?f)
                 (1+ beg) beg)))
    (put-text-property beg end 'face 'font-lock-string-face)))

(defvar treesit-python-settings
  (treesit-font-lock-rules
   :language 'python
   `((function_definition
      name: (identifier) @font-lock-function-name-face)

     (class_definition
      name: (identifier) @font-lock-type-face)

     (comment) @font-lock-comment-face

     (string) @treesit-python-string-fontify
     ((string) @font-lock-doc-face
      (:match "^\"\"\"" @font-lock-doc-face))
     (interpolation (identifier) @font-lock-variable-name-face)

     [,@treesit-python-keywords] @font-lock-keyword-face

     ((identifier) @font-lock-keyword-face
      (:match "^self$" @font-lock-keyword-face))

     ((identifier) @font-lock-builtin-face
      (:match ,(rx-to-string
                `(seq bol
                      (or ,@treesit-python-builtins
                          ,@treesit-python-special-attributes)
                      eol))
              @font-lock-builtin-face))

     (assignment left: (identifier)
                 @font-lock-variable-name-face)
     (pattern_list (identifier)
                   @font-lock-variable-name-face)
     (tuple_pattern (identifier)
                    @font-lock-variable-name-face)
     (list_pattern (identifier)
                   @font-lock-variable-name-face)
     (list_splat_pattern (identifier)
                         @font-lock-variable-name-face)
     (decorator) @font-lock-type-face
     ((identifier) @font-lock-type-face
      (:match ,(rx-to-string
                `(seq bol (or ,@treesit-python-exceptions)
                      eol))
              @font-lock-type-face))

     ;; Possibly unnecessary.
     (type (identifier) @font-lock-type-face))))

;;; Which-func

(defun treesit-python-which-func (&optional include-type)
  (let ((node (treesit-node-at (point)))
        (name-list ())
        (type 'def))
    (cl-loop while node
             if (pcase (treesit-node-type node)
                  ("function_definition"
                   (setq type 'def))
                  ("class_definition"
                   (setq type 'class))
                  (_ nil))
             do (push (treesit-node-text
                       (treesit-node-child-by-field-name node "name")
                       t)
                      name-list)
             do (setq node (treesit-node-parent node))
             finally return (concat (if include-type
                                        (format "%s " type)
                                      "")
                                    (string-join name-list ".")))))

;;; Imenu

(defun treesit-python-imenu-node-to-entry (node)
  (let ((name (treesit-node-text
               (treesit-node-child-by-field-name
                node "name")))
        (pos (treesit-node-start node)))
    (setq node (treesit-node-parent node))
    (while node
      (pcase (treesit-node-type node)
        ((or "function_definition" "class_definition")
         (setq name (concat (treesit-node-text
                             (treesit-node-child-by-field-name
                              node "name"))
                            "/" name))))
      (setq node (treesit-node-parent node)))
    (cons name pos)))

(defun treesit-python-imenu-create-index ()
  (let ((class-list (treesit-query-in
                     'python "(class_definition) @cap" nil nil t))
        (function-list (treesit-query-in
                        'python "(function_definition) @cap" nil nil t)))
    (append (mapcar #'treesit-python-imenu-node-to-entry class-list)
            (mapcar #'treesit-python-imenu-node-to-entry function-list))))



;;; Major mode

(define-derived-mode treesit-python-mode prog-mode "TS Python"
  "Python mode with tree-sitter support."
  (if (treesit-can-enable-p)
      (progn
        (setq-local treesit-font-lock-settings
                    treesit-python-settings)
        ;; Disable font-locking for parenthesizes and strings.
        (setq-local font-lock-defaults '(ignore t nil nil nil))
        (treesit-font-lock-enable)

        (setq-local treesit-defun-query
                    (treesit-query-compile
                     'python
                     '((class_definition) @cap
                       (function_definition) @cap)))
        (setq-local beginning-of-defun-function
                    #'treesit-beginning-of-defun
                    end-of-defun-function
                    #'treesit-end-of-defun)

        (setq-local comment-start "# ")
        (setq-local comment-start-skip "#+\\s-*")
        (setq-local parse-sexp-lookup-properties t)
        (setq-local parse-sexp-ignore-comments t)

        ;; I’m not ashamed.
        (setq-local indent-line-function #'python-indent-line-function)
        (setq-local indent-region-function #'python-indent-region)
        (setq-local electric-indent-inhibit t)
        (setq-local electric-indent-chars
                    (cons ?: electric-indent-chars))

        (add-hook 'which-func-functions #'treesit-python-which-fn nil t)

        (setq-local imenu-create-index-function
                    #'treesit-python-imenu-create-index))

    (error "Tree-sitter cannot be enabled, either tree-sitter library is not available, or the buffer is too large")
    ))

;;; Side effects

(add-to-list 'auto-mode-alist '("\\.py\\'" . treesit-python-mode))

(provide 'treesit-python-mode)

;;; treesit-python-mode.el ends here
