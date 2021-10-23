#lang pollen

◊define-meta[date]{<2021-09-28 Tue 10:12>}
◊define-meta[uuid]{484e573e-207f-11ec-bd91-975a51a5f3f1}
◊define-meta[tags]{Emacs}
◊define-meta[lang]{en}

◊meta{
  ◊title{RFC: Emacs tree-sitter integration}
}

◊link["https://tree-sitter.github.io/tree-sitter/"]{Tree-sitter} is a incremental parser that can provide a concrete syntax tree for the source code and is fast enough to parse on each key press. It has supported a wide range of languages, and support for more languages is on the way.

I’ve been working on a integration of tree-sitter library into Emacs’ core. The integration consists of two parts, first the direct translate of tree-sitter’s API, second the integration with Emacs’ font-lock and indent system.  The first part is completed and is rather uncontentious. I’d appreciate comments on the second: Is the interface easy to understand? Is it easy to use? Is it flexible enough for every language?

Whether you are a major mode author or just a interested Emacs user, I invite you to try hacking with this tree-sitter integration—recreate existing major mode features (font-lock, indent), create new features (structured editing, etc)—and tell me how well it works. Better yet, provide some suggestions on improving the interface.

◊section{Building Emacs with tree-sitter support}

◊subsection{Install tree-sittter}

First, install libtree-sitter, either by a package manager, or from source:

◊bcode{
  git clone https://github.com/tree-sitter/tree-sitter.git
  cd tree-sitter
  make
  make install
}

This should install libtree-sitter in standard location.

◊subsection{Build Emacs}

Then, build Emacs from my GitHub repository. Make sure you clone the ◊code{ts} branch.

◊bcode{
  git clone https://github.com/casouri/emacs.git --branch ts
  ./autogen.sh
  ./configure
  make
}

No need for special configure flags, tree-sitter is enabled automatically if libtree-sitter is present on the system. Now Emacs can be started by

◊bcode{
  src/emacs
}

◊subsection{Get language definitions}

To use tree-sitter features in any meaningful way, we also need the language definition, eg, libtree-sitter-c for C. I wrote a script for automatically retrieving and compiling some of the libraries. The following commands

◊bcode{
  git clone https://github.com/casouri/tree-sitter-module.git
  cd tree-sitter-module
  ./batch-new.sh
}

should produce libraries for C, JSON, Go, HTML, JavaScript, CSS and Python and store them in ◊code{dist} directory. From there you can copy these libraries to a standard path, or add that directory to ◊code{LD_LIBRARY_PATH}.

You can also find pre-built libraries in the release page: ◊link["https://github.com/casouri/tree-sitter-module/releases/tag/v2%2C0"]{◊em{tree-sitter-module release v2.0}}.

◊section{Basic tree-sitter features}

I suggest reading the tree-sitter node in the manual first, it covers how to create a parser, how to retrieve a node, how to pattern match nodes, and more. You can access the manual by typing

◊bcode{
  C-h i m elisp RET g Parsing Program Source RET
}

The command(s) above opens the Info reader, goes to ◊em{Elisp Reference Manual}, and opens the “Parsing Program Source” node, which contains manual for tree-sitter. Alternatively, you can read ◊link["./Parsing-Program-Source.html"]{the tree-sitter node} that I clipped from the HTML manuel.

Once you’ve read the manual, you can ◊code{(require 'tree-sitter)} and hack away!

The manual only documents basic features of tree-sitter, leaving out font-lock and indent integration, because I expect the latter to change. They are instead documented below.

◊section{Font-lock interface}

(From now on, I assume you have read the manual and I will use concepts introduced in the manual without explanation.)

If you are familiar with font-lock in Emacs, you know it is primarily configured by ◊code{font-lock-defaults}: major mode sets this variable with language-specific configuration, font-lock takes that variable and populate ◊code{font-lock-keywords}, which directly defines the pattern to fontify.

◊subsection{◊code{tree-sitter-font-lock-settings}}

◊fnref["ts-name"]{Tree-sitter} provides two analogues variables, ◊code{tree-sitter-font-lock-defaults} and ◊code{tree-sitter-font-lock-settings}. ◊code{tree-sitter-font-lock-settings} is a list of ◊code{SETTING}s where each ◊code{SETTING} looks like

◊bcode{
  (LANGUAGE QUERY)
}

◊code{LANGUAGE} is the language this setting should use, and ◊code{QUERY} is either a string or a sexp query. Each capture name in ◊code{QUERY} is either a face name, in which case the captured node is fontified in that face, or a function name, in which case the captured node is passed to the function for fontification. Specifically, the function is passed three arguments ◊code{(BEG END NODE)}, where ◊code{BEG} and ◊code{END} is the beginning and end position of the node in the buffer, for convenience.

An example ◊code{SETTING} for C is

◊bcode{
  (tree-sitter-c ; LANGUAGE
   ((null) @font-lock-constant-face
    (true) @font-lock-constant-face
    (false) @font-lock-constant-face)) ; QUERY
}

◊fndef["ts-name"]{From now on, “tree-sitter” refers to the Emacs integration of tree-sitter.}

◊subsection{◊code{tree-sitter-font-lock-defaults}}

Tree-sitter font-lock, like font-lock, support fontification at different levels of decoration (controlled by ◊code{font-lock-maximum-decoration}). And this is the primary purpose of ◊code{tree-sitter-font-lock-defaults}. Its value is a list of

◊bcode{
  (DEFAULT :KEYWORD VALUE...)
}

Where each ◊code{DEFAULT} may be a symbol or a list of symbols. The symbol should be either a variable containing ◊code{(LANGUAGE QUERY)}, or a function that returns that.  If ◊code{DEFAULT} is a list, each symbol corresponds to a decoration level. For example, if I want to implement three levels of decoration for C, I would populate ◊code{tree-sitter-font-lock-defaults} with

◊bcode{
  (((c-font-lock-settings-1
     c-font-lock-settings-2
     c-font-lock-settings-3)
    :KEYWORD VALUE...))
}

where ◊code{c-font-lock-settings-1} would contain, say,

◊bcode{
  (tree-sitter-c
   ((null) @font-lock-constant-face
    (true) @font-lock-constant-face
    (false) @font-lock-constant-face))
}

for those who need no more. And the other two levels could be for the rest mortals. As for ◊code{:KEYWORD} and ◊code{VALUE}, they are analogues to that in ◊code{font-lock-defaults}, used for specifying other configurations. Currently they are not used for tree-sitter font-lock.

To enable tree-sitter font-lock, a major mode should first assign ◊code{tree-sitter-font-lock-defaults}, then call ◊code{tree-sitter-font-lock-enable}. For example,

◊bcode{
(define-derived-mode ts-c-mode prog-mode "tree-sitter C"
  (setq-local tree-sitter-font-lock-defaults
              '((ts-c-tree-sitter-settings-1)))
  (tree-sitter-enable-font-lock))
}

◊section{Indentation}

In Emacs, indentation is provided by ◊code{indent-line-function}. Tree-sitter provides a convenient system, ◊em{tree-sitter-simple-indent}, to simplify the implementation of a indenting function. To use it, bind ◊code{indent-line-function} to ◊code{tree-sitter-indent}, and fill in indentation configurations in ◊code{tree-sitter-simple-indent-rules}.

◊code{tree-sitter-simple-indent-rules} is a list of rules, and each rule looks like

◊bcode{
(MATCHER ANCHOR OFFSET)
}

When indenting, ◊em{tree-sitter-simple-indent} finds the largest node that starts at the beginning of the current line, and matches it against each ◊code{MATCHER} in ◊code{tree-sitter-simple-indent-rules}. If ◊code{MATCHER} matches that node, ◊code{ANCHOR} and ◊code{OFFSET} determines how to indent—find the column of ◊code{ANCHOR} (which represents a point), and add ◊code{OFFSET} to it.

By now you must be wondering what the heck is ◊code{MATCHER}. It is a function that takes ◊code{(NODE PARENT BOL &rest _)} as arguments, if the rule should apply to ◊code{NODE}, it returns non-nil. ◊code{PARENT} and ◊code{BOL} (position of beginning of line) are provided just for convenience. The “◊code{&rest _}” part is required to allow the possibility to extend the interface in the future.

This function can do anything: check the type of that node, check the type of its parent, check whether this node is the first child node of its parent, etc. ◊code{ANCHOR} is also a function that takes theses arguments, but it returns a point, the “anchor”. If the rule determines that the node should be indented two columns inward comparing to its parent, ◊code{ANCHOR} should return the start of the parent node, and ◊code{OFFSET} should be 2.

For example, the following rule matches any line that starts with the ◊code{null} keyword, and indents the line inwards by two columns against the ◊code{null}’s parent node.

◊bcode{
  ((lambda (n p bol &rest _)
     (equal (tree-sitter-node-type n) "null")) ; MATCHER
   (lambda (n p bol &rest _)
     (tree-sitter-node-start
      (tree-sitter-node-parent n))) ; ANCHOR
   2) ; OFFSET
}

Of course, it is terribly tedious to write out every ◊code{MATCHER} and ◊code{ANCHOR} explicitly. ◊em{tree-sitter-simple-indent} provides some predefined ◊code{MATCHER} and ◊code{ANCHOR} functions. Most of them are higher-order functions: they takes an argument and returns a function.

◊code{MATCHER} presets:

◊dl{
  ◊dt{◊code{(parent-is TYPE)}}
  ◊dd{Check that the parent has type ◊code{TYPE}.}

  ◊dt{◊code{(node-is TYPE)}}
  ◊dd{Check that node has type ◊code{TYPE}.}

  ◊dt{◊code{
      (match NODE-TYPE PARENT-TYPE NODE-FIELD NODE-INDEX-MIN NODE-INDEX-MAX)
    }}
  ◊dd{
    ◊code{NODE-TYPE} checks for node’s type, ◊code{PARENT-TYPE} checks for parent’s type, ◊code{NODE-FIELD} checks for the field name for node int the parent, ◊code{NODE-INDEX-MIN} and ◊code{NODE-INDEX-MAX} limits the node’s index in the parent. Any argument left as nil are not checked. For example, to match the node that is the first child and has a parent of type ◊code{argument_list}, use
    ◊code{(match nil "argument_list" nil nil 0 0)}
  }

  ◊dt{◊code{(query QUERY)}}
  ◊dd{Queries the parent with ◊code{QUERY}. Matches if the node is captured by any capture name.}

  ◊dt{◊code{no-node}}
  ◊dd{Matches null node. When the current line is empty, there is no node at the beginning, so the node is nil.}
}

◊code{ANCHOR} presets:

◊dl{
  ◊dt{◊code{first-child}}
  ◊dd{Finds the first sibling of node, ie, the first child of the parent.}

  ◊dt{◊code{parent}}
  ◊dd{Finds the parent node.}

  ◊dt{◊code{prev-sibling}}
  ◊dd{Finds node’s first sibling.}

  ◊dt{◊code{no-indent}}
  ◊dd{Do nothing, don’t indent. This is useful for a indenting a line inside a multiline string, where masterful inactivity is most preferred.}

  ◊dt{◊code{prev-line}}
  ◊dd{Find the named node on the previous line. This can be used when indenting an empty line: just indent like the previous node.}
}

◊section{Some handy tools}

I have two handy tools for you to work with tree-sitter more easily: first, ◊code{tree-sitter-inspect-mode} will show the relevant information of the node at point in the mode-line; second, ◊code{tree-sitter-check-indent} can check the indent result against a stock major mode. Check out their docstring for more detail.

◊section{Feedback}

You can send a message to ◊link["https://lists.gnu.org/mailman/listinfo/emacs-devel"]{◊em{emacs-devel}}, or open an issue on the ◊link["https://github.com/casouri/emacs"]{GitHub repository}.

◊section{An example}

All these must be pretty confusing without seeing a concrete example, so here it is. This example code is for a demo C major mode, ◊code{ts-c-mode}, defined in the “◊code{;;; Lab}” section in ◊code{tree-sitter.el}. (Here is a ◊link["https://github.com/casouri/emacs/blob/350ae9cc19e478f08468443843f63bdf005d9d92/lisp/tree-sitter.el#L640"]{link to the file on GitHub}.)

Indent:

◊bcode{
(defvar ts-c-tree-sitter-indent-rules
  `((tree-sitter-c
     ;; Empty line.
     (no-node prev-line 0)

     ;; Function/struct definition body ◊"{}".
     ((match nil "function_definition" "body") parent 0)
     ((node-is "field_declaration_list") parent 0)

     ;; Call expression.
     ((parent-is "call_expression") parent 2)

     ;; If-else.
     ((match nil "if_statement" "condition") parent 2)
     ((match nil "if_statement" "consequence") parent 2)
     ((match nil "if_statement" "alternative") parent 2)
     ((match nil "switch_statement" "condition")  parent 2)
     ((node-is "else") parent 0)

     ;; Switch case.
     ((parent-is "case_statement") parent 2)
     ((node-is "case_statement") parent 0)

     ;; ◊"{" and ◊"}."
     ((node-is "compound_statement") parent 2)
     ((node-is "◊"}"") parent 0)

     ;; Multi-line string.
     ((parent-is "string_literal") no-indent 0)

     ;; List.
     ,@(cl-loop for type in '("compound_statement" "initializer_list"
                              "argument_list" "parameter_list"
                              "field_declaration_list")
                collect `((match nil ,type nil 0 0) parent 2)
                collect `((match nil ,type nil 1) first-sibling 0)))))
}

Font-lock:

◊bcode{
(defvar ts-c-tree-sitter-settings-1
  '(tree-sitter-c
    ((null) @font-lock-constant-face
     (true) @font-lock-constant-face
     (false) @font-lock-constant-face

     (comment) @font-lock-comment-face

     (system_lib_string) @ts-c-fontify-system-lib

     (unary_expression
      operator: _ @font-lock-negation-char-face)

     (string_literal) @font-lock-string-face
     (char_literal) @font-lock-string-face



     (function_definition
      declarator: (identifier) @font-lock-function-name-face)

     (declaration
      declarator: (identifier) @font-lock-function-name-face)

     (function_declarator
      declarator: (identifier) @font-lock-function-name-face)



     (init_declarator
      declarator: (identifier) @font-lock-variable-name-face)

     (parameter_declaration
      declarator: (identifier) @font-lock-variable-name-face)

     (preproc_def
      name: (identifier) @font-lock-variable-name-face)

     (enumerator
      name: (identifier) @font-lock-variable-name-face)

     (field_identifier) @font-lock-variable-name-face

     (parameter_list
      (parameter_declaration
       (identifier) @font-lock-variable-name-face))

     (pointer_declarator
      declarator: (identifier) @font-lock-variable-name-face)

     (array_declarator
      declarator: (identifier) @font-lock-variable-name-face)

     (preproc_function_def
      name: (identifier) @font-lock-variable-name-face
      parameters: (preproc_params
                   (identifier) @font-lock-variable-name-face))



     (type_identifier) @font-lock-type-face
     (primitive_type) @font-lock-type-face

     "auto" @font-lock-keyword-face
     "break" @font-lock-keyword-face
     "case" @font-lock-keyword-face
     "const" @font-lock-keyword-face
     "continue" @font-lock-keyword-face
     "default" @font-lock-keyword-face
     "do" @font-lock-keyword-face
     "else" @font-lock-keyword-face
     "enum" @font-lock-keyword-face
     "extern" @font-lock-keyword-face
     "for" @font-lock-keyword-face
     "goto" @font-lock-keyword-face
     "if" @font-lock-keyword-face
     "register" @font-lock-keyword-face
     "return" @font-lock-keyword-face
     "sizeof" @font-lock-keyword-face
     "static" @font-lock-keyword-face
     "struct" @font-lock-keyword-face
     "switch" @font-lock-keyword-face
     "typedef" @font-lock-keyword-face
     "union" @font-lock-keyword-face
     "volatile" @font-lock-keyword-face
     "while" @font-lock-keyword-face

     "long" @font-lock-type-face
     "short" @font-lock-type-face
     "signed" @font-lock-type-face
     "unsigned" @font-lock-type-face

     "#include" @font-lock-preprocessor-face
     "#define" @font-lock-preprocessor-face
     "#ifdef" @font-lock-preprocessor-face
     "#ifndef" @font-lock-preprocessor-face
     "#endif" @font-lock-preprocessor-face
     "#else" @font-lock-preprocessor-face
     "#elif" @font-lock-preprocessor-face
     )))
}
