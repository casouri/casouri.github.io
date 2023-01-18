#lang pollen

◊define-meta[date]{<2023-01-15 Sun>}
◊define-meta[uuid]{afb76ba2-8a39-11ed-998c-8f06c8e638dc}
◊define-meta[tags]{Emacs}
◊define-meta[lang]{en}

◊meta{
  ◊title{Tree-sitter Starter Guide}
}

This guide gives you a starting point on writing a tree-sitter major mode. Remember, don’t panic and check your manuals!

◊section{Build Emacs with tree-sitter}

You can either install tree-sitter by your package manager, or from
source:

◊bcode{
    git clone https://github.com/tree-sitter/tree-sitter.git
    cd tree-sitter
    make
    make install
}

To build and run Emacs 29:

◊bcode{
  git clone https://git.savannah.gnu.org/git/emacs.git -b emacs-29
  cd emacs
  ./autogen.sh
  ./configure
  make
  src/emacs
}

Require the tree-sitter package with ◊code{(require 'treesit)}. Note that tree-sitter always appear as ◊code{treesit} in symbols. Now check if Emacs is successfully built with tree-sitter library by evaluating ◊code{(treesit-available-p)}.

Tree-sitter stuff in Emacs can be categorized into two parts: the tree-sitter API itself, and integration with fontification, indentation, Imenu, etc. You can use shortdoc to glance over all the tree-sitter API functions by typing ◊code{M-x shortdoc RET treesit RET}. The integration are described in the rest of the post.

◊section{Install language definitions}

Tree-sitter by itself doesn’t know how to parse any particular language. It needs the language grammar (a dynamic library) for a language to be able to parse it.

First, find the repository for the language grammar, eg, ◊link["https://github.com/tree-sitter/tree-sitter-python"]{tree-sitter-python}. Take note of the Git clone URL of it, eg, ◊code{https://github.com/tree-sitter/tree-sitter-python.git}. Now check where is the parser.c file in that repository, usually it’s in ◊code{src}.

Make sure you have Git, C and C++ compiler, and run the ◊code{treesit-install-grammar} command, it will prompt for the URL and the directory of parser.c, leave other prompts at default unless you know what you are doing.

You can also manually clone the repository and compile it, and put the dynamic library at a standard library location. Emacs will be able to find it. If you wish to put it somewhere else, set ◊code{treesit-extra-load-path} so Emacs can find it.

◊section{Tree-sitter major modes}

Tree-sitter modes should be separate major modes, usually named ◊code{xxx-ts-mode}. I know I said tree-sitter always appear as ◊code{treesit} in symbols, this is the only exception.

If the tree-sitter mode and the “native” mode could share some setup code, you can create a “base mode”, which only contains the common setup.  For example, there is python-base-mode (shared), and both python-mode (native), and python-ts-mode (tree-sitter) derives from it.

In the tree-sitter mode, check if we can use tree-sitter with ◊code{treesit-ready-p}, it will emit a warning if tree-sitter is not ready (tree-sitter not built with Emacs, can’t find the language grammar, buffer too large, etc).

◊section{Fontification}

Tree-sitter works like this: It parses the buffer and produces a ◊link["https://en.wikipedia.org/wiki/Parse_tree"]{◊em{parse tree}}. You provide a query made of patterns and capture names, tree-sitter finds the nodes that match these patterns, tag the corresponding capture names onto the nodes and return them to you. The query function returns a list of ◊code{(capture-name . node)}.

For fontification, we simply use face names as capture names. And the captured node will be fontified in their capture name (the face).

The capture name could also be a function, in which case ◊code{(NODE OVERRIDE START END)} is passed to the function for fontification. ◊code{START} and ◊code{END} are the start and end of the region to be fontified.  The function should only fontify within that region.  The function should also allow more optional arguments with ◊code{&rest _}, for future extensibility.  For ◊code{OVERRIDE} check out the docstring of ◊code{treesit-font-lock-rules}.

◊subsection{Query syntax}

There are two types of nodes: “named nodes”, like ◊code{(identifier)}, ◊code{(function_definition)}, and “anonymous nodes”, like ◊code{"return"}, ◊code{"def"}, ◊code{"("}, ◊code{";"}. Parent-child relationship is expressed as

◊bcode{
  (parent (child) (child) (child (grand_child)))
}

Eg, an argument list ◊code{(1, "3", 1)} would be:

◊bcode{
  (argument_list "(" (number) (string) (number) ")")
}

Children could have field names:

◊bcode{
  (function_definition name: (identifier) type: (identifier))
}

To match any one in the list:

◊bcode{
  ["true" "false" "none"]
}

Capture names can come after any node in the pattern:

◊bcode{
  (parent (child) @child) @parent
}

The query above captures both the parent and the child.

The query below captures all the keywords with capture name
◊code{"keyword"}:

◊bcode{
  ["return" "continue" "break"] @keyword
}

These are the common syntax, check out the full syntax in the manual: ◊link["./html-manual/Pattern-Matching.html"]{Pattern Matching}. 

◊subsection{Query references}

But how do one come up with the queries? Take python for an example, open any python source file, type ◊code{M-x treesit-explore-mode RET}.  You should see the parse tree in a separate window, automatically updated as you select text or edit the buffer.  Besides this, you can consult the grammar of the language definition. For example, Python’s grammar file is at

◊link["https://github.com/tree-sitter/tree-sitter-python/blob/master/grammar.js"]{https://github.com/tree-sitter/tree-sitter-python/blob/master/grammar.js}

Neovim also has a bunch of ◊link["https://github.com/nvim-treesitter/nvim-treesitter/tree/master/queries"]{queries to reference from}.

The manual explains how to read grammar files in the bottom of ◊link["./html-manual/Language-Grammar.html"]{Language Grammar}.

◊subsection{Debugging queries}

If your query has problems, use ◊code{treesit-query-validate} to debug the query. It will pop a buffer containing the query (in text format) and mark the offending part in red. Set ◊code{treesit--font-lock-verbose} to ◊code{t} if you want the font-lock function to report what it’s doing.

◊subsection{Set up font-lock}

To enable tree-sitter font-lock, set ◊code{treesit-font-lock-settings} and ◊code{treesit-font-lock-feature-list} buffer-locally and call ◊code{treesit-major-mode-setup}. For example, see ◊code{python--treesit-settings} in python.el. Below is a snippet of it.

Note that like the current font-lock system, if the to-be-fontified region already has a face (ie, an earlier match fontified part/all of the region), the new face is discarded rather than applied. If you want later matches always override earlier matches, use the ◊code{:override} keyword.

Each rule should have a ◊code{:feature}, like ◊code{function-name}, ◊code{string-interpolation}, ◊code{builtin}, etc. This way users can enable/disable each feature individually.

Read the manual section ◊link["./html-manual/Parser_002dbased-Font-Lock.html"]{Parser-based Font-Lock} for more detail.

Example from python.el:

◊bcode{
(defvar python--treesit-settings
  (treesit-font-lock-rules
   :feature 'comment
   :language 'python
   '((comment) @font-lock-comment-face)

   :feature 'string
   :language 'python
   '((string) @python--treesit-fontify-string)

   :feature 'string-interpolation
   :language 'python
   :override t
   '((interpolation (identifier) @font-lock-variable-name-face))

   ...))
}

In ◊code{python-ts-mode}:

◊bcode{
(treesit-parser-create 'python)
(setq-local treesit-font-lock-settings python--treesit-settings)
(setq-local treesit-font-lock-feature-list
                '(( comment definition)
                  ( keyword string type)
                  ( assignment builtin constant decorator
                    escape-sequence number property string-interpolation )
                  ( bracket delimiter function operator variable)))
...
(treesit-major-mode-setup)
}

Concretely, something like this:

◊bcode{
(define-derived-mode python-ts-mode python-base-mode "Python"
  "Major mode for editing Python files, using tree-sitter library.

\\{python-ts-mode-map}"
  :syntax-table python-mode-syntax-table
  (when (treesit-ready-p 'python)
    (treesit-parser-create 'python)
    (setq-local treesit-font-lock-feature-list
                '(( comment definition)
                  ( keyword string type)
                  ( assignment builtin constant decorator
                    escape-sequence number property string-interpolation )
                  ( bracket delimiter function operator variable)))
    (setq-local treesit-font-lock-settings python--treesit-settings)
    (setq-local imenu-create-index-function
                #'python-imenu-treesit-create-index)
    (setq-local treesit-defun-type-regexp (rx (or "function" "class")
                                              "_definition"))
    (setq-local treesit-defun-name-function
                #'python--treesit-defun-name)
    (treesit-major-mode-setup)

    (when python-indent-guess-indent-offset
      (python-indent-guess-indent-offset))))
}

◊section{Indentation}

Indentation works like this: We have a bunch of rules that look like

◊bcode{
  (MATCHER ANCHOR OFFSET)
}

When the indenting a line, let ◊code{NODE} be the node at the beginning of the current line, we pass this node to the ◊code{MATCHER} of each rule, one of them will match the node (eg, “this node is a closing bracket!”). Then we pass the node to the ◊code{ANCHOR}, which returns a point (eg, the beginning of ◊code{NODE}’s parent). We find the column number of that point (eg, 4), add ◊code{OFFSET} to it (eg, 0), and that is the column we want to indent the current line to (4 + 0 = 4).

Matchers and anchors are functions that takes ◊code{(NODE PARENT BOL &rest _)}. Matches return nil/non-nil for no match/match, and anchors return the anchor point. An Offset is usually a number or a variable, but it can also be a function. Below are some convenient builtin matchers and anchors.

For ◊code{MATHCER} we have

◊bcode{
    (parent-is TYPE) => matches if PARENT’s type matches TYPE as regexp
    (node-is TYPE) => matches NODE’s type
    (query QUERY) => matches if querying PARENT with QUERY
                     captures NODE.

    (match NODE-TYPE PARENT-TYPE NODE-FIELD
           NODE-INDEX-MIN NODE-INDEX-MAX)

    => checks everything. If an argument is nil, don’t match that. Eg,
    (match nil TYPE) is the same as (parent-is TYPE)
}

For ◊code{ANCHOR} we have

◊bcode{
    first-sibling => start of the first sibling
    parent => start of parent
    parent-bol => BOL of the line parent is on.
    prev-sibling => start of previous sibling
    no-indent => current position (don’t indent)
    prev-line => start of previous line
}

There is also a manual section for indent: ◊link["./html-manual/Parser_002dbased-Indentation.html"]{Parser-based Indentation}.

When writing indent rules, you can use ◊code{treesit-check-indent} to
check if your indentation is correct. To debug what went wrong, set
◊code{treesit--indent-verbose} to ◊code{t}. Then when you indent, Emacs
tells you which rule is applied in the echo area.

Here is an example:

◊bcode{
(defvar typescript-mode-indent-rules
  (let ((offset 'typescript-indent-offset))
    `((typescript
       ;; This rule matches if node at point is ")", ANCHOR is the
       ;; parent node’s BOL, and offset is 0.
       ((node-is ")") parent-bol 0)
       ((node-is "]") parent-bol 0)
       ((node-is ">") parent-bol 0)
       ((node-is "\\.") parent-bol ,offset)
       ((parent-is "ternary_expression") parent-bol ,offset)
       ((parent-is "named_imports") parent-bol ,offset)
       ((parent-is "statement_block") parent-bol ,offset)
       ((parent-is "type_arguments") parent-bol ,offset)
       ((parent-is "variable_declarator") parent-bol ,offset)
       ((parent-is "arguments") parent-bol ,offset)
       ((parent-is "array") parent-bol ,offset)
       ((parent-is "formal_parameters") parent-bol ,offset)
       ((parent-is "template_substitution") parent-bol ,offset)
       ((parent-is "object_pattern") parent-bol ,offset)
       ((parent-is "object") parent-bol ,offset)
       ((parent-is "object_type") parent-bol ,offset)
       ((parent-is "enum_body") parent-bol ,offset)
       ((parent-is "arrow_function") parent-bol ,offset)
       ((parent-is "parenthesized_expression") parent-bol ,offset)
       ...))))
}

Then you set ◊code{treesit-simple-indent-rules} to your rules, and call ◊code{treesit-major-mode-setup}.

◊section{Imenu}

Set ◊code{treesit-simple-imenu-settings} and call ◊code{treesit-major-mode-setup}.

◊section{Navigation}

Set ◊code{treesit-defun-type-regexp}, ◊code{treesit-defun-name-function}, and call ◊code{treesit-major-mode-setup}.

◊section{C-like languages}

c-ts-mode.el has some goodies for handling indenting and filling block comments.

These two rules should take care of indenting block comments.

◊bcode{
((and (parent-is "comment") c-ts-mode--looking-at-star)
            c-ts-mode--comment-start-after-first-star -1)
((parent-is "comment") prev-adaptive-prefix 0)
}

Set ◊code{c-ts-mode-indent-block-type-regexp} and these two rules should take care of indenting statements in “{}” blocks and closing bracket “}”.

◊bcode{
((node-is "◊cbk{}") point-min c-ts-mode--close-bracket-offset)
((parent-is "compound_statement")
 point-min c-ts-mode--statement-offset)
}

◊code{c-ts-mode-comment-setup} will set up comment and filling for you.

◊section{Multi-language modes}

Refer to the manual: ◊link["./html-manual/Multiple-Languages.html"]{Multiple Languages}.

◊section{Common Tasks}

◊code{M-x shortdoc RET treesit RET} will give you a complete list.

How to...

◊b{Get the buffer text corresponding to a node?}

◊bcode{
  (treesit-node-text node)
}

Don’t confuse this with ◊code{treesit-node-string}.

◊b{Scan the whole tree for stuff?}

◊bcode{
(treesit-search-subtree)
(treesit-search-forward)
(treesit-induce-sparse-tree)
}

◊b{Find/move to to next node that...?}

◊bcode{
(treesit-search-forward node ...)
(treesit-search-forward-goto node ...)
}

◊b{Get the root node?}

◊bcode{
  (treesit-buffer-root-node)
}

◊b{Get the node at point?}

◊bcode{
  (treesit-node-at (point))
}

