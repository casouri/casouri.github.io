#lang pollen

◊define-meta[date]{<2022-06-16 Thu 14:23>}
◊define-meta[uuid]{8d006576-edba-11ec-94fa-4383400430f2}
◊define-meta[tags]{Emacs}
◊define-meta[lang]{en}

◊meta{
  ◊title{Emacs tree-sitter Integration, Updated}
}

Last September I posted an article calling for comments on the tree-sitter integration for Emacs, and described how to build and play with it. A lot has changed since then, and here is an updated version of that article.

◊link["https://tree-sitter.github.io/tree-sitter/"]{Tree-sitter} is a incremental parser that can provide a concrete syntax tree for the source code and is fast enough to parse on each key press. It has supported a wide range of languages, and support for more languages is on the way.

I’ve been working on a integration of tree-sitter library into Emacs’ core. The integration consists of two parts, first the direct translation of tree-sitter’s API and some additional higher-level functions, second the integration with Emacs’ high-level features: font-locking, indentation commands, structured navigation, etc.

If you are interested, follow the guide below to build and get familiar with the integration, and try implement something fun! If you encountered problems or have suggestions, open a discussion on ◊link["https://lists.gnu.org/mailman/listinfo/emacs-devel"]{emacs-devel mailing list}, or open an issue on the ◊link["https://github.com/casouri/emacs"]{GitHub mirror}. If you don’t know how to participate in the mailing list, my previous article: ◊link["https://archive.casouri.cat/note/2020/contributing-to-emacs/index.html#The-mailing-list"]{◊em{Contributing to Emacs}} goes over it in section 3.

◊section{Building Emacs with tree-sitter integration}

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
  git clone -b feature/tree-sitter master git://git.sv.gnu.org/emacs.git
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

should produce libraries for C, JSON, Go, HTML, JavaScript, CSS and Python and store them in ◊code{dist} directory. From there you can copy these libraries to a standard path, or add that directory to ◊code{LD_LIBRARY_PATH}, or add the directory to ◊code{treesit-extra-load-path}, eg, add this to your init file:

◊bcode{
  (add-to-list 'treesit-extra-load-path
                "/path/to/tree-sitter-module/dist")
}

You can also find pre-built libraries in the release page: ◊link["https://github.com/casouri/tree-sitter-module/releases/tag/v2%2C0"]{◊em{tree-sitter-module release v2.0}}.

◊section{A quickstart}

Let’s play with tree-sitter! Open a C file, or use the demo file I have here: ◊link["./demo.c"]{demo.c}. In the buffer, we can

◊bcode{
  ;; Create a parser
  (setq parser (treesit-parser-create 'c))

  ;; Retrieve the root node.
  (treesit-parser-root-node parser)

  ;; Retreive the node at point.
  (setq node (treesit-node-at (point)))

  ;; Get the parent of a node.
  (setq parent (treesit-node-parent node))

  ;; Get the node’s text content.
  (treesit-node-content parent)

  ;; Get the node’s sexp representation
  (treesit-node-string parent)
}

Now lets try some navigation. Set
