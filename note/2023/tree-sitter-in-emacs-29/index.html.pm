#lang pollen

◊define-meta[date]{<2023-01-15 Sun>}
◊define-meta[uuid]{fac62c4a-8599-11ed-a0db-5f97535421d3}
◊define-meta[tags]{Emacs}
◊define-meta[lang]{en}

◊meta{
  ◊title{Tree-sitter in Emacs 29 and Beyond}
}

Emacs’ release branch is now on complete feature freeze, meaning absolutely only bug fixes can happen on it. Now is a good time to talk about the state of ◊link["https://tree-sitter.github.io/tree-sitter/"]{tree-sitter} in Emacs: what do you get in Emacs 29, what you don’t, and what would happen going forward.

◊section{What’s in Emacs 29}

From a pure user’s perspective, Emacs 29 just adds some new built-in major modes which look more-or-less identical to the old ones. There aren’t any flashy cool features either. That sounds disappointing, but there are a lot of new stuff under the hood, a solid base upon which exciting things can emerge.

If Emacs 29 is built with the tree-sitter library, you have access to most of the functions in its C API, including creating parsers, parsing text, retrieving nodes from the parse tree, finding the parent/child/sibling node, pattern matching nodes with a DSL, etc. You also get a bunch of convenient functions built upon the primitive functions, like searching for a particular node in the parse tree, cherry picking nodes and building a sparse tree out of the parse tree, getting the node at point, etc. You can type ◊code{M-x shortdoc RET treesit RET} to view a list of tree-sitter functions. And because it’s Emacs, there is comprehensive manual coverage for everything you need to know. It’s in “Section 37, Parsing Program Source” of Emacs Lisp Reference Manual.

Emacs 29 has built-in tree-sitter major modes for C, C++, C#, Java, Rust, Go, Python, Javascript, Typescript, JSON, YAML, TOML, CSS, Bash, Dockerfile, CMake file. We tried to extend existing modes with tree-sitter at first but it didn’t work out too well, so now tree-sitter lives in separate major modes. The tree-sitter modes are usually called ◊code{xxx-ts-mode}, like ◊code{c-ts-mode} and ◊code{python-ts-mode}. The simplest way to enable them is to use ◊code{major-mode-remap-alist}. For example,

◊bcode{
  (add-to-list 'major-mode-remap-alist
               '(c-mode . c-ts-mode))
}

The built-in tree-sitter major modes have support for font-lock (syntax highlight), indentation, Imenu, which-func, and defun navigation.

For major mode developers, Emacs 29 includes integration for these features for tree-sitter, so major modes only need to supply language-specific information, and Emacs takes care of plugging tree-sitter into font-lock, indent, Imenu, etc.

◊subsection{Fontification}

In tree-sitter major modes, fontification is categorized into “features”, like “builtin”, “function”, “variable”, “operator”, etc. You can choose what “features” to enable for a mode. If you are feeling adventurous, it is also possible to add your own fontification rules.

To add/remove features for a major mode, use ◊code{treesit-font-lock-recompute-features} in its mode hook. For example,

◊bcode{
  (defun c-ts-mode-setup ()
    (treesit-font-lock-recompute-features
     '(function variable) '(definition)))

  (add-hook 'c-ts-mode-hook #'c-ts-mode-setup)
}

Features are grouped into decoration levels, right now there are 4 levels and the default level is 3. If you want to program in skittles, set ◊code{treesit-font-lock-level} to 4 ◊wink

◊subsection{Language grammars}

Tree-sitter major modes need corresponding langauge grammar to work. These grammars come in the form of dynamic libraries. Ideally the package manager will build them when building Emacs, like with any other dynamic libraries. But they can’t cover every language grammar out there, so you probably need to build them yourself from time to time. Emacs has a command for it: ◊code{treesit-install-language-grammar}. It asks you for the Git repository and other stuff and builds the dynamic library. Third-party major modes can instruct their users to add the recipe for building a language grammar like this:

◊bcode{
(add-to-list
 'treesit-language-source-alist
 '(python "https://github.com/tree-sitter/tree-sitter-python.git"))
}

Then typing ◊code{M-x treesit-install-language-grammar RET python} builds the language grammar without user-input.

◊subsection{Other stuff}

Things like indentation, Imenu, navigation, etc, should just work.

There is no code-folding, selection expansion, and structural navigation (except for defun) in Emacs 29. Folding and expansion should be trivial to implement in existing third-party packages. Structural navigation needs careful design and nontrivial changes to existing commands (ie, more work). So not in 29, unfortunately.

◊; ◊fndef["expand-region"]{I wrote a simple expansion package consisting of 400 LOC, among which 14 lines took care of tree-sitter: ◊link["https://github.com/casouri/lunarymacs/blob/438ad4cc053824122fe3275d1bdb603daf11ff16/site-lisp/expreg.el#L192"]{expreg.el}.}

◊section{Future plans}

◊subsection{Navigation}

The tree-sitter integration is far from complete. As mentioned earlier, structural navigation is still in the works. Right now Emacs allows you to define a “thing” by a regexp that matches node types, plus optionally a filter function that filters out nodes that matches the regexp but isn’t really the “thing”. Given the definition of a “thing”, Emacs has functions for finding the “things” around point (◊code{treesit--things-around}), finding the “thing” at point (◊code{treesit--thing-at-point}), and navigating around “things” (◊code{treesit--navigate-thing}). Besides moving around, these functions should be also useful for other things like folding blocks. Beware that, as the double dash suggests, these functions are experimental and could change.

I also have an idea for “abstract list elements”. Basically an abstract list element is anything repeatable in a grammar: defun, statement, arguments in argument list, etc. These things appear at every level of the grammar and seems like a very good unit for navigation.

◊subsection{Context extraction}

There is also potential for language-agnostic “context extraction” (for the lack of a better term) with tree-sitter. Right now we can get the name and span of the defun at point, but it doesn’t have to stop there, we can also get the parameter list, the type of the return value, the class/trait of the function, etc. Because it’s language agnostic, any tool using this feature will work on many languages all at once.

In fact, you can already extract useful things, to some degree, with the fontification queries written by major modes: using the query intended for the ◊code{variable} query, I can get all the variable nodes in a given range.

There are some unanswered questions though: (1) What would be the best function interface and data structure for such a feature? Should it use a plist like ◊code{(:name ... :params ...)}, or a cl-struct? (2) If a language is different enough from the “common pattern”, how useful does this feature remains? For example, there isn’t a clear parameter list in Haskell, and there could be several defun bodies that defines the same function. (3) Is this feature genuinely useful, or is it just something that looks cool? Only time and experiments can tell, I’m looking forward to see what people will do with tree-sitter in the wild ◊smile

◊subsection{Major mode fallback}

Right now there is no automatic falling back from tree-sitter major modes to “native” major modes when the tree-sitter library or language grammar is missing. Doing it right requires some change to the auto-mode facility. Hopefully we’ll see a good solution for it in Emacs 30. Right now, if you need automatic fallback, try something like this:

◊bcode{
(define-derived-mode python-auto-mode prog-mode "Python Auto"
  "Automatically decide which Python mode to use."
  (if (treesit-ready-p 'python t)
      (python-ts-mode)
    (python-mode)))
}

◊subsection{Other stuff}

Existing tree-sitter major modes are pretty basic and doesn’t have many bells and whistles, and I’m sure there are rough corners here and there. Of course, these things will improve over time.

Tree-sitter is very different and very new, and touches many parts of Emacs, so no one has experience with it and no one knows exactly how should it look like. Emacs 29 will give us valuable experience and feedback, and we can make it better and better in the future.

If you are interested, get involved! Read ◊link["note/2020/contributing-to-emacs/index.html"]{Contributing to Emacs} for some tips in getting involved with the Emacs development. Read ◊link["note/2023/tree-sitter-starter-guide/index.html"]{Tree-sitter Starter Guide} if you want to write a major mode using tree-sitter. And of course, docstrings and the manual is always your friend. If you have questions, you can ask on Reddit, or comment in this post’s public inbox (see the footer).

◊; ◊section{Realistic expectation of tree-sitter}

◊; The single most underrated benefit of tree-sitter is probably the productivity boost and the relief on maintenance burden. Anyone with average familiarity with Elisp and the target language can write a major mode in one or two nights. Compare that to the regex-based major modes, which takes months to write and maybe years iron out edge cases, and pages and pages of code to do things trivially done with tree-sitter.

◊; On the other hand, I don’t think tree-sitter’s “accuracy” is as ground breaking as many people seem to believe. “Regexp can only get you so far”, but it gets you pretty far. The real difference is, again, the productivity: you might be able to pull off a tricky syntax highlight with clever regexp and pages of code, but with tree-sitter it’s free—the parser has solved the problem and all you need is to ask for it. By extension, with tree-sitter, it is much less likely to produce some error than with regexp.

◊; And no, tree-sitter doesn’t solve C macros. A macro in C, by its nature, is an extension of the C grammar. ◊fnref["macro"]{It’s unreasonable to expect a parser to handle arbitrary macros.} At the end of the day you still need to use heuristics.

◊; Tree-sitter is very fast, but not orders of magnitude faster than regexp fontification, in my experience. Obliviously benchmarks vary across different modes, so take it with a grain of salt.

◊; From my (unscientific) ◊fnref["benchmark"]{benchmark}, scrolling in ◊code{c-ts-mode} is 1.5-2.5× faster than ◊code{c-mode}. But keep in mind that ◊code{c-mode} is often considered heavy and slow (partly because it does some degree of parsing). Parsing is indeed very fast in tree-sitter, especially when you only parse a little bit incrementally. But to do anything useful, the editor also needs to query the parse tree, which also takes some time, and ◊fnref["more-time"]{querying a larger buffer takes more time}.

◊; ◊fndef["benchmark"]{Some benchmark code from Eli and Alan, just for fun: ◊link["./bench.el"]{bench.el}.}

◊; ◊fndef["macro"]{Actually, Elixir’s grammar can support macros by homogenizing many constructs (not unlike Lisp). I persume it’s much harder for C to do but someone could give it a try.}

◊; ◊fndef["more-time"]{The root node gets larger as the buffer size increase, so it’ll take longer to query. But if you get a child node of the root that covers the region you want to fontify, the query time can generally remain constant. But it has complications: sometimes even the child gets larger along with the buffer, sometimes by querying the child you miss patterns that would be matched when querying the root.}

