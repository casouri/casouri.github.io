#lang pollen

◊define-meta[date]{<2024-12-16 Mon 21:19>}
◊define-meta[uuid]{7ca5d6e8-6360-11ef-a7d1-9fc47809e9cf}
◊define-meta[tags]{Emacs}
◊define-meta[lang]{en}

◊meta{
  ◊title{Tree-sitter Updates in Emacs 30}
}

A year has past the release of Emacs ◊om{29}; last time we added support for tree-sitter and several tree-sitter-based major modes. This time, there are more major modes, better support for multi-language modes, and more utility features.

The first three sections introduces changes visible to end-users, the rest are for package and major mode developers.

◊section{Derived mode check}

Now ◊code{(derived-mode-p 'c-mode)} returns ◊code{t} even in ◊code{c-ts-mode} (and similarly for other builtin tree-sitter modes). That means ◊code{.dir-locals.el} settings and yasnippets for ◊code{c-mode} will work for ◊code{c-ts-mode} too. However, ◊code{c-ts-mode} still doesn’t run ◊code{c-mode}’s major mode hooks. Also, there’s still no major mode fallback. But I think that can be solved by packages like ◊link["https://github.com/renzmann/treesit-auto"]{treesit-auto}.

This new inheritance doesn’t come automatically, someone needs to use ◊code{derived-mode-add-parents} to add the relationship.

◊section{New major modes}

There are some new built-in major modes: Elixir and ◊sc{hee}x mode, ◊sc{html} mode, Lua mode, ◊sc{php} mode with ◊sc{phpd}oc support, and Doxygen support for C/C++/Java mode. Kudos to Wilhelm for writing Elixir and ◊sc{hee}x mode, John for writing Lua mode, and Vincenzo for writing ◊sc{php} mode and Doxygen support! ◊sc{hee}x mode and ◊sc{php} mode really shows the potential of tree-sitter: without tree-sitter, it would take a lot of work to write a major mode for mixed languages like these; now tree-sitter takes care of all the hard work, and we can focus on writing the things we care about: font-lock and indentation rules, utility commands, etc.

When Wilhelm and Vincenzo were implementing multi-language major modes, they found bugs and missing features in Emacs, and provided invaluable feedback on emacs-devel and bug tracker. Their feedback and requests allow us to improve Emacs’ support for multi-languages. So if you’re writing a major mode or some package with tree-sitter and run into issues, don’t hesitate to reach out on emacs-devel or the bug tracker!

◊section{Sexp movement}

I’ll explain it a bit more in the next section, but the gist is that ◊code{forward-sexp} and ◊code{backward-sexp} can now use the parse tree for navigation, as long as the major mode adds support for them. Users can also change what’s considered a ◊code{sexp} (A statement? An expression? Or any node in the parse tree?) themselves, overriding the major mode’s setting.

◊section{Defining things}

Sections below are mostly for developers.

In the spirt of ◊code{thing-at-point}, a major mode or user can now define tree-sitter ◊em{things}: ◊code{defun}, ◊code{sexp}, ◊code{sentence}, ◊code{comment}, ◊code{text}, ◊code{block}, ◊code{args}, etc. The definition is flexible: it can be a regexp matching node names, or a predicate function, or a regexp plus a predicate. It can also be defined with logical operands ◊code{not} and ◊code{or}, like ◊code{(not sexp)}, or ◊code{(not "comment")}, ◊code{(or comment text)}.

Tree-sitter things are supported in every tree-sitter function . Once the major mode defines it, everyone can use it. Here are some things you can do with it:

Get the sexp at point ◊fnref["in-any-mode"]{in any tree-sitter major mode}: ◊code{(treesit-thing-at-point 'sexp)}. Get the sexp before point: ◊code{(treesit-thing-prev (point) 'sexp)}.

◊fndef["in-any-mode"]{Provided that the mode defined the thing.}

Generate a tree of all the defuns in a buffer:

◊bcode{
(treesit-induce-sparse-tree
 (treesit-buffer-root-node)
 'defun)
}

Traverse things:
◊ul{
  ◊li{◊code{treesit-beginning-of-thing}}
  ◊li{◊code{treesit-end-of-thing}}
  ◊li{◊code{treesit-navigate-things}}
}

I can also see packages reserving a particular thing, and have major modes add definition for that thing. In that case, it’s best to add the package prefix to avoid naming conflict.

At the moment, the following “standard” things are in use:
◊ul{
  ◊li{◊code{sexp}: Used by ◊code{forward-sexp}, etc.}
  ◊li{◊code{defun}: Used by ◊code{end-of-defun}, etc.}
  ◊li{◊code{sentence}: Used by ◊code{forward-sentence}. In imperative languages, it can be a statement.}
  ◊li{◊code{comment}: All types of comments.}
  ◊li{◊code{string}: All types of strings.}
  ◊li{◊code{text}: Any non-code. Comments, strings, and text in languages ◊sc{html} and jax.}
}

Like font-lock features, we’re starting with a basic list; if you have suggestions fore more things (perhaps you wrote a package that uses a thing that major modes should support), reach out on emacs-devel or debbugs.

◊section{Local parsers}

◊; Emacs 29 already came with support for mixing several languages in the same mode. But it wasn’t very well tested. During the year after, we had more experience with it and fixed many bugs; now multi-lang major modes like ◊sc{heex-ts-mode} and ◊code{php-ts-mode} run pretty smoothly.

Normally, even for the embedded language, there’s only one parser for that language in a buffer. Each individual embedded code block are “stitched together” and is parsed as a whole by that parser. The pro is we only need to create one parser, the cons are error in one code block might affect other code blocks, and sometimes it doesn’t even make sense to stitch multiple code blocks together.

That’s why we added local parsers. Each local parser is confined to a single code block. Emacs creates and manages parsers for each embedded code block automatically. ◊sc{phpd}oc and Doxygen support are possible thanks to local parsers. To use local parsers, just add the ◊code{:local t} flag in ◊code{treesit-range-rules}, Emacs handles the rest.

◊section{Other changes}

A small convenience improvement: ◊code{treesit-font-lock-rules} now supports the ◊code{:default-language} keyword, so major mode author don’t need to write ◊code{:language 'xxx} for every query anymore.

Each parser in the parser list now has a tag. By default, a parser has the ◊code{nil} tag, and ◊code{(treesit-parser-list)} returns all the parsers with ◊code{nil} tag (because the third optional argument ◊code{TAG} defaults to ◊code{nil}). That means if you don’t explicitly set a tag when creating a parser, it’ll show up when anyone calls ◊code{(treesit-parser-list)}. On the other hand, you can create a parser that doesn’t show up in the parser list if you give it a non-nil tag. The intended use-case is to create special purpose parsers that shouldn’t normally appear in the parser list.

Local parsers has the ◊code{embedded} tag, so they don’t appear in the parser list. You can get them by passing ◊code{embedded} to the ◊code{TAG} argument, or by passing the special value ◊code{t} to the ◊code{TAG} argument, which means return all parsers regardless of their tag.

There’s a new variable ◊code{treesit-language-remap-alist}. If a language A is mapped to another language B in this alist. Creating a parser of A actually uses the grammar of B. If someone wants to write a major mode for tree-sitter-cuda, which extends upon tree-sitter-cpp, they can map ◊code{cpp} to ◊code{cuda}, so the font-lock rules and indentation rules defined in ◊code{c++-ts-mode} can be borrowed to cuda mode verbatim.

Indirect buffers now gets individual parser lists. In Emacs 29, the origin buffer and all its indirect buffers share the same parser list. Now they each have their own parser list.

◊section{Better filling for C-style comment blocks}

This is not directly related to tree-sitter but it affects tree-sitter modes for all C-like languages. You see, all these tree-sitter major modes (C, C++, Java, Rust, Javascript, Typescript) uses C-style comment blocks, and they all use ◊code{c-ts-common.el} for things like filling the comment block, or setting up ◊code{comment-start}, etc.

Traditionally these kind of major modes use cc-mode’s utilities, but cc-mode is a beast on its own, and it’s not worth it to add that dependency for filling a comment block. (It’s not just code dependency, but also cc-mode’s own parsering facility, data structure, etc.)

Filling C-style comment block is harder than one might imagine. It’s quite involved and interesting, and worth a separate article on its own. Suffice to say that the filling logic is improved and works on even more styles of C comment blocks now. Below are a few among the ones that we support.

◊bcode{
  /* xxx  /**    /* xxx     
   * xxx   * xxx    xxx      
   */      */       xxx */

  /*======  /*    
   * xxx     | xxx
   *======/  */
}

And it goes beyond just filling, when you type return in a comment block, you expect the next line to be prefixed with some character (◊code{*} or ◊code{|} or space) and indented to the right place. Making that work for all those styles on top of the filling and keeping the code reasonably readable is a small miracle ◊smile{}

◊section{Primary parser}

If you are the author of a tree-sitter major mode, make sure to set ◊code{treesit-primary-parser} in your major mode if it has multiple languages! This is a new variable added in Emacs ◊om{30}, and setting it is vital for font-lock update to work properly in complex situations. Emacs makes a reasonable guess when the major mode doesn’t set it themselves (it sets the first parser in the parser list as the primary parser). But this guess doesn’t work reliably for multi-language major modes.

Besides Emacs itself, other packages can also make use of this variable. It’ll be better than ◊code{(car (treesit-parser-list))}, especially in multi-language modes.

Having an explicit primary parser allows Emacs to update the “changed region” after each buffer change correctly, especially for multi-language modes. For example, when the user types the closing block comment delimiter ◊code{*/}, not only does Emacs fontify the ◊code{*/} itself, it also needs to re-fontify the whole block comment, which previously weren’t fontified in comment face due to incomplete parse tree. You can read more about it in ◊code{treesit--font-lock-mark-ranges-to-fontify} and ◊code{treesit--pre-redisplay}.

◊section{Ready your major mode for Emacs 30}

Here’s a check list:

◊ol{
  ◊li{Define ◊code{treesit-primary-parser}.}
  ◊li{Define things in ◊code{treesit-thing-settings}, especially ◊code{sexp}.}
}

Err, that list is shorter than I thought. But I do have some more words for ◊code{sexp} thing.

Personally I use ◊code{forward/backward-sexp} commands for going over chunks of code—I set the mark, and hit C-M-f a few times to select several statements/expression/etc. So I found that defining ◊code{sexp} as “anything that’s not a trivial thing like comma or bracket” works pretty well. For example, this is the definition for ◊code{sexp} in ◊code{c-ts-mode}:

◊bcode{
  (not ,(rx (or "{" "}" "[" "]" "(" ")" ",")))
}

This way, if point is at the beginning of any thing, C-M-f will bring me to the end of that thing, be it an expression, statement, function, block, or whatever. I use it all the time and it’s very handy.

Some people expects a more ◊code{forward-word}-like behavior for sexp movement commands. For that kind of movement, major mode might need to carefully select the nodes that counts as a sexp and what doesn’t.

I don’t recommend the second approach because it’s time-consuming to define and will inevitably still produce unintuitive movements from time to time. Not worth it ◊sc{imho}. Also, we already have ◊code{forward/backward-word} for smaller movements, so sexp commands should be used for larger movements.

There’s some ongoing development by Juri on mixing the default ◊code{forward-sexp} function and tree-sitter’s. I’m looking forward to see what will come out of it.

◊section{Emacs 31}

Emacs has been making good progress in tree-sitter. At this point we have pretty good support for writing major modes with tree-sitter; I already see many tree-sitter major modes and packages out there. And we’ll continue making it easier to write major modes with tree-sitter. For example, we’ll add a baseline indentation rule, so major mode authors need to write less indentation rules.

We’ll also improve documentation on customizing a tree-sitter major mode, like adding font-lock rules and indentation rules. I might write a guide, or add a section in the manual, I’m not sure yet. Also, borrowing font-lock/indentation from anoter mode also needs to be easier.

There are still some unsolved issues. The lack of versioning for language grammars breaks major modes from time to time; installing tree-sitter grammar is not very easy; tree-sitter library still has bugs that results in incorrect parse tree or even causes Emacs to hang. These will be resolved, albeit slowly.

That’s about it! Stay tuned for the next update for Emacs ◊om{31}, and feel free to reach out in the meantime!