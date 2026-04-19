#lang pollen

◊define-meta[date]{<2026-01-02 Fri>}
◊define-meta[uuid]{9b331150-e845-11f0-8893-87dc919a8b04}
◊define-meta[tags]{Emacs}
◊define-meta[lang]{en}

◊meta{
  ◊title{The wonderful and arcane world of paragraph filling in Emacs}
}

I consider paragraph-filling an essential functionality of any text editor. I use it dozens of times every single day, mostly for filling comment blocks. Emacs has it bound to ◊code{M-q}; Vim has the “reflow paragraph” command ◊code{gwip}. Newer editors don’t have this built-in (boo!) but all have some form of plugin that achieves the same thing.

On the surface it seems straightforward enough: given a long line, find places to insert newlines so each line is less than a certain length (say, 80 columns). But obviously it goes much deeper than that.

When picking breakpoint, there are some complications, and CJK characters require some special treatment too. Emacs also needs to support filling paragraphs with bullet points and C comment blocks:

◊bcode{
  - Note that the filling code must detect that
    the second lines should start with some whitespace
    so everything aligns
  - And the next bullet should start a new paragraph.
}

◊bcode{
  /**
   * Emacs also has to support the two hundred and
   * thirty one C block comment styles.
   */
}

Let’s start with the more easily solvable complications.

◊section{Inserting line-breaks}

There are characters that can’t be at the end or beginning of a line. For example, you don’t want to leave ◊codewb{(} at the end of a line, and similarly, ◊codewb{)} at the beginning of one. So Emacs has to have a way to know which characters can be at the beginning/end of a line. The information is stored in a charactor category char-table (available by calling ◊code{category-table}). If you press ◊code{C-u C-x =} on a opening parenthesis, you’ll see this line in the report:

◊bcode{
    category: .:Base, <:Not at eol, a:ASCII, l:Latin, r:Roman
}

The ◊codewb{<:Not at eol} part says this character shouldn’t be at end-of-line when filling. The ◊codewb{<} is a category name: ◊codewb{<} means can’t be at end-of-line, ◊codewb{>} means can’t be at beginning-of-line, and ◊codewb{|} means can insert a breakpoint (a newline). (See more in the ◊link["https://www.gnu.org/software/emacs/manual/html_node/elisp/Categories.html"]{Elisp manual}.)

Since CJK text don’t use spaces, Emacs can insert a newline between almost every character. But there are exceptions. For example, ◊codewb{《} can’t be at the end of a line, ◊codewb{》} can’t be at the end, and something like ◊codewb{きゃ} can’t be separated (so ◊codewb{ゃ} can’t be at the beginning of a line). There’s a dedicated file for these rules: ◊link["https://cgit.git.savannah.gnu.org/cgit/emacs.git/tree/lisp/international/kinsoku.el"]{◊code{kinsoku.el}}. ◊sc{iirc} this file is even loaded in the build process and included in the dumped temacs image.


Also, CJK characters (and some other characters) are usually wider than Latin characters, so they are considered two characters wide when filling. This information is stored in ◊code{char-width-table}. I think Emacs reads it from ◊link["https://www.unicode.org/reports/tr44/"]{Unicode character database}?

Fun fact: my first patch to Emacs C code is to  ◊link["https://cgit.git.savannah.gnu.org/cgit/emacs.git/commit/?id=0d1ca2ac3805443690f3bcb6877251d9b74902c9"]{improve the redisplay engine for CJK line wrapping}.

◊section{Adaptive fill}

Suppose Emacs needs to fill a C comment block:

◊bcode{
// A comment block made of two
// C comment line. 
}

It has to know that the “◊code{// }” part is a common prefix that appears at the beginning of every line, so the filling algorithm should ignore it, and if Emacs needs to create a new line, the prefix needs to be added.

When ◊code{adaptive-fill-mode} is on, Emacs tries to auto-detect the fill-prefix by looking at the first two lines of a paragraph. Emacs uses a regexp, ◊code{adaptive-fill-regexp}, to match the beginning of the first and second line, if the regexp has the same match on both lines, the matched text will be used as the fill-prefix. In our example above, the “◊code{// }” matches on both lines, so all subsequent lines will have that prefix.

If the match from second lines doesn’t exactly match the match from first line, Emacs checks if every non-whitespace character in the second-line-match is in the first-line-match in the same position. For example, given the start of a C comment block:

◊bcode{
  /* aaa
   * bbb
}

The first-line-match is ◊codewb{/* }, the second-line-match is ◊codewb{ * }. The only non-whitespace character in second-line-match is the ◊codewb{*}, and since there’s a ◊codewb{*} in first-line-match in the same position, the second-line-match is used as the fill-prefix.

Take another not very realistic example I created just to illustrate the point:

◊bcode{
  %%%%%% aaa
   % % % bbb
}

The first-line-match is ◊codewb{%%%%%% }, and the second-line-match is ◊codewb{ % % % }, since each ◊codewb{%} in the second line has a counterpart in the first line in the same position, the fill-prefix will be ◊codewb{ % % % }.

If the second-line-match is all whitespace, that also counts. So in the following example, the fill-prefix is two whitespaces:

◊bcode{
  - markdown bullet point
    paragraph
}

The default value of ◊code{adaptive-fill-regexp} is ◊codewb{[-–!|#%;>*·•‣⁃◦ \t]*}, it basically matches all sorts of possible bullet points plus whitespace.

Now, if the paragraph only has one line, how does Emacs determine the prefix? For example, which option should we choose when filling this:

◊bcode{
  - markdown bullet point paragraph

  ↓ 1. fills to

  - markdown bullet point
    paragraph

  ↓ 2. fills to

  - markdown bullet point
  - paragraph
}

In this case, it uses another regexp, ◊code{adaptive-fill-first-line-regexp}. If the first-line-match doesn’t match this regepx, the fill-prefix will just be whitespaces with the same length of first-line-match. By default, ◊code{adaptive-fill-first-line-regexp} only matches whitespaces, effectively forcing the fill-prefix to always be whitespaces. So by default, Emacs always goes with option ◊om{1}.

There are some details I omitted for clarity. Checkout the ◊link["https://www.gnu.org/software/emacs/manual/html_node/elisp/Adaptive-Fill.html"]{manual}, and ◊link["https://cgit.git.savannah.gnu.org/cgit/emacs.git/tree/lisp/textmodes/fill.el?h=be9371cde31fd2cc58f6469ac8cbaeb3cf31ebee#n248"]{source} for the details.

◊section{C block comments}

It would be so nice if the cute tricks above are enough for C block comments, but alas, no. ◊code{c-mode} and the newer ◊code{c-ts-mode} have to do significantly more work to make as many C block comment styles fill as human would expect.

Let’s first see some block comment styles that ◊code{c-ts-mode} supports:

◊bcode{
/* The most basic one,
   aaa aaa aaa */

/* Another popular one,
 * aaa aaa aaa aaa aaa
 * aaa aaa aaa aaa aaa
 */

/* Or like this
   aaa aaa aaa
   aaa aaa aaa
 */

/*
 * This has to be supported
 * too
 */

/**
 * jsdoc, etc, uses this
 * style
 */

/*====================
 * Now some old-school
 * ones
 ====================*/

/*********************
 * Or like this
 * aaa aaa aaa
 *********************/

/*-------------------*
 | Sadly we don’t    |
 | support this one  |
 *-------------------*/

/*
 | But something
 | like this works
  */
}

When I was implementing filling for ◊code{c-ts-mode}, I had to start over, because the implementation in ◊code{c-mode} uses ◊code{cc-mode}’s engine that ◊code{c-ts-mode} doesn’t have. Plus, a PhD is probably a minimum requirement to understand the filling function in ◊code{c-mode}. I studied ◊code{c-fill-paragraph} just enough to imidate something similar for ◊code{c-ts-mode} and swiftly got out of there.

My implementation consists of two parts, one is the filling function (yes, we need to customize the filling function a bit too), another is a function to return the fill-prefix. Let’s look at the filling function first, ◊code{c-ts-common--fill-block-comment}.

The function calls ◊code{fill-region} to do tha actual work, but does some magic before and after. First, if ◊codewb{/*} and ◊code{*/} are on their own line, they’re excluded from the filling, by setting the narrowing before calling the ◊code{fill-region}. This way they stay on their own line.

Next, we need to make sure if ◊codewb{*/} isn’t on it’s own line to begin with, it doesn’t end up on it’s own line after the filling. ◊sc{iow}, this doesn’t happen:

◊bcode{
  /* aaa aaa aaa
     aaa aaa aaa */

  ↓ shouldn’t fill to:
  
  /* aaa aaa aaa
     aaa aaa aaa
     */

  ↓ should fill to:

  /* aaa aaa aaa
     aaa aaa
     aaa */
}

Here, the ◊codewb{*/} is like a closing parenthesis—it should never be at the beginning of a line. If the line has to break there, take the previous word to be in front of it.

We archive this with a simple trick: before calling ◊code{fill-region}, we change mask the whitespaces in front of ◊codewb{*/} with some non-whitespace character, say, ◊codewb{x}; and after the filling, we change it back:

◊bcode{
  /* aaa aaa aaa
     aaa aaa aaa */

  ↓ mask

  /* aaa aaa aaa
     aaa aaa aaax*/

  ↓ fill
  
  /* aaa aaa aaa
     aaa aaa
     aaax*/

  ↓ unmask
  
  /* aaa aaa aaa
     aaa aaa
     aaa */
}

That’s about it for the filling function. For the fill-prefix function (◊code{c-ts-common--adaptive-fill-prefix}), I implemented it as a giant switch case. This is ◊sc{imo} simpler to read and reason about. Emacs calls this function (instead of matching with ◊code{adaptive-fill-regexp}) to get the match and goes on with the logic we explained earlier. That means our fill-prefix function needs to know which line it’s reading and return an appropriate result that, once fed into the logic, causes Emacs to produce the desired filling behavior.

Sounds complicated, but all we’re doing is to make sure the first and second line returns the same match so it’s used as the fill-prefix. In particular, on the first line, even if it matches ◊codewb{/* }, our prefix function returns ◊codewb{ * } or simply whitespaces. Circumventing that “doesn’t quite match but non-whitespace appears in first-line-match in the same position” logic makes the cases easier to reason about, and makes it easier for someone to understand without knowledge of the adaptive-prefix thing, hopefully.

◊; Note to self: I don’t think my understanding is right. So to future me: don’t post this unless you go back and figured it out.
