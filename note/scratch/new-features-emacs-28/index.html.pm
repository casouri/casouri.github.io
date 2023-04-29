#lang pollen

◊define-meta[date]{<2021-12-30 Thu>}
◊define-meta[uuid]{4343b542-6948-11ec-89c8-0f8360c29a33}
◊define-meta[tags]{Emacs}
◊define-meta[lang]{en}

◊meta{
  ◊title{New Features in Emacs 28 that You Don’t Know About}
}

◊section{Better word wrapping for mixed CJK and Latin text}

Tldr: set ◊code{word-wrap-by-category} to ◊code{t}.

Here is a piece of text mixed with CJK and Latin scripts, word-wrapped by Emacs. You will notice something strange: Emacs breaks off the newline right after the word “word-wrap” (marked by the cursor). That is way too early. Instead, it should have broken off the new line like shown in the second screenshot. The difference between the to screenshot is that I set ◊code{word-wrap-by-category}, a new variable in Emacs 28, to ◊code{t} in the second screenshot. (Of course, both screenshot have ◊code{visual-line-mode} on, otherwise Emacs doesn’t word-wrap.)

◊fig{
  ◊image["./bad.png"]{A piece of text mixing CJK and Latin scripts}
  ◊figcap{The line break at the second line is is too early}
}

◊fig{
  ◊image["./good.png"]{A piece of text mixing CJK and Latin scripts}
  ◊figcap{This shows the correct (better) line break}
}

Before Emacs 28, Emacs word-wraps by simply looking for space characters. In out text, the only space character is the one after “word-wrap”, so Emacs breaks the line there. In Emacs 28, if ◊code{word-wrap-by-category} is non-nil, Emacs knows it can break line after CJK characters.

That’s not all, in CJK languages, certain punctuation are not to be at the end of a line, and some are not to be at the start. Opening punctuation marks like （“《 shouldn't be left at the end of a line, closing marks like 》”） shouldn’t be at the start. Someone call these widow/orphans glyph.

When ◊code{word-wrap-by-category} is non-nil, Emacs is also smart enough to avoid these widow and orphan glyph.

◊fig{
  ◊image["./orphan.png"]{A piece of text that avoids an orphan glyph}
  ◊figcap{Emacs avoided the orphan glyph by moving the 《 mark to the next line}
}

◊section{Custom window layout in GDB}

Emacs provides a multi-window UI for interacting with GDB: set ◊code{gdb-many-windows} to ◊code{t} and type ◊code{M-x gdb RET}. The layout of that multi-window interface is fixed before Emacs 28: there is no way of telling Emacs to remember moving the register window to the left every time you open GDB. In Emacs 28, there is now ◊code{gdb-save-window-configuration}, which saves the current window layout (window configuration) of the GDB interface to a file, and ◊code{gdb-load-window-configuration}, which loads the window layout.

Here is how to use them: open GDB, move windows around as you like, and call ◊code{gdb-save-window-configuration} to save it to a file. Next, set ◊code{gdb-default-window-configuration-file} to your file. Now every time you open GDB, your custom window layout is used.
