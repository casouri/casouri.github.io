#lang pollen

◊define-meta[date]{<2026-03-04 Wed 20:45>}
◊define-meta[uuid]{1d9c6eba-184e-11f1-ba78-2313d5918627}
◊define-meta[tags]{Emacs}
◊define-meta[lang]{en}

◊meta{
  ◊title{Line-column tracking in Emacs for tree-sitter}
  ◊subtitle{◊om{ep1.5}, line-column tracking}
}

(This is a follow-up of  ◊link["/note/2025/emacs-tree-sitter-in-depth"]{◊em{In-depth Review of Emacs tree-sitter integration}}.)

As mentioned previously, Emacs doesn’t keep track of the line and column number in a buffer and passes dummy values to tree-sitter. This works fine most of the time, but breaks down when the grammar actually needs the column information for parsing, like ◊link["https://github.com/tree-sitter/tree-sitter/issues/4001"]{Haskell}.

So now we have to have line and column tracking in Emacs. Since Emacs uses gap buffer for storing text, there’s no efficient way to calculate the line and column number of arbitrary point, and we have to use cache. Fortunately cache works very well for text editors because text edits made by humans are predominately linear, local edits, not random access.

The idea is simple: since most edits must occur around point, we just cache the line and column number (henceforth linecol) of point. Whenever there’s an edit and we need the linecol for the beginning, old end, and new end of that edit, we can just scan from point to these positions and calculate the linecol of them by counting newlines. In reality, we need to cache the linecol of quite a few more positions, and be super careful about which cache is valid at what time. Nonetheless, in the end we were able to implement the tracking system on top of the existing delicate code synchronizing visible ranges, and the performance with linecol tracking is virtually the same as without it (in my crude benchmarks).

◊section{Data structures}

We cache linecol in both the buffer and each parser. In a buffer, we cache point and the beginning and end of the visible portion (henceforth ◊code{PT}, ◊code{BEGV}, ◊code{ZV}). In each parser, we cache the visible beginning and end of the parser (henceforth ◊code{visi_beg}, ◊code{visi_end}).

Caches are stored in a struct ◊code{ts_linecol}, which is passed around in various functions. Emacs has rich and complex definition of “column”, but here the column is what tree-sitter uses: byte offset from the beginning of the line.

◊bcode-hl['c]{
struct ts_linecol
{
  ptrdiff_t bytepos;  /* Byte position in buffer (1-based) */
  ptrdiff_t line;     /* Line number (1-based) */
  ptrdiff_t col;      /* Column in bytes (0-based from BOL) */
};
}

One thing of note is that there are two levels of invalidness involved. For a linecol, it becomes invalid when the ◊code{bytepos} doesn’t match ◊code{line} and ◊code{col} anymore, and it happens when a buffer edit occurs in front of it or contains it. It can also happen that the ◊code{bytepos} stored in the linecol doesn’t match the actual position of ◊code{BEGV}, ◊code{ZV}, etc; let’s call this kind of situation "misaligned". We’ll need this distinction later.

◊section{Scanning}

Calculating the linecol of a position is simple: we start from a known valid ◊code{ts_linecol} (or beginning of buffer), scan the text between it and the target position either forward or backward, and count the number of newlines in-between. If scanning-forward, remember the position of last newline to calculate the column number; if scanning backward, scan a bit further to find the next newline to calculate the column. (See ◊link["https://github.com/emacs-mirror/emacs/blob/6c79b6bede776d987a228f34a1a33f4fecd03845/src/treesit.c#L1081"]{◊code{treesit_linecol_of_pos}}.)

Handling all the edge cases correctly requires a bit of patience. Harder still is to find a way to use existing functions (◊code{display_count_lines}) to implement the scanning instead of writing a simple scanner to count newlines. I had to countermand a specific cases where ◊code{display_count_lines}’s behavior doesn’t match what we want (see ◊link["https://github.com/emacs-mirror/emacs/blob/6c79b6bede776d987a228f34a1a33f4fecd03845/src/treesit.c#L1030"]{◊code{treesit-count-lines}}). But who knows what crazy edge case would come up when scanning the buffer, I’d rather hide behind ◊code{display_count_lines} and bear with its quirks.

We also need a function that, given the old linecol of a position, calculate the new linecol of a position after a buffer change. I won’t go too deep into it right now, but enough to say that without it, we’d have to update the linecol of ◊code{visi_beg} and ◊code{visi_end} by scanning from point every time; that distance can be far, defeating the usefulness of the cache.

The trick is to realize that when you know the content of the change, you know how many newlines are added/removed. And you can simply add/subtract lines from the old linecol to get the new linecol, even if the position is far away from the change.

There’s also a small twist: instead of saving the text before and after change and scan through them, we use ◊code{treesit_linecol_of_pos} to calculate linecol of ◊code{start}, ◊code{old_end} and ◊code{new_end} of the change. The line and column change can be inferred from these linecols. And because it’s a common pattern for a cache to be both invalid and misaligned, we also add a ◊code{target_bytepos}: after we got the new linecol of the position (fixes invalidness), we scan to ◊code{target_bytepos} to get the final linecol (fixes misalignment).

◊bcode-hl['c]{
static struct ts_linecol
compute_new_linecol_by_change (struct ts_linecol pos_linecol,
                               struct ts_linecol start_linecol,
                               struct ts_linecol old_end_linecol,
                               struct ts_linecol new_end_linecol,
                               ptrdiff_t target_bytepos);
}

Here’s how it works: First of all, if the change is completely after the position, the linecol is unchanged. If the changed region is completely before the position, the line difference between the old linecol (◊code{pos}) and new linecol (◊code{pos'}) is the line difference between ◊code{old_end} (◊code{oe}) and ◊code{new_end} (◊code{ne}):

◊bcode{
     Insert:               Delete:

     |     |   |           |     |   |
     s     oe  pos         s     oe  pos

     |  |   |              |         |   |
     s  ne  pos'           s         ne  pos'
}

As for column, notice that the column of ◊code{pos'} is just the column of ◊code{new_end} plus the distance between ◊code{old_end} and ◊code{pos}:

◊bcode{
       Suppose # is text, | is cursor:

       ################
       ########|########|
	       oe        pos

       Now, if we insert something:

       ################
       ########|OOOOO
       OOOOOOOOOO|########|
                 ne       pos'
}

And the same goes for deletion.

Finally, if the changed region includes the position, we just give up and scan from either ◊code{start} or ◊code{new_end}, whichever is closer. Most changes in text editor are not very large, so this is fine in practice.

◊section{Tracking}

Remember the lifecycle of parser updates for tree-sitter in Emacs: for each user edit, we report the change to each parser but don’t reparse yet. Only when the user requests nodes from the parser do we sync up the visible beg and end of the parser with the visible portion of the buffer, and reparse.

For the purpose of tracking lincol, the work is also split in two. When recording user edits, we now need to first update the linecol of our cache, then compute the correct lincol for ◊code{start}, ◊code{old_end}, and ◊code{new_end} to send to the parser when reporting the change. We also take the opportunity to update cached lincol for ◊code{BEGV}, ◊code{ZV}, and ◊code{PT}, so their lincol are always valid. (But the lincols of buffer-level caches can get misaligned when we later use them, since other things can move them.)

Then, before reparse, we need to update ◊code{BEGV} and ◊code{ZV}’s linecol, and use them for computing the pseudo edits we use to sync the visible region of the parser and buffer.

◊subsection{During buffer change}

The most important thing to keep in mind is that, at each point in time, which linecols are valid.

Before the buffer change is applied, the linecol of ◊code{PT} is still valid (although we don’t know if it’s misaligned or not, and we don’t care). We use it to compute the linecol of ◊code{start} and ◊code{old_end} (by scanning from it). The linecol of ◊code{start} will remain valid after the change and will be our source of truth in the post-change world.

After the change is applied in the buffer, we compute the linecol of ◊code{new_end} by scanning from ◊code{start}. Now we have the linecol of ◊code{start}, ◊code{old_end}, and ◊code{new_end}, that we can use to convert any old linecol into a valid one by ◊code{compute_new_linecol_by_change}. We use them to update the linecol of ◊code{visi_beg} and ◊code{visi_end} of the parser.

For reporting the edit to the parser, because the parser’s visible region starts at ◊code{visi_beg}, we need to send it the delta between ◊code{old_end} and ◊code{old_visi_beg} rather than just ◊code{old_end}. Similarly we need to calculate the delta between ◊code{start} and ◊code{new_visi_beg}, and between ◊code{new_end} and ◊code{new_visi_beg}.  Note that ◊code{old_end} must use ◊code{old_visi_beg}, because ◊em{they} are in the same past buffer state.

Finally, we use the linecol of ◊code{start}, ◊code{old_end}, and ◊code{new_end} to update the linecol of ◊code{BEGV}, ◊code{ZV}, and simply set the linecol of ◊code{PT} to that of ◊code{new_end}. Maybe ◊code{PT}’s position doesn’t really match ◊code{new_end} (ie, misaligned), but that’s fine; we only care that ◊code{PT}’s linecol cache is valid and nearby when next change happens.

◊subsection{During reparse}

Remember that we need to send a series of pseudo edits to the parser to synchronize the visible region of the buffer and parser before we reparse. And each of the edits needs linecols paired with ◊code{start}, ◊code{old_end}, and ◊code{new_end}. The good news is, there’s no buffer change happening, so all the cached linecols are valid, and we can scan around to get new ones easily.

We first update the linecol of ◊code{BEGV} and ◊code{ZV} by scanning from the cached linecol. The parser’s ◊code{visi_beg} and ◊code{visi_end} are already valid and aligned. Then we just compute the ◊code{start}, ◊code{old_end}, and ◊code{new_end} and make the ◊om{3-step} edits. Finally we store the updated linecol of ◊code{visi_beg} and ◊code{visi_end} to the parser.

◊section{Epilogue}

There you have it. By exploiting human’s text editing pattern and careful cache management, we added linecol tracking to Emacs at almost no cost. I did some rough benchmarks and there’s virtually no difference between tracking/no-tracking. But in the good ol’ tradition of “you don’t pay for what you don’t use”, linecol tracking is only enabled if the parser opts into tracking linecol. And by default we only opt-in Haskell parsers. You can opt-in by adding the language to ◊code{treesit-languages-require-line-column-tracking}.
