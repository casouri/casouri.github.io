#lang pollen

◊define-meta[date]{<2021-08-06 Fri 10:13>}
◊define-meta[uuid]{79af8cc8-f6c0-11eb-a83c-5b2a03ac1c87}
◊define-meta[tags]{Emacs}
◊define-meta[lang]{en}

◊meta{
  ◊title{List Unsaved Buffers Before Exiting Emacs}
}

When you hit ◊code{C-x C-c} to exit Emacs but there are unsaved buffers, Emacs asks you whether to save each buffer. The problem is, more often than not, I don’t remember why there is an outstanding change, and all I can do is to nervously say “yes”, hoping that it isn’t a typo I accidentally typed into that buffer.

Wouldn’t it be nice if Emacs can show me a list of unsaved buffers, and let me examine them and save/kill them? The function below does just that: bind it to ◊code{C-x C-c} and you will always exit Emacs with peace in your mind.

◊bcode{
(defun clean-exit ()
  "Exit Emacs cleanly.
If there are unsaved buffer, pop up a list for them to be saved
before existing. Replaces ‘save-buffers-kill-terminal’."
  (interactive)
  (if (frame-parameter nil 'client)
      (server-save-buffers-kill-terminal arg)
    (if-let ((buf-list (seq-filter (lambda (buf)
                                     (and (buffer-modified-p buf)
                                          (buffer-file-name buf)))
                                   (buffer-list))))
        (progn
          (pop-to-buffer (list-buffers-noselect t buf-list))
          (message "s to save, C-k to kill, x to execute"))
      (save-buffers-kill-emacs))))
}

