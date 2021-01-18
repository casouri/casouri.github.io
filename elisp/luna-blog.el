;;; luna-bblog.el --- Config for each blog site      -*- lexical-binding: t; -*-

;; Author: Yuan Fu <casouri@gmail.com>

;;; This file is NOT part of GNU Emacs

;;; Commentary:
;;

;;; Code:
;;

(require 'luna-publish)
(require 'cl-lib)
(require 'subr-x)

;;; Backports

(defun luna-f-list-directory (dir &optional full)
  "Return a list of directories in DIR.
Return full path if FULL is non-nil."
  (let ((default-directory dir))
    (seq-filter #'file-directory-p
                (directory-files
                 dir full directory-files-no-dot-files-regexp))))

(defun luna-f-directory-files (dir &optional full)
  "Return a list of regular files in DIR.
Return full path if FULL is non-nil."
  (let ((default-directory dir))
    (seq-filter #'file-regular-p
                (directory-files
                 dir full directory-files-no-dot-files-regexp))))

;;; Common

(defvar luna-blog-root (expand-file-name "~/p/casouri/")
  "Make sure there is a final slash.")

(defvar luna-blog-url "https://archive.casouri.cat/"
  "Make sure there is a final slash.")

(defvar luna-blog-note-info
  `(:blog-site-base
    ,(concat luna-blog-root "note/")
    :blog-site-root ,luna-blog-root
    :blog-url-base ,(concat luna-blog-url "note/")
    :blog-url-root ,luna-blog-url
    :blog-rss-title "Notes"
    :blog-rss-desc "RSS feed for my notes"
    :blog-link-source "https://github.com/casouri/casouri.github.io"
    :blog-link-license "https://creativecommons.org/licenses/by-sa/4.0/"
    :blog-dir-list-fn
    (lambda (info)
      (let (dir-list)
        (dolist (year-dir (luna-f-list-directory
                           (plist-get info :blog-site-base) t))
          (dolist (post-dir (luna-f-list-directory year-dir t))
            (push post-dir dir-list)))
        dir-list))))

(defvar luna-blog-rock-info
  `(:blog-site-base
    ,(concat luna-blog-root "rock/day/")
    :blog-site-root ,luna-blog-root
    :blog-url-base ,(concat luna-blog-url "rock/day/")
    :blog-url-root ,luna-blog-url
    :blog-rss-title "余日摇滚"
    :blog-rss-desc "音乐推荐☆DAZE☆"
    :blog-link-source "https://github.com/casouri/casouri.github.io"
    :blog-link-license "https://creativecommons.org/licenses/by-sa/4.0/"
    :blog-dir-list-fn
    (lambda (info)
      (directory-files (plist-get info :blog-site-base) t
                       "day-"))
    :blog-preprocess luna-blog-rock-distribute))

(defvar luna-blog-kitchen-info
  `(:blog-site-base
    ,(concat luna-blog-root "kitchen/")
    :blog-url-base ,(concat luna-blog-url "kitchen/")
    :blog-site-root ,luna-blog-root
    :blog-url-root ,luna-blog-url
    :blog-link-source "https://github.com/casouri/casouri.github.io"
    :blog-link-license "https://creativecommons.org/licenses/by-sa/4.0/"
    :blog-dir-list-fn
    (lambda (info)
      (let (dir-list)
        (dolist (year-dir (luna-f-list-directory
                           (plist-get info :blog-site-base) t))
          (dolist (post-dir (luna-f-list-directory year-dir t))
            (push post-dir dir-list)))
        dir-list))))

(setq luna-publish-project-alist
      `((note . ,luna-blog-note-info)
        (rock . ,luna-blog-rock-info)
        (kitchen . ,luna-blog-kitchen-info)))

;;; Notes

;;;; Macros

(defun luna-note-export-headers ()
  "Generate org headers for each post.
This is used as a local macro in index page of the note blog."
  (let ((header-list (luna-publish-post-info-list luna-blog-note-info)))
    ;; In each header we have (:title :date :tags :path).
    ;; Now we have a list of document-info, format them into headers.
    (substring-no-properties
     (string-join
      (mapcar
       (lambda (header)
         (let* ((abs-path (plist-get header :path))
                (relative-path (file-relative-name
                                abs-path
                                (plist-get luna-blog-note-info
                                           :blog-site-base))))
           (format "* [[./%s][%s]] %s\n\n"
                   (concat relative-path "/index.html")
                   (plist-get header :title)
                   (plist-get header :tags))))
       header-list)))))

;;;; Commands

(defun luna-new-note-blog (dir-name)
  "Make a new blog post with DIR-NAME."
  (interactive "MDirectory name: ")
  (let* ((year (substring (current-time-string) 20))
         (year-path (expand-file-name
                     year (plist-get luna-blog-note-info
                                     :blog-site-base)))
         (dir-path (expand-file-name dir-name year-path))
         (file-path (expand-file-name "index.org" dir-path)))
    ;; Create the post’s dir and org file and insert basic information.
    (unless (file-exists-p year-path)
      (mkdir year-path))
    (mkdir dir-path)
    (find-file file-path)
    (insert "#+SETUPFILE: ../../setup.org
#+TITLE:
#+DATE:
#+TAGS:
")
    (save-buffer)))

;;; Rock/day

;;;; Macros

(defun luna-blog-rock-day-count ()
  (let* ((site-base (plist-get luna-blog-rock-info :blog-site-base)))
    (length (luna-f-directory-files (expand-file-name "src" site-base)))))

(defun luna-blog-rock-this-day ()
  "Get the number of day of this buffer file.
The `default-directory' should be `day-xxx'."
  (let ((text (file-name-base (directory-file-name default-directory))))
    (string-match "^day-\\(.+\\)$" text)
    (string-to-number (match-string 1 text))))

(defun luna-blog-rock-generate-titles ()
  "Generate titles for index page."
  (let* ((days-count (luna-blog-rock-day-count)))
    (string-join
     (cl-loop for day-idx downfrom days-count to 1
              collect (format "* [[./day-%d/index.html][%d]]"
                              day-idx day-idx))
     "\n")))

;;;; Commands

(defun luna-blog-rock-distribute (info)
  "Distribute files src/day-x.org to day-x/index.org.
INFO is not used."
  (ignore info)
  (let* ((site-base (plist-get luna-blog-rock-info :blog-site-base))
         (days-count (luna-blog-rock-day-count)))
    (cl-loop
     for day-idx from 1 to days-count
     do (let* ((source (expand-file-name
                        (format "src/day-%d.org" day-idx)
                        site-base))
               (dest-dir (expand-file-name (format "day-%d" day-idx)
                                           site-base))
               (dest-path (expand-file-name "index.org" dest-dir)))
          ;; Copy src/day-x.org to day-x/index.org.
          (unless (file-exists-p dest-dir)
            (mkdir dest-dir))
          (when (file-newer-than-file-p source dest-path)
            (with-temp-buffer
              (insert-file-contents source)
              (write-file dest-path)))))))

(defun luna-new-rock ()
  "Make a new blog post of rock/day of DAY."
  (interactive)
  (let ((site-base (plist-get luna-blog-rock-info :blog-site-base))
        (day (1+ (luna-blog-rock-day-count))))
    (mkdir (expand-file-name (format "day-%d" day) site-base))
    (find-file (expand-file-name (format "src/day-%d.org" day) site-base))
    (goto-char (point-min))
    (insert "#+SETUPFILE: ../setup.org
#+TITLE: {{{day_title}}}
#+DATE:

{{{day_link}}}

{{{img()}}}

* - *

{{{BEGIN_LYRICS}}}
{{{END_LYRICS}}}
")
    (save-buffer)))

(defun luna-open-album-dir ()
  "Open ~/p/casouri/rock/day/album/."
  (interactive)
  (shell-command-to-string (format "open ~/p/casouri/rock/day/album/")))

(defun luna-blog-rock-add-line-break (beg end)
  "Add line breaks after each line in region between BEG and END."
  (interactive "r")
  (replace-regexp "$" " \\\\\\\\" nil beg end))

(defun luna-blog-rock-convert ()
  (interactive)
  (search-forward "#+BEGIN_EXAMPLE")
  (replace-match "{{{begin_lyrics}}}")
  (setq beg (1+ (match-end 0)))
  (search-forward "#+END_EXAMPLE")
  (setq end (match-beginning 0))
  (replace-match "{{{end_lyrics}}}")
  (luna-blog-rock-add-line-break beg end))

(defun luna-insert-album ()
  "Insert a album image name."
  (interactive)
  (insert (completing-read
           "Album: "
           (luna-f-directory-files
            (expand-file-name
             "album"
             (plist-get luna-blog-rock-info
                        :blog-site-base))))))

;;; Kitchen

(defun luna-new-kitchen-blog (type-name dir-name)
  "Make a new blog post in TYPE-NAME/DIR-NAME."
  (interactive (list (completing-read
                      "Type: " (luna-f-list-directory
                                (plist-get luna-blog-kitchen-info
                                           :blog-site-base)))
                     (read-string "Directory: ")))
  (let* ((site-base (plist-get luna-blog-kitchen-info :blog-site-base))
         (type-path (expand-file-name type-name site-base))
         (dir-path (expand-file-name dir-name type-path))
         (file-path (expand-file-name "index.org" dir-path)))
    ;; Create the post’s dir and org file and insert basic information.
    (unless (file-exists-p type-path)
      (mkdir type-path))
    (mkdir dir-path)
    (find-file file-path)
    (insert "#+SETUPFILE: ../../setup.org
#+TITLE:
#+DATE:
#+TAGS:
")
    (save-buffer)))

(provide 'luna-blog)

;;; luna-blog.el ends here
