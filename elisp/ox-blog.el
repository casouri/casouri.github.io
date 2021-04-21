;;; ox-blog.el --- Export backend for my blog      -*- lexical-binding: t; -*-

;; Author: Yuan Fu <casouri@gmail.com>

;;; This file is NOT part of GNU Emacs

;;; Commentary:

;;; Code:


(require 'ox-cjk-html)
(require 'subr-x)

(defun org-blog--parent (path)
  "Return the parent of PATH. PATH has to be absolute."
  (file-name-directory (directory-file-name path)))

;;; Like button

(defun org-blog-like-button (info)
  "Generate a like button.
INFO is the information channel."
  (let ((path (plist-get info :output-file)))
    (format "<div class=\"like-button\">
<form action=\"/like\" method=\"post\">
<input type=\"text\" name=\"path\" hidden value=\"/%s\" />
<button class=\"like\" type=\"submit\">❤ Like</button>
</form>
</div>"
            (file-relative-name
             path (plist-get info :blog-site-root)))))

;;; Postamble & preamble

(defun org-blog-postamble (info)
  "Generate a postamble.
INFO is the information channel."
  (let* ((spec (org-html-format-spec info))
         (author (cdr (assq ?a spec)))
         (date (cdr (assq ?d spec)))
         (modified-date (cdr (assq ?C spec))))
    (concat (if (plist-get info :blog-like-button)
                (org-blog-like-button info)
              "")
            (format "<div>
<p class=\"author\">Written by %s</p>
<p class=\"first-publish\">First Published in %s</p>
<p class-\"last-modified\">Last modified in %s</p>
<p>Send your comment to 
<a href=\"mailto:archive.casouri.cat@gmail.com\">
archive.casouri.cat@gmail.com</a></p>
</div>"
                    author date modified-date))))

(defun org-blog-breadcrumb (info)
  "Generate a breadcrumb for the current post.
INFO is the information channel."
  (let* ((file (plist-get info :input-file))
         (current-dir (org-blog--parent (file-name-directory file)))
         (root-dir (plist-get info :blog-site-root))
         (relative-path "../index.html")
         level-list)
    (while (and (not (< (length (directory-file-name current-dir))
                        (length (directory-file-name root-dir))))
                (not (equal current-dir "/")))
      ;; When there is an index.org in the parent directory and
      ;; it has a title...
      (when-let ((index (expand-file-name "index.org" current-dir))
                 (title (when (file-exists-p index)
                          (substring-no-properties
                           (with-temp-buffer
                             (insert-file-contents index)
                             (car (plist-get (org-export-get-environment)
                                             :title)))))))
        (push (format "<a href=\"%s\">%s</a><span> / </span> "
                      relative-path title)
              level-list))
      (setq relative-path (concat "../" relative-path)
            current-dir (org-blog--parent current-dir)))
    (concat "<div class=\"org-breadcrumb\">\n"
            (string-join level-list)
            "\n</div>")))

(defun org-blog-preamble (info)
  "Generate the UP|HOME    RSS|Source|License line.
INFO is the information channel."
  (let ((rss (plist-get info :blog-link-rss))
        (source (plist-get info :blog-link-source))
        (license (plist-get info :blog-link-license)))
    (concat
     "<div class=\"org-page-header\">\n"
     (org-blog-breadcrumb info)
     "\n"
     (concat "<div class=\"org-meta-header\">\n"
             (string-join
              (remove
               nil
               (list
                (when rss
                  (format "<a href=\"%s\">RSS</a>" rss))
                (when source
                  (format "<a href=\"%s\">Source</a>" source))
                (when (and source license)
                  (format "<a href=\"%s\">License</a>" license))))
              "<span> | </span>")
             "</div>")
     "\n</div>\n")))

;;; Headline w/ readble anchor

(defun org-blog-headline (headline contents info)
  "Export HEADLINE when human-readable anchor.
CONTENTS is the content under the headerline, INFO is the
information channel."
  (let* ((text (org-export-data
                (org-element-property :title headline) info))
         (id (url-encode-url (replace-regexp-in-string
                              " " "-" text)))
         (headline (org-element-put-property headline :CUSTOM_ID id)))
    (org-html-headline headline contents info)))

;;; Links

(defun org-blog-link (link desc info)
  "Normalize LINK before generating HTML.
DESC is the description. INFO is the information channel."
  (let ((path (org-element-property :path link)))
    (setq link (org-element-put-property
                link :path (ucs-normalize-NFC-string path)))
    (org-html-link link desc info)))

;;; TOC

(defun org-blog-inner-template (contents info)
  "Return body of document string after HTML conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (let ((toc (when-let ((depth (plist-get info :with-toc)))
               (org-html-toc depth info))))
    (concat
     (if (string-match-p (regexp-quote "{{TOC}}") contents)
         (string-replace "{{TOC}}" toc contents)
       (concat toc contents))
     ;; Footnotes section.
     (org-html-footnote-section info))))


;;; Quotes

(defun org-blog-paragraph (paragraph contents info)
  "Add span tags to full-width quotes."
  (let ((contents
         (with-temp-buffer
           (insert contents)
           (goto-char (point-min))
           (while (re-search-forward "[‘’“”]" nil t)
             (if (or (memq (aref char-script-table (char-after))
                           '(han cjk-misc))
                     (memq (aref char-script-table
                                 (char-before (1- (point))))
                           '(han cjk-misc)))
                 (replace-match
                  (concat "<span class=\"full-width-quote\">"
                          (match-string 0)
                          "</span>"))))
           (buffer-string))))
    (org-html-paragraph paragraph contents info)))

;;; Post

(org-export-define-derived-backend 'post 'cjk-html
  ;; Overwrite html backend defaults.
  :options-alist '((:html-postamble nil nil 'org-blog-postamble t)
                   (:html-preamble nil nil 'org-blog-preamble)
                   (:html-head-include-scripts nil "html-scripts" nil)
                   (:html-head-include-default-style nil "html-style" nil)
                   (:html-html5-fancy nil nil t)
                   ;; Blog custom options.
                   (:blog-link-rss "BLOG_LINK_RSS" nil nil)
                   (:blog-link-source "BLOG_LINK_SOURCE" nil nil)
                   (:blog-link-license "BLOG_LINK_LICENSE" nil nil)
                   (:blog-like-button "BLOG_LIKE_BUTTON" nil t nil)
                   ;; Blog publish options.
                   (:blog-site-root "BLOG_SITE_ROOT" nil nil)
                   (:blog-site-base "BLOG_SITE_BASE" nil nil)
                   (:blog-url-root "BLOG_URL_ROOT" nil nil)
                   (:blog-url-base "BLOG_URL_BASE" nil nil))
  :menu-entry '(?p "Export to blog post"
                   ((?h "As HTML file" org-blog-export-to-post)))
  :translate-alist '((headline . org-blog-headline)
                     (link . org-blog-link)
                     (inner-template . org-blog-inner-template)
                     (paragraph . org-blog-paragraph)))

(defun org-blog-export-to-post
    (&optional async subtreep visible-only body-only ext-plist)
  "Export to HTML with post backend.
See ‘org-export-to-file’ for ASYNC SUBTREEP VISIBLE-ONLY
BODY-ONLY EXT-PLIST."
  (interactive)
  (let* ((extension (concat "." (or (plist-get ext-plist :html-extension)
				    org-html-extension
				    "html")))
	 (file (org-export-output-file-name extension subtreep))
	 (org-export-coding-system org-html-coding-system))
    (org-export-to-file 'post file
      async subtreep visible-only body-only ext-plist)))

;;; RSS

(defun org-blog-abs-link (link desc info)
  "Export LINK to HTML.
Change relative links to absolute ones. DESC is the description,
INFO is the information channel."
  ;; https://orgmode.org/worg/dev/org-element-api.html#org85c642d
  (let ((path (org-element-property :path link))
        (type (org-element-property :type link)))
    (when (and (equal type "file")
               (string-prefix-p "." path))
      (let* ((abs-path (expand-file-name path))
             (site-root (directory-file-name
                         (plist-get info :blog-site-root)))
             (relative-path
              (concat "/" (file-relative-name abs-path site-root))))
        (setq link (org-element-put-property link :path relative-path))))
    ;; We bypass Org’s link dispatch. This way we get the desired
    ;; image format.
    (if (and (plist-get info :html-inline-images)
	     (org-export-inline-image-p
	      link (plist-get info :html-inline-image-rules)))
        ;; We don’t deal with the attributes list (pass a nil) because
        ;; it's kinda messy.
        (org-html--format-image
         (org-element-property :path link) nil info)
      (org-html-link link desc info))))

(defun org-blog-rss-item-template (contents info)
  "Export a RSS item.
CONTENTS is the HTML content, INFO is the information channel."
  (let* ((date (let ((time (cadar (plist-get info :date))))
                 (format-time-string
                  "%a, %d %b %Y %H:%M:%S %z"
                  (encode-time
                   0
                   (or (plist-get time :minute-start) 0)
                   (or (plist-get time :hour-start)   0)
                   (or (plist-get time :day-start)    0)
                   (or (plist-get time :month-start)  0)
                   (or (plist-get time :year-start)   0)))))
         (title (let ((title (and (plist-get info :with-title)
		                  (plist-get info :title))))
                  (org-export-data title info)))
         (link (url-encode-url (plist-get info :blog-rss-link))))
    (string-join
     (list "<item>"
           (format "<title>%s</title>" title)
           (format "<link>%s</link>" link)
           (format "<guid>%s</guid>" link)
           ;; (format "<description>%s</description>" intro)
           (format "<description><![CDATA[%s]]></description>"
                   contents)
           (format "<pubDate>%s</pubDate>" date)
           "</item>\n")
     "\n")))

(org-export-define-derived-backend 'rss-item 'post
  :options-alist '((:blog-rss-link "BLOG_RSS_LINK" nil "")
                   (:blog-rss-title "BLOG_RSS_TITLE" nil nil)
                   (:blog-rss-desc "BLOG_RSS_DESC" nil nil))
  :translate-alist '((link . org-blog-abs-link)
                     (template . org-blog-rss-item-template)))


(provide 'ox-blog)

;;; ox-blog.el ends here
