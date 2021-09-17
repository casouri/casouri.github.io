#lang racket

;;       vars
(provide author-en
         author-zh
         root-url
         root-path
         lozenge
         ;; helper
         list-join
         rfc3339
         get-language
         rel-path
         here-path
         here-file-path
         ;; RSS
         rss-updated
         absolutize-url
         ;; Common markup
         link
         image
         fnref
         fndef
         quick-table
         ul
         ol
         li
         bquote
         mono
         emph
         fig
         figcap
         code
         bcode
         rt
         center
         halt
         ;; Common template
         essential-html-meta
         breadcrumb
         header-info
         header-line
         footer
         like-button
         remove-meta
         ;; TOC, header, title
         toc
         section
         subsection
         article-title
         ;; Post-processing
         post-proc
         doc->html)

(require
 pollen/decode
 pollen/file
 pollen/tag
 pollen/core
 pollen/template
 pollen/pagetree
 pollen/setup
 pollen/cache
 pollen-count
 txexpr
 srfi/19
 net/url)

;;; Variable

(define author-zh "付禹安")
(define author-en "Yuan Fu")
(define root-url "https://archive.casouri.cat/")
(define root-path (string->path "/Users/yuan/p/casouri/"))

(define lozenge "◊")

;;; Helper

;; Insert SEP in between every elements in LST. Like string-join but
;; for lists.
(define (list-join lst sep)
  (case (length lst)
    [(0) '()]
    [(1) lst]
    [else (append (list (car lst) sep)
                  (list-join (cdr lst) sep))]))

;; NFC-normalize Unicode, then percent escape them in URL.
;; URL should be a string.
(define (sanitize-url url)
  (url->string (string->url (string-normalize-nfc url))))

;; Get the “lang” meta of the current document, if none specified,
;; return FALLBACK.
(define (get-language [fallback #f])
  (or (select-from-metas 'lang (current-metas))
      fallback))

;; Convert a txexpr to a string by concatenating every string element.
(define (txexpr->string tx)
  (foldr string-append "" (findf*-txexpr tx string?)))

(define (txexpr->id tx)
  (sanitize-url (txexpr->string tx)))

;; Return the path to PATH relative to HERE-PATH.
;; PATH can be a relative path against blog root, or
;; a absolute path under blog root. HERE-PATH should be a directory.
(define (rel-path path here-path)
  (let* ([root-rel-path (find-relative-path
                         root-path path
                         ;; This option basically says “if PATH is a
                         ;; relative path against ROOT-PATH, return
                         ;; PATH”.
                         #:more-than-root? #t)]
         [rel (find-relative-path
               here-path
               (build-path root-path root-rel-path))])
    ;; Technically ‘find-relative-path’ should return ./
    ;; when two paths are the same, but in reality it doesn't.
    ;; So we need to fix that ourselves.
    (path->string
     (if (equal? rel (build-path root-path root-rel-path))
         (build-path 'same)
         (build-path 'same rel)))))

;; Return the directory in where the current document is.
(define (here-path)
  (path-only (here-file-path)))

;; Return “here-path” meta.
(define (here-file-path)
  (select-from-metas 'here-path (current-metas)))

;;; RSS

(define (rss-updated page)
  (let ([updated (select-from-metas 'updated (cached-metas page))]
        [date (select-from-metas 'date (cached-metas page))])
    (when (and (false? updated)
               (false? date))
      (error "Couldn't find date nor updated meta, insert either ◊define-meta[date] or ◊define-meta[updated]"))
    (rfc3339 (or updated date))))

(define (absolutize-url doc path)
  (let ([absolutize (lambda (rel-path)
                      (string-append
                       root-url
                       (path->string
                        (find-relative-path
                         root-path
                         (simple-form-path
                          (build-path (path-only path)
                                      rel-path))))))])
    (decode doc
            #:txexpr-proc
            (lambda (tx)
              (cond
               [(and (attrs-have-key? tx 'href)
                     (relative-path? (attr-ref tx 'href)))
                (attr-set tx 'href (absolutize (attr-ref tx 'href)))]
               [(and (attrs-have-key? tx 'src)
                     (relative-path? (attr-ref tx 'src)))
                (attr-set tx 'src (absolutize (attr-ref tx 'src)))]
               [else tx])))))

;;; Common markup

;;;; Link
;; An URL link. ◊link[url]{text}. If TEXT is omited, use URL as text.
(define (link url . tx-elements)
  (let* ([url (sanitize-url url)]
         [tx-elements (if (empty? tx-elements)
                          (list url)
                          tx-elements)]
         [tx-elements (squeeze-last tx-elements)]
         [link-tx (txexpr 'a empty tx-elements)]
         [link-tx (attr-set link-tx 'href url)])
    link-tx))

;; Squeeze the last character in TX. Because link appends a small
;; figure at the end, and we don’t want space between the last
;; character and that figure. Return a list of tx-element.
;; For example,
;;    @link{《余日摇滚第72日》}。
;; produces a link
;;    《余日摇滚第72日》*。
;; but there are space between the asterisk and the closing bracket.
;; We don’t want that space so we squeeze the last character in a
;; link.
(define (squeeze-last tx)
  (cond
    ;; If a string, annotate the last character.
    [(string? tx)
     (let ([head (substring tx 0 (sub1 (string-length tx)))]
           [tail (string-ref tx (sub1 (string-length tx)))])
       (if (not (memq tail squeezed-marks))
           (list tx)
           (list head (txexpr 'span '((class "last-punc-in-link"))
                              (list (list->string (list tail)))))))]
    ;; If a txexpr or a list of tx-element, recurs.
    [(txexpr? tx) (list (txexpr (get-tag tx)
                                (get-attrs tx)
                                (squeeze-last (get-elements tx))))]
    [(list? tx) (append (take tx (sub1 (length tx)))
                        (squeeze-last (last tx)))]))

;; An image. SRC is the path to the image.
(define (image src #:style [style #f] #:class [class #f] alt)
  (txexpr 'img (append `((src ,(sanitize-url src))
                         (alt ,alt))
                       (append
                        (if style `((style ,style)) empty)
                        (if class `((class ,class)) empty)))
          empty))

;;;; Footnote
;;
;; Footnotes are annotated by two tags, footref and footdef. Footref
;; renders to the superscript inline, and footdef contains the
;; footnote’s content. They are not expanded in-place, rather,
;; post-proc will use decode-fnref and decode-fndef to expand them. In
;; order to number each footnote in the article, post-proc first use
;; build-footnote-id-list to collect all the footref’s by the order of
;; appearance, then when expanding each footref and footdef, they are
;; numbered according to the order collected.
;;
;; footref takes a single string as the identifier, footdef takes the
;; identifier followed by the content.

(define (fnref id . tx-elements)
  (txexpr 'fnref `((raw-id ,id)) tx-elements))

(define (fndef id . tx-elements)
  (txexpr 'fndef `((raw-id ,id)) tx-elements))

;; Collects a list of footnote id’s (strings) and return it.
(define (build-footnote-id-list doc)
  (map (lambda (tx) (attr-ref tx 'raw-id))
       (or (findf*-txexpr doc (lambda (tx)
                                (and (txexpr? tx)
                                     (eq? (get-tag tx) 'fnref))))
           empty)))

;; The list of footnote id’s that is used to determine the numbering
;; of each footnote.
(define footnote-id-list empty)

;; Footnote reference, ID can be either number or string. The footnote
;; reference shown on page isn't ID, but a automatically generated
;; number.
(define (decode-fnref tx)
  (if (not (eq? (get-tag tx) 'fnref))
      tx
      (let* ([id (attr-ref tx 'raw-id)]
             [content (get-elements tx)]
             [ref-id (format "footref:~a" id)]
             [def-id (format "footdef:~a" id)]
             [display-number (add1 (index-of footnote-id-list id))]
             [id-display (format "~a" display-number)])
        (attr-set*
         (apply link (string-append "#" def-id)
                (append content
                        (list (txexpr 'span '((class "inline-footref"))
                                      (list id-display)))))
         'id ref-id
         'class "footref-anchor obviously-a-link"
         'aria-label "Jump to footnote"))))

;; Footnote definition. ID should match a previous footnote reference.
(define (decode-fndef tx)
  (if (not (eq? (get-tag tx) 'fndef))
      tx
      (let* ([id (attr-ref tx 'raw-id)]
             [text (get-elements tx)]
             [ref-id (format "footref:~a" id)]
             [def-id (format "footdef:~a" id)]
             [display-number (add1 (index-of footnote-id-list id))]
             [id-display (format "~a" display-number)])
        (txexpr 'div `((id ,def-id)
                       (class "footdef"))
                (list
                 (txexpr 'div '((class "def-footref obviously-a-link"))
                         (list (attr-set*
                                (link (string-append "#" ref-id)
                                      id-display)
                                'aria-label "Jump back to main text")))
                 (txexpr 'div '((class "def-footdef"))
                         text))))))

;;;; Table

(define tr (default-tag-function 'tr))
(define td (default-tag-function 'td))
(define th (default-tag-function 'th))

;; Picks out separators SEP from strings in ELM-LIST.
;; Elements in ELM-LIST could be non-string.
;; E.g. ("a|b" "|c" d) -> ("a" "|" "b" "|" "c" d)
(define (string-pick-out elm-list sep)
  (append-map (lambda (elm)
                (if (string? elm)
                    (list-join (string-split elm sep #:trim? #f) sep)
                    (list elm)))
              elm-list))

;; Split ELM-LIST by SEP.
;; E.g. ("a" "|" "b" "|" "c" d) -> (("a") ("b") ("c" d))
(define (split-list elm-list sep [intermediate empty])
  (if (empty? elm-list)
      (list intermediate)
      (if (equal? (car elm-list) sep)
          ;; Current elm is a separator, save INTERMEDIATE to return
          ;; list.
          (cons intermediate
                (split-list (cdr elm-list) sep empty))
          ;; Current elm is not a separator, save elm to INTERMEDIATE.
          (split-list (cdr elm-list) sep
                      (append intermediate (list (car elm-list)))))))

;; Define a table where rows are separated by newlines and columns are
;; separated by “|”. The first row is considered the header row.
(define (quick-table . elm-list)
  ;; “Pick out” all the bars and newlines.
  (let* ([maybe-trim (lambda (elm)
                       (if (string? elm) (string-trim elm) elm))]
         [trimmed-list (map maybe-trim elm-list)]
         [seped-list (string-pick-out
                      (string-pick-out elm-list "|") "\n")]
         ;; Now we have a 3d matrix (each cell is a list)
         [matrix (map (lambda (row) (split-list row "|"))
                      (split-list seped-list "\n"))]
         ;; Turns a list of elements into an row txexpr (th/td).
         [process-row
          (lambda (row cell-tag)
            (txexpr 'tr empty
                    (map (lambda (cell)
                           (txexpr cell-tag empty
                                   (map maybe-trim cell)))
                         row)))])
    (txexpr 'table empty
            (list
             (txexpr 'thead empty (list (process-row (car matrix) 'th)))
             (txexpr 'tbody empty
                     (map (lambda (row) (process-row row 'td))
                          (cdr matrix)))))))

;;;; Misc

(define ul (default-tag-function 'ul))
(define ol (default-tag-function 'ol))
(define li (default-tag-function 'li))

(define bquote (default-tag-function 'blockquote))

(define mono (default-tag-function 'span #:class "mono"))
(define code (default-tag-function 'code))
(define bcode (default-tag-function 'pre #:class "code-block"))

(define emph (default-tag-function 'span #:class "cjk-emphasize"))

(define fig (default-tag-function 'figure))
(define figcap (default-tag-function 'figcaption))

;; Ruby.
(define (rt . text)
  (@ (txexpr 'rp empty '("("))
     (txexpr 'rt empty text)
     (txexpr 'rp empty '(")"))))

(define center (default-tag-function 'div #:class "center"))

;; Sometimes the automatic squeezing cannot produce the correct
;; result, and we need to manually squeeze the character.
(define halt (default-tag-function 'span #:class "squeeze"))

;;; Common template

;; Some essential HTML head elements.
(define (essential-html-meta stylesheet-rel-path)
  (list
   (txexpr 'meta '((charset "utf-8")))
   ;; This meta is needed by CSS media queries that detects
   ;; mobile/desktop/etc.
   (txexpr 'meta '((name "viewport")
                   (content
                    "width=device-width, initial-scale=1")))
   (txexpr 'link `((rel "stylesheet")
                   (type "text/css")
                   (href ,(rel-path stylesheet-rel-path
                                    (here-path)))))
   (txexpr 'link `((rel "icon")
                   (type "image/png")
                   (href ,(rel-path "favicon.png" (here-path)))))))

;; Returns the parent directory of PATH. Doesn’t matter if PATH is a
;; directory path or file path.
(define (path-parent path)
  (simple-form-path
   (build-path (path-only path) 'up)))

;; A breadcrumb that looks like Home / sub-site / sub-sub-site. Only
;; include a level when there is an index.html.pm in that directory,
;; the name is taken from the “title” tag. Returns a list of
;; txexpr-element.
(define (breadcrumb [path #f] [rel-link #f])
  ;; First, elevate DIR and REL-LINK to point to parent directory.
  (let* ([dir (path-parent
               (or path
                   (simple-form-path
                    (select-from-metas 'here-path (current-metas)))))]
         [rel-link (build-path 'up (or rel-link "index.html"))]
         [root (simple-form-path (current-project-root))]
         [index-page (build-path dir "index.html.pm")])
    (if (string>? (path->string dir)
                  (path->string root))
        ;; Recursion case. 
        (if (file-exists? index-page)
            (let ([dir-title (or (select 'title (cached-doc index-page))
                                 (select 'h1 (cached-doc index-page)))])
              (append (breadcrumb dir rel-link)
                      (list (link (path->string rel-link) dir-title)
                            " ∕ ")))
            (breadcrumb dir rel-link))
        ;; Final case, link to home.
        (list (link (path->string rel-link) "Home") " ∕ "))))

;; Header information that looks like RSS | Source | License. Returns
;; a list of txexpr-element.
(define (header-info [rss-link #f])
  (let ([rss-link (or (select-from-metas 'rss-link (current-metas))
                      rss-link)])
    (list-join
     (remove
      #f
      (list
       (if rss-link (link rss-link "RSS") #f)
       (link "https://github.com/casouri/casouri.github.io" "Source")
       (link "https://creativecommons.org/licenses/by-sa/4.0/"
             "License")))
     "│")))

;; A header line with breadcrumbs on the left and RSS stuff on the
;; right.
(define (header-line #:rss [rss-link #f])
  (txexpr 'header '((id "header")
                    (class "obviously-a-link"))
          (list (txexpr 'nav empty (breadcrumb))
                (txexpr 'div empty (header-info rss-link)))))

;; A footer that displays author, written date, and comment. Returns a
;; txexpr. LANG can be either "zh" or "en".
(define (footer lang)
  (let* ([timestamp (select-from-metas 'date (current-metas))]
         [timestamp
          (and timestamp
               (list-ref (regexp-match #rx"<(.+)>" timestamp) 1))]
         [zh-en (lambda (zh en) (if (equal? lang "zh") zh en))]
         [author (zh-en author-zh author-en)])
    (txexpr
     'div empty
     (list (txexpr 'p empty
                   (list (string-append (zh-en "作者 " "Written by ")
                                        author)))
           (txexpr 'p empty
                   (list (string-append (zh-en "写于 " "Published on ")
                                        (or timestamp
                                            (zh-en "（没有记录）"
                                                   "(no record)")))))
           (txexpr
            'p empty
            (list
             (zh-en "评论 发邮件给 " "Comment by sending a message to ")
             (link "mailto:archive.casouri.cat@gmail.com"
                   "archive.casouri.cat@gmail.com")))))))

;; A like button that posts to “/like”
(define (like-button)
  (let* ([rel-path (find-relative-path
                    (current-project-root)
                    ;; ->output-path removes .pm extension.
                    (->output-path
                     (select-from-metas 'here-path (current-metas))))]
         ;; Add initial "/" to keep compatible with the old blog.
         [path-string (path->string (build-path "/" rel-path))])
    (txexpr 'div '((class "like-button"))
            (list (txexpr 'form '((action "/like")
                                  (method "post"))
                          (list (txexpr 'input
                                        `((name "path")
                                          (type "hidden")
                                          (value ,path-string))
                                        '())
                                (txexpr 'button '((class "like")
                                                  (type "submit"))
                                        (list "❤ Like"))))))))

;; Removes the “meta” tag in DOC and return the new DOC.
(define (remove-meta doc)
  (let-values ([(rest _)
                (splitf-txexpr
                 doc
                 (lambda (tx)
                   (and (txexpr? tx) (eq? (get-tag tx) 'meta))))])
    rest))

;;; Post processing

;; Clean up indent, expand footnotes, decode paragraphs.
(define (post-proc doc)
  (set! footnote-id-list (build-footnote-id-list doc))
  (set! doc (remove-meta doc))
  (set! doc (decode
             doc
             #:txexpr-elements-proc ignore-indent
             #:exclude-tags '(pre)))
  (set! doc (decode
             doc
             #:txexpr-proc (compose1 decode-fndef
                                     decode-fnref)
             #:string-proc process-punc
             #:exclude-tags '(pre code)))
  (set! doc (decode
             doc
             #:txexpr-elements-proc decode-paragraphs
             #:exclude-tags '(figure pre)))
  doc)

;; Converts DOC to HTML with post processing.
(define (doc->html doc)
  (cond [(null? doc) ""]
        [(eq? (get-tag doc) 'root)
         (->html (post-proc doc) #:splice? #t)]
        [(and (not (txexpr? doc)) (list? doc))
         (->html (post-proc (txexpr 'root empty doc)) #:splice? #t)]
        [else (->html (post-proc doc))]))

;;;; TOC, header, title

;; Collect all first-level headlines (sections) into a list. Each
;; headline is converted to a pure string.
(define (collect-headline tx)
  (if (txexpr? tx)
      (if (eq? (get-tag tx) 'h2)
          (list (txexpr
                 'li empty
                 ;; Link to the headline.
                 (list (apply link (string-append "#" (txexpr->id tx))
                              (get-elements tx)))))
          (append-map collect-headline (get-elements tx)))
      empty))

;; Generate a table of content for DOC. It contains links to each h2
;; headline.
(define (toc doc)
  (let* ([lang (get-language "en")]
         [zh-en (lambda (zh en) (if (equal? lang "zh") zh en))]
         [headlines (collect-headline doc)])
    (if (null? headlines)
        empty
        (txexpr 'nav '((id "toc")
                       (class "obviously-a-link"))
                (list (txexpr 'h2 empty
                              (list (zh-en "目录" "Table of Contents")))
                      (txexpr 'ol empty headlines))))))

;; h2.
(define (section . elm-list)
  (let ([tx (txexpr 'h2 empty elm-list)])
    (attr-set* tx 'id (txexpr->id tx)
               'class "section")))

;; h3.
(define (subsection . elm-list)
  (let ([tx (txexpr 'h3 empty elm-list)])
    (attr-set* tx 'id (txexpr->id tx)
               'class "subsection")))

;; Grabs the “title” tag from DOC.
(define (article-title doc)
  (txexpr 'h1 '((class "title")) (select* 'title doc)))

;;;; Ignore indents

;; Clean up indent in ELM-LIST.
(define (ignore-indent elm-list)
  (map (lambda (elm)
         (if (string? elm)
             ;; Annihilate any space after a newline.
             (regexp-replace #rx"\n +" elm "\n")
             elm))
       elm-list))

;;;; Squeeze quotation marks

;; If CHAR is a CJK character, return #t. “CJK character” is narrowly
;; defined as lying in 4E00-9FFF and 3000-303F.
(define (cjk? char)
  (let ([code (char->integer char)])
    (or (<= #x4E00 code #x9FFF) ; CJK Unified Ideographs
        (<= #x3000 code #x303F) ; CJK Symbols and Punctuation
        )))

(define squeezed-marks-left (string->list "，。、：；？！”》）』」〗】"))
(define squeezed-marks-right (string->list "“《（『「〖【"))
(define squeezed-marks (append squeezed-marks-left squeezed-marks-right))

;; Add full-width quotation marks and squeezes other full-width
;; punctuation marks. For consecutive full-width punctuation
;; characters: wrap them in <span class"squeeze full-width"> tags. For
;; quotation marks that ought to be full-width: wrap them in <span
;; class="full-width-mark"> tags. A quotation mark should be
;; full-width if it is next to a CJK character, as determined by
;; function “cjk?” (that function only detects common Chinese characters
;; and punctuation, but other East Asian languages don’t use curly quote
;; anyway).
;;
;; This is a recursive function, BEG and POINT are internal states.
;; BEG tracks the beginning of the non-mark text before POINT, POINT
;; points to the currently processing character. TEXT contains the
;; whole string.
(define (process-punc text [beg 0] [point 0])
  ;; It is possible for POINT to be greater than the string’s length,
  ;; because sometimes we increment POINT by two.
  (if (>= point (string-length text))
      ;; Termination clause.
      (let ([text-before-including-point
             (substring text beg (min point (string-length text)))])
        (list text-before-including-point))
      ;; Recursion clause. In this clause it is guaranteed that
      ;; POINT < TEXT-LEN.
      (let* ([char (string-ref text point)]
             [text-len (string-length text)]
             [next-char (if (< point (sub1 text-len))
                            (string-ref text (add1 point))
                            #f)]
             [prev-char (if (> point 0)
                            (string-ref text (sub1 point))
                            #f)]
             [make-squeeze (lambda (mark)
                             (txexpr 'span
                                     '((class "squeeze full-width-mark"))
                                     (list mark)))]
             [make-full (lambda (mark)
                          (txexpr 'span
                                  '((class "full-width-mark"))
                                  (list mark)))])
        (cond
         ;; If this char is a LEFT cjk mark and the next one is also
         ;; a cjk mark, squeeze this char.
         [(and (memq char squeezed-marks-left)
               (memq next-char squeezed-marks))
          (let* ([text-before-span (substring text beg point)]
                 [this-mark (make-squeeze (list->string (list char)))])
            ;; TEXT-BEFORE-SPAN <span>THIS-MARK</span> REST
            (append (list text-before-span this-mark)
                    (process-punc text (add1 point) (add1 point))))]
         ;; If the this char is RIGHT cjk mark, previous char is a
         ;; RIGHT cjk mark, we need to squeeze this mark. If prev mark
         ;; is LEFT, it would have squeeze itself, so this char don’t
         ;; need to. If prev char is not a mark, this char don’t need
         ;; to squeeze either.
         [(and (memq char squeezed-marks-right)
               (memq prev-char squeezed-marks-right))
          ;; TEXT-BEFORE-SPAN <span>THIS-MARK</span> REST
          (let* ([text-before-span (substring text beg point)]
                 [this-mark (make-squeeze
                             (list->string (list char)))])
            (append (list text-before-span this-mark)
                    (process-punc text (add1 point) (add1 point))))]
         ;; If the next char is not a cjk punctuation mark,
         ;; but this char is a curly quote that should be
         ;; full-width, we still need to annotate it.
         [(and (memq char (string->list "“”‘’"))
               ;; Previous or next char is CJK?
               (or (and next-char (cjk? next-char))
                   (and prev-char (cjk? prev-char))))
          (let* ([text-before-span (substring text beg point)]
                 [this-mark (make-full (list->string (list char)))])
            (append (list text-before-span this-mark)
                    (process-punc text (add1 point) (add1 point))))]
         ;; Handle “——”. The two dashes must be in one span, otherwise
         ;; there is a gap between them.
         [(and (memq char (string->list "——"))
               ;; Next char is also dash?
               (memq next-char (string->list "——")))
          (let* ([text-before-span (substring text beg point)]
                 [this-and-next-mark (make-full "——")])
            (append (list text-before-span this-and-next-mark)
                    (process-punc text (+ 2 point) (+ 2 point))))]
         ;; If nothing special, then mark this mark with
         ;; "full-width-mark", in case we want to use different font
         ;; for punctuation and body text.
         [(and (not (memq char (string->list "“”‘’")))
               (memq char squeezed-marks))
          (let ([text-before-span (substring text beg point)]
                [this-mark (make-full
                            (list->string (list char)))])
            (append (list text-before-span this-mark)
                    (process-punc text (add1 point) (add1 point))))]
         ;; Else just increment POINT.
         [else (process-punc text beg (add1 point))]))))

;;; RSS

;; Convert an Org Mode timestamp to a date object.
(define (decode-org-timestamp timestamp)
  (string->date timestamp (if (eq? (string-length timestamp) 16)
                              "<~Y-~m-~d ~a>"
                              "<~Y-~m-~d ~a ~H:~M>")))

;; Convert an Org Mode timestamp to a RFC3339 timestamp.
(define (rfc3339 timestamp)
  (date->string (decode-org-timestamp timestamp)
                "~Y-~m-~dT~H:~M:00.00-05:00"))
