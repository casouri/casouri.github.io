<!DOCTYPE html>
<html lang="◊(get-language "en")">
  <head>
    <title>◊(->html (select 'title doc))</title>
    ◊(->html (essential-html-meta "note/note-style.css"))
  </head>
  <body>

    ◊(->html (header-line #:rss (rel-path "note/atom.xml" (here-path))))

    <main id="body">
      <article>
        ◊(->html (article-title doc))
        ◊(->html (toc doc))
        ◊(->html (post-proc doc) #:splice? #t)
      </article>
    </main>

    <footer id="postamble">
      ◊(->html (like-button))
      ◊(->html (footer (get-language "en")))
    </footer>
  </body>
</html>
