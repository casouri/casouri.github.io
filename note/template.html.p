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
        ◊(doc->html (article-title doc))
        ◊(doc->html (toc doc))
        ◊(doc->html doc)
      </article>
    </main>

    <footer id="postamble">
      ◊(->html (like-button))
      ◊(->html (footer (get-language "en") doc (or (select 'title doc) "No title")))
    </footer>
  </body>
</html>
