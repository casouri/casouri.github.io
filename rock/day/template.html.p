<!DOCTYPE html>
<html lang="zh">
  <head>
    <title>◊(day-title (symbol->string here))</title>
    ◊(->html (essential-html-meta "rock/day/style.css"))
    ◊(maybe-custom-palate)
  </head>

  <body>
    ◊(->html (header-line #:rss "note/atom.xml"))

    <main id="body">
      <article>
        ◊(doc->html (synthesis-body doc (here-path)))
      </article>
    </main>

    <footer id="postamble">
      ◊(->html (like-button))
      ◊(->html (footer "zh" doc (day-title (symbol->string here))))
    </footer>
  </body>
</html>
