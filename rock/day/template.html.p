<!DOCTYPE html>
<html lang="zh">
  <head>
    <title>◊(day-title (symbol->string here))</title>
    ◊(->html (essential-html-meta "rock/day/style.css"))
    ◊(maybe-custom-palate)
  </head>

  <body>
    ◊(->html (header-line #:rss
                          (rel-path "rock/day/atom.xml" (here-path))))

    <main id="body">
      <article>
        ◊(doc->html (synthesis-body doc (here-path)))
      </article>
    </main>

    <footer id="postamble">
      ◊(->html (like-button))
      ◊(->html (footer "zh" doc))
    </footer>
  </body>
</html>
