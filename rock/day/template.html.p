<!DOCTYPE html>
<html lang="zh">
  <head>
    <title>◊(day-title (symbol->string here))</title>
    ◊(->html (essential-html-meta "rock/day/style.css"))
  </head>
  <body>

    ◊(->html (header-line #:rss
                          (rel-path "rock/day/atom.xml" (here-path))))

    <main id="body">
      <article>
        ◊(->html (synthesis-body (post-proc doc) (symbol->string here))
        #:splice? #t)  
      </article>
    </main>

    <footer id="postamble">
      ◊(->html (like-button))
      ◊(->html (footer "zh"))
    </footer>
  </body>
</html>
