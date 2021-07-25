<!DOCTYPE html>
<html lang="zh">
  <head>
    <title>◊(day-title (symbol->string here))</title>
    ◊(->html (essential-html-meta "rock/day/style.css"))
  </head>

  <style>
    html {
       --piece-info-foreground:
           ◊(select-from-metas 'highlight-color metas);
       --foreground: ◊(select-from-metas 'foreground-color metas);
       --background: ◊(select-from-metas 'background-color metas);
    }
  </style>

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
      ◊(->html (footer "zh"))
    </footer>
  </body>
</html>
