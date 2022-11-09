<!DOCTYPE html>
<html lang="zh">
  <head>
    <title>Rock/day Extra</title>
    ◊(->html (essential-html-meta "rock/day/style.css"))
  </head>

  <body>
    ◊(->html (header-line #:rss "rock/day/atom.xml"))

    <main id="body">
      <article>
        ◊(doc->html doc)
      </article>
    </main>

    <footer id="postamble">
    </footer>
  </body>
</html>
