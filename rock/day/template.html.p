<!DOCTYPE html>
<html lang="zh">
  <head>
    <meta charset="utf-8" />
    <title>◊(day-title (symbol->string here))</title>
    <link rel="icon" type="image/png" href="../../../favicon.png" />
    <link rel="stylesheet" type="text/css" href="../style.css" />
  </head>
  <body>

    ◊(->html (header-line #:rss "../atom.xml"))

    <main id="body">
      <article>
        ◊(->html (synthesis-body doc (symbol->string here)))  
      </article>
    </main>

    <footer id="postamble">
      ◊(->html (like-button))
      ◊(->html (zh-footer))
    </footer>
  </body>
</html>
