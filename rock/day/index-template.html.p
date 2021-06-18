<!DOCTYPE html>
<html lang="zh">
  <head>
    <meta charset="utf-8" />
    <title>◊(->html (select 'h1 doc))</title>
    <link rel="stylesheet" type="text/css" href="◊(or (select-from-metas 'stylesheet metas) "./style.css")" />
    <link rel="icon" type="image/png" href="../../favicon.png" />
  </head>
  <body>
    ◊(->html (header-line))
    <main>
      ◊(->html doc #:splice? #t)
    </main>
  </body>
</html>
