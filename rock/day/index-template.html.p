<!DOCTYPE html>
<html>
  <head>
    <meta charset="UTF-8">
    <title>◊(->html (select 'h1 doc))</title>
    <link rel="stylesheet" type="text/css" href="◊(or (select-from-metas 'stylesheet metas) "./style.css")">
    <link rel="icon" type="image/png" href="../../favicon.png">
  </head>
  <body>
    
    ◊(->html (header-line))
    
    ◊(->html doc #:splice? #t)
  </body>
</html>
