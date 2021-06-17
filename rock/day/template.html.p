<!DOCTYPE html>
<html>
  <head>
    <meta charset="UTF-8">
    <title>◊(day-title (symbol->string here))</title>
    <link rel="icon" type="image/png" href="../../../favicon.png">
    <link rel="stylesheet" type="text/css" href="../style.css">
  </head>
  <body>

    ◊(->html (header-line #:rss "../atom.xml"))

    <div id="body">
      ◊(->html (synthesis-body doc (symbol->string here)))
    </div>

    <div id="postamble">
      ◊(->html (like-button))
      ◊(->html (zh-footer))
    </div>
  </body>
</html>
