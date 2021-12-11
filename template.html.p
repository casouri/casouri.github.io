<!DOCTYPE html>
<html lang="◊(get-language "en")">
  <head>
    <title>◊(->html (select 'title doc))</title>
    ◊(->html (essential-html-meta "note/note-style.css"))
  </head>
  <body>
    ◊(->html (header-line))

    <main id="body">
      ◊(doc->html (article-title doc))
      ◊(doc->html doc)
    </main>
  </body>
</html>
