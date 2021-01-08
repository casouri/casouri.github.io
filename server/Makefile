.PHONY: like view

like:
	sqlite3 database.sqlite3 -box "select uri, count(*) as c \
	from like \
	group by uri order by c;"

view:
	sqlite3 database.sqlite3 -box "select uri, count(*) as c \
	from view \
	where code=200 \
	group by uri order by c;"

database:
	sqlite3 database.sql ".read create.sql"

