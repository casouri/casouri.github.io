.PHONY: like view

like:
	sqlite3 database.sql -box "select uri, count(*) from like \
	group by uri;"

view:
	sqlite3 database.sql -box "select uri, count(*) from view \
	group by uri;"

database:
	sqlite3 database.sql ".read create.sql"

