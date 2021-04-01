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

firewall:
	sudo iptables -A INPUT -p tcp --dport 443 -j ACCEPT
	sudo iptables -A INPUT -p tcp --dport 80 -j ACCEPT
	sudo iptables -A INPUT -p tcp --dport 4386 -j ACCEPT
	sudo iptables -A INPUT -p tcp --dport 4387 -j ACCEPT
	sudo iptables -t nat -A PREROUTING -p tcp --dport 443 -j \
	REDIRECT --to-port 4386
	sudo iptables -t nat -A PREROUTING -p tcp --dport 80 -j \
	REDIRECT --to-port 4387

