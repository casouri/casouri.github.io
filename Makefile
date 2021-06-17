.PHONY: rock

pull:
	git pull --rebase

rock:
	touch rock/day/atom.xml.pm rock/day/index.html.pm \
	rock/day/index/index.html.pm

	raco pollen render -p rock/day/day-*/*.html.pm \
	rock/day/index.html.pm rock/day/atom.xml.pm \
	rock/day/index/index.html.pm

	tidy -m --wrap 74 --break-before-br yes --indent auto \
	rock/day/**/index.html

	tidy -m --wrap 74 --indent auto -xml rock/day/atom.xml
