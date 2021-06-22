.PHONY: rock

pull:
	git pull --rebase

rock:
	touch rock/day/atom.xml.pm rock/day/index.html.pm \
	rock/day/index/index.html.pm

	raco pollen render -p rock/day/day-*/*.html.pm \
	rock/day/index.html.pm rock/day/atom.xml.pm \
	rock/day/index/index.html.pm

	tidy -quiet -modify -wrap 74 --break-before-br yes \
	--indent auto --tidy-mark no \
	rock/day/**/index.html rock/day/index.html || true

	tidy -quiet -modify --wrap 74 --indent auto -xml --tidy-mark no \
	rock/day/atom.xml || true
