.PHONY: rock note

SHELL=fish

all: note rock

pull:
	git pull --rebase

note:
# Touch homepage and topic indexes
	touch note/index.html.pm note/topics/*.html.pm
# Render HTML files. ‘raco pollen’ has to be called from root dir,
# because some functions assume (current-project-root) is this dir.
	raco pollen render -p note/index.html.pm \
	note/**/index.html.pm
# Rendering note/index.html.pm also generates topic pages in topics/,
# render them.
	raco pollen render -p note/topics/*.html.pm

# Tidy HTML files.
	tidy -quiet -modify -wrap 74 --break-before-br yes \
	--indent auto --tidy-mark no \
	$(shell find note -name '*.html.pm' | sed 's/.pm$///g;') || true
# Only tidy index.html files that has accompanying Pollen source (aka
# new posts).

rock:
# Create hard links
	cd rock/day; ./publish.sh
# Touch homepage, index and rss
	touch rock/day/atom.xml.pm rock/day/index.html.pm \
	rock/day/index/index.html.pm
# Render HTML files. ‘raco pollen’ has to be called from root dir,
# because some functions assume (current-project-root) is this dir.
	raco pollen render -p rock/day/day-*/*.html.pm \
	rock/day/index.html.pm rock/day/atom.xml.pm \
	rock/day/index/index.html.pm
# Tidy HTML files.
	tidy -quiet -modify -wrap 74 --break-before-br yes \
	--indent auto --tidy-mark no \
	rock/day/**/index.html rock/day/index.html || true
# Tidy XML file (rss).
	tidy -quiet -modify --wrap 74 --indent auto -xml --tidy-mark no \
	rock/day/atom.xml || true
