.PHONY: rock note next clean

SHELL=fish
TIDY_FLAGS=-quiet -modify -wrap 74 --break-before-br yes --tidy-mark no --gnu-emacs yes

all: note rock

pull:
	git pull --rebase

note:
# Touch homepage and topic indexes.
	touch note/index.html.pm note/topics/*.html.pm
# Touch RSS files.
	touch note/atom.xml.pp note/emacs-feed.xml.pp
# Render HTML and XML files. ‘raco pollen’ has to be called from root
# dir, because some functions assume (current-project-root) is this
# dir. Don’t use --subdir option, it’s buggy.
#	raco pollen render -p note/index.html.pm \
#	note/**/index.html.pm note/topics/*.html.pm \
#	note/atom.xml.pp note/emacs-feed.xml.pp \
#	next/index.html.pm
	raco pollen render --jobs 4 note/**/*.html.pm \
	note/atom.xml.pp note/emacs-feed.xml.pp \
# Tidy HTML files. Don’t enable -indent, because it messes up pre tags
# (adds spaces in front of the first line). --gnu-emacs shows
# filenames with warnings. --show-filename does the same but is not
# supported in v5.6.0.
	tidy -quiet -modify -wrap 74 --break-before-br yes \
	--tidy-mark no --gnu-emacs yes \
	$(shell find note -name '*.html.pm' | sed 's/.pm$///g;') \
	next/index.html || true
# Tidy RSS feed.
	tidy -xml $(TIDY_FLAGS) note/atom.xml

next:
	raco pollen render --subdir --jobs 4 next

	tidy $(TIDY_FLAGS) next/index.html || true


rock:
# Touch homepage, index and rss.
	touch rock/day/atom.xml.pp rock/day/index.html.pm \
	rock/day/index/index.html.pm
# Render HTML and XML files. ‘raco pollen’ has to be called from root
# dir, because some functions assume (current-project-root) is this
# dir. Don’t use --subdir option, it’s buggy (missing breadcrumbs).
#	raco pollen render -p rock/day/collection/*.pm \
#	rock/day/index.html.pm rock/day/atom.xml.pp \
#	rock/day/index/index.html.pm
	raco pollen render --jobs 4 rock/day/collection/*.html.pm
	raco pollen render --jobs 4 rock/day/index.html.pm rock/day/index/index.html.pm rock/day/atom.xml.pp
# Tidy HTML files.
	tidy $(TIDY_FLAGS) --indent auto \
	rock/day/collection/*.html \
	rock/day/extra/day-67-lyrics.html \
	rock/day/index.html \
	rock/day/index/index.html || true
# Tidy RSS feed.
	tidy -xml $(TIDY_FLAGS) --indent auto rock/day/atom.xml

clean:
	raco pollen reset
