
FILES = overview.ms tables.ms wt-pep.ms

pepsy.info: $(FILES)
	ditroff -ms $(FILES)

DESCRIPTION: $(FILES)
	-rm -f ../DESCRIPTION
	nroff -ms $(FILES) > DESCRIPTION
	ln DESCRIPTION ..
