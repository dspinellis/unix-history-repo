#
# Copyright (c) 1986 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	6.2 (Berkeley) 5/20/86
#
FILES=	00macs.me 00title.me intro.me build.me types.me\
	setup.me files.me named.boot.primary\
	named.boot.secondary named.boot.cache resolv.conf\
	named.ca named.local ucbhosts ucbhosts.rev manage.me
ME=	-me
T=	-Tlp
NROFF=	nroff -rb3
ITROFF=	itroff
DITROFF= ditroff
PRINTER= -Pdp

ditroff: $(FILES)
	dtbl $(FILES)| $(DITROFF) $(PRINTER) $(ME) $(FLAGS)

itroff: $(FILES)
	tbl $(FILES)| $(ITROFF) $(ME) $(FLAGS)

nroff: $(FILES)
	tbl $(FILES)| $(NROFF) $(ME) $(FLAGS) > file

cat: $(FILES)
	@cat $(FILES)

clean:

spell: $(FILES)
	@for i in $(FILES); do \
		echo $$i; \
		spell $$i | sort | comm -23 - spell.ok > $$i.spell; \
	done
