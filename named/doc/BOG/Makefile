# Copyright (c) 1986, 1988 Regents of the University of California.
# All rights reserved.
#
# Redistribution and use in source and binary forms are permitted
# provided that this notice is preserved and that due credit is given
# to the University of California at Berkeley. The name of the University
# may not be used to endorse or promote products derived from this
# software without specific prior written permission. This software
# is provided ``as is'' without express or implied warranty.
#
#	@(#)Makefile	6.3 (Berkeley) 2/28/88
#
FILES=	00macs.me 00title.me intro.me build.me types.me\
	setup.me files.me named.boot.primary\
	named.boot.secondary named.boot.cache resolv.conf\
	root.cache named.local ucbhosts ucbhosts.rev manage.me
ME=	-me
NROFF=	nroff -rb3
PRINTER= -Pdp
TBL=	dtbl $(PRINTER)
TROFF= ditroff $(PRINTER)

troff: $(FILES)
	$(TBL) $(FILES)| $(TROFF) $(ME) $(FLAGS)

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
