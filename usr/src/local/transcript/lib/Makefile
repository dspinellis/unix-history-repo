# transcript/lib/Makefile.bsd
#
# Copyright (c) 1985 Adobe Systems Incorporated. All Rights Reserved. 
#
# RCSID: $Header: Makefile.bsd,v 2.1 85/11/24 12:19:06 shore Rel $

PSLIBDIR = /usr/local/lib/ps

FILES = banner.pro enscript.pro pstext.pro \
	ps4014.pro ps630.pro ps4014.pro pscat.pro psplot.pro psdit.pro \
	ehandler.ps uartpatch.ps bogusmsg.ps \
	font.map

PRG = psint.sh psbad.sh

OWNER = daemon
GROUP = tex

programs all:
	sed -e s,XPSLIBDIRX,$(PSLIBDIR),g psint.proto > psint.sh

install: 
	cd $(PSLIBDIR); rm -rf - ${FILES} ${PRG} *.afm 
	cp ${FILES} ${PRG} *.afm $(PSLIBDIR)
	cd $(PSLIBDIR); chown $(OWNER) ${FILES} ${PRG} *.afm; \
		chgrp $(GROUP) ${FILES} ${PRG} *.afm; \
		chmod 664 ${FILES} *.afm; \
		chmod 775 ${PRG}
	cd $(PSLIBDIR); \
		ln psint.sh psif; ln psint.sh psof; ln psint.sh psnf; \
		ln psint.sh pstf; ln psint.sh psgf; ln psint.sh psvf; \
		ln psint.sh psdf; ln psint.sh pscf; ln psint.sh psrf; \
		ln psbad.sh psbad
	-rm -rf $(PSLIBDIR)/troff.font $(PSLIBDIR)/ditroff.font
	-mkdir $(PSLIBDIR)/troff.font $(PSLIBDIR)/ditroff.font
	cp troff.font/* $(PSLIBDIR)/troff.font
	cd $(PSLIBDIR)/troff.font; make ${MFLAGS} install
	cd $(PSLIBDIR)/troff.font; chown $(OWNER) . * ; chgrp $(GROUP) . *
	cp ditroff.font/* $(PSLIBDIR)/ditroff.font
	-cd $(PSLIBDIR)/ditroff.font; make ${MFLAGS} install
	-cd $(PSLIBDIR)/ditroff.font; chown $(OWNER) . * ; chgrp $(GROUP) . *

clean:
	rm -f - *.BAK *.CKP .emacs_[0-9]*
	cd troff.font; make ${MFLAGS} clean
	cd ditroff.font; make ${MFLAGS} clean
