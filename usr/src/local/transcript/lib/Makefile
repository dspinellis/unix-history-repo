# @(#)Makefile	1.2 (Berkeley) %G%
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

OWNER = bin
GROUP = bin

programs all:
	sed -e s,XPSLIBDIRX,$(PSLIBDIR),g psint.proto > psint.sh

install: 
	cd $(PSLIBDIR); rm -rf - ${FILES} ${PRG} *.afm 
	cp -p ${FILES} ${PRG} *.afm $(PSLIBDIR)
	cd $(PSLIBDIR); chown $(OWNER).$(GROUP) ${FILES} ${PRG} *.afm; \
		chmod 664 ${FILES} *.afm; \
		chmod 775 ${PRG}
	cd $(PSLIBDIR); \
		ln psint.sh psif; ln psint.sh psof; ln psint.sh psnf; \
		ln psint.sh pstf; ln psint.sh psgf; ln psint.sh psvf; \
		ln psint.sh psdf; ln psint.sh pscf; ln psint.sh psrf; \
		ln psbad.sh psbad
	-rm -rf $(PSLIBDIR)/troff.font $(PSLIBDIR)/ditroff.font
	-mkdir $(PSLIBDIR)/troff.font $(PSLIBDIR)/ditroff.font
	cd troff.font; make ${MFLAGS} install
	cd ditroff.font; make ${MFLAGS} install

clean:
	rm -f - *.BAK *.CKP .emacs_[0-9]*
	cd troff.font; make ${MFLAGS} clean
	cd ditroff.font; make ${MFLAGS} clean
