#	%M%	%I%	%E%
#
DESTDIR=
UTILITY=${DESTDIR}/usr/spool/notes/.utilities

all:    src
	cd src; make ${MFLAGS} DESTDIR=${DESTDIR} all

clean:
	cd src; make ${MFLAGS} clean
	@rm -f placehelp placebin
	@echo binaries and objects removed

install: placehelp placebin
	touch install
	@echo notefiles installed
	cd man; make ${MFLAGS} DESTDIR=${DESTDIR} install

placehelp:	help/access.help help/dir.help help/index.help
placehelp:	help/lim.help help/read.help
	install -c -m 644 help/access.help $(UTILITY)/access.help
	install -c -m 644 help/dir.help $(UTILITY)/dir.help
	install -c -m 644 help/index.help $(UTILITY)/index.help
	install -c -m 644 help/lim.help $(UTILITY)/lim.help
	install -c -m 644 help/read.help $(UTILITY)/read.help
	@echo ------ help installation done
	touch placehelp

placebin:
	cd src; make DESTDIR=${DESTDIR} placebin
