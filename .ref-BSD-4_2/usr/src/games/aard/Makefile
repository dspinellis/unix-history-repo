#	Makefile	4.1	83/08/13
#
DESTDIR=

aard:
	../ddl/ddlcomp aard < aard.ddl > ddlcomp.out

install:
	install aard ${DESTDIR}/usr/games/lib/aard
	install -c -m 755 frontend ${DESTDIR}/usr/games/aardvark

clean:
	rm -f ddlcomp.out
