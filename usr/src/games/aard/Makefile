DESTDIR=

aard:
	../ddl/ddlcomp aard < aard.ddl > ddlcomp.out

install:
	install aard ${DESTDIR}/usr/games/lib/aard
	install frontend ${DESTDIR}/usr/games/aardvark

clean:
	rm -f ddlcomp.out
