#	@(#)makefile	5.7 (Berkeley) %G%

all: FRC
	./lispconf ${MACHINE}_4_3
	make -f Makefile ${MFLAGS} DESTDIR=${DESTDIR} copylibrary
	make -f Makefile ${MFLAGS}

install: FRC
	make -f Makefile ${MFLAGS} DESTDIR=${DESTDIR} install

clean: FRC
	make -f Makefile ${MFLAGS} clean

depend: FRC
	make -f Makefile ${MFLAGS} depend

FRC:
