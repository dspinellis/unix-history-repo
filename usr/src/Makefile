#	@(#)Makefile	4.9	(Berkeley)	%G%
#
DESTDIR=
CFLAGS=	-O

# Programs that live in subdirectories, and have makefiles of their own.
#
LIBDIR= lib usr.lib
SRCDIR=	bin usr.bin etc ucb new games local

all:	${LIBDIR} ${SRCDIR}

${LIBDIR} ${SRCDIR}: FRC
	cd $@; make ${MFLAGS}

build: FRC
	echo installing /usr/include
	cd include; make ${MFLAGS} install
	cd lib; make ${MFLAGS}
	echo installing /lib
	cd lib; make ${MFLAGS} install
	cd usr.lib; make ${MFLAGS}
	echo installing /usr/lib
	cd usr.lib; make ${MFLAGS} install
	-for i in ${SRCDIR}; do (cd $$i; make ${MFLAGS}); done

FRC:

install:
	-for i in ${SUBDIR}; do \
		(cd $$i; make ${MFLAGS} DESTDIR=${DESTDIR} install); done

installsrc:
	-for i in ${SRCDIR}; do \
		(cd $$i; make ${MFLAGS} DESTDIR=${DESTDIR} install); done

tags:
	for i in include lib usr.lib; do \
		(cd $$i; make ${MFLAGS} TAGSFILE=../tags tags); \
	done
	sort -u +0 -1 -o tags tags

clean:
	rm -f a.out core *.s *.o
	for i in ${LIBDIR} ${SRCDIR}; do (cd $$i; make ${MFLAGS} clean); done
