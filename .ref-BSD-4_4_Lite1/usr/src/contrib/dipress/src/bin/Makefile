#
# Copyright (c) 1984, 1985, 1986 Xerox Corp.
#
DESTBIN = ../../bin
DESTLIB = ../../lib
DESTMAN = ../../man
DESTINCLUDE = ../../include
MFLAGS = DESTBIN=../${DESTBIN} DESTLIB=../${DESTLIB} DESTMAN=../${DESTMAN} \
	DESTINCLUDE=../${DESTINCLUDE}
CFLAGS=	-O

# Programs that live in subdirectories, and have makefiles of their own.
#
SUBDIR=	ipfe iptotext ipmetrics makextdev maha mp2res plot2ip charset dipress restotext stackres texttoip ipf


all:	${SUBDIR}

${SUBDIR}: FRC
	cd $@; make ${MFLAGS}

FRC:

install: ${DESTBIN}/qip
	for i in ${SUBDIR}; do \
		(cd $$i; make ${MFLAGS} install); done

${DESTBIN}/qip: qip
	cp qip ${DESTBIN}

lint:
	for i in ${SUBDIR}; do \
		(cd $$i; make ${MFLAGS} lint); done

clean:
	rm -f a.out core *.s *.o
	for i in ${SUBDIR}; do \
		(cd $$i; make ${MFLAGS} clean); done
