#	Makefile	4.3	85/05/21
#
# Possible options:
#	make		compile source for terminal tables below
#	make <term>	compile table for a specific <term>
#	make install	move tables to ${DEST}
#	make links	make links for ease of documentation
#	make clean	remove tab*.o files lying around
ALL=	37 lpr 300 300-12 302 302-12 382 382-12 450 450-12 833 833-12 \
	epson itoh itoh12 nec nec12 nec-t qume qume12 xerox xerox12 \
	x-ecs x-ecs12
DESTDIR=
DEST=	${DESTDIR}/usr/lib/term

all:	${ALL}

37:	tab37.o
lpr:	tablpr.o
300:	tab300.o code.300
300-12:	tab300-12.o code.300
302:	tab302.o code.300
302-12:	tab302-12.o code.300
382:	tab382.o code.300
382-12: tab382-12.o code.300
450:	tab450.o code.300
450-12:	tab450-12.o code.300
833:	tab833.o code.aj833
833-12: tab833-12.o code.aj833
epson:	tabepson.o
itoh:	tabitoh.o code.itoh
itoh12: tabitoh12.o code.itoh
nec:	tabnec.o code.nec
nec12:	tabnec12.o code.nec
nec-t:	tabnec-t.o
qume:	tabqume.o
qume12:	tabqume12.o
xerox:	tabxerox.o code.xerox
xerox12:tabxerox12.o code.xerox
x-ecs:	tabx-ecs.o code.x-ecs
x-ecs12:tabx-ecs12.o code.x-ecs

install:
	for file in tab*.o; do \
		install -s -m 755 $$file ${DEST}/`basename $$file .o`;\
	done
	-if test ! -f ${DEST}/tabtn300; \
	then \
		make links; \
	fi
	install -c -m 644 README ${DEST}/README

links:
	rm -f ${DEST}/tabtn300; ln ${DEST}/tablpr ${DEST}/tabtn300
	rm -f ${DEST}/tabcrt; ln ${DEST}/tablpr ${DEST}/tabcrt
	rm -f ${DEST}/tab300s; ln ${DEST}/tab302 ${DEST}/tab300s
	rm -f ${DEST}/tab300s-12; ln ${DEST}/tab302-12 ${DEST}/tab300s-12
	rm -f ${DEST}/tabdtc; ln ${DEST}/tab302 ${DEST}/tabdtc
	rm -f ${DEST}/tabdtc12; ln ${DEST}/tab302-12 ${DEST}/tabdtc12
	rm -f ${DEST}/tabipsi; ln ${DEST}/tab450 ${DEST}/tabipsi
	rm -f ${DEST}/tabipsi12; ln ${DEST}/tab450-12 ${DEST}/tabipsi12

clean:
	rm -f tab*.o errs
