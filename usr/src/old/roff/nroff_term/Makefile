#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	4.9	(Berkeley)	%G%
#
# Possible options:
#	make		compile source for all terminal tables below
#	make <term>	compile table for a specific <term>
#	make install	move tables to ${DEST}
#	make clean	remove tab*.o files lying around
CFLAGS=	-I../common_source
LIBC=	/lib/libc.a
DEST=	${DESTDIR}/usr/libdata/term
SRCS=	tab300-12.c tab300.c tab302-12.c tab302.c tab37.c tab382-12.c \
	tab382.c tab450-12.c tab450.c tab833-12.c tab833.c tabepson.c \
	tabitoh.c tabitoh12.c tablpr.c tabnec-t.c tabnec.c tabnec12.c \
	tabnec25-t.c tabqume.c tabqume12.c tabx-ecs.c tabx-ecs12.c \
	tabxerox.c tabxerox12.c
OBJS=	tab300-12.o tab300.o tab302-12.o tab302.o tab37.o tab382-12.o \
	tab382.o tab450-12.o tab450.o tab833-12.o tab833.o tabepson.o \
	tabitoh.o tabitoh12.o tablpr.o tabnec-t.o tabnec.o tabnec12.o \
	tabnec25-t.o tabqume.o tabqume12.o tabx-ecs.o tabx-ecs12.o \
	tabxerox.o tabxerox12.o
ALL=	tab37 tablpr tab300 tab300-12 tab302 tab302-12 tab382 tab382-12 \
	tab450 tab450-12 tab833 tab833-12 tabepson tabitoh tabitoh12 \
	tabnec tabnec12 tabnec-t tabnec25-t tabqume tabqume12 tabxerox \
	tabxerox12 tabx-ecs tabx-ecs12

all: ${ALL}

tab37:		mktab.o tab37.o
tablpr:		mktab.o tablpr.o
tab300:		mktab.o tab300.o 
tab300-12:	mktab.o tab300-12.o 
tab302:		mktab.o tab302.o 
tab302-12:	mktab.o tab302-12.o 
tab382:		mktab.o tab382.o 
tab382-12:	mktab.o tab382-12.o 
tab450:		mktab.o tab450.o 
tab450-12:	mktab.o tab450-12.o 
tab833:		mktab.o tab833.o 
tab833-12:	mktab.o tab833-12.o 
tabepson:	mktab.o tabepson.o
tabitoh:	mktab.o tabitoh.o 
tabitoh12:	mktab.o tabitoh12.o 
tabnec:		mktab.o tabnec.o 
tabnec12:	mktab.o tabnec12.o 
tabnec-t:	mktab.o tabnec-t.o
tabnec25-t:	mktab.o tabnec25-t.o
tabqume:	mktab.o tabqume.o
tabqume12:	mktab.o tabqume12.o
tabxerox:	mktab.o tabxerox.o 
tabxerox12:	mktab.o tabxerox12.o 
tabx-ecs:	mktab.o tabx-ecs.o 
tabx-ecs12:	mktab.o tabx-ecs12.o 

${ALL}:
	${CC} mktab.o $@.o -o mktab
	./mktab > $@

clean: FRC
	rm -f ${OBJS} ${ALL} mktab.o mktab core

cleandir: clean
	rm -f ${MAN} tags .depend

depend: FRC
	mkdep ${CFLAGS} ${SRCS}

install: FRC
	install -c -o bin -g bin -m 755 ${ALL} ${DEST}
	rm -f ${DEST}/tabtn300; ln ${DEST}/tablpr ${DEST}/tabtn300
	rm -f ${DEST}/tabcrt; ln ${DEST}/tablpr ${DEST}/tabcrt
	rm -f ${DEST}/tab300s; ln ${DEST}/tab302 ${DEST}/tab300s
	rm -f ${DEST}/tab300s-12; ln ${DEST}/tab302-12 ${DEST}/tab300s-12
	rm -f ${DEST}/tabdtc; ln ${DEST}/tab302 ${DEST}/tabdtc
	rm -f ${DEST}/tabdtc12; ln ${DEST}/tab302-12 ${DEST}/tabdtc12
	rm -f ${DEST}/tabipsi; ln ${DEST}/tab450 ${DEST}/tabipsi
	rm -f ${DEST}/tabipsi12; ln ${DEST}/tab450-12 ${DEST}/tabipsi12
	install -c -o bin -g bin -m 644 README ${DEST}/README

lint: FRC
	lint ${CFLAGS} ${SRCS}

tags: FRC
	ctags ${SRCS}

FRC:
