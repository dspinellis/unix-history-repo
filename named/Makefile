#
# Copyright (c) 1987, 1988 Regents of the University of California.
# All rights reserved.
#
# Redistribution and use in source and binary forms are permitted
# provided that the above copyright notice and this paragraph are
# duplicated in all such forms and that any documentation,
# advertising materials, and other materials related to such
# distribution and use acknowledge that the software was developed
# by the University of California, Berkeley.  The name of the
# University may not be used to endorse or promote products derived
# from this software without specific prior written permission.
# THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
# IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
# WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
#
#	@(#)Makefile	4.19 (Berkeley) 2/8/89
#
DEFINES= -DDEBUG -DSTATS
### -DALLOW_T_UNSPEC -Dmalloc=rt_malloc -Dfree=rt_free
### ALLOC=storage.o
CFLAGS=	-O ${DEFINES}

RES=
LIBS=	${RES}
LIBC=	/lib/libc.a

SRCS=	db_dump.c db_load.c db_lookup.c db_reload.c db_save.c db_update.c \
	ns_forw.c ns_init.c ns_main.c ns_maint.c ns_req.c ns_resp.c \
	ns_sort.c ns_stats.c db_glue.c xfer.c
HDRS=	db.h ns.h
OBJS=	db_dump.o db_load.o db_lookup.o db_reload.o db_save.o db_update.o \
	ns_forw.o ns_init.o ns_main.o ns_maint.o ns_req.o ns_resp.o \
	ns_sort.o ns_stats.o db_glue.o $(ALLOC)
XFEROBJ=   xfer.o db_glue.o

SUBDIR=	tools
MAN=	named.0

all: named xfer
#	for i in ${SUBDIR}; do \
#		(cd $$i; make ${MFLAGS} $@); done

named: ${OBJS} ${LIBC}
	sh newvers.sh
	${CC} -o $@ version.c ${OBJS} ${LIBS}

xfer:	${XFEROBJ} ${LIBC}
	${CC} -o $@ ${XFEROBJ} ${LIBS}

clean:
	rm -f ${OBJS} ${XFEROBJ} core named xfer version.o version.c
#	for i in ${SUBDIR}; do \
#		(cd $$i; make ${MFLAGS} $@); done

cleandir: clean
	rm -f ${MAN} tags .depend
#	for i in ${SUBDIR}; do \
#		(cd $$i; make ${MFLAGS} $@); done

depend .depend: ${SRCS}
	mkdep ${CFLAGS} ${SRCS}
#	for i in ${SUBDIR}; do \
#		(cd $$i; make ${MFLAGS} $@); done

install: ${MAN}
	install -s -o bin -g bin -m 755 named ${DESTDIR}/etc/named
	install -s -o bin -g bin -m 755 xfer ${DESTDIR}/etc/named-xfer
	install -c -o bin -g bin -m 444 ${MAN} ${DESTDIR}/usr/man/cat8
	install -c -o bin -g bin -m 755 named.restart ${DESTDIR}/etc
	install -c -o bin -g bin -m 755 named.reload ${DESTDIR}/etc
#	for i in ${SUBDIR}; do \
#		(cd $$i; make ${MFLAGS} $@); done

lint: ${SRCS} ${HDRS}
	lint -Dlint ${CFLAGS} ${SRCS}
#	for i in ${SUBDIR}; do \
#		(cd $$i; make ${MFLAGS} $@); done

tags: ${SRCS}
	ctags -t ${SRCS} ${HDRS}
#	for i in ${SUBDIR}; do \
#		(cd $$i; make ${MFLAGS} $@); done
