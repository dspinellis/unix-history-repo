#
# Copyright (c) 1987 The Regents of the University of California.
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
#	@(#)Makefile	4.11 (Berkeley) %G%
#
CFLAGS=	-O
LIBC=	/lib/libc.a
DLSRCS=	deliv1.c deliv2.c
DLOBJS=	deliv1.o deliv2.o
HTSRCS=	deliv2.c glue4.c glue5.c hunt1.c hunt2.c hunt3.c hunt5.c hunt6.c \
	hunt7.c hunt8.c hunt9.c refer3.c shell.c tick.c
HTOBJS=	deliv2.o glue4.o glue5.o hunt1.o hunt2.o hunt3.o hunt5.o hunt6.o \
	hunt7.o hunt8.o hunt9.o refer3.o shell.o tick.o
INSRCS=	inv1.c inv2.c inv3.c inv5.c inv6.c deliv2.c
INOBJS=	inv1.o inv2.o inv3.o inv5.o inv6.o deliv2.o
MKSRCS=	mkey1.c mkey2.c mkey3.c deliv2.c
MKOBJS=	mkey1.o mkey2.o mkey3.o deliv2.o
RFSRCS=	deliv2.c glue1.c glue2.c glue3.c glue4.c glue5.c hunt2.c hunt3.c \
	hunt5.c hunt6.c hunt7.c hunt8.c hunt9.c mkey3.c refer0.c refer1.c \
	refer2.c refer4.c refer5.c refer6.c refer7.c refer8.c shell.c
RFOBJS=	deliv2.o glue1.o glue2.o glue3.o glue4.o glue5.o hunt2.o hunt3.o \
	hunt5.o hunt6.o hunt7.o hunt8.o hunt9.o mkey3.o refer0.o refer1.o \
	refer2.o refer4.o refer5.o refer6.o refer7.o refer8.o shell.o
WHSRCS=	what1.c what2.c what3.c what4.c shell.c mkey3.c
WHOBJS=	what1.o what2.o what3.o what4.o shell.o mkey3.o
SRCS=	${DLSRCS} ${HTSRCS} ${INSRCS} ${MKSRCS} ${RFSRCS} ${WHSRCS}
OBJS=	${DLOBJS} ${HTOBJS} ${INOBJS} ${MKOBJS} ${RFOBJS} ${WHOBJS} \
	addbib.o lookbib.o sortbib.o
MAN=	addbib.0 lookbib.0 refer.0 roffbib.0 sortbib.0

all: addbib deliv hunt inv lookbib mkey refer sortbib whatabout

addbib lookbib sortbib: ${LIBC}
	${CC} ${CFLAGS} -o $@ $@.c

deliv: ${DLOBJS} ${LIBC}
	${CC} ${CFLAGS} ${DLOBJS} -o $@

hunt: ${HTOBJS} ${LIBC}
	${CC} ${CFLAGS} ${HTOBJS} -o $@

inv: ${INOBJS} ${LIBC}
	${CC} ${CFLAGS} ${INOBJS} -o $@

mkey: ${MKOBJS} ${LIBC}
	${CC} ${CFLAGS} ${MKOBJS} -o $@

refer: ${RFOBJS} ${LIBC}
	${CC} ${CFLAGS} ${RFOBJS} -o $@

# refpart doesn't compile
#
#RPSRCS=	deliv2.c glue4.c glue5.c mkey3.c refer0.c refer1.c refer2.c \
#	refer3.c refer4.c refer5.c refer6.c refer7.c refer8.c
#RPOBJS=	deliv2.o glue4.o glue5.o mkey3.o refer0.o refer1.o refer2.o \
#	refer3.o refer4.o refer5.o refer6.o refer7.o refer8.o
#refpart: ${RPOBJS} ${LIBC}
#	${CC} ${CFLAGS} ${RPOBJS} -o $@

whatabout: ${WHOBJS} ${LIBC}
	${CC} ${CFLAGS} ${WHOBJS} -o $@

clean:
	rm -f ${OBJS} core addbib deliv hunt inv lookbib mkey refer \
		refpart sortbib whatabout

cleandir: clean
	rm -f ${MAN} tags .depend

depend: ${SRCS}
	mkdep -p ${CFLAGS} addbib.c sortbib.c lookbib.c
	mkdep ${CFLAGS} ${SRCS}

install: ${MAN}
	-[ -d ${DESTDIR}/usr/lib/refer ] || mkdir ${DESTDIR}/usr/lib/refer
	install -s -o bin -g bin -m 755 mkey inv hunt ${DESTDIR}/usr/lib/refer
	install -s -o bin -g bin -m 755 addbib lookbib sortbib refer \
		${DESTDIR}/usr/bin
	install -c -o bin -g bin -m 755 roffbib.sh ${DESTDIR}/usr/bin/roffbib
	install -c -o bin -g bin -m 755 indxbib.sh ${DESTDIR}/usr/bin/indxbib
	install -c -o bin -g bin -m 755 tmac.bib ${DESTDIR}/usr/lib/tmac
	install -c -o bin -g bin -m 444 ${MAN} ${DESTDIR}/usr/man/cat1
	rm -f ${DESTDIR}/usr/man/cat1/indxbib.0
	ln ${DESTDIR}/usr/man/cat1/lookbib.0 ${DESTDIR}/usr/man/cat1/indxbib.0
	(cd /usr/dict/papers; /bin/sh runinv)

lint: FRC
	lint ${CFLAGS} ${DLSRCS}
	lint ${CFLAGS} ${HTSRCS}
	lint ${CFLAGS} ${INSRCS}
	lint ${CFLAGS} ${MKSRCS}
	lint ${CFLAGS} ${RFSRCS}
#	lint ${CFLAGS} ${RPSRCS}
	lint ${CFLAGS} ${WHSRCS}

tags: FRC
	ctags ${DLSRCS}
	ctags -a ${HTSRCS}
	ctags -a ${INSRCS}
	ctags -a ${MKSRCS}
	ctags -a ${RFSRCS}
#	ctags -a ${RPSRCS}
	ctags -a ${WHSRCS}
	sort -o tags tags
