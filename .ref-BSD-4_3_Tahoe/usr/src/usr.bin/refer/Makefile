#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	4.10	(Berkeley)	12/12/87
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
SRCS=	addbib.c lookbib.c ${DLSRCS} ${HTSRCS} ${INSRCS} ${MKSRCS} ${RFSRCS} \
	${WHSRCS}
OBJS=	addbib.o lookbib.o sortbib.o ${DLOBJS} ${HTOBJS} ${INOBJS} ${MKOBJS} \
	${RFOBJS} ${WHOBJS}

all:	addbib deliv hunt inv lookbib mkey refer sortbib whatabout

addbib: addbib.o
	${CC} ${CFLAGS} addbib.o -o $@

deliv: ${DLOBJS} ${LIBC}
	${CC} ${CFLAGS} ${DLOBJS} -o $@

hunt: ${HTOBJS} ${LIBC}
	${CC} ${CFLAGS} ${HTOBJS} -o $@

inv: ${INOBJS} ${LIBC}
	${CC} ${CFLAGS} ${INOBJS} -o $@

lookbib: lookbib.o
	${CC} ${CFLAGS} lookbib.o -o $@

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

sortbib: sortbib.o
	${CC} ${CFLAGS} sortbib.o -o $@

whatabout: ${WHOBJS} ${LIBC}
	${CC} ${CFLAGS} ${WHOBJS} -o $@

clean: FRC
	rm -f ${OBJS} core addbib deliv hunt inv lookbib mkey refer \
		refpart sortbib whatabout

depend: FRC
	mkdep ${CFLAGS} ${SRCS}

install: FRC
	-[ -d ${DESTDIR}/usr/lib/refer ] || mkdir ${DESTDIR}/usr/lib/refer
	install -s -o bin -g bin -m 755 mkey ${DESTDIR}/usr/lib/refer/mkey
	install -s -o bin -g bin -m 755 inv ${DESTDIR}/usr/lib/refer/inv
	install -s -o bin -g bin -m 755 hunt ${DESTDIR}/usr/lib/refer/hunt
	install -s -o bin -g bin -m 755 refer ${DESTDIR}/usr/bin/refer
	install -s -o bin -g bin -m 755 addbib ${DESTDIR}/usr/bin/addbib
	install -s -o bin -g bin -m 755 sortbib ${DESTDIR}/usr/bin/sortbib
	install -s -o bin -g bin -m 755 lookbib ${DESTDIR}/usr/bin/lookbib
	install -c -o bin -g bin -m 755 roffbib.sh ${DESTDIR}/usr/bin/roffbib
	install -c -o bin -g bin -m 755 indxbib.sh ${DESTDIR}/usr/bin/indxbib
	install -c -o bin -g bin -m 755 tmac.bib ${DESTDIR}/usr/lib/tmac
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

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

addbib.o: addbib.c /usr/include/stdio.h /usr/include/ctype.h
addbib.o: /usr/include/signal.h /usr/include/machine/trap.h
lookbib.o: lookbib.c /usr/include/stdio.h /usr/include/ctype.h
deliv1.o: deliv1.c /usr/include/stdio.h
deliv2.o: deliv2.c /usr/include/stdio.h deliv2.c /usr/include/stdio.h
glue4.o: glue4.c /usr/include/stdio.h /usr/include/ctype.h
glue5.o: glue5.c /usr/include/stdio.h /usr/include/ctype.h
hunt1.o: hunt1.c /usr/include/stdio.h /usr/include/assert.h
hunt2.o: hunt2.c refer..c /usr/include/stdio.h /usr/include/ctype.h
hunt2.o: /usr/include/assert.h
hunt3.o: hunt3.c refer..c /usr/include/stdio.h /usr/include/ctype.h
hunt3.o: /usr/include/assert.h
hunt5.o: hunt5.c /usr/include/stdio.h /usr/include/sys/types.h
hunt5.o: /usr/include/sys/stat.h
hunt6.o: hunt6.c /usr/include/stdio.h /usr/include/assert.h
hunt7.o: hunt7.c /usr/include/stdio.h /usr/include/assert.h
hunt8.o: hunt8.c /usr/include/stdio.h /usr/include/assert.h
hunt9.o: hunt9.c
refer3.o: refer3.c refer..c /usr/include/stdio.h /usr/include/ctype.h
refer3.o: /usr/include/assert.h
shell.o: shell.c
tick.o: tick.c /usr/include/stdio.h /usr/include/sys/types.h
tick.o: /usr/include/sys/timeb.h
inv1.o: inv1.c /usr/include/stdio.h /usr/include/assert.h
inv2.o: inv2.c /usr/include/stdio.h /usr/include/assert.h
inv3.o: inv3.c
inv5.o: inv5.c /usr/include/stdio.h
inv6.o: inv6.c /usr/include/stdio.h /usr/include/assert.h
deliv2.o: deliv2.c /usr/include/stdio.h
mkey1.o: mkey1.c /usr/include/stdio.h
mkey2.o: mkey2.c /usr/include/stdio.h /usr/include/ctype.h
mkey3.o: mkey3.c /usr/include/stdio.h
deliv2.o: deliv2.c /usr/include/stdio.h deliv2.c /usr/include/stdio.h
glue1.o: glue1.c /usr/include/stdio.h
glue2.o: glue2.c
glue3.o: glue3.c refer..c /usr/include/stdio.h /usr/include/ctype.h
glue3.o: /usr/include/assert.h
glue4.o: glue4.c /usr/include/stdio.h /usr/include/ctype.h
glue5.o: glue5.c /usr/include/stdio.h /usr/include/ctype.h
hunt2.o: hunt2.c refer..c /usr/include/stdio.h /usr/include/ctype.h
hunt2.o: /usr/include/assert.h
hunt3.o: hunt3.c refer..c /usr/include/stdio.h /usr/include/ctype.h
hunt3.o: /usr/include/assert.h
hunt5.o: hunt5.c /usr/include/stdio.h /usr/include/sys/types.h
hunt5.o: /usr/include/sys/stat.h
hunt6.o: hunt6.c /usr/include/stdio.h /usr/include/assert.h
hunt7.o: hunt7.c /usr/include/stdio.h /usr/include/assert.h
hunt8.o: hunt8.c /usr/include/stdio.h /usr/include/assert.h
hunt9.o: hunt9.c
mkey3.o: mkey3.c /usr/include/stdio.h
refer0.o: refer0.c refer..c /usr/include/stdio.h /usr/include/ctype.h
refer0.o: /usr/include/assert.h
refer1.o: refer1.c /usr/include/signal.h /usr/include/machine/trap.h refer..c
refer1.o: /usr/include/stdio.h /usr/include/ctype.h /usr/include/assert.h
refer2.o: refer2.c refer..c /usr/include/stdio.h /usr/include/ctype.h
refer2.o: /usr/include/assert.h
refer4.o: refer4.c refer..c /usr/include/stdio.h /usr/include/ctype.h
refer4.o: /usr/include/assert.h
refer5.o: refer5.c refer..c /usr/include/stdio.h /usr/include/ctype.h
refer5.o: /usr/include/assert.h
refer6.o: refer6.c refer..c /usr/include/stdio.h /usr/include/ctype.h
refer6.o: /usr/include/assert.h
refer7.o: refer7.c refer..c /usr/include/stdio.h /usr/include/ctype.h
refer7.o: /usr/include/assert.h
refer8.o: refer8.c refer..c /usr/include/stdio.h /usr/include/ctype.h
refer8.o: /usr/include/assert.h
shell.o: shell.c
what1.o: what1.c what..c /usr/include/stdio.h /usr/include/ctype.h
what1.o: /usr/include/sys/types.h /usr/include/sys/stat.h /usr/include/assert.h
what2.o: what2.c /usr/include/stdio.h /usr/include/ctype.h
what3.o: what3.c what..c /usr/include/stdio.h /usr/include/ctype.h
what3.o: /usr/include/sys/types.h /usr/include/sys/stat.h /usr/include/assert.h
what4.o: what4.c what..c /usr/include/stdio.h /usr/include/ctype.h
what4.o: /usr/include/sys/types.h /usr/include/sys/stat.h /usr/include/assert.h
shell.o: shell.c
mkey3.o: mkey3.c /usr/include/stdio.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
