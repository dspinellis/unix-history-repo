#
#	@(#)Makefile.dist	5.4 (Berkeley) 8/15/90
#	$Id: Makefile,v 1.1 1993/06/01 01:08:20 vixie Exp vixie $
#

## ++Copyright++ 1987, 1988, 1990
## -
## Copyright (c) 1987, 1988, 1990 Regents of the University of California.
## All rights reserved.
## 
## Redistribution and use in source and binary forms, with or without
## modification, are permitted provided that the following conditions
## are met:
## 1. Redistributions of source code must retain the above copyright
##    notice, this list of conditions and the following disclaimer.
## 2. Redistributions in binary form must reproduce the above copyright
##    notice, this list of conditions and the following disclaimer in the
##    documentation and/or other materials provided with the distribution.
## 3. All advertising materials mentioning features or use of this software
##    must display the following acknowledgement:
## 	This product includes software developed by the University of
## 	California, Berkeley and its contributors.
## 4. Neither the name of the University nor the names of its contributors
##    may be used to endorse or promote products derived from this software
##    without specific prior written permission.
## 
## THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
## ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
## IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
## ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
## FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
## DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
## OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
## HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
## LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
## OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
## SUCH DAMAGE.
## -
## Portions Copyright (c) 1993 by Digital Equipment Corporation.
## 
## Permission to use, copy, modify, and distribute this software for any
## purpose with or without fee is hereby granted, provided that the above
## copyright notice and this permission notice appear in all copies, and that
## the name of Digital Equipment Corporation not be used in advertising or
## publicity pertaining to distribution of the document or software without
## specific, written prior permission.
## 
## THE SOFTWARE IS PROVIDED "AS IS" AND DIGITAL EQUIPMENT CORP. DISCLAIMS ALL
## WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES
## OF MERCHANTABILITY AND FITNESS.   IN NO EVENT SHALL DIGITAL EQUIPMENT
## CORPORATION BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL
## DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR
## PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS
## ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
## SOFTWARE.
## -
## --Copyright--

SHELL	= /bin/sh
DESTDIR =
DESTBIN = ${DESTDIR}/usr/etc

CC= cc
SHELL= /bin/sh
CDEBUG= -O
INCL=
RES=
LIBS=
INDOT=
INSTALL= install -c -o bin -g bin -m 755

CFLAGS = ${CDEBUG}

SRCS=	db_dump.c db_load.c db_lookup.c db_reload.c db_save.c db_update.c \
	ns_forw.c ns_init.c ns_main.c ns_maint.c ns_req.c ns_resp.c \
	ns_sort.c ns_stats.c db_glue.c storage.c dmalloc.c
HDRS=	db.h ns.h
OBJS=	db_dump.o db_load.o db_lookup.o db_reload.o db_save.o db_update.o \
	ns_forw.o ns_init.o ns_main.o ns_maint.o ns_req.o ns_resp.o \
	ns_sort.o ns_stats.o db_glue.o storage.o dmalloc.o
XFERSRCS=  named-xfer.c db_glue.c
XFEROBJ=   named-xfer.o db_glue.o storage.o dmalloc.o
MAN=	named-xfer.0 named.reload.0 named.restart.0

all: named named-xfer

named: ${OBJS} ${RES} version.o
	${CC} ${CDEBUG} ${LDFLAGS} -o $@ version.o ${OBJS} ${RES} ${LIBS}

version.o: version.c

version.c: Version.c Makefile
	(u=$${USER-root} d=`pwd` h=`hostname` t=`date`; \
	sed -e "s|%WHEN%|$${t}|" -e "s|%VERSION%|${VER}|" \
	    -e "s|%WHOANDWHERE%|$${u}@$${h}:$${d}|" \
	    < Version.c > version.c)

named-xfer:	${XFEROBJ} ${RES}
	${CC} ${CDEBUG} ${LDFLAGS} -o $@ ${XFEROBJ} ${RES} ${LIBS}

centerline_named:
	#load -I../include ${CFLAGS} ${SRCS} version.c ${RES} ${LIBS}

centerline_obj:
	#load -I../include ${CFLAGS} ${OBJS} version.o ${RES} ${LIBS}

centerline_xfer:
	#load -DXFER ${CFLAGS} ${XFERSRCS} ${RES} ${LIBS}

clean:
	rm -f ${OBJS} ${XFEROBJ} core named named-xfer version.o version.c
	rm -f *~ *.BAK *.CKP
	rm -f ${MAN} tags .depend core
	rm -f *.orig

depend .depend: ${SRCS} ${XFERSRCS}
	mkdep ${SRCS} ${XFERSRCS}

install: ${MAN}
	$(INSTALL) -s named ${DESTDIR}/usr/sbin/${INDOT}named
	$(INSTALL) -s named-xfer ${DESTDIR}/usr/libexec/${INDOT}named-xfer
	install -c -o bin -g bin -m 555 named.reload.reno \
	    ${DESTDIR}/usr/sbin/named.reload
	install -c -o bin -g bin -m 555 named.restart.reno \
	    ${DESTDIR}/usr/sbin/named.restart
	install -c -o bin -g bin -m 444 $(MAN) ${DESTDIR}/usr/share/man/cat8

named-xfer.0: named-xfer.8
	nroff -man named-xfer.8 > named-xfer.0

named.restart.0: named.restart.8
	nroff -man named.restart.8 > named.restart.0

named.reload.0: named.reload.8
	nroff -man named.reload.8 > named.reload.0

lint: ${SRCS} ${HDRS} ${XFERSRCS}
	lint -Dlint ${CFLAGS} ${SRCS} ${XFERSRCS}

tags: ${SRCS} ${XFERSRCS} Makefile
	ctags -t `echo ${SRCS} ${HDRS} ${XFERSRCS}|tr ' ' '\012'|sort -u`

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.
