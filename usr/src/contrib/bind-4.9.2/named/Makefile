#
#	@(#)Makefile.dist	5.4 (Berkeley) 8/15/90
#	$Id: Makefile,v 4.9.1.7 1993/12/06 00:43:02 vixie Exp $
#

## ++Copyright++ 1987, 1988, 1990
## -
## Copyright (c) 1987, 1988, 1990
##    The Regents of the University of California.  All rights reserved.
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
VER	= 4.9.2

INDOT=
#(sunos)
#INDOT=in.

HOSTNAMECMD = hostname || uname -n

#(sunos, ultrix, other 4.[23] descendents, irix4, irix5)
#DESTSBIN = ${DESTDIR}/usr/etc
#DESTEXEC = ${DESTDIR}/usr/etc
#PIDDIR = /etc
INCL=	../include

#(bsd/386, 4.4bsd, other net2 descendents)
DESTSBIN = ${DESTDIR}/usr/sbin
DESTEXEC = ${DESTDIR}/usr/libexec
PIDDIR = /var/run
#INCL= 

CC= cc
SHELL= /bin/sh
CDEBUG= -O
LIBS=
_PATH_XFER = ${DESTEXEC}/${INDOT}named-xfer
DEFS = -D_PATH_XFER=\"${_PATH_XFER}\"

#(sgi irix4)
#CC = cc -xansi -signed -D__STDC__ -D_BSD_SIGNALS

#(sgi irix5)
#CC = cc -xansi -signed -D__BIT_TYPES_DEFINED__ -D_BSD_SIGNALS

CFLAGS = ${CDEBUG} -I${INCL} ${DEFS}

HDRS=	db_defs.h db_glob.h ns_defs.h ns_glob.h named.h pathnames.h
SRCS=	db_dump.c db_load.c db_lookup.c db_reload.c db_save.c db_update.c \
	ns_forw.c ns_init.c ns_main.c ns_maint.c ns_req.c ns_resp.c \
	ns_sort.c ns_stats.c db_glue.c storage.c dmalloc.c ns_validate.c \
	ns_ncache.c db_secure.c
OBJS=	db_dump.o db_load.o db_lookup.o db_reload.o db_save.o db_update.o \
	ns_forw.o ns_init.o ns_main.o ns_maint.o ns_req.o ns_resp.o \
	ns_sort.o ns_stats.o db_glue.o storage.o dmalloc.o ns_validate.o \
	ns_ncache.o db_secure.o
XFERSRCS=  named-xfer.c db_glue.c
XFEROBJ=   named-xfer.o db_glue.o storage.o dmalloc.o

all: named named-xfer named.reload named.restart

named: ${OBJS} version.o
	${CC} ${CDEBUG} ${LDFLAGS} -o $@ version.o ${OBJS} ${LIBS}

version.o: version.c

version.c: Version.c Makefile ${SRCS} ${HDRS}
	(u=$${USER-root} d=`pwd` h=`${HOSTNAMECMD}` t=`date`; \
	sed -e "s|%WHEN%|$${t}|" -e "s|%VERSION%|${VER}|" \
	    -e "s|%WHOANDWHERE%|$${u}@$${h}:$${d}|" \
	    < Version.c > version.c)

named.reload: named.reload.sh Makefile
	sed -e "s|%PIDDIR%|${PIDDIR}|" \
	    < named.reload.sh > named.reload

named.restart: named.restart.sh Makefile
	sed -e "s|%PIDDIR%|${PIDDIR}|" \
	    -e "s|%INDOT%|${INDOT}|" \
	    -e "s|%DESTSBIN%|${DESTSBIN}|" \
	    < named.restart.sh > named.restart

named-xfer:	${XFEROBJ}
	${CC} ${CDEBUG} ${LDFLAGS} -o $@ ${XFEROBJ} ${LIBS}

clean:
	rm -f ${OBJS} ${XFEROBJ} core named named-xfer version.o version.c
	rm -f *~ *.BAK *.CKP
	rm -f tags .depend core named.reload named.restart
	rm -f *.orig

depend .depend: ${SRCS} ${XFERSRCS}
	mkdep -I${INCL} ${SRCS} ${XFERSRCS}

install:
	${INSTALL} -c -s -o bin -g bin -m 555 \
		named ${DESTDIR}${DESTSBIN}/${INDOT}named
	${INSTALL} -c -s -o bin -g bin -m 555 \
		named-xfer ${DESTDIR}${_PATH_XFER}
	${INSTALL} -c -o bin -g bin -m 555 \
		named.restart ${DESTDIR}${DESTSBIN}/${INDOT}named.restart
	${INSTALL} -c -o bin -g bin -m 555 \
		named.reload ${DESTDIR}${DESTSBIN}/${INDOT}named.reload
	@echo "*** Install symlinks if needed ***"

lint: ${SRCS} ${HDRS} ${XFERSRCS}
	lint -Dlint ${CFLAGS} ${SRCS} ${XFERSRCS}

tags: ${SRCS} ${XFERSRCS} Makefile
	ctags -t `echo ${SRCS} ${HDRS} ${XFERSRCS}|tr ' ' '\012'|sort -u`

$(SRCS):: $(HDRS)

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.
