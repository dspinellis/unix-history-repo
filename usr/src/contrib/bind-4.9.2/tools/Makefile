#
#	@(#)Makefile	4.9 (Berkeley) 10/10/88
#	$Id: Makefile,v 4.9.1.6 1993/12/06 00:43:14 vixie Exp $
#

## ++Copyright++ 1987
## -
## Copyright (c) 1987
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

DESTDIR=
CC= cc
SHELL= /bin/sh
MAKE= make

CDEBUG= -g

INCL =  ../include

LEX=	lex
LIBS=	-ll
PIDDIR = /etc
DESTBIN = /usr/bin
DESTSBIN = /usr/etc
DESTEXEC = /usr/etc
DESTMAN = /usr/man
DESTHELP= /usr/lib

#(net2 and its descendents)
#LEX = lex -I
#LIBS = -ll -lutil
#PIDDIR = /var/run
#DESTBIN = /usr/bin
#DESTSBIN = /usr/sbin
#DESTEXEC = /usr/libexec
#DESTMAN = /usr/share/man
#DESTHELP= /usr/share/misc

LDFLAGS=
MFLAGS = "CC=$(CC)" "LEX=$(LEX)" "CDEBUG=$(CDEBUG)" "SHELL=$(SHELL)" \
	"INCL=../$(INCL)" "LIBS=$(LIBS)" "LDFLAGS=$(LDFLAGS)" \
	"DESTDIR=$(DESTDIR)" "PIDDIR=${PIDDIR}" "DESTMAN=${DESTMAN}" \
	"DESTBIN=${DESTBIN}" "DESTSBIN=${DESTSBIN}" "DESTEXEC=${DESTEXEC}" \
	"DESTHELP=${DESTHELP}" "INSTALL=${INSTALL}"

CFLAGS=	$(CDEBUG) -I$(INCL)
SRCS=	nsquery.c nstest.c host.c dig.c
NSQUERY_O= nsquery.o
NSTEST_O= nstest.o
HOST_O=	host.o
DIG_O=	dig.o nslookup/list.o nslookup/subr.o nslookup/debug.o nslookup/send.o

SUBDIRS = nslookup 
BINARIES = nsquery nstest host dig

all: ${SUBDIRS} ${BINARIES}

nsquery: ${NSQUERY_O} Makefile
	${CC} ${CFLAGS} ${LDFLAGS} -o $@ ${NSQUERY_O} ${LIBS}

# test programs and utilities
nstest: ${NSTEST_O} Makefile
	${CC} ${CFLAGS} ${LDFLAGS} -o $@ ${NSTEST_O} ${LIBS}

host: ${HOST_O} Makefile
	${CC} ${CFLAGS} ${LDFLAGS} -o $@ ${HOST_O} ${LIBS}

dig: ${DIG_O} ${RES} ${COMPLIB} Makefile
	${CC} ${CFLAGS} ${LDFLAGS} -o $@ ${DIG_O} ${LIBS}

${SUBDIRS}: FRC
	cd $@; make ${MFLAGS}

clean: FRC
	@for x in ${SUBDIRS}; do \
		(cd $$x; pwd; make ${MFLAGS} clean); \
	done
	rm -f ${BINARIES} core
	rm -f *.BAK *.CKP *~ *.o

depend: ${SRCS}
	@for x in ${SUBDIRS}; do \
		(cd $$x; pwd; make ${MFLAGS} depend); \
	done
	mkdep -p -I${INCL} ${SRCS}

install: FRC
	@for x in ${SUBDIRS}; do \
		(cd $$x; pwd; make ${MFLAGS} DESTDIR=${DESTDIR} install); \
	done
	@set -x; for x in ${BINARIES}; do \
		${INSTALL} -s -c -m 755 $$x ${DESTDIR}${DESTBIN}/$$x; \
	done

lint: ${SRCS}
	@(cd nslookup; pwd; make ${MFLAGS} lint)
	lint ${CFLAGS} ${SRCS}

tags: ${SRCS}
	cd nslookup; make ${MFLAGS} tags
	ctags ${SRCS}

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.
