#
#	@(#)Makefile	4.9 (Berkeley) 10/10/88
#	$Id: Makefile,v 1.1 1993/06/01 00:57:47 vixie Exp vixie $
#

## ++Copyright++ 1987
## -
## Copyright (c) 1987 Regents of the University of California.
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

DESTDIR=
CC= cc
SHELL= /bin/sh
CDEBUG= -O
INCL=
RES=
LEX=	lex
LIBS=	
LDFLAGS=
MFLAGS = "CC=$(CC)" "LEX=$(LEX)" "CDEBUG=$(CDEBUG)" "SHELL=$(SHELL)" \
	"LIBS=$(LIBS)" "LDFLAGS=$(LDFLAGS)" \
	"DESTDIR=$(DESTDIR)"
CFLAGS=	$(CDEBUG)
SRCS=	nsquery.c nstest.c host.c dig.c
NSQUERY_O= nsquery.o
NSTEST_O= nstest.o
HOST_O=	host.o
DIG_O=	dig.o nslookup/list.o nslookup/subr.o nslookup/debug.o nslookup/send.o

SUBDIRS = nslookup 
BINARIES = nsquery nstest host dig

all: ${SUBDIRS} ${BINARIES}

nsquery: ${NSQUERY_O} ${RES} Makefile
	${CC} ${CFLAGS} ${LDFLAGS} -o $@ ${NSQUERY_O} ${RES} ${LIBS}

# test programs and utilities
nstest: ${NSTEST_O} ${RES} Makefile
	@echo "The routines in libresolv must be compiled with the -DDEBUG"
	@echo "option or nstest will not have any output."
	${CC} ${CFLAGS} ${LDFLAGS} -o $@ ${NSTEST_O} ${RES} ${LIBS}

host: ${HOST_O} ${RES} Makefile
	${CC} ${CFLAGS} ${LDFLAGS} -o $@ ${HOST_O} ${RES} ${LIBS}

dig: ${DIG_O} ${RES} Makefile
	${CC} ${CFLAGS} ${LDFLAGS} -o $@ ${DIG_O} ${RES} ${LIBS}

${SUBDIRS}: FRC
	cd $@; make ${MFLAGS}

clean: FRC
	for x in ${SUBDIRS}; do \
		(cd $$x; make ${MFLAGS} clean); \
	done
	rm -f ${BINARIES} core
	rm -f *.BAK *.CKP *~ *.o

depend: ${SRCS}
	for x in ${SUBDIRS}; do \
		(cd $$x; make ${MFLAGS} depend); \
	done
	mkdep -p ${SRCS}

install: FRC
	for x in ${SUBDIRS}; do \
		(cd $$x; make ${MFLAGS} DESTDIR=${DESTDIR} install); \
	done
	for x in ${BINARIES}; do \
		install -s -c -m 755 $$x ${DESTDIR}/usr/bin/$$x; \
	done

lint: ${SRCS}
	cd nslookup; make ${MFLAGS} lint
	lint ${CFLAGS} ${SRCS}

tags: ${SRCS}
	cd nslookup; make ${MFLAGS} tags
	ctags ${SRCS}

FRC:
