#
#	@(#)Makefile	5.20 (Berkeley) 10/2/89
#	$Id: Makefile,v 1.1 1993/06/01 10:28:02 vixie Exp vixie $
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

DESTDIR =
DESTBIN = ${DESTDIR}/usr/bin
DESTHELP= ${DESTDIR}/usr/share/misc
DESTMAN8= ${DESTDIR}/usr/share/man/man8
DESTCAT8= ${DESTDIR}/usr/share/man/cat8
MAN8EXT = 8

CC= cc
SHELL= /bin/sh
CDEBUG= -O
LDFLAGS =

LIBS= -ll -lutil
DEFS=

CFLAGS=	${CDEBUG} ${DEFS}
CSRCS=	main.c getinfo.c debug.c send.c skip.c list.c subr.c
SRCS=	${CSRCS} commands.c
OBJS=	main.o getinfo.o debug.o send.o skip.o list.o subr.o commands.o

all: nslookup 

nslookup: ${OBJS} ${RES}
	${CC} ${CDEBUG} ${LDFLAGS} -o $@ ${OBJS} ${RES} ${LIBS}

clean:
	rm -f ${OBJS} core nslookup commands.c lex.yy.c lex.yy.o
	rm -f *.BAK *.CKP *~

cleandir: clean
	rm -f tags .depend

depend: ${SRCS}
	mkdep ${DEFS} ${SRCS}

install: nslookup
	install -c -s -o bin -g bin -m 755 nslookup ${DESTBIN}/nslookup
	install -c -o bin -g bin -m 444 nslookup.help ${DESTHELP}/nslookup.help

lint: ${SRCS}
	lint ${SRCS}

tags: ${CSRCS}
	ctags ${CSRCS}
