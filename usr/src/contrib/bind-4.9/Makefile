# Makefile for bind-4.9 top level
# vixie@decwrl December, 1992 [original]
#
# $Id: Makefile,v 1.1 1993/06/01 00:11:11 vixie Exp vixie $

## ++Copyright++ 1989
## -
## Copyright (c) 1989 Regents of the University of California.
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

# this makefile has been expunged of a lot of the detail needed to build
# on systems other than 4.4bsd.  the official bind kit is on gatekeeper.dec.com
# in ~ftp/pub/BSD/bind.  get that one if you are trying to get this stuff to
# work on random non-4.4bsd computers.

VER = 4.9.1
SHELL = /bin/sh

#(BSD4.4, BSD/386, BSD4.3-Reno)
LEX = lex -I
LIBS = -ll -lutil
COMPAT =

MAKE=make
CC=cc
LDFLAGS =
CDEBUG = -O
TOP = .
MFLAGS = "CC=${CC}" "CDEBUG=${CDEBUG}" "LIBS=${LIBS}" "VER=${VER}" \
	"INDOT=${INDOT}" "LEX=${LEX}" \
	"SHELL=${SHELL}" "LDFLAGS=${LDFLAGS}" "COMPAT=${COMPAT}"

MACHINE = native
DST = $(MACHINE).b
SRC = ..

SUBDIRS = tools named man

all clean install depend: FRC
	@set -ex; for x in `basename "${COMPAT}"` $(SUBDIRS); do \
		(cd $$x; $(MAKE) $(MFLAGS) $@); \
	done

FRC:
