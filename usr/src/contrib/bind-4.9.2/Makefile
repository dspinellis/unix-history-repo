# Makefile for bind-4.9 top level
# vixie@decwrl December, 1992 [original]
#
# $Id: Makefile,v 4.9.1.16 1993/12/06 00:45:47 vixie Exp $

## ++Copyright++ 1989
## -
## Copyright (c) 1989
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

VER = 4.9.2
SHELL = /bin/sh
MAKE = make
DESTDIR =

# The default build parameters are given for 4.4 BSD.  They should
# be overriden for other operating systems as indicated below.

#(BSD4.4, BSD/386, other net2-alikes)
#
# DO NOT COMMENT THESE OUT.  OVERRIDE THEM BELOW.
#
CC = cc
INDOT =
LEX = lex -I
LIBS = -ll -lutil
PIDDIR = /var/run
DESTBIN = /usr/bin
DESTSBIN = /usr/sbin
DESTEXEC = /usr/libexec
DESTMAN = /usr/share/man
DESTHELP = /usr/share/misc
COMPINCL = compat/include
LDS = 
CATEXT = 0
INSTALL_COMPAT = install
INSTALL = install
RANLIB = ranlib

#(DEC AXP OSF/1)
#LEX = lex
#LIBS = -ll -lutil
#PIDDIR = /var/run
#DESTBIN = /usr/bin
#DESTSBIN = /usr/sbin
#DESTEXEC = /usr/lbin
#DESTMAN = /usr/share/man
#DESTHELP = /usr/share/misc
#COMPINCL = compat/include
#CATEXT = 0
#INSTALL_COMPAT = install-compat
#INSTALL = installbsd

#(irix4)
#CC = cc -xansi -signed -D__STDC__ -D_BSD_SIGNALS -DSIG_FN=int
#LEX = lex
#LIBS = -ll 
#DESTSBIN = /usr/etc
#DESTEXEC = /usr/etc
#DESTMAN = /usr/catman/local
#DESTHELP = /usr/lib
#INSTALL = bsdinst.sh
#RANLIB = :

#(irix5)
#CC = cc -xansi -signed -D__STDC__ -D_BSD_SIGNALS -DSIG_FN=void
#LEX = lex
#LIBS = -ll 
#DESTSBIN = /usr/etc
#DESTEXEC = /usr/etc
#DESTMAN = /usr/share/catman/local
#DESTHELP = /usr/lib
#INSTALL = bsdinst.sh
#RANLIB = :

#(sunos4.x)
#CC = cc -DBSD
#INDOT = in.

#(ultrix, sunos, other 4.[23]bsd-alikes)
#LEX = lex
#LIBS = -ll
#PIDDIR = /etc
#DESTBIN = /usr/bin
#DESTSBIN = /usr/etc
#DESTEXEC = /usr/etc
#DESTMAN = /usr/man
#DESTHELP = /usr/lib
#COMPINCL = compat/include
#CATEXT = $$$$N
#INSTALL_COMPAT = install-compat
#INSTALL = install

#(solaris2.c/sunos5.x)
#use same definitions as for sunos4.x except for the following
#be careful with installation - note in particular that the system ships
#with /usr/sbin/in.named (with in.) and /usr/sbin/named-xfer (without in.)
#to emulate this naming, INDOT should be empty but named should be installed
#as in.named.  also, under solaris2.x, use 'make install' at your own risk.
#CC = cc -DSYSV
#INDOT =
#LIBS = -ll -lnsl -lsocket
#DESTSBIN = /usr/sbin
#DESTEXEC = /usr/sbin
#LDS = :
#RANLIB = :
#ARPREF = `lorder
#ARSUFF = | tsort`

#(osf/1)
#DESTEXEC = /usr/sbin
#PIDDIR = /var/run

#(hpux9.0)
#CC = cc -DSYSV
#DESTSBIN = /usr/etc
#DESTEXEC = /usr/etc
#DESTHELP = /usr/lib

LDFLAGS =
CDEBUG = -O
INCL = include

####################################################################
############ no user servicable parts beyond this point ############
####################################################################

MFLAGS = "CC=${CC}" "CDEBUG=${CDEBUG}" "LIBS=${LIBS}" \
	"INCL=../${INCL}" "INDOT=${INDOT}" \
	"LEX=${LEX}" "SHELL=${SHELL}" "LDFLAGS=${LDFLAGS}" \
	"DESTDIR=${DESTDIR}" "PIDDIR=${PIDDIR}" "DESTMAN=${DESTMAN}" \
	"DESTBIN=${DESTBIN}" "DESTSBIN=${DESTSBIN}" "DESTEXEC=${DESTEXEC}" \
	"INSTALL=${INSTALL}" "DESTHELP=${DESTHELP}" CATEXT='${CATEXT}' \
	"RANLIB=${RANLIB}" "LDS=${LDS}" 'ARPREF=${ARPREF}' 'ARSUFF=${ARSUFF}'

MACHINE = native
DST = $(MACHINE).b
SRC = ..

SUBDIRS = named tools man

all clean depend:: FRC
	@for x in $(SUBDIRS); do \
		(cd $$x; pwd; $(MAKE) $(MFLAGS) $@); \
	done

clean:: FRC
	-test -d doc/bog && (cd doc/bog; pwd; $(MAKE) $(MFLAGS) $@)
	(cd conf; rm -f *~ *.CKP *.BAK *.orig)
	rm -f *~ *.CKP *.BAK *.orig

all:: FRC

depend:: FRC

install: FRC
	@for x in $(SUBDIRS); do \
		y=`if [ "X$$x" = "Xcompat" ]; \
			then echo ${INSTALL_COMPAT}; \
			else echo install; \
		   fi`; \
		(cd $$x; pwd; $(MAKE) $(MFLAGS) $$y); \
	done

tar: FRC
	( cd doc/bog; make file.psf )
	find * -name RCS -print >/tmp/bindtarXf
#	gnutar -c -Z -f ../${VER}.`date +%y%m%d`.tar.Z -X /tmp/bindtarXf .
	gnutar -c -Z -f ../${VER}.tar.Z -X /tmp/bindtarXf .
	rm -f /tmp/bindtarXf

links: $(DST)

$(DST): FRC
	@set -ex; mkdir $(DST); cd $(DST); \
	ln -s $(SRC) SRC; \
 	cp SRC/Makefile .; chmod +w Makefile; \
	ln -s SRC/include SRC/conf .; \
	( mkdir tools; cd tools; ln -s ../SRC/tools SRC; \
	  ln -s SRC/*.[ch] .; \
	  cp SRC/Makefile .; chmod +w Makefile; \
	  ( mkdir nslookup; cd nslookup; ln -s ../SRC/nslookup SRC; \
	    cp SRC/Makefile .; chmod +w Makefile; \
	    ln -s SRC/[a-z]* .; \
	  ); \
	); \
	( mkdir res; cd res; ln -s ../SRC/res SRC; \
	  cp SRC/Makefile .; chmod +w Makefile; \
	  ln -s SRC/*.[ch] .; \
	); \
	( mkdir compat; cd compat; ln -s ../SRC/compat SRC; \
	  cp SRC/Makefile .; chmod +w Makefile; \
	  ln -s SRC/include .; \
	  ( mkdir lib; cd lib; ln -s ../SRC/lib SRC; \
	    cp SRC/Makefile .; chmod +w Makefile; \
	    ln -s SRC/*.c .; \
	  ); \
	); \
	( mkdir named; cd named; ln -s ../SRC/named SRC; \
	  cp SRC/Makefile .; chmod +w Makefile; \
	  ln -s SRC/*.[ch] SRC/*.sh .; \
	); \
	( mkdir man; cd man; ln -s ../SRC/man SRC; \
	  cp SRC/Makefile .; chmod +w Makefile; \
	  ln -s SRC/*.[13578] .; \
	)

FRC:
