# This file was generated automatically by configure.  Do not edit.
VPATH = .
links = 
host_alias = hp300bsd
host_cpu = m68k
host_vendor = hp
host_os = bsd
target_alias = hp300bsd
target_cpu = m68k
target_vendor = hp
target_os = bsd
ALL=all.internal
#
# Makefile
#   Copyright (C) 1990, 1991, 1992 Free Software Foundation
#
# This file is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */
#

srcdir = .

prefix = /usr/local

exec_prefix = $(prefix)
bindir = $(exec_prefix)/bin
libdir = $(exec_prefix)/lib

datadir = $(prefix)/lib
mandir = $(prefix)/man
man1dir = $(mandir)/man1
man2dir = $(mandir)/man2
man3dir = $(mandir)/man3
man4dir = $(mandir)/man4
man5dir = $(mandir)/man5
man6dir = $(mandir)/man6
man7dir = $(mandir)/man7
man8dir = $(mandir)/man8
man9dir = $(mandir)/man9
infodir = $(prefix)/info
includedir = $(prefix)/include
docdir = $(datadir)/doc

MKDOC=./chew
SHELL = /bin/sh

INSTALL = install -c
INSTALL_PROGRAM = $(INSTALL)
INSTALL_DATA = $(INSTALL)

AR = ar
AR_FLAGS = qv
BISON = bison
MAKEINFO = makeinfo
RANLIB = ranlib
CFLAGS = -g

#### Host, target, and site specific Makefile fragments come in here.
###

.c.o:
	$(CC) $(CFLAGS) -c $(H_CFLAGS) -I.. -I$(srcdir)/.. -I$(srcdir)/../../include $<

# main GDB source directory

DOCFILES = aoutx.texi  archive.texi archures.texi \
	bfd.texi  cache.texi coffcode.texi \
	core.texi  format.texi  libbfd.texi  \
	opncls.texi  reloc.texi  section.texi  \
	syms.texi  targets.texi init.texi ctor.texi

PROTOS = archive.p archures.p bfd.p \
	 core.p format.p \
	libbfd.p opncls.p reloc.p \
	section.p syms.p targets.p  \
	format.p  core.p init.p

IPROTOS = cache.ip libbfd.ip reloc.ip init.ip archures.ip ctor.ip coffcode.ip

# SRCDOC, SRCPROT, SRCIPROT only used to sidestep Sun Make bug in interaction 
# between VPATH and suffix rules.  If you use GNU Make, perhaps other Makes,
# you don't need these three:
SRCDOC = $(srcdir)/../aoutx.h  $(srcdir)/../archive.c \
	$(srcdir)/../archures.c $(srcdir)/../bfd.c \
	$(srcdir)/../cache.c $(srcdir)/../coffcode.h \
	$(srcdir)/../core.c  $(srcdir)/../format.c \
	$(srcdir)/../libbfd.c $(srcdir)/../opncls.c \
	$(srcdir)/../reloc.c  $(srcdir)/../section.c \
	$(srcdir)/../syms.c  $(srcdir)/../targets.c \
	$(srcdir)/../init.c

SRCPROT = $(srcdir)/../archive.c $(srcdir)/../archures.c \
	$(srcdir)/../bfd.c $(srcdir)/../coffcode.h $(srcdir)/../core.c \
	$(srcdir)/../format.c $(srcdir)/../libbfd.c \
	$(srcdir)/../opncls.c $(srcdir)/../reloc.c \
	$(srcdir)/../section.c $(srcdir)/../syms.c \
	$(srcdir)/../targets.c $(srcdir)/../init.c    

SRCIPROT = $(srcdir)/../cache.c $(srcdir)/../libbfd.c \
	$(srcdir)/../reloc.c $(srcdir)/../cpu-h8300.c \
	$(srcdir)/../cpu-i960.c $(srcdir)/../archures.c \
	$(srcdir)/../init.c $(srcdir)/../ctor.c

STAGESTUFF = $(DOCFILES) *.info*

all install:

info: bfd.info

install-info: info
	-parent=`echo $(infodir)|sed -e 's@/[^/]*$$@@'`; \
	if [ -d $$parent ] ; then true ; else mkdir $$parent ; fi
	-if [ -d $(infodir) ] ; then true ; else mkdir $(infodir) ; fi
	for i in *.info* ; do \
		$(INSTALL_DATA) $$i $(infodir)/$$i ; \
	done

docs: $(MKDOC) protos bfd.info bfd.dvi bfd.ps

$(MKDOC): chew.c
	$(CC) $(CFLAGS) -o $(MKDOC) $(H_CFLAGS) -I.. -I$(srcdir)/.. -I$(srcdir)/../../include $(srcdir)/chew.c $(LOADLIBES)

protos: libbfd.h libcoff.h bfd.h

aoutx.texi: $(MKDOC) $(srcdir)/../aoutx.h $(srcdir)/doc.str
	$(MKDOC) -f $(srcdir)/doc.str <$(srcdir)/../aoutx.h >aoutx.texi

archive.texi: $(MKDOC) $(srcdir)/../archive.c $(srcdir)/doc.str
	$(MKDOC) -f $(srcdir)/doc.str <$(srcdir)/../archive.c >archive.texi 

archures.texi:  $(srcdir)/../archures.c $(srcdir)/doc.str
	$(MKDOC) -f $(srcdir)/doc.str < $(srcdir)/../archures.c >archures.texi 

bfd.texi: $(MKDOC) $(srcdir)/../bfd.c $(srcdir)/doc.str
	$(MKDOC) -f $(srcdir)/doc.str < $(srcdir)/../bfd.c >bfd.texi

cache.texi: $(MKDOC) $(srcdir)/../cache.c $(srcdir)/doc.str
	$(MKDOC) -f $(srcdir)/doc.str < $(srcdir)/../cache.c >cache.texi

coffcode.texi:	$(MKDOC) $(srcdir)/../coffcode.h $(srcdir)/doc.str
	$(MKDOC) -f $(srcdir)/doc.str <$(srcdir)/../coffcode.h >coffcode.texi

core.texi: $(MKDOC) $(srcdir)/../core.c $(srcdir)/doc.str
	$(MKDOC) -f $(srcdir)/doc.str <$(srcdir)/../core.c >core.texi

format.texi: $(MKDOC) $(srcdir)/../format.c $(srcdir)/doc.str
	$(MKDOC) -f $(srcdir)/doc.str <$(srcdir)/../format.c >format.texi

libbfd.texi: $(MKDOC) $(srcdir)/../libbfd.c $(srcdir)/doc.str
	$(MKDOC) -f $(srcdir)/doc.str < $(srcdir)/../libbfd.c >libbfd.texi 

opncls.texi: $(MKDOC) $(srcdir)/../opncls.c $(srcdir)/doc.str
	$(MKDOC) -f $(srcdir)/doc.str  <$(srcdir)/../opncls.c >opncls.texi

reloc.texi : $(MKDOC) $(srcdir)/../reloc.c
	$(MKDOC) -f $(srcdir)/doc.str <$(srcdir)/../reloc.c >reloc.texi 

section.texi: $(MKDOC) $(srcdir)/../section.c $(srcdir)/doc.str
	$(MKDOC) -f $(srcdir)/doc.str <$(srcdir)/../section.c >section.texi 

syms.texi : $(MKDOC) $(srcdir)/../syms.c
	$(MKDOC) -f $(srcdir)/doc.str <$(srcdir)/../syms.c >syms.texi

targets.texi: $(MKDOC) $(srcdir)/../targets.c $(srcdir)/doc.str
	$(MKDOC) -f $(srcdir)/doc.str <$(srcdir)/../targets.c >targets.texi

init.texi: $(MKDOC) $(srcdir)/../init.c $(srcdir)/doc.str
	$(MKDOC) -f $(srcdir)/doc.str <$(srcdir)/../init.c >init.texi

ctor.texi: $(MKDOC) $(srcdir)/../ctor.c $(srcdir)/doc.str
	$(MKDOC) -f $(srcdir)/doc.str <$(srcdir)/../ctor.c >ctor.texi


libbfd.h: $(srcdir)/../libbfd-in.h	\
	$(srcdir)/../init.c		\
	$(srcdir)/../libbfd.c		\
	$(srcdir)/../init.c		\
	$(srcdir)/../libbfd.c		\
	$(srcdir)/../cache.c		\
	$(srcdir)/../ctor.c		\
	$(srcdir)/../reloc.c		\
	$(srcdir)/../cpu-h8300.c	\
	$(srcdir)/../cpu-i960.c		\
	$(srcdir)/../archures.c		\
	$(srcdir)/../elf.c		\
	$(MKDOC)
	cat $(srcdir)/../libbfd-in.h >libbfd.h 
	$(MKDOC) -i -f $(srcdir)/proto.str  < $(srcdir)/../init.c >>libbfd.h
	$(MKDOC) -i -f $(srcdir)/proto.str  < $(srcdir)/../libbfd.c >>libbfd.h
	$(MKDOC) -i -f $(srcdir)/proto.str  < $(srcdir)/../init.c >>libbfd.h
	$(MKDOC) -i -f $(srcdir)/proto.str  < $(srcdir)/../libbfd.c >>libbfd.h
	$(MKDOC) -i -f $(srcdir)/proto.str  < $(srcdir)/../cache.c >>libbfd.h
	$(MKDOC) -i -f $(srcdir)/proto.str  < $(srcdir)/../ctor.c >>libbfd.h
	$(MKDOC) -i -f $(srcdir)/proto.str  < $(srcdir)/../reloc.c >>libbfd.h
	$(MKDOC) -i -f $(srcdir)/proto.str  < $(srcdir)/../cpu-h8300.c >>libbfd.h
	$(MKDOC) -i -f $(srcdir)/proto.str  < $(srcdir)/../cpu-i960.c >>libbfd.h
	$(MKDOC) -i -f $(srcdir)/proto.str  < $(srcdir)/../archures.c >>libbfd.h
	$(MKDOC) -i -f $(srcdir)/proto.str  < $(srcdir)/../elf.c >>libbfd.h

libcoff.h: $(srcdir)/../libcoff-in.h	\
	$(srcdir)/../coffcode.h \
	$(MKDOC)
	cat $(srcdir)/../libcoff-in.h >libcoff.h
	$(MKDOC) -i -f $(srcdir)/proto.str  < $(srcdir)/../coffcode.h >>libcoff.h

bfd.h: $(srcdir)/../bfd-in.h 		\
	$(srcdir)/../init.c 		\
	$(srcdir)/../opncls.c 		\
	$(srcdir)/../libbfd.c 		\
	$(srcdir)/../section.c 		\
	$(srcdir)/../archures.c		\
	$(srcdir)/../reloc.c		\
	$(srcdir)/../syms.c		\
	$(srcdir)/../archive.c		\
	$(srcdir)/../core.c		\
	$(srcdir)/../targets.c		\
	$(srcdir)/../format.c		\
	$(MKDOC)
	cat $(srcdir)/../bfd-in.h >bfd.h
	$(MKDOC) -f $(srcdir)/proto.str<$(srcdir)/../init.c >>bfd.h
	$(MKDOC) -f $(srcdir)/proto.str<$(srcdir)/../opncls.c >>bfd.h
	$(MKDOC) -f $(srcdir)/proto.str<$(srcdir)/../libbfd.c >>bfd.h
	$(MKDOC) -f $(srcdir)/proto.str<$(srcdir)/../section.c >>bfd.h
	$(MKDOC) -f $(srcdir)/proto.str<$(srcdir)/../archures.c >>bfd.h
	$(MKDOC) -f $(srcdir)/proto.str<$(srcdir)/../reloc.c >>bfd.h
	$(MKDOC) -f $(srcdir)/proto.str<$(srcdir)/../syms.c >>bfd.h
	$(MKDOC) -f $(srcdir)/proto.str<$(srcdir)/../bfd.c >>bfd.h
	$(MKDOC) -f $(srcdir)/proto.str<$(srcdir)/../archive.c >>bfd.h
	$(MKDOC) -f $(srcdir)/proto.str<$(srcdir)/../core.c >>bfd.h
	$(MKDOC) -f $(srcdir)/proto.str<$(srcdir)/../targets.c >>bfd.h
	$(MKDOC) -f $(srcdir)/proto.str<$(srcdir)/../format.c >>bfd.h	
	echo "#endif" >>bfd.h


clean-info:
	rm -f $(STAGESTUFF) *.p *.ip  bfd.dvi bfd.ps *~* *# bfd.?? \
	bfd.??? $(MKDOC) bfd.h libbfd.h libcoff.h texput.log 

clean:

bfd.info: $(DOCFILES) bfd.texinfo
	$(MAKEINFO) -o bfd.info $(srcdir)/bfd.texinfo

bfd.dvi: $(DOCFILES) bfd.texinfo
	TEXINPUTS=${TEXIDIR}:.:$$TEXINPUTS tex bfd.texinfo
#	texindex bfd.??
#	TEXINPUTS=${TEXIDIR}:.:$$TEXINPUTS tex bfd.texinfo

bfd.ps: bfd.dvi
	dvips bfd -o
	
quickdoc: $(DOCFILES) bfd.texinfo
	TEXINPUTS=${TEXIDIR}:.:$$TEXINPUTS tex bfd.texinfo

stage1: force
	- mkdir stage1
	- mv -f $(STAGESTUFF) stage1

stage2: force
	- mkdir stage2
	- mv -f $(STAGESTUFF) stage2

stage3: force
	- mkdir stage3
	- mv -f $(STAGESTUFF) stage3

against=stage2

comparison: force
	for i in $(STAGESTUFF) ; do cmp $$i $(against)/$$i || exit 1 ; done

de-stage1: force
	- (cd stage1 ; mv -f $(STAGESTUFF) ..)
	- rmdir stage1

de-stage2: force
	- (cd stage2 ; mv -f $(STAGESTUFF) ..)
	- rmdir stage2

de-stage3: force
	- (cd stage3 ; mv -f $(STAGESTUFF) ..)
	- rmdir stage3

force:

Makefile: $(srcdir)/Makefile.in $(host_makefile_frag) $(target_makefile_frag)
	$(SHELL) ./config.status

