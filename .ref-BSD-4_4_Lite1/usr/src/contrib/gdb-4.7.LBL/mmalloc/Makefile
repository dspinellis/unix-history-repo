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
#   Copyright 1992 Free Software Foundation, Inc.
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

#
# Makefile for mmalloc directory
#

# Directory containing source files.  Don't clean up the spacing,
# this exact string is matched for by the "configure" script.
srcdir = .

prefix = /usr/local

exec_prefix = $(prefix)

bindir =	$(exec_prefix)/bin
libdir =	$(exec_prefix)/lib

datadir =	$(prefix)/lib
mandir =	$(prefix)/man
man1dir =	$(mandir)/man1
man2dir =	$(mandir)/man2
man3dir =	$(mandir)/man3
man4dir =	$(mandir)/man4
man5dir =	$(mandir)/man5
man6dir =	$(mandir)/man6
man7dir =	$(mandir)/man7
man8dir =	$(mandir)/man8
man9dir =	$(mandir)/man9
infodir =	$(prefix)/info
includedir =	$(prefix)/include
docdir =	$(datadir)/doc

SHELL =		/bin/sh

INSTALL =	install -c
INSTALL_PROGRAM = $(INSTALL)
INSTALL_DATA =	$(INSTALL)

AR =		ar
AR_FLAGS =	qv
CFLAGS =	-g
BISON =		bison
MAKEINFO =	makeinfo
RANLIB =	ranlib
RM =		rm

TARGETLIB =	libmmalloc.a

CFILES =	mcalloc.c mfree.c mmalloc.c mmcheck.c mmemalign.c mmstats.c \
		mmtrace.c mrealloc.c mvalloc.c mmap-sup.c attach.c detach.c \
		keys.c sbrk-sup.c

HFILES =	mmalloc.h

OFILES =	mcalloc.o mfree.o mmalloc.o mmcheck.o mmemalign.o mmstats.o \
		mmtrace.o mrealloc.o mvalloc.o mmap-sup.o attach.o detach.o \
		keys.o sbrk-sup.o

#### Host, target, and site specific Makefile fragments come in here.
###

.c.o:
	$(CC) -c $(CFLAGS) -I. -I$(srcdir)/../include $(HDEFINES) $<

# Do we want/need any config overrides?
#	 

STAGESTUFF =	$(TARGETLIB) *.o

all:		$(TARGETLIB)

info:
clean-info:
install-info:
check:

install:	all
		-parent=`echo $(libdir)|sed -e 's@/[^/]*$$@@'`; \
		if [ -d $$parent ] ; then true ; else mkdir $$parent ; fi
		-if [ -d $(libdir) ] ; then true ; else mkdir $(libdir) ; fi
		$(INSTALL_DATA) $(TARGETLIB) $(libdir)/$(TARGETLIB).n
		$(RANLIB) $(libdir)/$(TARGETLIB).n
		mv -f $(libdir)/$(TARGETLIB).n $(libdir)/$(TARGETLIB)

$(TARGETLIB):	$(OFILES)
		$(RM) -rf $@
		$(AR) $(AR_FLAGS) $@ $(OFILES)
		$(RANLIB) $@

$(OFILES) :	$(HFILES)

.always.:
# Do nothing.

.PHONEY: all etags tags ls clean stage1 stage2 .always.

stage1:		force
		-mkdir stage1
		-mv -f $(STAGESTUFF) stage1

stage2:		force
		-mkdir stage2
		-mv -f $(STAGESTUFF) stage2

stage3:		force
		-mkdir stage3
		-mv -f $(STAGESTUFF) stage3

stage4:		force
		-mkdir stage4
		-mv -f $(STAGESTUFF) stage4

against=stage2

comparison:	force
		for i in *.o ; do cmp $$i $(against)/$$i || exit 1 ; done

de-stage1:	force
		-(cd stage1 ; mv -f * ..)
		-rmdir stage1

de-stage2:	force
		-(cd stage2 ; mv -f * ..)
		-rmdir stage2

de-stage3:	force
		-(cd stage3 ; mv -f * ..)
		-rmdir stage3

de-stage4:	force
		-(cd stage4 ; mv -f * ..)
		-rmdir stage4

etags tags:	TAGS

TAGS:		$(CFILES)
		etags $(HFILES) $(CFILES)

ls:
		@echo Makefile $(HFILES) $(CFILES)

# Need to deal with profiled libraries, too.

clean:
		rm -f *.a *.o core errs *~ \#* TAGS *.E a.out errors 

force:

Makefile:	$(srcdir)/Makefile.in $(host_makefile_frag) \
		$(target_makefile_frag)
		$(SHELL) ./config.status
