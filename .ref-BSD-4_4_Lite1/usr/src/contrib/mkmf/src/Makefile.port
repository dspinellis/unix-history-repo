# Copyright (c) 1991, 1993 Peter J. Nicklin.
# Copyright (c) 1991, 1993 Version Technology.
# All Rights Reserved.
#
# $License: VT.1.1 $
# Redistribution and use in source and binary forms,  with or without
# modification,  are permitted provided that the following conditions
# are met:  (1) Redistributions of source code must retain the  above
# copyright  notice,  this  list  of  conditions  and  the  following
# disclaimer.  (2) Redistributions in binary form must reproduce  the
# above  copyright notice,  this list of conditions and the following
# disclaimer in the  documentation  and/or other  materials  provided
# with  the  distribution.  (3) All advertising materials  mentioning
# features or  use  of  this  software  must  display  the  following
# acknowledgement:  ``This  product  includes  software  developed by
# Version Technology.''  Neither the name of Version  Technology  nor
# the  name  of  Peter J. Nicklin  may  be used to endorse or promote
# products derived from this software without specific prior  written
# permission.
#
# THIS SOFTWARE IS PROVIDED BY VERSION TECHNOLOGY ``AS IS''  AND  ANY
# EXPRESS OR IMPLIED WARRANTIES,  INCLUDING,  BUT NOT LIMITED TO, THE
# IMPLIED  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
# PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL  VERSION  TECHNOLOGY  BE
# LIABLE  FOR ANY DIRECT,  INDIRECT,  INCIDENTAL, SPECIAL, EXEMPLARY,
# OR  CONSEQUENTIAL DAMAGES   (INCLUDING,   BUT   NOT   LIMITED   TO,
# PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;  LOSS OF USE, DATA, OR
# PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY
# OF  LIABILITY,  WHETHER  IN  CONTRACT,  STRICT LIABILITY,  OR  TORT
# (INCLUDING NEGLIGENCE OR OTHERWISE)  ARISING  IN ANY WAY OUT OF THE
# USE OF THIS SOFTWARE,  EVEN  IF  ADVISED OF THE POSSIBILITY OF SUCH
# DAMAGE.
#
# Report problems and direct questions to nicklin@netcom.com
#
# $Header: Makefile,v 4.5 93/05/26 00:19:08 nicklin Exp $
#
# Author: Peter J. Nicklin
#
CFLAGS	      =	-O -DINSTALLDIR=\"$(INSTALLDIR)\"

#CFLAGS	      =	-O -DUNICOS5 -DINSTALLDIR=\"$(INSTALLDIR)\" # Cray Unicos 5.x

#CFLAGS       = -O -DSYSV -DINSTALLDIR=\"$(INSTALLDIR)\" # Apollo Domain/OS (sys5)

#CFLAGS	      =	-O -DBSD4X -DINSTALLDIR=\"$(INSTALLDIR)\" # Generic BSD

DEST	      = $(DESTDIR)$(INSTALLDIR)/bin

EXTHDRS	      =

HDRS	      = Mkmf.h \
		RELEASE.h \
		config.h \
		defaultrul.h \
		defaultsfx.h \
		dir.h \
		dlist.h \
		getarg.h \
		hash.h \
		inckey.h \
		macro.h \
		null.h \
		path.h \
		rule.h \
		slist.h \
		stringx.h \
		suffix.h \
		target.h \
		true.h \
		yesno.h

INSTALL	      = cp

INSTALLDIR    = /usr/contrib

LD	      = $(CC)

LDFLAGS	      =

LIBS	      =

MAKEFILE      = Makefile

OBJS	      = Mkmf.o \
		badopt.o \
		buildlist.o \
		depend.o \
		dlist.o \
		editmf.o \
		getcwp.o \
		getproject.o \
		hash.o \
		iolin.o \
		macro.o \
		misc.o \
		mksrclist.o \
		mksymlink.o \
		mustfopen.o \
		optpath.o \
		pathcat.o \
		pathhead.o \
		pperror.o \
		rule.o \
		slappend.o \
		slinit.o \
		slrm.o \
		slsort.o \
		slvect.o \
		slvtol.o \
		strpcpy.o \
		strsav.o \
		suffix.o \
		usage.o \
		warn.o

PRINT	      = pr

PROGRAM       = mkmf

SHELL	      = /bin/sh

SRCS	      = Mkmf.c \
		badopt.c \
		buildlist.c \
		depend.c \
		dlist.c \
		editmf.c \
		getcwp.c \
		getproject.c \
		hash.c \
		iolin.c \
		macro.c \
		misc.c \
		mksrclist.c \
		mksymlink.c \
		mustfopen.c \
		optpath.c \
		pathcat.c \
		pathhead.c \
		pperror.c \
		rule.c \
		slappend.c \
		slinit.c \
		slrm.c \
		slsort.c \
		slvect.c \
		slvtol.c \
		strpcpy.c \
		strsav.c \
		suffix.c \
		usage.c \
		warn.c

all:		$(PROGRAM)

$(PROGRAM):     $(OBJS) $(LIBS)
		@echo "Linking $(PROGRAM) ..."
		@$(LD) $(LDFLAGS) $(OBJS) $(LIBS) -o $(PROGRAM)
		@echo "done"

clean:;		@rm -f $(OBJS) core

clobber:;	@rm -f $(OBJS) $(PROGRAM) core tags

depend:;	@mkmf -f $(MAKEFILE)

echo:;		@echo $(HDRS) $(SRCS)

index:;		@ctags -wx $(HDRS) $(SRCS)

install:	$(PROGRAM)
		@echo Installing $(PROGRAM) in $(DEST)
		@-strip $(PROGRAM)
		@if [ $(DEST) != . ]; then \
		(rm -f $(DEST)/$(PROGRAM); $(INSTALL) $(PROGRAM) $(DEST)); fi

print:;		@$(PRINT) $(HDRS) $(SRCS)

tags:           $(HDRS) $(SRCS); @ctags $(HDRS) $(SRCS)
###
Mkmf.o: RELEASE.h Mkmf.h config.h getarg.h hash.h null.h path.h target.h \
	slist.h suffix.h yesno.h defaultrul.h defaultsfx.h inckey.h
badopt.o: null.h
buildlist.o: Mkmf.h config.h dir.h hash.h null.h path.h slist.h stringx.h \
	suffix.h yesno.h
depend.o: Mkmf.h config.h dlist.h hash.h macro.h stringx.h null.h path.h \
	slist.h yesno.h
dlist.o: Mkmf.h dlist.h hash.h null.h slist.h yesno.h
editmf.o: Mkmf.h config.h dlist.h hash.h macro.h stringx.h null.h slist.h \
	yesno.h
getcwp.o: null.h
getproject.o: null.h
hash.o: null.h hash.h macro.h stringx.h config.h true.h
iolin.o: yesno.h
macro.o: Mkmf.h hash.h macro.h stringx.h config.h null.h slist.h yesno.h
misc.o: Mkmf.h config.h dir.h hash.h macro.h stringx.h null.h path.h target.h \
	suffix.h yesno.h
mksrclist.o: Mkmf.h hash.h macro.h stringx.h config.h null.h path.h slist.h \
	yesno.h
mksymlink.o: Mkmf.h config.h hash.h macro.h stringx.h null.h path.h slist.h \
	yesno.h
optpath.o: path.h
pathcat.o: path.h
pathhead.o: null.h path.h
rule.o: Mkmf.h null.h rule.h slist.h stringx.h config.h suffix.h yesno.h
slappend.o: macro.h stringx.h config.h null.h slist.h
slinit.o: null.h slist.h
slrm.o: null.h slist.h
slsort.o: null.h slist.h yesno.h
slvect.o: null.h slist.h
slvtol.o: null.h slist.h
strsav.o: null.h stringx.h config.h
suffix.o: Mkmf.h hash.h macro.h stringx.h config.h null.h suffix.h yesno.h
usage.o: null.h
warn.o: null.h
