# $Id: Makefile,v 5.16 1991/10/07 17:32:46 eggert Exp $
# Copyright (C) 1982, 1988, 1989 Walter Tichy
# Copyright 1990, 1991 by Paul Eggert
#   Distributed under license by the Free Software Foundation, Inc.
#
# This file is part of RCS.
#
# RCS is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.
#
# RCS is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with RCS; see the file COPYING.  If not, write to
# the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
#
# Report problems and direct all questions to:
#
#    rcs-bugs@cs.purdue.edu
#

# default target
default :: all

# See README for more information on the configuration section.
# ----- start of configuration section -----

#(Unix

 CC = cc
#CC = bsdcc -qlanglvl=ansi# AIX
#CC = cc -g# debug
#CC = gcc -g# GCC

 CC_D =

 CC_O = -O
#CC_O =# debug
#CC_O = -O -R# BSD compilers that do not support `const'
#CC_O = -O -fdelayed-branch -finline-functions -fomit-frame-pointer -fstrength-reduce# GCC options for brave souls

 CC_W =
#CC_W = -Wall -Wcast-qual -Wpointer-arith -Wshadow -Wwrite-strings# GCC

 CFLAGS = $(CC_D) $(CC_O) $(CC_W)

 COMPAT2 = 0
#COMPAT2 = 1

#DIFF = $(DIFFPREFIX)gdiff
#DIFF = $(DIFFPREFIX)rdiff# short for ``RCS diff'' on some hosts
DIFF = $(DIFFPREFIX)diff

#DIFFPREFIX = /usr/local/bin/# GNU diff -- must be version 1.15 or later
#DIFFPREFIX = /bin/# traditional diff
DIFFPREFIX = /usr/bin/# traditional diff (alternate name)

#DIFF_FLAGS = -an# GNU diff
DIFF_FLAGS = -n# traditional diff

#DIFF_L = 1# GNU diff
DIFF_L = 0# traditional diff

 DIFF_SUCCESS = 0
#DIFF_SUCCESS = EXIT_SUCCESS
 DIFF_FAILURE = 1
#DIFF_FAILURE = EXIT_FAILURE
 DIFF_TROUBLE = 2
#DIFF_TROUBLE = (EXIT_FAILURE*2)

#DIFF3 = $(DIFF)3# GNU diff3
#DIFF3 = /usr/lib/diff3# traditional diff3
#DIFF3 = /usr/5lib/diff3prog# other aliases for traditional diff3
#DIFF3 = /usr/lib/diff3prog
#DIFF3 = /usr/lib/rdiff3
 DIFF3 = /usr/bin/diff3

#DIFF3_BIN = 1# GNU diff
 DIFF3_BIN = 0# traditional diff

 ED = /bin/ed

#EXECUTABLE_GROUP = staff# BSD unix installation
 EXECUTABLE_GROUP = bin# BSD unix installation
#EXECUTABLE_PERMISSIONS = -g $(EXECUTABLE_GROUP) -m 775# BSD unix installation
 EXECUTABLE_PERMISSIONS = -g $(EXECUTABLE_GROUP) -m 555# BSD unix installation
#INSTALL = install -c $(EXECUTABLE_PERMISSIONS)# BSD unix installation
 INSTALL = install -s -o bin $(EXECUTABLE_PERMISSIONS)# BSD unix installation
#INSTALL = cp# traditional Unix installation

 LDFLAGS =

 LDLIBS =

 LINK = $(CC) $(LDFLAGS)

 LINT = lint -abchx# traditional and BSD lint
#LINT = lint# System V lint

 MAKE = make

 OTHER_OBJECT =

#RCSDIR = /usr/local/bin
 RCSDIR = /usr/contrib/bin
#RCSDIR = /bin
#RCSDIR = /usr/bin

 RCSPREFIX = $(RCSDIR)/
#RCSPREFIX =

 REMOVE = rm -f

 SENDMAIL = "/usr/libexec/mail.local"
#SENDMAIL = "/bin/mail"
#SENDMAIL = "/etc/delivermail", "-w"
#SENDMAIL = "/usr/bin/mail"
#SENDMAIL = "/usr/lib/sendmail"
#SENDMAIL = "mail"
#SENDMAIL =# for impoverished hosts that lack electronic mail

 TESTPREFIX =

 o = .o
#o = .s# Minix/PC with ACK cc

 x =

#)
# On non-Unix hosts you must manually create and edit conf.h from conf.heg.

# ----- end of configuration section -----
# You shouldn't have to change anything past this point.


# Avoid brain damage in some versions of 'make'.
SHELL = /bin/sh

# all commands
RCSCOMMANDS = ci$x co$x ident$x merge$x rcs$x rcsdiff$x rcsmerge$x rlog$x

all :: $(RCSCOMMANDS)

install :: all
	$(INSTALL) ci$x $(DESTDIR)$(RCSDIR)
	$(INSTALL) co$x $(DESTDIR)$(RCSDIR)
	$(INSTALL) ident$x $(DESTDIR)$(RCSDIR)
	$(INSTALL) merge$x $(DESTDIR)$(RCSDIR)
	$(INSTALL) rcs$x $(DESTDIR)$(RCSDIR)
	$(INSTALL) rcsdiff$x $(DESTDIR)$(RCSDIR)
	$(INSTALL) rcsmerge$x $(DESTDIR)$(RCSDIR)
	$(INSTALL) rlog$x $(DESTDIR)$(RCSDIR)

# Install RCS and (if applicable) GNU diff before running these tests.
# To test RCS before installing it, see README.
RCSTEST = PATH=$(RCSDIR):$(DIFFPREFIX).:$$PATH sh $(TESTPREFIX)rcstest
installtest ::
	$(RCSTEST)
installdebug ::
	$(RCSTEST) -v

clean ::
	$(REMOVE) a.* *$o conf.h conf.error $(RCSCOMMANDS) rcsclean$x

#(Unix
conf.h : conf.sh Makefile
	$(REMOVE) a.*
	CC='$(CC)' CFLAGS='$(CFLAGS)' \
	COMPAT2='$(COMPAT2)' \
	DIFF3='$(DIFF3)' DIFF3_BIN='$(DIFF3_BIN)' \
	DIFF='$(DIFF)' DIFF_FLAGS='$(DIFF_FLAGS)' DIFF_L='$(DIFF_L)' \
	DIFF_SUCCESS='$(DIFF_SUCCESS)' DIFF_FAILURE='$(DIFF_FAILURE)' DIFF_TROUBLE='$(DIFF_TROUBLE)' \
	ED='$(ED)' \
	LDFLAGS='$(LDFLAGS)' LDLIBS='$(LDLIBS)' \
	RCSPREFIX='$(RCSPREFIX)' \
	SENDMAIL='$(SENDMAIL)' \
	sh -x conf.sh >a.h 2>conf.error
	mv a.h $@
	$(REMOVE) a.*
#)

ci = ci$o rcslex$o rcssyn$o rcsgen$o rcsedit$o rcskeys$o rcsmap$o \
	rcsrev$o rcsutil$o rcsfnms$o partime$o maketime$o rcskeep$o \
	rcsfcmp$o $(OTHER_OBJECT)
ci$x : $(ci)
	$(LINK) $(ci) $(LDLIBS) -o $@

co = co$o rcslex$o rcssyn$o rcsgen$o rcsedit$o rcskeys$o rcsmap$o \
	rcsrev$o rcsutil$o rcsfnms$o partime$o maketime$o rcskeep$o $(OTHER_OBJECT)
co$x : $(co)
	$(LINK) $(co) $(LDLIBS) -o $@

ident = ident$o rcsmap$o $(OTHER_OBJECT)
ident$x : $(ident)
	$(LINK) $(ident) $(LDLIBS) -o $@

merge = merge$o merger$o rcsfnms$o rcslex$o \
	rcsmap$o rcsrev$o rcssyn$o rcsutil$o \
	rcskeep$o rcskeys$o $(OTHER_OBJECT)
merge$x : $(merge)
	$(LINK) $(merge) $(LDLIBS) -o $@

rlog = rlog$o rcslex$o rcsmap$o rcssyn$o rcsrev$o rcsutil$o partime$o \
	maketime$o rcsfnms$o rcskeep$o rcskeys$o $(OTHER_OBJECT)
rlog$x : $(rlog)
	$(LINK) $(rlog) $(LDLIBS) -o $@

rcs = rcs$o rcslex$o rcssyn$o rcsrev$o rcsutil$o rcsgen$o rcsedit$o rcskeys$o \
	rcsmap$o rcsfnms$o rcskeep$o $(OTHER_OBJECT)
rcs$x : $(rcs)
	$(LINK) $(rcs) $(LDLIBS) -o $@

rcsclean = rcsclean$o rcsedit$o rcsfcmp$o rcsfnms$o rcsgen$o rcskeys$o \
	rcslex$o rcsmap$o rcsrev$o rcssyn$o rcsutil$o rcskeep$o $(OTHER_OBJECT)
rcsclean$x : $(rcsclean)
	$(LINK) $(rcsclean) $(LDLIBS) -o $@

rcsdiff = rcsdiff$o rcsutil$o rcsfnms$o rcsmap$o rcsrev$o rcssyn$o rcslex$o \
	maketime$o partime$o rcskeep$o rcskeys$o $(OTHER_OBJECT)
rcsdiff$x : $(rcsdiff)
	$(LINK) $(rcsdiff) $(LDLIBS) -o $@

rcsmerge = rcsmerge$o merger$o rcsutil$o rcsfnms$o rcsmap$o rcsrev$o rcssyn$o \
	rcslex$o rcskeep$o rcskeys$o $(OTHER_OBJECT)
rcsmerge$x : $(rcsmerge)
	$(LINK) $(rcsmerge) $(LDLIBS) -o $@

SOURCE=	ci.c co.c ident.c maketime.c merge.c merger.c partime.c rcs.c \
	rcsclean.c rcsdiff.c rcsedit.c rcsfcmp.c rcsfnms.c rcsgen.c \
	rcskeep.c rcskeys.c rcslex.c rcsmap.c rcsmerge.c rcsrev.c rcssyn.c \
	rcsutil.c rlog.c
OBJECT=	ci$o co$o ident$o maketime$o merge$o merger$o partime$o rcs$o \
	rcsclean$o rcsdiff$o rcsedit$o rcsfcmp$o rcsfnms$o rcsgen$o \
	rcskeep$o rcskeys$o rcslex$o rcsmap$o rcsmerge$o rcsrev$o rcssyn$o \
	rcsutil$o rlog$o

lint :: conf.h
	$(LINT) $(CC_D) -Dlint=1 $(SOURCE)

conf_h = conf.h
$(OBJECT) : $(conf_h) rcsbase.h
