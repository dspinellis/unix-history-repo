#$Header: /usr/src/local/bin/rcs/src/RCS/Makefile,v 1.13 89/05/02 14:53:40 narten Exp $
# Copyright (C) 1982, 1988, 1989 Walter Tichy
# All rights reserved.
#
# Redistribution and use in source and binary forms are permitted
# provided that the above copyright notice and this paragraph are
# duplicated in all such forms and that any documentation,
# advertising materials, and other materials related to such
# distribution and use acknowledge that the software was developed
# by Walter Tichy.
# THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
# IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
# WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
#
# Report all problems and direct all questions to:
#   rcs-bugs@cs.purdue.edu
# 
#
#
#
#
#
#
#
#               INSTRUCTIONS
#               ============
# 0.  If you are porting to different hardware,
#     check the instructions in rcsbase.h
#
# Note: instructions 1-5 are normally taken care of by the calling make,
# which passes in DIFF, DIFF3, RCSDIR, OLDDIR, OS, and LOCKING.
#
# 1. Early version of diff did not support options needed by RCS.
#    Newer versions of diff, support them and diff is no longer distributed with
#    RCS. The following definitions apply only if you have both old versions
#    and new versions of diff, and you want RCS to use the new versions.
#    The default should be :
#

DIFF      = /usr/bin/diff
DIFF3     = /usr/bin/diff3

# 2.  Figure out where to put the RCS commands; define RCSDIR accordingly.

RCSDIR  = /usr/contrib


# 3.  Figure out where to put your old RCS commands; define OLDDIR
#     accordingly. This is not needed if you don't have an old release of RCS.
OLDDIR    = /usr/old/bin

# 4.  Set the operating system variable OS. If you run on 4.1 BSD, do not
#     define OS:
#     OS  =
#     If you run on USG Unix or UN*X, use:
#     OS  = -DUSG
#     If you run on Version 6 Unix, use:
#     OS  = -DV6
#     If you run on 4.2 or 4.3 BSD, use:
#     OS = -DV4_2BSD
OS        = -DV4_2BSD

#     Also, set SIGNAL_TYPE to be what your signal() handlers yield.
#     Old-fashioned signal handlers yield 'int'; the newer ones 'void'.

SIGNAL_TYPE = void

# 5.  Decide whether you want strict locking as the default or not.
#     If you want strict locking, use:
#     LOCKING = 1
#     otherwise:
#     LOCKING = 0
LOCKING   = 1

# 6.  Decide what loader flags you need, and define LDFLAGS accordingly.
#     On the Vax you need none, on PDP-11s with split i/d you need -i.
#LDFLAGS  = -i
LDFLAGS   =

# 7.  RCS can maintain a log of the use of the co and ci commands. If you
#     want the logging, you must
#     a) define SNOOP (that's where the process that writes the log goes),
#     b) define SNOOPFILE (that's the file where SNOOP writes the log;
#        it must be owned by the same user that owns SNOOP),
#     c) include SNOOP and SNOOPFILE in the CFLAGS macro.
#     If you don't want the logging of command usage, omit the SNOOP and
#     SNOOPFILE definitions from CFLAGS.
#     The two variants of the CFLAG macros are given below.
SNOOPFILE = /var/log/rcslog
SNOOP	= /usr/contrib/snoop

DEFINES= -DCOMPAT2 $(OS) -DSIGNAL_TYPE=$(SIGNAL_TYPE) -DSTRICT_LOCKING=$(LOCKING) -DDIFF='"$(DIFF)"' -DCO='"${RCSDIR}/co"' -DMERGE='"${RCSDIR}/merge"' 
CFLAGS	= -O $(DEFINES)
LINTFLAGS = -c -u
LINT = lint $(LINTFLAGS) -Dlint $(DEFINES)

# 8.  If you don't have release 2 RCS files around (e.g., if you don't
#     have release 2 of RCS), remove the word -DCOMPAT2 from whatever
#     variant of CFLAGS you selected. -DCOMPAT2 assures that release 2
#     RCS files can still be read. (After all release 2 RCS files have
#     been updated with release 3 or 4 ci or rcs, you can remake RCS without
#     -DCOMPAT2.) Also, make sure that the co commands at the end of this
#     file are commented out; otherwise, Make will complain.
#
# 9.  Now you are ready. Try to make 'depend' and 'all'. If all went well, make
#     'install' if you want no logging of command usage, 'installog' otherwise.
#
# 10. IMPORTANT! When you have RCS installed, rename old
#     release 2 RCS files as follows (if you have any):
#     If the working file was "f.c" and the RCS file "f.c.v", rename the
#     RCS file to "f.c,v". If the working file was "f.c" and the RCS file
#     "f.v", rename the RCS file "f.c,v". Thus, suffixes are no longer
#     dropped and RCS files end in ",v" rather than ".v".
#
# 11. If you want to maintain RCS with itself, be sure you preserve the
#     original revision numbers, dates, etc. This is done by checking the
#     files in with the -k option. When done, remove the comments from the
#     co-rules at the end of this file.
#
BCOMMANDS   =   ci ident rcs rcsdiff rcsmerge rlog co
# binary comands

RCSCOMMANDS = merge $(BCOMMANDS)
# all commands

BOBJECTS = ci.o co.o ident.o rcs.o rcsdiff.o rcsedit.o\
	rcsfcmp.o rcsfnms.o rcsgen.o rcskeep.o rcskeys.o rcslex.o rcsmerge.o\
	rcsrev.o rcssyn.o rcsutil.o rlog.o snoop.o\
	rcssynTST.o rcsrevTST.o rcsfnmsTST.o rcsfcmpTST.o rcskeepTST.o

all:	$(RCSCOMMANDS)

INSTALL = install

$(BOBJECTS):	rcsbase.h

partime.o maketime.o co.o ci.o rcs.o rlog.o rcsutil.o: time.h

install:	all 
#	Save the old commands if you have any.
#	-cd ${RCSDIR}; mv $(RCSCOMMANDS) ${RCSDIR}$(OLDDIR)
	${INSTALL} -m 775 -o bin -g bin -s ci ${RCSDIR}
	${INSTALL} -m 775 -o bin -g bin -s ident ${RCSDIR}
	${INSTALL} -m 775 -o bin -g bin -s rcs ${RCSDIR}
	${INSTALL} -m 775 -o bin -g bin -s rcsdiff ${RCSDIR}
	${INSTALL} -m 775 -o bin -g bin -s rcsmerge ${RCSDIR}
	${INSTALL} -m 775 -o bin -g bin -s rlog ${RCSDIR}
	${INSTALL} -m 775 -o bin -g bin -s co ${RCSDIR}
	${INSTALL} -m 775 -o bin -g bin -c merge ${RCSDIR}
#	${INSTALL} -m 775 -o bin -g bin -c rcsclean ${RCSDIR}
#	${INSTALL} -m 775 -o bin -g bin -c rcsfreeze.sh ${RCSDIR}/rcsfreeze

installog:	installsnoop install

installsnoop:	snoop
	install -m 4775 -o bin -g bin -s snoop ${RCSDIR}$(SNOOP)
	chmod u+s ${RCSDIR}$(SNOOP)
	touch ${RCSDIR}$(SNOOPFILE)
	chown bin.bin ${RCSDIR}$(SNOOPFILE)
	chmod 644 ${RCSDIR}$(SNOOPFILE)
# CAUTION: You may have to do a chown of SNOOP and SNOOPFILE (if not owned by root).
lint: ci.lint co.lint ident.lint rlog.lint rcs.lint rcsdiff.lint rcsmerge.lint snoop.lint

clean:
	rm -f *.o; rm -f $(RCSCOMMANDS) snoop

cleandir: clean
	rm -f tags .depend

CIFILES = ci.o rcslex.o rcssyn.o rcsgen.o rcsedit.o rcskeys.o rcsrev.o rcsutil.o rcsfnms.o partime.o maketime.o localzone.o rcskeep.o rcsfcmp.o

CISRC = ci.c rcslex.c rcssyn.c rcsgen.c rcsedit.c rcskeys.c rcsrev.c rcsutil.c rcsfnms.c partime.c maketime.c localzone.c rcskeep.c rcsfcmp.c

ci.lint:
	$(LINT) $(CISRC)
	
ci:	$(CIFILES)
	${CC} $(LDFLAGS) -o ci $(CIFILES)

COFILES = co.o rcslex.o rcssyn.o rcsgen.o rcsedit.o rcskeys.o rcsrev.o rcsutil.o rcsfnms.o partime.o maketime.o localzone.o

COSRC = co.c rcslex.c rcssyn.c rcsgen.c rcsedit.c rcskeys.c rcsrev.c rcsutil.c rcsfnms.c partime.c maketime.c localzone.c
co.lint:
	$(LINT) $(COSRC)
co:	$(COFILES)
	${CC} $(LDFLAGS) -o co $(COFILES)

ident:	ident.o rcskeys.o
	${CC} $(LDFLAGS) -o ident ident.o rcskeys.o

ident.lint:
	$(LINT) ident.c rcskeys.c

merge:	merge.sh
	sed -e '/^#/d' \
		-e 's:DIFF=.*$$:DIFF=$(DIFF):' \
		-e 's:DIFF3=.*$$:DIFF3=$(DIFF3):' \
		merge.sh > merge
#	This takes out the comment lines and substitutes in DIFF and DIFF3.
#	(Comments are not permitted in some older shells.)
	chmod 755 merge

RLOG = rlog.o rcslex.o rcssyn.o rcsrev.o rcsutil.o partime.o maketime.o localzone.o rcsfnms.o
RLOGSRC = rlog.c rcslex.c rcssyn.c rcsrev.c rcsutil.c partime.c maketime.c localzone.c rcsfnms.c
rlog:	$(RLOG)
	${CC} $(LDFLAGS) -o rlog $(RLOG)

rlog.lint:
	$(LINT) $(RLOGSRC)

RCS = rcs.o rcslex.o rcssyn.o rcsrev.o rcsutil.o rcsgen.o rcsedit.o rcskeys.o rcsfnms.o

RCSSRC = rcs.c rcslex.c rcssyn.c rcsrev.c rcsutil.c rcsgen.c rcsedit.c rcskeys.c rcsfnms.c
rcs:	$(RCS)
	${CC} $(LDFLAGS) -o rcs $(RCS)

rcs.lint:
	$(LINT) $(RCSSRC)

rcsclean:	rcsclean.sh
	sed -e '/^#/d' rcsclean.sh > rcsclean
#	This takes out the comments (not permitted in older shells).
	chmod 755 rcsclean

RCSDIFF = rcsdiff.o rcsutil.o rcsfnms.o rcsrev.o rcssyn.o rcslex.o
RCSDIFFSRC = rcsdiff.c rcsutil.c rcsfnms.c rcsrev.c rcssyn.c rcslex.c

rcsdiff:	$(RCSDIFF)
	${CC} ${LDFLAGS} -o rcsdiff $(RCSDIFF)

rcsdiff.lint:
	$(LINT) $(RCSDIFFSRC)
RCSMERGE = rcsmerge.o rcsutil.o rcsfnms.o rcsrev.o rcssyn.o rcslex.o
RCSMERGESRC = rcsmerge.c rcsutil.c rcsfnms.c rcsrev.c rcssyn.c rcslex.c
rcsmerge:	$(RCSMERGE)
	${CC} ${LDFLAGS} -o rcsmerge $(RCSMERGE)
rcsmerge.lint:
	$(LINT) $(RCSMERGESRC)
	
snoop:	snoop.o
	${CC} ${LDFLAGS} -o snoop snoop.o
snoop.lint:
	$(LINT) snoop.c

SOURCE=	ci.c co.c curdir.c ident.c maketime.c localzone.c partime.c rcs.c \
	rcsclean.c rcsdiff.c rcsedit.c rcsfcmp.c rcsfnms.c rcsgen.c \
	rcskeep.c rcskeys.c rcslex.c rcsmerge.c rcsrev.c rcssyn.c rcsutil.c \
	rlog.c snoop.c

HFILES=	rcsbase.h time.h

depend:	${SOURCE} ${HFILES}
	mkdep ${CFLAGS} ${SOURCE}
