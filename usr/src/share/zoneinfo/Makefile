# @(#)Makefile	4.31

# Change the line below for your time zone (after finding the zone you want in
# the time zone files, or adding it to a time zone file).
# Alternately, if you discover you've got the wrong time zone, you can just
#	zic -l rightzone

LOCALTIME=	Factory

# If you want something other than Eastern United States time as a template
# for handling POSIX-style time zone environment variables,
# change the line below (after finding the zone you want in the
# time zone files, or adding it to a time zone file).
# Alternately, if you discover you've got the wrong time zone, you can just
#	zic -p rightzone

POSIXRULES=	US/Eastern

# Use an absolute path name for TZDIR unless you're just testing the software.

TZDIR=		/etc/zoneinfo

# If you always want time values interpreted as "seconds since the epoch
# (not counting leap seconds)", use
# 	REDO=		posix_only
# below.  If you always want right time values interpreted as "seconds since
# the epoch" (counting leap seconds)", use
#	REDO=		right_only
# below.  If you want both sets of data available, with leap seconds not
# counted normally, use
#	REDO=		posix_right
# below.  If you want both sets of data available, with leap seconds counted
# normally, use
#	REDO=		right_posix
# below.

REDO=		posix_right

# You may want to change this define if you're just testing the software.
# Alternatively, you can put these functions in /lib/libc.a, removing
# the old "ctime.o".  This is the
# ideal solution if you are able.  Build libz.a, extract the files, and
# then add them to libc.a.

TZLIB=		/usr/lib/libz.a

# If you're running on a System V-style system and don't want lint grief,
# add
#	-DUSG
# to the end of the "CFLAGS=" line.
#
# If you're running on a system where "strchr" is known as "index",
# (for example, a 4.[012]BSD system), add
#	-Dstrchr=index
# to the end of the "CFLAGS=" line.
#
# If you're running on a system with a "mkdir" function, feel free to add
#	-Demkdir=mkdir
# to the end of the "CFLAGS=" line
#
# If you want to use System V compatibility code, add
#	-DUSG_COMPAT
# to the end of the "CFLAGS=" line.
#
# If your system has a "GMT offset" field in its "struct tm"s
# (or if you decide to add such a field in your system's "time.h" file),
# add the name to a define such as
#	-DTM_GMTOFF=tm_gmtoff
# or
#	-DTM_GMTOFF=_tm_gmtoff
# to the end of the "CFLAGS=" line.
#
# If your system has a "GMT offset" field in its "struct tm"s
# (or if you decide to add such a field in your system's "time.h" file),
# add the name to a define such as
#	-DTM_ZONE=tm_zone
# or
#	-DTM_ZONE=_tm_zone
# to the end of the "CFLAGS=" line.
#
# If you want code inspired by certain emerging standards, add
#	-DSTD_INSPIRED
# to the end of the "CFLAGS=" line.
#
# If you want Source Code Control System ID's left out of object modules, add
#	-DNOID
# to the end of the "CFLAGS=" line.
#
# If you'll never want to handle solar-time-based time zones, add
#	-DNOSOLAR
# to the end of the "CFLAGS=" line
# (and comment out the "SDATA=" line below).
#
# If you want to allocate state structures in localtime, add
#	-DALL_STATE
# to the end of the "CFLAGS=" line.
#
# If you want an "altzone" variable (a la System V Release 3.1), add
#	-DALTZONE
# to the end of the "CFLAGS=" line.
#
# If you want a "gtime" function (a la MACH), add
#	-DCMUCS
# to the end of the "CFLAGS=" line

CFLAGS=

# LINTFLAGS is set for SunOS 4.0.
# If you're using System V, you'll want to comment out the "LINTFLAGS=" line.

LINT=		lint
LINTFLAGS=	-hbaxc

SHAR=		shar

################################################################################

CC=		cc -DTZDIR=\"$(TZDIR)\"

TZCSRCS=	zic.c localtime.c asctime.c \
		scheck.c ialloc.c emkdir.c getopt.c link.c
TZCOBJS=	zic.o localtime.o asctime.o \
		scheck.o ialloc.o emkdir.o getopt.o link.o
TZDSRCS=	zdump.c localtime.c asctime.c \
		ialloc.c getopt.c link.c
TZDOBJS=	zdump.o localtime.o asctime.o \
		ialloc.o getopt.o link.o
DATESRCS=	date.c localtime.c getopt.c logwtmp.c
DATEOBJS=	date.o localtime.o getopt.o logwtmp.o
LIBSRCS=	localtime.c asctime.c difftime.c
LIBOBJS=	localtime.o asctime.o difftime.o
HEADERS=	tzfile.h nonstd.h stdio.h stdlib.h time.h
NONLIBSRCS=	zic.c zdump.c scheck.c ialloc.c emkdir.c getopt.c link.c
NEWUCBSRCS=	date.c logwtmp.c
SOURCES=	$(HEADERS) $(LIBSRCS) $(NONLIBSRCS) $(NEWUCBSRCS)
DOCS=		Patchlevel.h \
		README Theory \
		date.1 newctime.3 tzfile.5 zic.8 zdump.8 \
		Makefile
YDATA=		africa antarctica asia australasia \
		europe northamerica southamerica pacificnew etcetera factory
NDATA=		systemv
SDATA=		solar87 solar88 solar89
TDATA=		$(YDATA) $(NDATA) $(SDATA)
DATA=		$(YDATA) $(NDATA) $(SDATA) leapseconds
USNO=		usno1988 usno1989
ENCHILADA=	$(DOCS) $(SOURCES) $(DATA) $(USNO)

all:		REDID_BINARIES zdump date $(TZLIB)

# We want to use system's logwtmp in preference to ours if available.

date:		$(DATEOBJS)
		ar r ,lib.a logwtmp.o
		-ranlib ,lib.a
		$(CC) $(CFLAGS) date.o localtime.o getopt.o -lc ,lib.a -o $@
		rm -f ,lib.a

REDID_BINARIES:	zic $(DATA) $(REDO)
		./zic -d $(TZDIR) -l $(LOCALTIME) -p $(POSIXRULES)
		touch $@

posix_only:	zic $(TDATA)
		./zic -d $(TZDIR) -L /dev/null $(TDATA)

right_only:	zic leapseconds $(TDATA)
		./zic -d $(TZDIR) -L leapseconds $(TDATA)

other_two:	zic leapseconds $(TDATA)
		./zic -d $(TZDIR)/posix -L /dev/null $(TDATA)
		./zic -d $(TZDIR)/right -L leapseconds $(TDATA)

posix_right:	posix_only other_two

right_posix:	right_only other_two

zdump:		$(TZDOBJS)
		$(CC) $(CFLAGS) $(LFLAGS) $(TZDOBJS) -o $@

# The "ar d" command below ensures that obsolete object files are eliminated
# from the library.

$(TZLIB):	$(LIBOBJS)
		ar ru $@ $(LIBOBJS)
		-ar d $@ timemk.o ctime.o
		-ranlib $@

zic:		$(TZCOBJS)
		$(CC) $(CFLAGS) $(LFLAGS) $(TZCOBJS) -o $@

SHARS:		SHAR1.Z SHAR2.Z SHAR3.Z SHAR4.Z SHAR5.Z SHAR6.Z SHAR7.Z

SHAR1.Z:	$(DOCS)
		$(SHAR) $(DOCS) | compress > $@

SHAR2.Z:	$(HEADERS) $(LIBSRCS)
		$(SHAR) $(HEADERS) $(LIBSRCS) | compress > $@

SHAR3.Z:	$(NONLIBSRCS)
		$(SHAR) $(NONLIBSRCS) | compress > $@

SHAR4.Z:	$(YDATA) $(NDATA) leapseconds
		$(SHAR) $(YDATA) $(NDATA) leapseconds | compress > $@

SHAR5.Z:	$(SDATA)
		$(SHAR) $(SDATA) | compress > $@

SHAR6.Z:	$(USNO)
		$(SHAR) $(USNO) | compress > $@

SHAR7.Z:	$(NEWUCBSRCS)
		$(SHAR) $(NEWUCBSRCS) | compress > $@

sure:		$(SOURCES)
		for i in "$(TZCSRCS)" "$(TZDSRCS)" "$(DATESRCS)" "$(LIBSRCS)"; \
		do $(LINT) $(LINTFLAGS) $(CFLAGS) -DTZDIR=\"$(TZDIR)\" $$i ; \
		done

# "/usr/5bin/lint -p" can dump core on SunOS 4.0, so no -p below. . .

SURE:		sure $(ENCHILADA)
		make sure LINT=/usr/5bin/lint LINTFLAGS=-DUSG
		make sure LINT=gcc LINTFLAGS="-c -O -ansi -pedantic -Wall"
		rm -f zic zdump *.o
		spell $(ENCHILADA)

clean:
		rm -f core *.o *.out REDID_BINARIES zdump zic date SHAR* ,*

CLEAN:		clean
		sccs clean

names:
		@echo $(ENCHILADA)

asctime.o:	nonstd.h stdio.h time.h tzfile.h
difftime.o:	nonstd.h time.h
emkdir.o:	nonstd.h stdio.h stdlib.h
ialloc.o:	nonstd.h stdlib.h
link.o:		nonstd.h stdio.h
localtime.o:	nonstd.h stdio.h stdlib.h time.h tzfile.h
scheck.o:	nonstd.h stdio.h stdlib.h
zic.o:		nonstd.h stdio.h stdlib.h time.h tzfile.h

.KEEP_STATE:
