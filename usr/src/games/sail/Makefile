#
# Copyright (c) 1980 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.3 (Berkeley) %G%
#

CTAGS = ctags -w
CFLAGS = -O

CFILES = main.c pl_main.c pl_1.c pl_2.c pl_3.c pl_4.c pl_5.c pl_6.c pl_7.c \
	dr_main.c dr_1.c dr_2.c dr_3.c dr_4.c dr_5.c lo_main.c \
	assorted.c game.c globals.c misc.c parties.c sync.c version.c
OBJS = main.o pl_main.o pl_1.o pl_2.o pl_3.o pl_4.o pl_5.o pl_6.o pl_7.o \
	dr_main.o dr_1.o dr_2.o dr_3.o dr_4.o dr_5.o lo_main.o \
	assorted.o game.o globals.o misc.o parties.o sync.o version.o
HFILES = externs.h player.h driver.h machdep.h
OTHERFILES = Makefile sail.6
JUNKFILES = sail tags make.out
LIBS = -lcurses -ltermlib

SAILMASTER = daemon
LOGFILE = lib/saillog
TARGET = $(DESTDIR)/usr/games

all: sail

sail: $(OBJS)
	$(CC) -o sail $(OBJS) $(LIBS)

lint:
	lint -z $(CFILES) -lcurses

tags: $(CFILES) $(HFILES)
	$(CTAGS) $(CFILES) $(HFILES)

$(CFILES) $(HFILES) $(OTHERFILES):
	sccs get $@

clean:
	rm -f $(OBJS) $(JUNKFILES)

depend:

install: sail $(TARGET)/$(LOGFILE)
	install -s -m 4755 -o $(SAILMASTER) sail $(TARGET)/sail

logfile $(TARGET)/$(LOGFILE):
	cp /dev/null $(TARGET)/$(LOGFILE)
	chown $(SAILMASTER) $(TARGET)/$(LOGFILE)
	chmod 644 $(TARGET)/$(LOGFILE)

main.o: externs.h machdep.h
pl_main.o: player.h externs.h machdep.h
pl_1.o: player.h externs.h machdep.h
pl_2.o: player.h externs.h machdep.h
pl_3.o: player.h externs.h machdep.h
pl_4.o: player.h externs.h machdep.h
pl_5.o: player.h externs.h machdep.h
pl_6.o: player.h externs.h machdep.h
pl_7.o: player.h externs.h machdep.h
dr_main.o: driver.h externs.h machdep.h
dr_1.o: driver.h externs.h machdep.h
dr_2.o: driver.h externs.h machdep.h
dr_3.o: driver.h externs.h machdep.h
dr_4.o: externs.h machdep.h
dr_5.o: externs.h machdep.h
lo_main.o: externs.h machdep.h
assorted.o: externs.h machdep.h
game.o: externs.h machdep.h
globals.o: externs.h machdep.h
misc.o: externs.h machdep.h
parties.o: externs.h machdep.h
sync.o: externs.h machdep.h
