#
# Sail - Wooden Ships And Iron Men
#
# Makefile	@(#)Makefile	2.7 85/03/04
#
CC=	cc
CTAGS=	ctags -w
CFLAGS=	-O

CFILES=		main.c pl_main.c player1.c player2.c player3.c player4.c \
		player5.c player6.c player7.c dr_main.c driver1.c driver2.c \
		driver3.c driver4.c driver5.c lo_main.c assorted.c game.c \
		globals.c misc.c parties.c sync.c version.c
OBJS=		main.o pl_main.o player1.o player2.o player3.o player4.o \
		player5.o player6.o player7.o dr_main.o driver1.o driver2.o \
		driver3.o driver4.o driver5.o lo_main.o assorted.o game.o \
		globals.o misc.o parties.o sync.o version.o
HFILES=		externs.h player.h driver.h machdep.h
OTHERFILES=	Makefile sail.6
JUNKFILES=	make.out
LIBS=		-lcurses -ltermlib

SAILMASTER=	daemon
LOGFILE=	lib/saillog
TARGET=		$(DESTDIR)/usr/games

all: sail

sail: $(OBJS)
	$(CC) -o sail $(OBJS) $(LIBS)

lint:
	lint -z $(CFILES) -lcurses

tags: $(CFILES) $(HFILES)
	$(CTAGS) $(CFILES) $(HFILES)

$(CFILES) $(HFILES) $(OTHERFILES):
	sccs get $@;

clean: 
	rm -f $(OBJS) $(JUNKFILES)

install: sail sail.6 $(TARGET)/$(LOGFILE)
	install -c -s -m 4755 -o $(SAILMASTER) sail $(TARGET)/sail
	install -c sail.6 $(DESTDIR)/usr/man/man6

logfile $(TARGET)/$(LOGFILE):
	cp /dev/null $(TARGET)/$(LOGFILE)
	chown $(SAILMASTER) $(TARGET)/$(LOGFILE)
	chmod 644 $(TARGET)/$(LOGFILE)
