#
# libove@libove.det.dec.com (Jay Vassos-Libove)
#
# Makefile for screen
#
# See config.h for configuration options.
#

# If you choose to compile with the tried and true:
#CC= cc
#CFLAGS= -O
#CFLAGS= -g -DTMPTEST -DDEBUG
#CFLAGS= -g 
#LDFLAGS=

# If you're using GNU C, be sure to use the -traditional flag:
CC= gcc
#CFLAGS = -O -fstrength-reduce -fcombine-regs -finline-functions #-Wall
#CFLAGS= -traditional -O
#CFLAGS= -g -traditional -DTMPTEST
#CFLAGS= -g -traditional -DTMPTEST -DDEBUG
CFLAGS= -g -O -fstrength-reduce -fcombine-regs -finline-functions \
	-DBSDI # -DDEBUG
#LDFLAGS= -s

#LIBS= nmalloc.o -ltermcap 
LIBS= -ltermcap 

CFILES=	screen.c ansi.c help.c fileio.c mark.c window.c socket.c
OFILES=	screen.o ansi.o help.o fileio.o mark.o window.o socket.o

screen: $(OFILES)
	$(CC) $(CFLAGS) $(LDFLAGS) -o screen $(OFILES) $(LIBS)

screen.o: screen.c screen.h config.h patchlevel.h extern.h
	$(CC) $(CFLAGS) -c screen.c

help.o: help.c screen.h config.h ansi.h extern.h
	$(CC) $(CFLAGS) -c help.c

fileio.o: fileio.c screen.h config.h extern.h
	$(CC) $(CFLAGS) -c fileio.c

ansi.o: ansi.c screen.h config.h ansi.h extern.h
	$(CC) $(CFLAGS) -c ansi.c

mark.o: mark.c screen.h config.h ansi.h extern.h
	$(CC) $(CFLAGS) -c mark.c

window.o: window.c config.h screen.h  extern.h
	$(CC) $(CFLAGS) -c window.c

socket.o: socket.c config.h screen.h  extern.h
	$(CC) $(CFLAGS) -c socket.c

clean:
	rm -f $(OFILES) screen

lint:
	lint -I. $(CFILES)

saber:
	#load $(CFLAGS) screen.c ansi.c $(LIBS)
