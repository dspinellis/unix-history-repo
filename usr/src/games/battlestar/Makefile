# @(#)Makefile	1.1 %G%

CFLAGS=		-O
OBJS=		battlestar.o com1.o com2.o com3.o com4.o com5.o com6.o com7.o \
		init.o cypher.o getcom.o parse.o room.o save.o fly.o misc.o \
		globals.o \
		wlist.o dayfile.o nightfile.o dayobjs.o nightobjs.o
CFILES=		battlestar.c com1.c com2.c com3.c com4.c com5.c com6.c com7.c \
		init.c cypher.c getcom.c parse.c room.c save.c fly.c misc.c \
		globals.c
LIBS=		-lcurses -ltermlib
LINTFLAGS=	-h
JUNKFILES=	battlestar tags mkfile mkwlist mkobjs \
		wlist.c dayfile.c nightfile.c dayobjs.c nightobjs.c
DATAFILES=	dayfile nightfile battlewords dayobjs nightobjs
DIR=		$(DEST)/usr/games
LOGFILE=	$(DIR)/lib/battlestar.log
MAN=		$(DEST)/usr/man/man6/battlestar.6

battlestar: ${OBJS}
	cc ${OBJS} ${LIBS} -o battlestar

mkfile: mkfile.c
	$(CC) $(CFLAGS) -o mkfile mkfile.c

dayfile.c: dayfile mkfile
	mkfile dayfile

nightfile.c: nightfile mkfile
	mkfile nightfile

mkwlist: mkwlist.c
	$(CC) $(CFLAGS) -o mkwlist mkwlist.c

wlist.c: battlewords mkwlist
	mkwlist battlewords

mkobjs: mkobjs.c
	$(CC) $(CFLAGS) -o mkobjs mkobjs.c

dayobjs.c: dayobjs mkobjs
	mkobjs dayobjs

nightobjs.c: nightobjs mkobjs
	mkobjs nightobjs

lint: ${CFILES} 
	lint ${LINTFLAGS} ${CFILES} ${LIBS} 

install: battlestar $(MAN) $(LOGFILE)
	install -s -c battlestar $(DIR)/battlestar

logfile $(LOGFILE):
	cp /dev/null $(LOGFILE)
	chmod 666 $(LOGFILE)

man $(MAN):
	install -c battlestar.6 $(MAN)

clean:
	rm -f ${OBJS} ${JUNKFILES}
