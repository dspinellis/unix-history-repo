# Makefile for phoon, deltime, parsetime, and libtws (stolen from mh).


# Valid options:
#   BSD42      Set this if your system is BSD 4.2 or later.
#   SYS5       Set this if your system is System V.
#   EUROPE     Makes nn/nn/nn mean dd/mm/yy instead of mm/dd/yy.
#   ATZ        This has something to do with alpha-numeric time zones.
#   DSTXXX     This has something to do with daylight savings time.
#   HUJI       I don't
#   INETONLY           know what
#   LEXDEBUG                     the rest of these
#   ONECASE                                        do.
OPTIONS	=	-DBSD42 -DATZ -DDSTXXX


CC      =	cc
CFLAGS  =	-O $(OPTIONS)
LDFLAGS =	-ns

.SUFFIXES:	.man .cat
.man.cat:
		nroff -h -man $< > $@


all:		phoon phoon.cat deltime deltime.cat parsetime libtws.cat


phoon:		phoon.o libtws.a
		$(CC) $(LDFLAGS) -o phoon phoon.o -lm libtws.a

phoon.o:	phoon.c tws.h


deltime:	deltime.o libtws.a
		$(CC) $(LDFLAGS) -o deltime deltime.o libtws.a

deltime.o:	deltime.c tws.h


parsetime:	parsetime.o libtws.a
		$(CC) $(LDFLAGS) -o parsetime parsetime.o libtws.a

parsetime.o:	parsetime.c tws.h


libtws.a:	dtime.o dtimep.o lexstring.o
		ar r libtws.a dtime.o dtimep.o lexstring.o
		ranlib libtws.a


dtime.o:	dtime.c tws.h


dtimep.o:	dtimep.c tws.h

dtimep.c:	dtimep.lex
		lex -nt dtimep.lex | sed -f lexedit.sed > dtimep.c


lexstring.o:	lexstring.c
		$(CC) $(CFLAGS) -c -DONECASE lexstring.c


clean:
		-rm -f dtimep.c *.o libtws.a phoon deltime parsetime *.cat phoon.shar core

phoon.shar:	phoon.shar1 phoon.shar2

phoon.shar1:	README Makefile deltime.c deltime.man dtime.c dtimep.lex
		shar -v -c -p X README Makefile deltime.c deltime.man dtime.c dtimep.lex > phoon.shar1

phoon.shar2:	lexedit.sed lexstring.c libtws.man parsetime.c phoon.c phoon.man tws.h
		shar -v -c -p X lexedit.sed lexstring.c libtws.man parsetime.c phoon.c phoon.man tws.h > phoon.shar2
