#	@(#)Makefile	1.1 %G%
# transcript/src/Makefile.bsd
#
# Copyright (C) 1985 Adobe Systems Incorporated
#
# RCSID: $Header: Makefile.bsd,v 2.1 85/11/24 11:48:44 shore Rel $


LPROGS = psbanner pscomm psrv pstext
BPROGS = enscript ps4014 ps630 pscat pscatmap psdit psplot psrev psgrind

OBJECTS = psbanner.o pscomm.o psrv.o pstext.o \
	enscript.o ps4014.o ps630.o pscat.o pscatmap.o \
	psdit.o psplot.o psrev.o \
	psutil.o

#
# These values are automatically inserted into the appropriate
# variables in config.c when it is created from config.proto.
# They should be set up here for the local site.
#
PSLIBDIR = /usr/local/lib/ps
TROFFFONTDIR = /usr/local/lib/ps/troff.font
DITDIR = /usr/local/lib/font
PSTEMPDIR = /tmp

CFLAGS = -O -DBSD

programs all:	${LPROGS} ${BPROGS}

enscript: enscript.o psutil.o config.o getopt.o
	cc ${CFLAGS} -o enscript enscript.o psutil.o config.o getopt.o

psgrind: psgrind.o psutil.o config.o
	cc ${CFLAGS} -o psgrind psgrind.o psutil.o config.o

pscat: pscat.o psutil.o config.o getopt.o
	cc ${CFLAGS} -o pscat pscat.o psutil.o config.o getopt.o

psbanner: psbanner.o psutil.o config.o
	cc ${CFLAGS} -o psbanner psbanner.o psutil.o config.o

pscomm: pscomm.o psutil.o config.o
	cc ${CFLAGS} -o pscomm pscomm.o psutil.o config.o

pstext: pstext.o psutil.o config.o
	cc ${CFLAGS} -o pstext pstext.o psutil.o config.o

psplot: psplot.o psutil.o getopt.o config.o
	cc ${CFLAGS} -o psplot psplot.o psutil.o getopt.o config.o

psrv: psrv.o psutil.o config.o
	cc ${CFLAGS} -o psrv psrv.o psutil.o config.o

psrev: psrev.o psutil.o getopt.o config.o
	cc ${CFLAGS} -o psrev psrev.o psutil.o getopt.o config.o

ps630: ps630.o psutil.o config.o getopt.o
	cc ${CFLAGS} -o ps630 ps630.o psutil.o config.o getopt.o

ps4014: ps4014.o psutil.o config.o getopt.o
	cc ${CFLAGS} -o ps4014 ps4014.o psutil.o config.o getopt.o -lm

pscatmap: pscatmap.o psutil.o config.o
	cc ${CFLAGS} -o pscatmap pscatmap.o psutil.o config.o

psdit: psdit.o psutil.o config.o
	cc ${CFLAGS} -o psdit psdit.o psutil.o config.o 

${OBJECTS}: transcript.h
pscomm.o psbanner.o pscat.o: psspool.h
pscat.o pscatmap.o: action.h 

config.c: config.proto ../config
	-rm -f - config.c
	sed	-e s,PSLIBDIR,$(PSLIBDIR),g \
		-e s,TROFFFONTDIR,$(TROFFFONTDIR),g \
		-e s,DITDIR,$(DITDIR),g \
		-e s,PSTEMPDIR,$(PSTEMPDIR),g \
		config.proto >config.c

install: ${LPROGS} ${BPROGS}
	-cd $$PSLIBDIR; rm -f ${LPROGS} ${BPROGS}
	install -c -o $$OWNER -g $$GROUP -s psbanner	$$PSLIBDIR
	install -c -o $$OWNER -g $$GROUP -s pscomm	$$PSLIBDIR
	install -c -o $$OWNER -g $$GROUP -s psrv	$$PSLIBDIR
	install -c -o $$OWNER -g $$GROUP -s pstext	$$PSLIBDIR
	install -c -o $$OWNER -g $$GROUP -s enscript	$$PSLIBDIR
	install -c -o $$OWNER -g $$GROUP -s ps4014	$$PSLIBDIR
	install -c -o $$OWNER -g $$GROUP -s ps630	$$PSLIBDIR
	install -c -o $$OWNER -g $$GROUP -s pscat	$$PSLIBDIR
	install -c -o $$OWNER -g $$GROUP -s pscatmap	$$PSLIBDIR
	install -c -o $$OWNER -g $$GROUP -s psdit	$$PSLIBDIR
	install -c -o $$OWNER -g $$GROUP -s psplot	$$PSLIBDIR
	install -c -o $$OWNER -g $$GROUP -s psrev	$$PSLIBDIR
	-cd $$BINDIR ; rm -f ${BPROGS}
	for i in ${BPROGS} ;\
	do ln -s $$PSLIBDIR/$$i $$BINDIR ;\
	done

clean:
	rm -f - *.o config.c ${BPROGS} ${LPROGS} *BAK *CKP .emacs* *~

