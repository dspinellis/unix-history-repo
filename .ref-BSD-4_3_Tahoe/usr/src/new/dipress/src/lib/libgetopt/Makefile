# "getopt" -- for systems that don't have it already
#
# Copyright (c) 1984, 1985, 1986 Xerox Corp.
#

DESTINCLUDE=../../../include
DESTLIB=../../../lib


libgetopt.a: getopt.o
	ar r libgetopt.a getopt.o


# parts of the Interpress library

getopt.o:
	$(CC) -c -I$(DESTINCLUDE) getopt.c

llib-lgetopt.ln: getopt.c
	lint -Cgetopt -I$(DESTINCLUDE) getopt.c

# helpers

lint:
	lint -au -I$(DESTINCLUDE) getopt.c

install: $(DESTLIB)/libgetopt.a  $(DESTLIB)/llib-lgetopt.ln

$(DESTLIB)/libgetopt.a: libgetopt.a
	install -c libgetopt.a  $(DESTLIB)
	ranlib $(DESTLIB)/libgetopt.a

$(DESTLIB)/llib-lgetopt.ln: llib-lgetopt.ln
	install -c llib-lgetopt.ln $(DESTLIB)

clean:
	rm -f libgetopt.a llib-lgetopt.ln *.o
