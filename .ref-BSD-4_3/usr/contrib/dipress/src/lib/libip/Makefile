# Rules for the Interpress library: the literal and operator level routines
#
# Copyright (c) 1984, 1985 Xerox Corp.
#

DESTINCLUDE=../../../include
DESTLIB=../../../lib


libip.a: literal.o operator.o 
	ar r libip.a literal.o operator.o


# parts of the Interpress library

literal.o: $(DESTINCLUDE)/iptokens.h $(DESTINCLUDE)/literal.h
	$(CC) -c -I$(DESTINCLUDE) literal.c

operator.o: $(DESTINCLUDE)/iptokens.h $(DESTINCLUDE)/literal.h $(DESTINCLUDE)/operator.h
	$(CC) -c -I$(DESTINCLUDE) operator.c

llib-lip.ln: literal.c operator.c
	lint -Cip -I$(DESTINCLUDE) literal.c operator.c

# helpers

lint:
	lint -au -I$(DESTINCLUDE) literal.c operator.c

install: libip.a llib-lip.ln
	install -c libip.a  $(DESTLIB)
	ranlib $(DESTLIB)/libip.a
	install -c llib-lip.ln $(DESTLIB)

clean:
	rm -f libip.a llib-lip.ln *.o
