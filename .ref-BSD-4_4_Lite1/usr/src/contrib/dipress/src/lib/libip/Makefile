# Copyright (c) 1984, 1985 Xerox Corp.
#
# Rules for the Interpress library: the literal and operator level routines
#

DESTINCLUDE=../../../include
DESTLIB=../../../lib


all: libip.a libip_p.a

libip.a: literal.o operator.o 
	ar r libip.a literal.o operator.o

libip_p.a: literal_p.o operator_p.o
	ar r libip_p.a literal_p.o operator_p.o

# parts of the Interpress library

literal.o: $(DESTINCLUDE)/iptokens.h $(DESTINCLUDE)/literal.h
	$(CC) -c -I$(DESTINCLUDE) literal.c

literal_p.o: $(DESTINCLUDE)/iptokens.h $(DESTINCLUDE)/literal.h
	$(CC) -p -c -I$(DESTINCLUDE) literal.c
	mv literal.o literal_p.o

operator.o: $(DESTINCLUDE)/iptokens.h $(DESTINCLUDE)/literal.h $(DESTINCLUDE)/operator.h
	$(CC) -c -I$(DESTINCLUDE) operator.c

operator_p.o: $(DESTINCLUDE)/iptokens.h $(DESTINCLUDE)/literal.h $(DESTINCLUDE)/operator.h
	$(CC) -p -c -I$(DESTINCLUDE) operator.c
	mv operator.o operator_p.o

llib-lip.ln: literal.c operator.c
	lint -Cip -I$(DESTINCLUDE) literal.c operator.c

# helpers

lint:
	lint -au -I$(DESTINCLUDE) literal.c operator.c

install: $(DESTLIB)/libip.a  $(DESTLIB)/libip_p.a $(DESTLIB)/llib-lip.ln

$(DESTLIB)/libip.a: libip.a
	install -c libip.a  $(DESTLIB)
	ranlib $(DESTLIB)/libip.a

$(DESTLIB)/libip_p.a: libip_p.a
	install -c libip_p.a  $(DESTLIB)
	ranlib $(DESTLIB)/libip_p.a

$(DESTLIB)/llib-lip.ln: llib-lip.ln
	install -c llib-lip.ln $(DESTLIB)

clean:
	rm -f libip.a libip_p.a llib-lip.ln *.o
