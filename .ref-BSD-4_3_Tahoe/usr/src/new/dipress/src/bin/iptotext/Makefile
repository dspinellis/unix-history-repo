#
# Copyright (c) 1984, 1985 Xerox Corp.
#
DESTINCLUDE=../../../include
DESTBIN=../../../bin
DESTLIB=../../../lib

CFLAGS= -g

iptotext: iptotext.o
	cc -g -o iptotext iptotext.o

iptotext.o: $(DESTINCLUDE)/iptokens.h $(DESTINCLUDE)/ipnames.h
	cc -g -c -I$(DESTINCLUDE) iptotext.c

lint:
	lint -ab -I$(DESTINCLUDE) iptotext.c $(DESTLIB)/llib-lip.ln -lc

clean:
	rm -f iptotext *.o

install: $(DESTBIN)/iptotext

$(DESTBIN)/iptotext: iptotext
	install -c -s iptotext $(DESTBIN)
