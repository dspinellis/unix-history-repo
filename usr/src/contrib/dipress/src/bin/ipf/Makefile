#
# Copyright (c) 1984, 1985 Xerox Corp.
#
DESTINCLUDE=../../../include
DESTBIN=../../../bin
DESTLIB=../../../lib

ipf: ipf.o $(DESTLIB)/libgetopt.a
	cc -o ipf ipf.o $(DESTLIB)/libgetopt.a

ipf.o: $(DESTINCLUDE)/iptokens.h $(DESTINCLUDE)/ipnames.h
	cc -c -I$(DESTINCLUDE) ipf.c

lint: 
	lint -ab -I$(DESTINCLUDE) ipf.c $(DESTLIB)/llib-lip.ln \
		$(DESTLIB)/llib-lgetopt.ln -lc

clean:
	rm -f ipf *.o

install: $(DESTBIN)/ipf

$(DESTBIN)/ipf: ipf
	install -c -s ipf $(DESTBIN)
