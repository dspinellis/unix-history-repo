#
# Copyright (c) 1984, 1985 Xerox Corp.
#
DESTINCLUDE=../../../include
DESTBIN=../../../bin
DESTLIB=../../../lib

ipf: ipf.o getopt.o
	cc -o ipf ipf.o getopt.o

ipf.o: $(DESTINCLUDE)/iptokens.h $(DESTINCLUDE)/ipnames.h
	cc -c -I$(DESTINCLUDE) ipf.c

lint: 
	lint -ab -I$(DESTINCLUDE) ipf.c getopt.c $(DESTLIB)/llib-lip.ln -lc
clean:
	rm -f ipf *.o

install: $(DESTBIN)/ipf

$(DESTBIN)/ipf: ipf
	install -c -s ipf $(DESTBIN)
