#
# Copyright (c) 1984, 1985,1986 Xerox Corp.
#
DESTINCLUDE=../../../include
DESTBIN=../../../bin
DESTLIB=../../../lib

ipfe: ipfe.o
	cc -o ipfe ipfe.o

ipfe.o: $(DESTINCLUDE)/iptokens.h $(DESTINCLUDE)/ipnames.h
	cc -c -I$(DESTINCLUDE) ipfe.c

lint: 
	lint -b -I$(DESTINCLUDE) ipfe.c $(DESTLIB)/llib-lip.ln -lc
clean:
	rm -f ipfe *.o

install: $(DESTBIN)/ipfe

$(DESTBIN)/ipfe: ipfe
	install -c -s ipfe $(DESTBIN)
