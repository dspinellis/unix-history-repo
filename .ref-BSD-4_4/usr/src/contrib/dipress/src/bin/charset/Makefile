# Makefile for Unix Interpress utilities
#
# Copyright (c) 1984, 1985, 1986 Xerox Corp.
#
# HISTORY
# 03-Jul-86  Lee Moore (lee) at Xerox Webster Research Center
#	Added "-lm" to the load line as a favor to Pyramids.
#	Converted for use with "getopt".
#
#     Dec 1, 1985	Lee Moore, Xerox WRC
#	added lint
#

DESTBIN=../../../bin
DESTINCLUDE=../../../include
DESTLIB=../../../lib

charset: charset.o $(DESTLIB)/libip.a $(DESTLIB)/libgetopt.a
	$(CC) $(CFLAGS) -o charset charset.o $(DESTLIB)/libip.a \
		$(DESTLIB)/libgetopt.a -lm

charset.o: $(DESTINCLUDE)/iptokens.h $(DESTINCLUDE)/literal.h $(DESTINCLUDE)/operator.h
	$(CC) $(CFLAGS) -I$(DESTINCLUDE) -c charset.c

lint:
	lint -ab -I$(DESTINCLUDE) charset.c $(DESTLIB)/llib-lip.ln \
		$(DESTLIB)/llib-lgetopt.ln -lc
	

install: $(DESTBIN)/charset

$(DESTBIN)/charset: charset
	install -c -s charset $(DESTBIN)

clean:
	rm -f charset test.ip *.o

test:
	charset -p357 -otest.ip -s12 TroffMathExtra
