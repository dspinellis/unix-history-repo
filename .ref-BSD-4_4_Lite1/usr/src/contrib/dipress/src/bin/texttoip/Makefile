# Makefile for Unix Interpress utilities
#
# Copyright (c) 1984, 1985, 1986 Xerox Corp.
#
# HISTORY
# 03-Jul-86  Lee Moore (lee) at Xerox Webster Research Center
#	Added "-lm" to the load as a favor to pyramids.
#
#  Dec  1 1985 Lee Moore   Xerox Webster Research Center
#	added lint command


DESTBIN=../../../bin
DESTINCLUDE=../../../include
DESTLIB=../../../lib

texttoip: texttoip.o textlex.o $(DESTLIB)/libip.a
	$(CC) $(CFLAGS) -o texttoip texttoip.o textlex.o \
		$(DESTLIB)/libip.a -ll -lm

# Rules for object files -- include file dependencies must be noted here

textlex.o: $(DESTINCLUDE)/iptokens.h texttoken.h $(DESTINCLUDE)/ipnames.h
	$(CC) $(CFLAGS) -I$(DESTINCLUDE) -c textlex.c

textlex.c: textlex.l

texttoip.o: $(DESTINCLUDE)/iptokens.h texttoken.h $(DESTINCLUDE)/literal.h
	$(CC) $(CFLAGS) -I$(DESTINCLUDE) -c texttoip.c

# Lint

lint: textlex.c
	lint -I$(DESTINCLUDE) texttoip.c textlex.c $(DESTLIB)/llib-lip.ln -lc

# Installation

install: $(DESTBIN)/texttoip

$(DESTBIN)/texttoip: texttoip
	install -c -s texttoip $(DESTBIN)/texttoip

# Convenience rules:  cleaning

clean:
	rm -f texttoip textlex.c *.o
