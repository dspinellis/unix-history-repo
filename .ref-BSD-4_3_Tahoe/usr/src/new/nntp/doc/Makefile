#
# Makefile for manual pages for nntp service
#
# Created by Stan Barber; Mon Apr  6 13:25:27 CDT 1987
#

# change these to suit your system
ROFF	= ditroff
PRINTER	= lw301
MANDIR	= /usr/man

# from here down should be constant
SHELL	= /bin/sh
MACROS	= -man
MANPAGES= nntpd.8c mkgrdates.8c getactive.1
EXTRACT	= ./config.sh

all: $(MANPAGES)
	@echo "Done"

nntpd.8c: nntpd.dst $(EXTRACT)
	$(EXTRACT) nntpd.dst nntpd.8c

mkgrdates.8c: mkgrdates.dst $(EXTRACT)
	$(EXTRACT) mkgrdates.dst mkgrdates.8c

getactive.1: getactive.dst $(EXTRACT)
	$(EXTRACT) getactive.dst getactive.1

install: $(MANPAGES)
	mv nntpd.8c $(MANDIR)/man8/nntpd.8c
	mv mkgrdates.8c $(MANDIR)/man8/mkgrdates.8c
	mv getactive.1 $(MANDIR)/man1/getactive.1

print: $(MANPAGES)
	$(ROFF) $(MACROS) -P$(PRINTER) nntpd.8c
	$(ROFF) $(MACROS) -P$(PRINTER) mkgrdates.8c
	$(ROFF) $(MACROS) -P$(PRINTER) getactive.1

clean:
	rm -f $(MANPAGES)

distrib: clean
	rm -rf SCCS
