ALL= dig.0 host.0 named.0 nslookup.0

.SUFFIXES: .0 .1 .8

.8.0:
	groff -man -t -Tlatin1 $*.8 > $@

.1.0:
	groff -man -t -Tlatin1 $*.1 > $@

all: $(ALL)

clean:; rm -f $(ALL)

install:
	install -c -m 444 dig.0 $(DESTDIR)/usr/share/man/cat1/dig.0
	install -c -m 444 host.0 $(DESTDIR)/usr/share/man/cat1/host.0
	install -c -m 444 named.0 $(DESTDIR)/usr/share/man/cat8/named.0
	install -c -m 444 nslookup.0 $(DESTDIR)/usr/share/man/cat1/nslookup.0
