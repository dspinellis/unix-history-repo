CFLAGS=-O
all: tab300 tab300S tab300X tab300s tab37 tab450-12 tab450 tab450X tab450-12-8
all: tabtn300 tabx1700

tab300: tab300.o
	mv tab300.o tab300
tab300S: tab300S.o
	mv tab300S.o tab300S
tab300X: tab300X.o
	mv tab300X.o tab300X
tab300s: tab300s.o
	mv tab300s.o tab300s
tab37: tab37.o
	mv tab37.o tab37
tab450-12: tab450-12.o
	mv tab450-12.o tab450-12
tab450: tab450.o
	mv tab450.o tab450
tab450X: tab450X.o
	mv tab450X.o tab450X
tab450-12-8: tab450-12-8.o
	mv tab450-12-8.o tab450-12-8
tabtn300: tabtn300.o
	mv tabtn300.o tabtn300
tabx1700: tabx1700.o
	mv tabx1700.o tabx1700

install:
	install tab300 $(DESTDIR)/usr/lib/term
	install tab300S $(DESTDIR)/usr/lib/term
	install tab300X $(DESTDIR)/usr/lib/term
	install tab300s $(DESTDIR)/usr/lib/term
	install tab37 $(DESTDIR)/usr/lib/term
	install tab450-12-8 $(DESTDIR)/usr/lib/term
	install tab450-12 $(DESTDIR)/usr/lib/term
	install tab450 $(DESTDIR)/usr/lib/term
	install tab450X $(DESTDIR)/usr/lib/term
	install tabtn300 $(DESTDIR)/usr/lib/term
	install tabx1700 $(DESTDIR)/usr/lib/term
clean:
	: already clean


