#	Makefile	4.3	87/06/18
#
DESTDIR=
CFLAGS=	-O

calendar: calendar.c
	${CC} ${CFLAGS} -o calendar calendar.c

install:
	install -s calendar $(DESTDIR)/usr/lib
	install -c -m 755 calendar.sh $(DESTDIR)/usr/bin/calendar

clean:
	rm -f calendar

depend:
