
v6mail: mach.o v6mail.o
	cc -n -s -z -o v6mail v6mail.o mach.o

mach.o: mach.c mach.h
	cc -O -c mach.c

v6mail.o: v6mail.c mach.h
	cc -O -c v6mail.c

install: v6mail
	install -s v6mail ${DESTDIR}/usr/net/bin
	chown root ${DESTDIR}/usr/net/bin/v6mail
	chmod 4755 ${DESTDIR}/usr/net/bin/v6mail

clean:
	rm -f *.o v6mail
