#	@(#)Makefile	1.6 (Coimbra) 93/06/03

PROG=	nettest
SRCS=	main.c  getnetent.c getnetbyname.c getnetnamadr.c getnetbyaddr.c
OBJS=	getnetent.o getnetbyname.o getnetnamadr.o getnetbyaddr.o
CFLAGS=-O -DLIBC_SCCS
LDFLAGS=
LIBS=-lresolv # SunOS needs this


nettest:	main.o $(OBJS) 
	cc $(LDFLAGS) main.o $(OBJS) $(LIBS) -o $@

all:	nettest netstat rfc1101.shar

nettest.local:	main.o
	cc $(LDFLAGS) main.o $(LIBS) -o $@

rfc1101.shar:	README Makefile $(SRCS)
	shar README Makefile $(SRCS) > $@

clean_all:	clean
	rm -f nettest nettest.local netstat rfc1101.shar

clean:	
	rm -f main.o netstat.o $(OBJS)

NETSTAT_SOURCES= /usr/src/usr.bin/netstat/host.c \
	   /usr/src/usr.bin/netstat/inet.c  /usr/src/usr.bin/netstat/if.c \
	   /usr/src/usr.bin/netstat/main.c  /usr/src/usr.bin/netstat/mbuf.c \
	   /usr/src/usr.bin/netstat/route.c /usr/src/usr.bin/netstat/unix.c \
	   /usr/src/usr.bin/netstat/ns.c    /usr/src/usr.bin/netstat/iso.c \
	   /usr/src/sys/netiso/tp_astring.c

netstat:	netstat.o $(OBJS) 
	cc $(LDFLAGS) netstat.o $(OBJS) $(LIBS) -lkvm -o $@

netstat.o:	$(NETSTAT_SOURCES)
	-mkdir /tmp/netstat
	cp $(NETSTAT_SOURCES) /usr/src/usr.bin/netstat/netstat.h /tmp/netstat
	cd /tmp/netstat; cc -c $(CFLAGS) *.c; ld -r -o $@ *.o
	mv /tmp/netstat/$@ . ; rm -r /tmp/netstat
