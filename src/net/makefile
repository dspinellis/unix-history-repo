DESTDIR = /new
OBJS = -lS
ARCH = /mnt/src/cmd/net/
RDIR = $(DESTDIR)/usr/bin/
CRYPT = c.o
OTHER = bin
MWRITEMODE = 4711
#
# makefile for Berkeley Unix network
# Eric Schmidt -- March 9, 1979
#
# the constants above are machine dependent.
# to make net source:
#
#	make
#
# to install in right places:
#
#	make install DESTDIR=
#
# to remove all `.o' and executable files:
#
#	make clean
#
CFLAGS = -DOLDPROT -O
NDIR = $(DESTDIR)/usr/net/bin/
all: netdaemon net netq netrm netcp netlpr netmail sendmail mmail mwrite \
	netstart nsh done
install:
	-cpall net netcp netdaemon netstart mmail mwrite sendmail $(NDIR)
	-cpall netrm netq netlpr netmail $(RDIR)
	-cp nsh /usr/net/network/nsh
	-rm -f netcp netmail sendmail netdaemon netstart mmail mwrite \
		netlpr net netq netrm nsh
	-chown $(OTHER) $(NDIR)netdaemon $(NDIR)netstart $(NDIR)mmail \
		$(NDIR)mwrite $(NDIR)sendmail $(RDIR)netmail \
		$(NDIR)netcp $(RDIR)netlpr
	-chown root $(NDIR)net $(RDIR)netq $(RDIR)netrm
	-chown network /usr/net/network/nsh
	-chmod 711 $(NDIR)netcp $(RDIR)netmail $(NDIR)sendmail $(NDIR)mmail \
		$(RDIR)netlpr /usr/net/network/nsh
	-chmod $(MWRITEMODE) $(NDIR)mwrite
	-chmod 700 $(NDIR)netdaemon $(NDIR)netstart
	-chmod 4711 $(NDIR)net $(RDIR)netq $(RDIR)netrm
	-rm -f $(RDIR)netcp $(RDIR)net
	-ln $(NDIR)net		$(RDIR)net
	-ln $(NDIR)netcp	$(RDIR)netcp
	-rm -f $(RDIR)rcp $(RDIR)rlpr $(RDIR)readmail $(RDIR)sendmail
	-ln $(RDIR)netcp	$(RDIR)rcp
	-ln $(RDIR)netlpr	$(RDIR)rlpr
	-ln $(RDIR)netmail	$(RDIR)readmail
	-ln $(NDIR)sendmail	$(RDIR)sendmail
clean:
	-rm -f netdaemon net mwrite netq netcp netlpr sendmail \
	mmail netmail netrm netstart nsh interact \
	listen receive speedr speeds store nettest\
	echo1 echo2 \
	netdaemon.o net.o mwrite.o netq.o netcp.o netlpr.o sendmail.o \
	mmail.o netmail.o netrm.o netstart.o nsh.o interact.o \
	listen.o receive.o speedr.o speeds.o store.o nettest.o \
	echo1.o echo2.o nbs.o prot.o sub.o mach.o 
netdaemon: netdaemon.o sub.o prot.o mach.o nbs.o $(CRYPT)
	cc -s -n -o netdaemon netdaemon.o sub.o prot.o mach.o nbs.o $(CRYPT) $(OBJS)
net:	net.o sub.o mach.o nbs.o $(CRYPT)
	cc -s -n -o net net.o sub.o mach.o nbs.o $(CRYPT) $(OBJS)
netq: 	netq.o sub.o mach.o
	cc -s -n -o netq netq.o sub.o mach.o $(OBJS)
netrm:	netrm.o sub.o mach.o
	cc -s -n -o netrm netrm.o sub.o mach.o $(OBJS)
netcp:	netcp.o sub.o mach.o
	cc -s -n -o netcp netcp.o sub.o mach.o $(OBJS)
netlpr:	netlpr.o sub.o mach.o
	cc -s -n -o netlpr netlpr.o sub.o mach.o $(OBJS)
netmail: netmail.o sub.o mach.o
	cc -s -n -o netmail netmail.o sub.o mach.o $(OBJS)
sendmail: sendmail.o sub.o mach.o
	cc -s -n -o sendmail sendmail.o sub.o mach.o $(OBJS)
mwrite:	mwrite.o sub.o mach.o
	cc -n -s -o mwrite mwrite.o sub.o mach.o $(OBJS)
mmail:	mmail.o sub.o mach.o
	cc -n -s -o mmail mmail.o sub.o mach.o $(OBJS)
netstart: netstart.o mach.o
	cc -s -n -o netstart netstart.o mach.o $(OBJS)
nsh:	nsh.o sub.o mach.o
	cc -n -s -o nsh nsh.o sub.o mach.o $(OBJS)
interact: interact.o sub.o prot.o mach.o
	cc -s -n -o interact interact.o sub.o prot.o mach.o $(OBJS)
listen:	listen.o sub.o prot.o mach.o
	cc -s -n -o listen listen.o sub.o prot.o mach.o $(OBJS)
speeds:	speeds.o sub.o prot.o mach.o
	cc -s -n -o speeds speeds.o sub.o prot.o mach.o $(OBJS)
speedr:	speedr.o sub.o prot.o mach.o
	cc -s -n -o speedr speedr.o sub.o prot.o mach.o $(OBJS)
store:	store.o sub.o prot.o mach.o
	cc -s -n -o store store.o sub.o prot.o mach.o $(OBJS)
receive: receive.o sub.o prot.o mach.o
	cc -s -n -o receive receive.o sub.o prot.o mach.o $(OBJS)
nettest: nettest.o sub.o mach.o
	cc -s -n -o nettest nettest.o sub.o mach.o $(OBJS)
setmode:	setmode.o
	cc -s -o setmode setmode.o $(OBJS)
echo1:	echo1.o
	cc -s -n -o echo1 echo1.o sub.o $(OBJS)
echo2:	echo2.o
	cc -s -n -o echo2 echo2.o sub.o $(OBJS)
prmail:	prmail.o mach.o
	cc -s -n -o prmail prmail.o mach.o $(OBJS)
	-mv prmail $(NDIR)
	-chmod 711 $(NDIR)/prmail
done:
	-echo DONE!
print:
	pr README makefile *.h *.c | lpr
lintit:
	lint $(CFLAGS) *.c
lint1:
	echo netdaemon.c sub.c prot.c mach.c nbs.c
	lint $(CFLAGS) netdaemon.c sub.c prot.c mach.c nbs.c
	echo net.c sub.c mach.c nbs.c
	lint $(CFLAGS) net.c sub.c mach.c nbs.c
	echo netcp.c sub.c mach.c
	lint $(CFLAGS) netcp.c sub.c mach.c
archive:
	-ar rnv net.a makefile README *.h *.c $(CRYPT)
	mv net.a $(ARCH)
	chmod 644 $(ARCH)/net.a
