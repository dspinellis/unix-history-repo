DESTDIR = /new
OBJS =
ARCH = /usr/ucb/src/ucb/net/
RDIR = $(DESTDIR)/usr/ucb/
CRYPT =
OTHER = schmidt
MWRITEMODE = 711
NETLPRMODE = 711
NETLPROWN = schmidt
CC = cc
LDFLAGS = -s -n
# CSVAX version
# makefile for Berkeley Unix network
# Eric Schmidt -- November 1, 1979
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
# The files can be grouped in these ways:
#	Commands that can be put in "new" directories for users to try:
#		netq, netlpr, netrm, netmail, netcp
#	Commands that are internal and must work on incoming/outgoing requests:
#		prmail, sendberkmail, mmail, mwrite
# 	Commands that must be dumped in and tried, ready or not:
#		netdaemon, net, netstart, nsh
CFLAGS = -O
NDIR = $(DESTDIR)/usr/net/bin/
all: netdaemon net netq netrm netcp netlpr netmail sendberkmail mmail mwrite \
	netstart nsh prmail filecat chktroff done
install:
	-cp net netcp netdaemon netstart mmail mwrite \
		sendberkmail prmail filecat chktroff $(NDIR)
	-cp netrm netq netlpr netmail $(RDIR)
	-cp netlog.sh $(RDIR)/netlog
	-cp nettroff.sh $(RDIR)/nettroff
	-cp nsh $(DESTDIR)/usr/net/network/nsh
	-rm -f netcp netmail sendberkmail netdaemon netstart mmail mwrite \
		netlpr net netq netrm nsh prmail filecat chktroff
	-chown $(OTHER) $(NDIR)netdaemon $(NDIR)netstart $(NDIR)mmail \
		$(NDIR)mwrite $(NDIR)sendberkmail $(RDIR)netmail \
		$(NDIR)netcp $(NDIR)prmail $(NDIR)filecat $(NDIR)chktroff
	-chown root $(NDIR)net $(RDIR)netq $(RDIR)netrm
	-chown $(NETLPROWN) $(RDIR)netlpr
	-chown network ${DESTDIR}/usr/net/network/nsh
	-chmod 711 $(NDIR)netcp $(RDIR)netmail $(NDIR)sendberkmail \
		$(NDIR)mmail $(NDIR)prmail ${DESTDIR}/usr/net/network/nsh $(NDIR)filecat $(NDIR)chktroff
	-chmod $(MWRITEMODE) $(NDIR)mwrite
	-chmod $(NETLPRMODE) $(RDIR)netlpr
	-chmod 700 $(NDIR)netdaemon $(NDIR)netstart
	-chmod 4711 $(NDIR)net $(RDIR)netq $(RDIR)netrm
	-rm -f $(RDIR)netcp $(RDIR)net
	-ln $(NDIR)net		$(RDIR)net
	-ln $(NDIR)netcp	$(RDIR)netcp
clean:
	-rm -f netdaemon net mwrite netq netcp netlpr sendberkmail \
	mmail netmail netrm netstart nsh interact \
	listen receive speedr speeds store nettest prmail chktroff filecat\
	netdaemon.o net.o mwrite.o netq.o netcp.o netlpr.o sendberkmail.o \
	mmail.o netmail.o netrm.o netstart.o nsh.o prmail.o interact.o \
	listen.o receive.o speedr.o speeds.o store.o nettest.o \
	nbs.o prot.o sub.o mach.o netrc.o filecat.o setup.o
othernet: interact listen speedr speeds store receive nettest setmode
othernetclean:
	-rm -f interact interact.o listen listen.o speedr speedr.o \
	speeds speeds.o store store.o receive receive.o nettest nettest.o \
	setmode setmode.o
netdaemon: netdaemon.o sub.o prot.o mach.o netrc.o nbs.o setup.o $(CRYPT)
	$(CC) -n -z -o netdaemon netdaemon.o sub.o prot.o mach.o setup.o\
		netrc.o nbs.o $(CRYPT) $(OBJS)
net:	net.o sub.o mach.o netrc.o nbs.o $(CRYPT)
	$(CC) -n -z -o net net.o sub.o mach.o netrc.o nbs.o $(CRYPT) $(OBJS)
netq: 	netq.o sub.o mach.o
	$(CC) $(LDFLAGS) -o netq netq.o sub.o mach.o $(OBJS)
netrm:	netrm.o sub.o mach.o
	$(CC) $(LDFLAGS) -o netrm netrm.o sub.o mach.o $(OBJS)
netcp:	netcp.o sub.o mach.o netrc.o
	$(CC) $(LDFLAGS) -o netcp netcp.o sub.o  netrc.o mach.o $(OBJS)
netlpr:	netlpr.o sub.o mach.o netrc.o
	$(CC) $(LDFLAGS) -o netlpr netlpr.o sub.o netrc.o mach.o $(OBJS)
netmail: netmail.o sub.o mach.o netrc.o
	$(CC) $(LDFLAGS) -o netmail netmail.o netrc.o sub.o mach.o $(OBJS)
sendberkmail: sendberkmail.o sub.o mach.o
	$(CC) $(LDFLAGS) -o sendberkmail sendberkmail.o sub.o mach.o $(OBJS)
mwrite:	mwrite.o sub.o mach.o
	$(CC) $(LDFLAGS) -o mwrite mwrite.o sub.o mach.o $(OBJS)
mmail:	mmail.o sub.o mach.o
	$(CC) $(LDFLAGS) -o mmail mmail.o sub.o mach.o $(OBJS)
netstart: netstart.o mach.o setup.o sub.o netrc.o
	$(CC) $(LDFLAGS) -o netstart netstart.o mach.o setup.o sub.o netrc.o \
		$(OBJS)
nsh:	nsh.o sub.o mach.o
	$(CC) $(LDFLAGS) -o nsh nsh.o sub.o mach.o $(OBJS)
filecat: filecat.o
	$(CC) $(LDFLAGS) -o filecat filecat.o $(OBJS)
chktroff: chktroff.o
	$(CC) $(LDFLAGS) -o chktroff chktroff.o
prmail:	prmail.o mach.o sub.o
	$(CC) $(LDFLAGS) -o prmail prmail.o sub.o mach.o $(OBJS)
interact: interact.o sub.o prot.o mach.o setup.o
	$(CC) $(LDFLAGS) -o interact interact.o sub.o prot.o mach.o setup.o \
		$(OBJS)
listen:	listen.o sub.o prot.o mach.o setup.o
	$(CC) $(LDFLAGS) -o listen listen.o sub.o prot.o mach.o setup.o \
		$(OBJS)
speeds:	speeds.o sub.o prot.o mach.o setup.o
	$(CC) $(LDFLAGS) -o speeds speeds.o sub.o prot.o mach.o setup.o $(OBJS)
speedr:	speedr.o sub.o prot.o mach.o setup.o
	$(CC) $(LDFLAGS) -o speedr speedr.o sub.o prot.o mach.o setup.o $(OBJS)
store:	store.o sub.o prot.o mach.o setup.o
	$(CC) $(LDFLAGS) -o store store.o sub.o prot.o mach.o  setup.o $(OBJS)
receive: receive.o sub.o prot.o mach.o setup.o
	$(CC) $(LDFLAGS) -o receive receive.o sub.o prot.o mach.o setup.o\
		$(OBJS)
nettest: nettest.o sub.o mach.o
	$(CC) $(LDFLAGS) -o nettest nettest.o sub.o mach.o $(OBJS)
setmode:	setmode.o
	$(CC) -s -o setmode setmode.o $(OBJS)
done:
	-echo DONE!
print:
	pr BUGLIST READ_ME TODO makefile *.h *.c | lpr
lintit:
	lint *.c
lint1:
	echo netdaemon.c sub.c prot.c mach.c nbs.c setup.c netrc.c
	lint netdaemon.c sub.c prot.c mach.c nbs.c setup.c netrc.c
	echo net.c sub.c mach.c nbs.c netrc.c
	lint net.c sub.c mach.c nbs.c netrc.c
	echo netcp.c sub.c mach.c
	lint netcp.c sub.c mach.c
archive:
	-ar rnv net.a makefile READ_ME *.h *.c $(CRYPT)
	mv net.a $(DESTDIR)$(ARCH)
	chmod 644 $(ARCH)/net.a
