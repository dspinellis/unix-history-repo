default : bsd

# Makefile for Unix/Xenix rz and sz programs
# the makefile is not too well tested yet
CC=gcc -O -fstrength-reduce
OFLAG=
SIZE=size
TLBFILES= zmodem.h zm.c zmr.c crctab.c sz.c rz.c \
	 vmodem.h vvmodem.c vrzsz.c init.com crc.c

ARCFILES= $(TLBFILES) README rbsb.c gz ptest.sh *.t minirb.c genie.c *.1

nothing:
	@echo
	@echo "Please study the #ifdef's in crctab.c, rbsb.c, rz.c and sz.c,"
	@echo "make any necessary hacks for oddball or merged SYSV/BSD systems,"
	@echo "then type 'make SYSTEM' where SYSTEM is one of:"
	@echo
	@echo "	sysvr3	SYSTEM 5.3 Unix with mkdir(2)"
	@echo "	sysv	SYSTEM 3/5 Unix"
	@echo "	xenix	Xenix"
	@echo "	x386	386 Xenix"
	@echo "	bsd	Berkeley 4.x BSD, Ultrix, V7"
	@echo

usenet:unix
	shar -f /tmp/rzsz README Makefile zmodem.h zm.c sz.c rz.c rbsb.c \
	 init.com crc.c vmodem.h vvmodem.c vrzsz.c crctab.c minirb.c \
	 zmr.c *.1 gz ptest.sh *.t

shar:unix
	shar -f /tmp/rzsz -m 2000000 README Makefile zmodem.h zm.c \
	 zmr.c init.com vmodem.h vvmodem.c vrzsz.c sz.c rz.c crctab.c \
	 genie.c crc.c rbsb.c minirb.c *.1 gz ptest.sh *.t

unixforum: shar
	compress -b12 /tmp/rzsz.sh

tlbcmd:
	mktlb.sh README. $(TLBFILES)

unix:
	undos $(ARCFILES)

dos:
	todos $(ARCFILES)

arc:
	rm -f /tmp/rzsz.arc
	arc aq /tmp/rzsz README Makefile zmodem.h zm.c sz.c rz.c \
	 vmodem.h vvmodem.c vrzsz.c crctab.c rbsb.c \
	 zmr.c genie.c init.com crc.c *.1 gz ptest.sh *.t minirb.c
	chmod og-w /tmp/rzsz.arc
	mv /tmp/rzsz.arc /t/yam

zoo:
	rm -f /tmp/rzsz.zoo
	zoo a /tmp/rzsz README Makefile zmodem.h zm.c sz.c rz.c \
	 vmodem.h vvmodem.c vrzsz.c crctab.c rbsb.c *.1 \
	 zmr.c genie.c init.com crc.c gz ptest.sh *.t minirb.c
	touch /tmp/rzsz.zoo
	chmod og-w /tmp/rzsz.zoo
	mv /tmp/rzsz.zoo /t/yam

tags:
	ctags sz.c rz.c zm.c zmr.c rbsb.c

.PRECIOUS:rz sz

xenix:
	$(CC) $(CFLAGS) $(OFLAG) -M0 -Ox -K -i -DTXBSIZE=16384 -DNFGVMIN -DREADCHECK sz.c -lx -o sz
	$(SIZE) sz
	-ln sz sb
	-ln sz sx
	$(CC) $(CFLAGS) $(OFLAG) -M0 -Ox -K -i -DMD rz.c -o rz
	$(SIZE) rz
	-ln rz rb
	-ln rz rx

x386:
	$(CC) $(CFLAGS) $(OFLAG) -Ox -DMD rz.c -o rz
	$(SIZE) rz
	-ln rz rb
	-ln rz rx
	$(CC) $(CFLAGS) $(OFLAG) -Ox -DTXBSIZE=32768 -DNFGVMIN -DREADCHECK sz.c -lx -o sz
	$(SIZE) sz
	-ln sz sb
	-ln sz sx

sysv:
	$(CC) $(CFLAGS) $(OFLAG) -DMD rz.c -o rz
	$(SIZE) rz
	-ln rz rb
	-ln rz rx
	$(CC) $(CFLAGS) $(OFLAG) -DSV -DTXBSIZE=32768 -DNFGVMIN sz.c -o sz
	$(SIZE) sz
	-ln sz sb
	-ln sz sx

sysvr3:
	$(CC) $(CFLAGS) $(OFLAG) -DMD=2 rz.c -o rz
	$(SIZE) rz
	-ln rz rb
	-ln rz rx
	$(CC) $(CFLAGS) $(OFLAG) -DSV -DTXBSIZE=32768 -DNFGVMIN sz.c -o sz
	$(SIZE) sz
	-ln sz sb
	-ln sz sx

bsd:
	$(CC) $(CFLAGS) $(OFLAG) -DMD=2 -Dstrchr=index -DV7 -DSIGTYPE=void rz.c -o rz
	$(SIZE) rz
	-ln rz rb
	-ln rz rx
	$(CC) $(CFLAGS) $(OFLAG) -DV7 -DTXBSIZE=32768 -DNFGVMIN -DSIGTYPE=void sz.c -o sz
	$(SIZE) sz
	-ln sz sb
	-ln sz sx

sz: nothing
sb: nothing
rz: nothing
rb: nothing
