lpr:	lpr.c spool.c
	cc -n -s -O lpr.c -o lpr

lpd:	lpd.c daemon.c daemon0.c
	cc -n -s -O lpd.c -o lpd

install:
	cp lpr /bin/lpr
	cp lpd /usr/lib/lpd
	rm lpr
	rm lpd
	echo make sure /etc/rc contains
	echo rm /usr/spool/lpd/lock; /usr/lib/lpd
	mkdir /usr/spool
	mkdir /usr/spool/lpd
