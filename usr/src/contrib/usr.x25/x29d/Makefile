x29d:	x29d.o
	cc x29d.o -o x29d ../lib/x25lib.a

install:	x29d
	install -s -m 740 x29d /etc/x29d

x29d.o:	../h/x29.h
	cc -O -c -DCCITT x29d.c

lint:
	lint -hbaxu -DCCITT x29d.c

clean:
	rm -f core *.o *.CKP .emacs*
