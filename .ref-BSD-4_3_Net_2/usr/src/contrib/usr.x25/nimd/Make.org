CFLAGS=-DCCITT -c -O

nimd:	nim.o ../lib/x25lib.a buf.o commands.o x29.o
	cc nim.o buf.o commands.o x29.o ../lib/x25lib.a -o nimd

install: nimd
	install -c -m 740 -s nimd /etc/nimd

nim.o:	nim.c buf.h nim.h
	cc ${CFLAGS} nim.c

buf.o:	buf.c buf.h
	cc ${CFLAGS} buf.c

commands.o: commands.c nim.h
	cc ${CFLAGS} commands.c

x29.o:	x29.c nim.h
	cc ${CFLAGS} x29.c

lint:	/tmp
	lint -hbaxu -DCCITT nim.c buf.c x29.c commands.c

clean:
	rm -f core *.o *.CKP .emacs*
