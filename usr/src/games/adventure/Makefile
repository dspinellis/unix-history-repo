#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	4.5	(Berkeley)	%G%
#
CFLAGS=	-O
LIBC=	/lib/libc.a
LFLGS=	-n		# should be -i on small machines, -n on a vax
SRCS=	done.c init.c io.c main.c save.c subr.c vocab.c wizard.c
OBJS=	done.o init.o io.o main.o save.o subr.o vocab.o wizard.o

all: advent

advent: a.out
	rm -f advent
	echo y | ./a.out

a.out: ${OBJS} ${LIBC}
	${CC} ${LDFLAGS} ${CFLAGS} ${OBJS}

PROG:	${OBJS} ${LIBC}
	${CC} -o $@ ${CFLAGS} ${OBJS}

save.o: save.c			# vax C optimizer bug - infinite loop
	${CC} -c save.c

# always recomple okplay
okplay.o: FRC

advsaver: advsaver.s
	${AS} -o $@ advsaver.s

clean: FRC
	rm -f ${OBJS} core advent a.out

depend: FRC
	mkdep ${CFLAGS} ${SRCS}

install: FRC
	install -o bin -g bin -m 755 -s advent ${DESTDIR}/usr/games/adventure
#	cp okplay /usr/games/adventure
#	cp advsaver /usr/games/lib
#	dd if=/usr/games/lib/adventure of=/usr/games/lib/advheader bs=1 count=16
#	chmod 711 /usr/games/lib
#	chmod 700 /usr/games/lib/adventure
#	chmod 755 /usr/games/lib/advheader
#	chmod 755 /usr/games/lib/advsaver
#	chmod 4711 /usr/games/adventure
#	chown games /usr/games/adventure /usr/games/lib/adventure /usr/games/lib/advheader /usr/games/lib/advsaver
#	rm advent a.out okplay 

lint: FRC
	lint ${CFLAGS} ${SRCS}

tags: FRC
	ctags ${SRCS}

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

done.o: done.c
done.o:hdr.h
init.o: init.c
init.o:hdr.h
io.o: io.c
io.o:hdr.h
io.o: /usr/include/stdio.h
main.o: main.c
main.o:hdr.h
save.o: save.c /usr/include/a.out.h /usr/include/sys/exec.h
subr.o: subr.c
subr.o:hdr.h
vocab.o: vocab.c
vocab.o:hdr.h
wizard.o: wizard.c
wizard.o:hdr.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
