#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	4.7	(Berkeley)	%G%
#
CFLAGS=	-O
LIBC=	/lib/libc.a
SRCS=	done.c init.c io.c main.c save.c subr.c vocab.c wizard.c
OBJS=	done.o init.o io.o main.o save.o subr.o vocab.o wizard.o

advent: ${OBJS} ${LIBC}
	${CC} -n ${CFLAGS} ${OBJS}
	rm -f $@
	echo y | ./a.out

save.o: save.c			# vax C optimizer bug - infinite loop
	${CC} -c save.c

clean: FRC
	rm -f ${OBJS} core advent a.out

depend: FRC
	mkdep ${CFLAGS} ${SRCS}

install: FRC
	install -s -o games -g bin -m 700 advent ${DESTDIR}/usr/games/hide/adventure
	(cd ${DESTDIR}/usr/games; rm -f adventure; ln -s dm adventure; chown games.bin adventure)

lint: FRC
	lint ${CFLAGS} ${SRCS}

tags: FRC
	ctags ${SRCS}

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

done.o: done.c hdr.h
init.o: init.c hdr.h
io.o: io.c hdr.h /usr/include/stdio.h
main.o: main.c hdr.h
save.o: save.c /usr/include/a.out.h /usr/include/sys/exec.h
subr.o: subr.c hdr.h
vocab.o: vocab.c hdr.h
wizard.o: wizard.c hdr.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
