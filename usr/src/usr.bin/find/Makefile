#	@(#)Makefile	4.4	(Berkeley)	87/06/18
#
DESTDIR=
CFLAGS=	-O
LFLAGS=	

# The programs themselves
#
PROG=	find
LIBS=	bigram code

# Sources
#
SRCS=	find.c bigram.c code.c

all: ${PROG} ${LIBS}

find: find.c
	cc ${CFLAGS} -o find find.c

bigram: bigram.c
	cc ${CFLAGS} -o bigram bigram.c

code: code.c
	cc ${CFLAGS} -o code code.c

install: ${PROG} ${LIBS}
	install -s ${PROG} ${DESTDIR}/usr/bin/${PROG}
	-mkdir ${DESTDIR}/usr/lib/find
	install -s bigram ${DESTDIR}/usr/lib/find/bigram
	install -s code ${DESTDIR}/usr/lib/find/code
	install -c -m 755 updatedb.csh ${DESTDIR}/usr/lib/find/updatedb

clean:
	rm -f a.out core ${LIBS} ${PROG}

depend:
	cat </dev/null >x.c
	for i in ${SRCS}; do \
		(echo `basename $$i .c`.o: $$i >>makedep; \
		/bin/grep '^#[ 	]*include' x.c $$i | sed \
			-e 's,<\(.*\)>,"/usr/include/\1",' \
			-e 's/:[^"]*"\([^"]*\)".*/: \1/' \
			-e 's/\.c/.o/' >>makedep); done
	echo '/^# DO NOT DELETE THIS LINE/+2,$$d' >eddep
	echo '$$r makedep' >>eddep
	echo 'w' >>eddep
	cp Makefile Makefile.bak
	ed - Makefile < eddep
	rm eddep makedep x.c
	echo '# DEPENDENCIES MUST END AT END OF FILE' >> Makefile
	echo '# IF YOU PUT STUFF HERE IT WILL GO AWAY' >> Makefile
	echo '# see make depend above' >> Makefile

# DO NOT DELETE THIS LINE -- make depend uses it

find.o: find.c
find.o: /usr/include/stdio.h
find.o: /usr/include/sys/param.h
find.o: /usr/include/sys/dir.h
find.o: /usr/include/sys/stat.h
find.o: /usr/include/signal.h
bigram.o: bigram.c
bigram.o: /usr/include/stdio.h
code.o: code.c
code.o: /usr/include/stdio.h
# DEPENDENCIES MUST END AT END OF FILE
# IF YOU PUT STUFF HERE IT WILL GO AWAY
# see make depend above
