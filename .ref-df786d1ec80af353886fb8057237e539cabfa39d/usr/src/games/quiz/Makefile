#	@(#)Makefile	4.1	(Berkeley)	%G%
#
DESTDIR=
CFLAGS=	-O

quiz: quiz.c
	cc ${CFLAGS} -o quiz quiz.c

install:
	install quiz ${DESTDIR}/usr/games/quiz
	cp -r quiz.k ${DESTDIR}/usr/games/lib

clean:
	rm -f a.out core *.s *.o quiz

depend:
	cat </dev/null >x.c
	for i in quiz; do \
		(echo $$i: $$i.c >>makedep; \
		/bin/grep '^#[ 	]*include' x.c $$i.c | sed \
			-e 's,<\(.*\)>,"/usr/include/\1",' \
			-e 's/:[^"]*"\([^"]*\)".*/: \1/' \
			-e 's/\.c//' >>makedep); done
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

quiz: quiz.c
quiz: /usr/include/stdio.h
quiz: /usr/include/signal.h
# DEPENDENCIES MUST END AT END OF FILE
# IF YOU PUT STUFF HERE IT WILL GO AWAY
# see make depend above
