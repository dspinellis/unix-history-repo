DESTDIR=
CFLAGS=	-O
STD=	jot lam rs

all:	${STD}

${STD}:
	cc ${CFLAGS} -o $@ $@.c

install:
	install -s jot ${DESTDIR}/usr/new/jot
	install -s lam ${DESTDIR}/usr/new/lam
	install -s rs ${DESTDIR}/usr/new/rs

clean:
	rm -f a.out core *.s *.o
	rm -f ${STD}

depend:
	cat </dev/null >x.c
	for i in ${STD}; do \
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

jot: jot.c
jot: /usr/include/stdio.h
jot: /usr/include/ctype.h
lam: lam.c
lam: /usr/include/stdio.h
rs: rs.c
rs: /usr/include/stdio.h
rs: /usr/include/ctype.h
# DEPENDENCIES MUST END AT END OF FILE
# IF YOU PUT STUFF HERE IT WILL GO AWAY
# see make depend above
