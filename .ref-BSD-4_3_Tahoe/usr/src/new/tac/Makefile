#	Makefile	1.1	6/9/85
#
DESTDIR=
BINDIR=	/usr/new
MANTYPE= n

CFLAGS=	-O

STD=	tac
SCRIPT=	tmail
MAN=	tac.1 tmail.1

all:	${STD}

${STD}:
	cc ${CFLAGS} -o $@ $@.c

install: all
	install -s tac ${DESTDIR}${BINDIR}/tac
	install -m 755 -c tmail.sh ${DESTDIR}${BINDIR}/tmail
	-for i in ${MAN}; do \
	    (install -m 644 -c $$i \
	    ${DESTDIR}/usr/man/man${MANTYPE}/`basename $$i .1`.${MANTYPE}); \
	    done

clean:
	rm -f *.o ${STD} Makefile.bak makedep eddep a.out core errs 

depend:
	rm -f makedep
	for i in ${STD}; do \
		(${CC} -M $$i.c |sed 's/.o: /: /' >>makedep); done
	echo '/^# DO NOT DELETE THIS LINE/+2,$$d' >eddep
	echo '$$r makedep' >>eddep
	echo 'w' >>eddep
	chmod u+w Makefile
	cp Makefile Makefile.bak
	ed - Makefile < eddep
	rm -f eddep makedep
	echo '# DEPENDENCIES MUST END AT END OF FILE' >> Makefile
	echo '# IF YOU PUT STUFF HERE IT WILL GO AWAY' >> Makefile
	echo '# see make depend above' >> Makefile

# DO NOT DELETE THIS LINE -- make depend uses it

tac: tac.c
tac: /usr/include/sys/types.h
tac: /usr/include/sys/stat.h
tac: /usr/include/stdio.h
# DEPENDENCIES MUST END AT END OF FILE
# IF YOU PUT STUFF HERE IT WILL GO AWAY
# see make depend above
