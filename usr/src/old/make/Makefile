# Description file for the Make command
# Makefile 4.5 87/05/05

OBJECTS=ident.o main.o doname.o misc.o files.o dosys.o gram.o 
SOURCES=ident.c main.c doname.c misc.c files.c dosys.c gram.c
CFLAGS=	-O -DASCARCH -I. -I/usr/src/bin/make 
LIBES= 
LINT=	lint -chapx

all:	make

make:	${OBJECTS}
	${CC} -o $@ ${CFLAGS} ${OBJECTS} ${LIBES}

${OBJECTS}: defs

clean:
	rm -f ${OBJECTS} gram.c make a.out errs core

install:
	install -s make ${DESTDIR}/bin/make

lint: FRC
	${LINT} ${SOURCES}
	rm -f gram.c

tags: FRC
	ctags ${SOURCES}

FRC:

depend: ${SOURCES}
	cc -M ${CFLAGS} ${SOURCES} | sed -e 's, ./, ,g' | \
	    awk ' { if ($$1 != prev) \
		{ if (rec != "") print rec; rec = $$0; prev = $$1; } \
		else { if (length(rec $$2) > 78) { print rec; rec = $$0; } \
		else rec = rec " " $$2 } } \
		END { print rec } ' >makedep
	echo '/^# DO NOT DELETE THIS LINE/+2,$$d' >eddep
	echo '$$r makedep' >>eddep
	echo 'w' >>eddep
	cp Makefile Makefile.bak
	ed - Makefile < eddep
	rm eddep makedep
	echo '# DEPENDENCIES MUST END AT END OF FILE' >> Makefile
	echo '# IF YOU PUT STUFF HERE IT WILL GO AWAY' >> Makefile
	echo '# see make depend above' >> Makefile

# DO NOT DELETE THIS LINE -- make depend uses it

ident.o: ident.c
main.o: main.c defs /usr/include/stdio.h /usr/include/ctype.h
main.o: /usr/include/sys/param.h /usr/include/sys/types.h /usr/include/signal.h
main.o: /usr/include/machine/machparam.h /usr/include/sys/dir.h
main.o: /usr/include/signal.h /usr/include/sys/stat.h
doname.o: doname.c defs /usr/include/stdio.h /usr/include/ctype.h
doname.o: /usr/include/sys/param.h /usr/include/sys/types.h
doname.o: /usr/include/signal.h /usr/include/machine/machparam.h
doname.o: /usr/include/sys/dir.h /usr/include/strings.h /usr/include/signal.h
misc.o: misc.c defs /usr/include/stdio.h /usr/include/ctype.h
misc.o: /usr/include/sys/param.h /usr/include/sys/types.h /usr/include/signal.h
misc.o: /usr/include/machine/machparam.h /usr/include/sys/dir.h
files.o: files.c /usr/include/fcntl.h defs /usr/include/stdio.h
files.o: /usr/include/ctype.h /usr/include/sys/param.h /usr/include/sys/types.h
files.o: /usr/include/signal.h /usr/include/machine/machparam.h
files.o: /usr/include/sys/dir.h /usr/include/sys/stat.h /usr/include/ar.h
files.o: /usr/include/a.out.h /usr/include/sys/exec.h /usr/include/sys/stat.h
dosys.o: dosys.c defs /usr/include/stdio.h /usr/include/ctype.h
dosys.o: /usr/include/sys/param.h /usr/include/sys/types.h
dosys.o: /usr/include/signal.h /usr/include/machine/machparam.h
dosys.o: /usr/include/sys/dir.h /usr/include/signal.h /usr/include/errno.h
dosys.o: /usr/include/sys/stat.h
gram.o: gram.c defs /usr/include/stdio.h /usr/include/ctype.h
gram.o: /usr/include/sys/param.h /usr/include/sys/types.h /usr/include/signal.h
gram.o: /usr/include/machine/machparam.h /usr/include/sys/dir.h
# DEPENDENCIES MUST END AT END OF FILE
# IF YOU PUT STUFF HERE IT WILL GO AWAY
# see make depend above
