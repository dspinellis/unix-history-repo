SCCSID = "@(#)Makefile 1.11 %G%"

CFLAGS = -O
DESTDIR =
SRCDIR = /usr/ucb
LIBDIR = /usr/lib
PASCALDIR = /usr/src/cmd/pascal
LIBPCDIR = /usr/src/lib/libpc
CC = cc
OPT = /lib/c2
AS = as
RM = rm -f

PSHDR =	OPnames.h whoami.h objfmt.h opc.c pic.c
PCHDR =	libpc.h

PXHDR =	vars.h machdep.h

PXSRC =	int.c interp.c utilities.c

PXOBJ =	int.o interp.o utilities.o

PXUTL =	interp.sed clean.sed version.c

sources: ${PXHDR} ${PXSRC} ${PXUTL}
${PSHDR}:
	cp ${PASCALDIR}/$@ $@
${PCHDR}:
	cp ${LIBPCDIR}/$@ $@

.c.o:
	${CC} ${CFLAGS} -c $*.c

px: Version.c ${PXOBJ}
	${CC} ${CFLAGS} -o px Version.c ${PXOBJ} -lpc -lm
Version.c: version.c
	${CC} ${CFLAGS} -o version version.c
	./version >Version.c
	${RM}  version
interp.o interp.s: interp.c
	${CC} -S interp.c
	sed -f interp.sed <interp.s >tmp
	${OPT} tmp interp.s
	/usr/lib/pc2 <interp.s >tmp
	mv tmp interp.s
	as -o interp.o interp.s
h02opcs.h: OPnames.h opc.c
	${CC} ${CFLAGS} opc.c -o opc
	./opc >h02opcs.h
	${RM} opc

install: px
	cp px ${DESTDIR}/usr/ucb/px

clean:
	${RM}  *.o *.s px version opc* pic* OPnames.h objfmt.h libpc.h\
		h02opcs.h errs lpr core tmp

prt: interp.s
	sed -f clean.sed <interp.s

grind: sources ${PSHDR} ${PCHDR}
	${SRCDIR}/ctags -v *.h *.c *.s | sort -f >index
	${SRCDIR}/vgrind -t -x index >lpr
	${CC} ${CFLAGS} -o pic pic.c
	${SRCDIR}/vgrind -t -n READ_ME makefile lst >>lpr
	${SRCDIR}/vgrind -t ${PCHDR} ${PXHDR} objfmt.h whoami.h >>lpr
	${SRCDIR}/vgrind -t ${PXSRC} >>lpr
	./pic >lst
	${SRCDIR}/vpr lst
	${SRCDIR}/vpr -t lpr
	${RM} index pic lst

depend:	sources
	/bin/grep '^#[ 	]*include' *.c \
		| sed '/<.*>/d' \
		| sed 's/:[^"]*"\([^"]*\)".*/: \1/' \
		| sed 's/\.c/.o/' >makedep
	echo '/^# DO NOT DELETE THIS LINE/+2,$$d' >eddep
	echo '$$r makedep' >>eddep
	echo 'w' >>eddep
	cp makefile makefile.bak
	ed - makefile < eddep
	rm eddep makedep
	echo '# DEPENDENCIES MUST END AT END OF FILE' >> makefile
	echo '# IF YOU PUT STUFF HERE IT WILL GO AWAY' >> makefile
	echo '# see make depend above' >> makefile

# DO NOT DELETE THIS LINE -- make depend uses it
# DEPENDENCIES MUST END AT END OF FILE
int.o: whoami.h
int.o: vars.h
int.o: libpc.h
int.o: objfmt.h
interp.o: whoami.h
interp.o: objfmt.h
interp.o: vars.h
interp.o: h02opcs.h
interp.o: machdep.h
interp.o: libpc.h
opc.o: OPnames.h
utilities.o: whoami.h
utilities.o: vars.h
utilities.o: h02opcs.h
# DEPENDENCIES MUST END AT END OF FILE
# IF YOU PUT STUFF HERE IT WILL GO AWAY
# see make depend above
