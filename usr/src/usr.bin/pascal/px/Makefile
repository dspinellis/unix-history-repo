SCCSID = "@(#)Makefile 1.4 %G%"

CFLAGS = -O
DEFS = -DVAX -DOBJ
DESTDIR =/usr/ucb
LIBDIR =/usr/lib
PASCALDIR = /usr/src/cmd/pascal
LIBPCDIR = /usr/src/lib/libpc
CC = cc
OPT = /lib/c2
AS = as
RM = rm -f

PSHDR =	OPnames.h objfmt.h opc.c pic.c
PCHDR =	libpc.h h01errs.h

PXHDR =	vars.h machdep.h

PXSRC =	int.c interp.c except.c utilities.c

PXOBJ =	int.o interp.o except.o utilities.o

PXUTL =	make.ed1 panics interp.sed clean.sed version.c

sources: ${PXHDR} ${PXSRC} ${PXUTL}
${PSHDR}:
	cp ${PASCALDIR}/$@ $@
${PCHDR}:
	cp ${LIBPCDIR}/$@ $@

.c.o:
	${CC} ${CFLAGS} ${DEFS} -c $*.c

px: Version.c ${PXOBJ}
	${CC} ${CFLAGS} -o px Version.c ${PXOBJ} -lpc -lnm
Version.c: version.c
	${CC} ${CFLAGS} -o version version.c
	./version >Version.c
	${RM}  version
interp.o interp.s: interp.c
	${CC} ${DEFS} -S interp.c
	sed -f interp.sed <interp.s >tmp
	/usr/lib/pc2 <tmp >interp.s
	${OPT} interp.s tmp
	mv tmp interp.s
	as -o interp.o interp.s
panics.h: panics make.ed1
	ex - <make.ed1
h02opcs.h: OPnames.h opc.c
	${CC} ${CFLAGS} opc.c -o opc
	./opc >h02opcs.h
	${RM} opc

install: px
	cp px ${DESTDIR}/px

clean:
	${RM}  *.o *.s px version opc* pic* OPnames.h objfmt.h libpc.h\
		panics.h h01errs.h h02opcs.h errs lpr core tmp

prt: interp.s
	sed -f clean.sed <interp.s

grind: sources
	@vpr READ_ME makefile
	@${CC} ${CFLAGS} -o pic pic.c
	@./pic | vpr
	@/usr/ucb/vgrind vars.h objfmt.h machdep.h
	@/usr/ucb/vgrind ${PXSRC}
	@${RM} pic*

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
except.o: panics.h
except.o: vars.h
int.o: vars.h
int.o: objfmt.h
interp.o: vars.h
interp.o: panics.h
interp.o: h02opcs.h
interp.o: machdep.h
interp.o: h01errs.h
interp.o: libpc.h
opc.o: OPnames.h
utilities.o: vars.h
utilities.o: panics.h
utilities.o: h02opcs.h
# DEPENDENCIES MUST END AT END OF FILE
# IF YOU PUT STUFF HERE IT WILL GO AWAY
# see make depend above
