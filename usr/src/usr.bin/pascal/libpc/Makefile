SCCSID = "@(#)Makefile 1.11 %G%"

CFLAGS = -O

LIB = ${DESTDIR}/usr/lib
SRCDIR = ${DESTDIR}/usr/ucb
PASDIR = ${DESTDIR}/usr/src/cmd/pascal

INSTALL= /bin/cp
LD= /bin/ld
RM = /bin/rm -f
RANLIB= ranlib
AR= /bin/ar

CMDS =	ACTFILE.c ADDT.c ARGV.c ASRT.c BUFF.c CARD.c CHR.c CLCK.c \
	CTTOT.c DATE.c DEFNAME.c DISPOSE.c EXPO.c FCALL.c FLUSH.c \
	FNIL.c FRTN.c FSAV.c GET.c GOTO.s HALT.c IN.c INCT.c LINO.c \
	LLIMIT.c LN.c MAX.c MULT.c NAM.c NEW.c NEWZ.c NIL.c PACK.c \
	PAGE.c PRED.c PUT.c RANDOM.c RANG4.c READ4.c READ8.c READC.c \
	READE.c READLN.c RELEQ.c RELNE.c RELSGE.c RELSGT.c RELSLE.c \
	RELSLT.c RELTGE.c RELTGT.c RELTLE.c RELTLT.c REMOVE.c RESET.c \
	REWRITE.c ROUND.c RSNG4.c SCLCK.c SEED.c SQRT.c STLIM.c \
	SUBSC.c SUBSCZ.c SUBT.c SUCC.c TEOF.c TEOLN.c TIME.c TRUNC.c \
	UNIT.c UNPACK.c WRITEC.c WRITEF.c WRITES.c WRITLN.c

SYS =	ERROR.c GETNAME.c IOSYNC.c PCEXIT.c PCLOSE.c PCSTART.c PERROR.c \
	PFLUSH.c PMFLUSH.c RAND.s SETRACE.s UNSYNC.c UNWIND.c blkclr.c blkcpy.c

EXTN =	APPEND.c SEEK.c TELL.c

HDRS =	whoami.h h00vars.h

OBJS =	ACTFILE.o ADDT.o ARGV.o ASRT.o BUFF.o CARD.o CHR.o CLCK.o\
	CTTOT.o DATE.o DEFNAME.o DISPOSE.o EXPO.o FCALL.o FLUSH.o\
	FNIL.o FRTN.o FSAV.o GET.o GOTO.o HALT.o IN.o INCT.o LINO.o \
	LLIMIT.o LN.o MAX.o MULT.o NAM.o NEW.o NEWZ.o NIL.o PACK.o\
	PAGE.o PRED.o PUT.o RANDOM.o RANG4.o READ4.o READ8.o READC.o\
	READE.o READLN.o RELEQ.o RELNE.o RELSGE.o RELSGT.o RELSLE.o\
	RELSLT.o RELTGE.o RELTGT.o RELTLE.o RELTLT.o REMOVE.o RESET.o\
	REWRITE.o ROUND.o RSNG4.o SCLCK.o SEED.o SQRT.o STLIM.o\
	SUBSC.o SUBSCZ.o SUBT.o SUCC.o TEOF.o TEOLN.o TIME.o TRUNC.o\
	UNIT.o UNPACK.o WRITEC.o WRITEF.o WRITES.o WRITLN.o

SYOBJ =	ERROR.o GETNAME.o IOSYNC.o PCEXIT.o PCLOSE.o PCSTART.o PERROR.o\
	PFLUSH.o PMFLUSH.o RAND.o SETRACE.o UNSYNC.o UNWIND.o blkclr.o blkcpy.o

EXOBJ =	APPEND.o SEEK.o TELL.o

sources: ${CMDS} ${SYS} ${EXTN} ${HDRS}

.s.o:
	sed -f mcount.sed $*.s | ${AS} -o tmp
	-${LD} -X -r -o profiled/$@ tmp
	${AS} -o tmp $*.s
	-${LD} -X -r -o $@ tmp

.c.o:
	${CC} -p ${CFLAGS} -c $*.c
	mv $@ tmp
	-${LD} -X -r -o profiled/$@ tmp
	${CC} ${CFLAGS} -c $*.c
	mv $@ tmp
	-${LD} -X -r -o $@ tmp

libpc libpc_p: ${OBJS} ${SYOBJ} ${EXOBJ}
	cd profiled; ${AR} cru ../libpc_p ${OBJS} ${SYOBJ} ${EXOBJ}
	${RANLIB} libpc_p
	${AR} cru libpc ${OBJS} ${SYOBJ} ${EXOBJ}
	${RANLIB} libpc

install: libpc libpc_p
	${INSTALL} libpc_p ${LIB}/libpc_p.a
	${RANLIB} ${LIB}/libpc_p.a
	${INSTALL} libpc ${LIB}/libpc.a
	${RANLIB} ${LIB}/libpc.a

whoami.h:
	cp ${PASDIR}/whoami.h whoami.h

clean:
	${RM} libpc libpc_p *.o profiled/*.o a.out lpr tmp core errs

print:
	ls -l >lst
	${SRCDIR}/vprint lst makefile *.h [A-Z][A-Z]*.[cs] [a-z][a-z]*.[cs]
	${RM} lst

grind: sources
	${SRCDIR}/ctags -v *.h *.c *.s | sort -f >index
	${SRCDIR}/vgrind -t -x index >lpr
	${SRCDIR}/vgrind -t -n makefile >>lpr
	${RM} index
	${SRCDIR}/vgrind -t *.h *.c >>lpr
	${SRCDIR}/vgrind -t -n GOTO.s RAND.s SETRACE.s >>lpr
	${SRCDIR}/vpr -t lpr

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
ACTFILE.o: h00vars.h
APPEND.o: h00vars.h
ARGV.o: h00vars.h
BUFF.o: h00vars.h
CTTOT.o: whoami.h
CTTOT.o: h00vars.h
DEFNAME.o: h00vars.h
DISPOSE.o: h00vars.h
ERROR.o: h00vars.h
FCALL.o: h00vars.h
FLUSH.o: h00vars.h
FNIL.o: h00vars.h
FRTN.o: h00vars.h
FSAV.o: h00vars.h
GET.o: h00vars.h
GETNAME.o: h00vars.h
HALT.o: h00vars.h
IN.o: h00vars.h
INCT.o: h00vars.h
IOSYNC.o: h00vars.h
LINO.o: h00vars.h
LLIMIT.o: h00vars.h
MAX.o: h00vars.h
NAM.o: h00vars.h
NEW.o: h00vars.h
NEWZ.o: h00vars.h
NIL.o: h00vars.h
PAGE.o: h00vars.h
PCEXIT.o: h00vars.h
PCLOSE.o: h00vars.h
PCSTART.o: h00vars.h
PERROR.o: h00vars.h
PFLUSH.o: h00vars.h
PMFLUSH.o: h00vars.h
PUT.o: h00vars.h
RANDOM.o: h00vars.h
READ4.o: h00vars.h
READ8.o: h00vars.h
READC.o: h00vars.h
READE.o: h00vars.h
READLN.o: h00vars.h
RELEQ.o: h00vars.h
RELNE.o: h00vars.h
RELSGE.o: h00vars.h
RELSGT.o: h00vars.h
RELSLE.o: h00vars.h
RELSLT.o: h00vars.h
RELTGE.o: h00vars.h
RELTGT.o: h00vars.h
RELTLE.o: h00vars.h
RELTLT.o: h00vars.h
REMOVE.o: h00vars.h
RESET.o: h00vars.h
REWRITE.o: h00vars.h
SEED.o: h00vars.h
SEEK.o: h00vars.h
STLIM.o: h00vars.h
TELL.o: h00vars.h
TEOF.o: h00vars.h
TEOLN.o: h00vars.h
UNIT.o: h00vars.h
UNSYNC.o: h00vars.h
WRITEC.o: h00vars.h
WRITEF.o: h00vars.h
WRITES.o: h00vars.h
WRITLN.o: h00vars.h
# DEPENDENCIES MUST END AT END OF FILE
# IF YOU PUT STUFF HERE IT WILL GO AWAY
# see make depend above
