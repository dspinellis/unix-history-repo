SCCSID = "@(#)Makefile 1.2 %G%"
CFLAGS = -O
LIB = ${DESTDIR}/usr/lib
LD= ld
RM = /bin/rm -f
RANLIB= ranlib
AR= ar
GRIND = vgrind
PRINT = vprint
VPR = vpr
GET = sccs get

CMDS =	ACTFILE.c ADDT.c APPEND.c ARGV.c ASRT.c BUFF.c CARD.c CHR.c CLCK.c \
	CTTOT.c DATE.c DEFNAME.c DISPOSE.c EXPO.c FCALL.c FLUSH.c FNIL.c \
	FRTN.c FSAV.c GET.c GOTO.s HALT.c IN.c INCT.c LINO.c LLIMIT.c \
	LN.c MAX.c MULT.c NAM.c NARGCHK.s NEW.c NEWZ.c NIL.c PACK.c \
	PAGE.c PRED.c PUT.c RANDOM.c RANG4.c READ4.c READ8.c READC.c \
	READE.c READLN.c RELEQ.c RELNE.c RELSGE.c RELSGT.c RELSLE.c \
	RELSLT.c RELTGE.c RELTGT.c RELTLE.c RELTLT.c REMOVE.c RESET.c \
	REWRITE.c ROUND.c RSNG4.c SCLCK.c SEED.c SEEK.c SQRT.c STLIM.c \
	SUBSC.c SUBSCZ.c SUBT.c SUCC.c TEOF.c TEOLN.c TIME.c TRUNC.c \
	UNIT.c UNPACK.c WRITEC.c WRITEF.c WRITES.c WRITLN.c

SYS =	ERROR.c GETNAME.c IOSYNC.c PCEXIT.c PCLOSE.c PCSTART.c PFLUSH.c \
	PMFLUSH.c SETRACE.s UNSYNC.c UNWIND.c ZFRAME.c

HDRS =	h00vars.h ashdr.s errdata make.ed1

OBJS =	ACTFILE.o ADDT.o APPEND.o ARGV.o ASRT.o BUFF.o CARD.o CHR.o CLCK.o\
	CTTOT.o DATE.o DEFNAME.o DISPOSE.o EXPO.o FCALL.o FLUSH.o FNIL.o\
	FRTN.o FSAV.o GET.o GOTO.o HALT.o IN.o INCT.o LINO.o LLIMIT.o\
	LN.o MAX.o MULT.o NAM.o NARGCHK.o NEW.o NEWZ.o NIL.o PACK.o PAGE.o\
	PRED.o PUT.o RANDOM.o RANG4.o READ4.o READ8.o READC.o\
	READE.o READLN.o RELEQ.o RELNE.o RELSGE.o RELSGT.o RELSLE.o\
	RELSLT.o RELTGE.o RELTGT.o RELTLE.o RELTLT.o REMOVE.o RESET.o\
	REWRITE.o ROUND.o RSNG4.o SCLCK.o SEED.o SEEK.o SQRT.o\
	STLIM.o SUBSC.o SUBSCZ.o SUBT.o SUCC.o TEOF.o TEOLN.o TIME.o\
	TRUNC.o UNIT.o UNPACK.o WRITEC.o WRITEF.o WRITES.o WRITLN.o

SYOBJ =	ERROR.o GETNAME.o IOSYNC.o PCEXIT.o PCLOSE.o PCSTART.o PFLUSH.o\
	PMFLUSH.o SETRACE.o UNSYNC.o UNWIND.o ZFRAME.o

sources: ${CMDS} ${SYS} ${HDRS}
${CMDS} ${SYS}: ${HDRS}
	${GET} ${REL} $@
${HDRS}:
	${GET} ${REL} $@

.s.o:
	cat ashdr.s $*.s | ${AS} -o tmp
	-${LD} -x -r -o $@ tmp

.c.o:
	${CC} ${CFLAGS} -c $*.c
	mv $@ tmp
	-${LD} -x -r -o $@ tmp

libpc: h01errs.h ${OBJS} ${SYOBJ}
	${RM} libpc
	${AR} cr libpc ${OBJS} ${SYOBJ}
	${RANLIB} libpc

h01errs.h: errdata make.ed1 ashdr.s
	ed - errdata <make.ed1
	${RM} E.s *.o

install: libpc
	cp libpc ${LIB}/libpc.a
	${RANLIB} ${LIB}/libpc.a

new: libpc
	cp libpc ${NEWLIB}/libpc.a
	${RANLIB} ${NEWLIB}/libpc.a

clean:
	${RM} libpc *.o a.out lpr tmp core errs
	sccs clean

print:
	ls -l >lst
	${PRINT} lst makefile *.h [A-Z][A-Z]*.[cs] [a-z][a-z]*.[cs]
	${RM} lst

grind: sources
	${VGRIND} -t -n makefile >lpr
	cat </dev/null >index
	${VGRIND} -t *.h *.c >>lpr
	${VGRIND} -t -x index >lindex
	${VGRIND} -t -n ashdr.s GOTO.s SETRACE.s NARGCHK.s >>lpr
	${VPR} -t lindex lpr
	${RM} lpr lindex index
