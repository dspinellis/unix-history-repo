CFLAGS = -O
LIB = ${DESTDIR}/usr/lib
LD= ld
RM = /bin/rm -f
RANLIB= ranlib
AR= ar
LORDER= lorder
GRIND = vgrind
PRINT = vprint
VPR = vpr

CMDS =	ACTFILE.o ADDT.o ARGV.o ASRT.o BUFF.o CARD.o CHR.o CLCK.o CTTOT.o\
	DATE.o DEFNAME.o DISPOSE.o EXPO.o FCALL.o FLUSH.o FNIL.o\
	FRTN.o FSAV.o GET.o GOTO.o HALT.o IN.o INCT.o LINO.o LLIMIT.o\
	LN.o MAX.o MULT.o NAM.o NARGCHK.o NEW.o NEWZ.o NIL.o PACK.o PAGE.o\
	PRED.o PUT.o RANDOM.o RANG4.o READ4.o READ8.o READC.o\
	READE.o READLN.o RELEQ.o RELNE.o RELSGE.o RELSGT.o RELSLE.o\
	RELSLT.o RELTGE.o RELTGT.o RELTLE.o RELTLT.o REMOVE.o RESET.o\
	REWRITE.o ROUND.o RSNG4.o SCLCK.o SEED.o SEEK.o SQRT.o\
	STLIM.o SUBSC.o SUBSCZ.o SUBT.o SUCC.o TEOF.o TEOLN.o TIME.o\
	TRUNC.o UNIT.o UNPACK.o WRITEC.o WRITEF.o WRITES.o WRITLN.o

SYOBJ =	GETNAME.o IOSYNC.o PCLOSE.o PCSTART.o ERROR.o PFLUSH.o\
	PMFLUSH.o SETRACE.o UNSYNC.o UNWIND.o ZFRAME.o

.s.o:
	cat ashdr.s $*.s | ${AS} -o tmp
	-${LD} -x -r -o $@ tmp

.c.o:
	${CC} ${CFLAGS} -c $*.c
	mv $@ tmp
	-${LD} -x -r -o $@ tmp

pclib: h01errs.h PCEXIT.o ${CMDS} ${SYOBJ}
	${RM} pclib
	${AR} cr pclib PCEXIT.o `${LORDER} ${CMDS} ${SYOBJ} | tsort`
	${RANLIB} pclib

h01errs.h: errdata
	ed - <make.ed1
	${RM} E.s *.o

install: pclib
	cp pclib ${LIB}/libpc.a
	${RANLIB} ${LIB}/libpc.a

new: pclib
	cp pclib ${NEWLIB}/libpc.a
	${RANLIB} ${NEWLIB}/libpc.a

clean:
	${RM} pclib *.o a.out lpr tmp core errs

print:
	ls -l >lst
	${PRINT} lst makefile *.h [A-Z][A-Z]*.[cs] [a-z][a-z]*.[cs]
	${RM} lst

grind:
	${VGRIND} -t -n makefile >lpr
	cat </dev/null >index
	${VGRIND} -t *.h *.c >>lpr
	${VGRIND} -t -x index >lindex
	${VGRIND} -t -n ashdr.s GOTO.s SETRACE.s >>lpr
	${VPR} -t lindex lpr
	${RM} lpr lindex index
