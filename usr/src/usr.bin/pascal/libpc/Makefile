#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	1.23	(Berkeley)	%G%
#
CFLAGS=	-O
CMDS=	ACTFILE.c ADDT.c ARGV.c ASRT.c ASRTS.c ATAN.c BUFF.c CARD.c \
	CASERNG.c CHR.c CLCK.c COS.c CTTOT.c DATE.c DEFNAME.c \
	DFDISPOSE.c DISPOSE.c EXCEPT.c EXP.c EXPO.c FCALL.c FLUSH.c \
	FNIL.c FRTN.c FSAV.c GET.c HALT.c IN.c INCT.c LINO.c \
	LLIMIT.c LN.c MAX.c MULT.c NAM.c NEW.c NIL.c PACK.c \
	PAGE.c PRED.c PUT.c RANDOM.c RANG4.c READ4.c READ8.c READC.c \
	READE.c READLN.c RELEQ.c RELNE.c RELSGE.c RELSGT.c RELSLE.c \
	RELSLT.c RELTGE.c RELTGT.c RELTLE.c RELTLT.c REMOVE.c RESET.c \
	REWRITE.c ROUND.c RSNG4.c SCLCK.c SEED.c SIN.c SQRT.c STLIM.c \
	SUBSC.c SUBSCZ.c SUBT.c SUCC.c TEOF.c TEOLN.c TIME.c TRUNC.c \
	UNIT.c UNPACK.c WRITEC.c WRITEF.c WRITES.c WRITLN.c
SYS=	ERROR.c GETNAME.c IOSYNC.c PCEXIT.c PCLOSE.c PCSTART.c \
	PFCLOSE.c PERROR.c PFLUSH.c PMFLUSH.c UNSYNC.c \
	blkclr.c blkcpy.c
EXTN=	APPEND.c SEEK.c TELL.c
SRCS=	${CMDS} ${SYS} ${EXTN}
COBJS=	ACTFILE.o ADDT.o ARGV.o ASRT.o ASRTS.o ATAN.o BUFF.o CARD.o \
	CASERNG.o CHR.o CLCK.o COS.o CTTOT.o DATE.o DEFNAME.o \
	DFDISPOSE.o DISPOSE.o EXCEPT.o EXP.o EXPO.o FCALL.o FLUSH.o \
	FNIL.o FRTN.o FSAV.o GET.o HALT.o IN.o INCT.o LINO.o \
	LLIMIT.o LN.o MAX.o MULT.o NAM.o NEW.o NIL.o PACK.o\
	PAGE.o PRED.o PUT.o RANDOM.o RANG4.o READ4.o READ8.o READC.o\
	READE.o READLN.o RELEQ.o RELNE.o RELSGE.o RELSGT.o RELSLE.o\
	RELSLT.o RELTGE.o RELTGT.o RELTLE.o RELTLT.o REMOVE.o RESET.o\
	REWRITE.o ROUND.o RSNG4.o SCLCK.o SEED.o SIN.o SQRT.o STLIM.o\
	SUBSC.o SUBSCZ.o SUBT.o SUCC.o TEOF.o TEOLN.o TIME.o TRUNC.o\
	UNIT.o UNPACK.o WRITEC.o WRITEF.o WRITES.o WRITLN.o
SYOBJ =	ERROR.o GETNAME.o IOSYNC.o PCEXIT.o PCLOSE.o PCSTART.o \
	PFCLOSE.o PERROR.o PFLUSH.o PMFLUSH.o UNSYNC.o \
	blkclr.o blkcpy.o
EXOBJ =	APPEND.o SEEK.o TELL.o
OBJS=	${COBJS} ${SYOBJ} ${EXOBJ}

.c.o:
	${CC} -p ${CFLAGS} -c $*.c
	mv $@ tmp.o
	-ld -X -r -o profiled/$@ tmp.o
	${CC} ${CFLAGS} -c $*.c
	mv $@ tmp.o
	-ld -x -r -o $@ tmp.o

all: libpc libpc_p

libpc libpc_p: ${OBJS}
	cd profiled; ar cru ../libpc_p ${OBJS}
	ranlib libpc_p
	ar cru libpc ${OBJS}
	ranlib libpc

clean: FRC
	rm -f ${OBJS} core libpc libpc_p profiled/*.o tmp tmp.o

depend: FRC
	mkdep ${CFLAGS} ${SRCS}

install: FRC
	install -o bin -g bin -m 644 libpc_p ${DESTDIR}/usr/lib/libpc_p.a
	ranlib ${DESTDIR}/usr/lib/libpc_p.a
	install -o bin -g bin -m 644 libpc ${DESTDIR}/usr/lib/libpc.a
	ranlib ${DESTDIR}/usr/lib/libpc.a

lint: FRC
	lint ${CFLAGS} ${SRCS}

tags: FRC
	ctags ${SRCS}

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

ACTFILE.o: ACTFILE.c h00vars.h /usr/include/stdio.h whoami.h
ADDT.o: ADDT.c
ARGV.o: ARGV.c h00vars.h /usr/include/stdio.h whoami.h
ASRT.o: ASRT.c
ASRTS.o: ASRTS.c
ATAN.o: ATAN.c /usr/include/math.h
BUFF.o: BUFF.c h00vars.h /usr/include/stdio.h whoami.h
CARD.o: CARD.c
CASERNG.o: CASERNG.c
CHR.o: CHR.c
CLCK.o: CLCK.c
COS.o: COS.c /usr/include/math.h
CTTOT.o: CTTOT.c whoami.h h00vars.h /usr/include/stdio.h whoami.h
DATE.o: DATE.c
DEFNAME.o: DEFNAME.c h00vars.h /usr/include/stdio.h whoami.h
DFDISPOSE.o: DFDISPOSE.c h00vars.h /usr/include/stdio.h whoami.h libpc.h
DISPOSE.o: DISPOSE.c h00vars.h /usr/include/stdio.h whoami.h
EXCEPT.o: EXCEPT.c /usr/include/signal.h
EXP.o: EXP.c /usr/include/math.h
EXPO.o: EXPO.c
FCALL.o: FCALL.c h00vars.h /usr/include/stdio.h whoami.h
FLUSH.o: FLUSH.c h00vars.h /usr/include/stdio.h whoami.h
FNIL.o: FNIL.c h00vars.h /usr/include/stdio.h whoami.h
FRTN.o: FRTN.c h00vars.h /usr/include/stdio.h whoami.h
FSAV.o: FSAV.c h00vars.h /usr/include/stdio.h whoami.h
GET.o: GET.c h00vars.h /usr/include/stdio.h whoami.h
HALT.o: HALT.c h00vars.h /usr/include/stdio.h whoami.h
IN.o: IN.c h00vars.h /usr/include/stdio.h whoami.h
INCT.o: INCT.c h00vars.h /usr/include/stdio.h whoami.h
LINO.o: LINO.c h00vars.h /usr/include/stdio.h whoami.h
LLIMIT.o: LLIMIT.c h00vars.h /usr/include/stdio.h whoami.h
LN.o: LN.c /usr/include/math.h
MAX.o: MAX.c h00vars.h /usr/include/stdio.h whoami.h
MULT.o: MULT.c
NAM.o: NAM.c h00vars.h /usr/include/stdio.h whoami.h
NEW.o: NEW.c h00vars.h /usr/include/stdio.h whoami.h
NIL.o: NIL.c h00vars.h /usr/include/stdio.h whoami.h
PACK.o: PACK.c
PAGE.o: PAGE.c h00vars.h /usr/include/stdio.h whoami.h
PRED.o: PRED.c
PUT.o: PUT.c h00vars.h /usr/include/stdio.h whoami.h
RANDOM.o: RANDOM.c h00vars.h /usr/include/stdio.h whoami.h
RANG4.o: RANG4.c
READ4.o: READ4.c h00vars.h /usr/include/stdio.h whoami.h /usr/include/errno.h
READ8.o: READ8.c h00vars.h /usr/include/stdio.h whoami.h /usr/include/errno.h
READC.o: READC.c h00vars.h /usr/include/stdio.h whoami.h
READE.o: READE.c h00vars.h /usr/include/stdio.h whoami.h
READLN.o: READLN.c h00vars.h /usr/include/stdio.h whoami.h
RELEQ.o: RELEQ.c h00vars.h /usr/include/stdio.h whoami.h
RELNE.o: RELNE.c h00vars.h /usr/include/stdio.h whoami.h
RELSGE.o: RELSGE.c h00vars.h /usr/include/stdio.h whoami.h
RELSGT.o: RELSGT.c h00vars.h /usr/include/stdio.h whoami.h
RELSLE.o: RELSLE.c h00vars.h /usr/include/stdio.h whoami.h
RELSLT.o: RELSLT.c h00vars.h /usr/include/stdio.h whoami.h
RELTGE.o: RELTGE.c h00vars.h /usr/include/stdio.h whoami.h
RELTGT.o: RELTGT.c h00vars.h /usr/include/stdio.h whoami.h
RELTLE.o: RELTLE.c h00vars.h /usr/include/stdio.h whoami.h
RELTLT.o: RELTLT.c h00vars.h /usr/include/stdio.h whoami.h
REMOVE.o: REMOVE.c h00vars.h /usr/include/stdio.h whoami.h
RESET.o: RESET.c h00vars.h /usr/include/stdio.h whoami.h
REWRITE.o: REWRITE.c h00vars.h /usr/include/stdio.h whoami.h
ROUND.o: ROUND.c
RSNG4.o: RSNG4.c
SCLCK.o: SCLCK.c
SEED.o: SEED.c h00vars.h /usr/include/stdio.h whoami.h
SIN.o: SIN.c /usr/include/math.h
SQRT.o: SQRT.c /usr/include/math.h
STLIM.o: STLIM.c h00vars.h /usr/include/stdio.h whoami.h
SUBSC.o: SUBSC.c
SUBSCZ.o: SUBSCZ.c
SUBT.o: SUBT.c
SUCC.o: SUCC.c
TEOF.o: TEOF.c h00vars.h /usr/include/stdio.h whoami.h
TEOLN.o: TEOLN.c h00vars.h /usr/include/stdio.h whoami.h
TIME.o: TIME.c
TRUNC.o: TRUNC.c
UNIT.o: UNIT.c h00vars.h /usr/include/stdio.h whoami.h
UNPACK.o: UNPACK.c
WRITEC.o: WRITEC.c h00vars.h /usr/include/stdio.h whoami.h
WRITEF.o: WRITEF.c h00vars.h /usr/include/stdio.h whoami.h
WRITES.o: WRITES.c h00vars.h /usr/include/stdio.h whoami.h
WRITLN.o: WRITLN.c h00vars.h /usr/include/stdio.h whoami.h
ERROR.o: ERROR.c /usr/include/stdio.h /usr/include/signal.h
GETNAME.o: GETNAME.c h00vars.h /usr/include/stdio.h whoami.h libpc.h
IOSYNC.o: IOSYNC.c h00vars.h /usr/include/stdio.h whoami.h
PCEXIT.o: PCEXIT.c h00vars.h /usr/include/stdio.h whoami.h
PCLOSE.o: PCLOSE.c h00vars.h /usr/include/stdio.h whoami.h libpc.h
PCSTART.o: PCSTART.c /usr/include/signal.h h00vars.h /usr/include/stdio.h
PCSTART.o: whoami.h libpc.h
PFCLOSE.o: PFCLOSE.c h00vars.h /usr/include/stdio.h whoami.h libpc.h
PERROR.o: PERROR.c /usr/include/stdio.h /usr/include/signal.h
PFLUSH.o: PFLUSH.c h00vars.h /usr/include/stdio.h whoami.h
PMFLUSH.o: PMFLUSH.c h00vars.h /usr/include/stdio.h whoami.h
UNSYNC.o: UNSYNC.c h00vars.h /usr/include/stdio.h whoami.h
blkclr.o: blkclr.c
blkcpy.o: blkcpy.c
APPEND.o: APPEND.c h00vars.h /usr/include/stdio.h whoami.h
SEEK.o: SEEK.c h00vars.h /usr/include/stdio.h whoami.h
TELL.o: TELL.c h00vars.h /usr/include/stdio.h whoami.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
