#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	1.25	(Berkeley)	%G%
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
	UNIT.c UNPACK.c VWRITEF.c WRITEC.c WRITEF.c WRITES.c WRITLN.c
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
	UNIT.o UNPACK.o VWRITEF.o WRITEC.o WRITEF.o WRITES.o WRITLN.o
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
	@echo building normal libpc
	@ar cru libpc ${OBJS}
	ranlib libpc
	@echo building profiled libpc
	@cd profiled; ar cru ../libpc_p ${OBJS}
	ranlib libpc_p

clean:
	rm -f ${OBJS} core libpc libpc_p profiled/*.o tmp tmp.o

cleandir: clean
	rm -f ${MAN} tags .depend

depend: ${SRCS}
	mkdep ${CFLAGS} ${SRCS}

install: ${MAN}
	install -o bin -g bin -m 644 libpc_p ${DESTDIR}/usr/lib/libpc_p.a
	ranlib -t ${DESTDIR}/usr/lib/libpc_p.a
	install -o bin -g bin -m 644 libpc ${DESTDIR}/usr/lib/libpc.a
	ranlib -t ${DESTDIR}/usr/lib/libpc.a

lint: ${SRCS}
	lint ${CFLAGS} ${SRCS}

tags: ${SRCS}
	ctags ${SRCS}
