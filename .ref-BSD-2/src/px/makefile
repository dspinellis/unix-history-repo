AS=	-as
CFLAGS=	-O -DHZ=60.0
ASMS=	00head.s 00int.s 02rel.s 02relset.s 03bool.s 04as.s 05lv.s  \
	06add.s 07sub.s 10mul.s 12div.s 13mod.s 14neg.s  \
	16dvd.s 17ind.s 17rv.s 20con.s 21rang.s 24case.s 24pxp.s 25set.s  \
	26for.s 27conv.s 30atof.s 30getname.s 30io.s 30iosubs.s 30read.s  \
	30write.s 34fun.s E.s opcode.s wait.s
OBJS=	as.o exit.o int.o palloc.o pcttot.o pdattim.o perror.o pwrite.o

all: a.out a.outNOID

a.out:	${OBJS}
	ld -X -i ../pascal/assubs/pcrt0.o *.o ../pascal/fpterp/fp.o -lc -l

a.outNOID: ${OBJS}
	ld -X -n ../pascal/assubs/pcrt0.o *.o ../pascal/fpnofetchi/fp.o\
		-lc -l -o a.outNOID

00head.s:
	echo 'FP = 0' > 00head.s

as.o:	${ASMS}
	rm -f as.o
	${AS} -o as.o ${ASMS}

int.c palloc.c pcttot.c perror.c: E.h

E.s E.h: Edata
	ed - Edata < Emake

clean:
	-rm -f a.out a.outNOID core E.h E.s 00head.s
	-rm -f *.o

print:
	@pr makefile README
	@ls -ls | pr
	@pr ../opcodes/picture
	@pr 0x.h int.c exit.c
	@pr [0-9][0-9]*.s
	@pr p*.c

install:
	cp a.out /usr/ucb/px
	cp a.outNOID /usr/ucb/px34
