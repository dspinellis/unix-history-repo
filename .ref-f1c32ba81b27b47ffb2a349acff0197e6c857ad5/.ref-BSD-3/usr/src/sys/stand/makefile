CFLAGS = -O -DSTANDALONE
RELOC=	50000
DRIVERS=hp.o ht.o mba.o up.o uba.o

all:	/usr/lib/libsa.a srt0.o boot cat ls mkfs restor tpmkfs tprestor

/usr/lib/libsa.a:	sys.o conf.o hp.o ht.o mba.o up.o uba.o prf.o machdep.o 
	ar crv /usr/lib/libsa.a $?

${DRIVERS}:
	cc -c -S ${COPTS} $*.c
	/lib/c2 -i $*.s $*.os
	as -o $*.o $*.os
	rm $*.s $*.os

boot:	boot.o relsrt0.o /usr/lib/libsa.a
	ld -N -T ${RELOC} relsrt0.o boot.o -lsa -lc
	cp a.out b.out; strip b.out; dd if=b.out of=boot ibs=32 skip=1; rm b.out

cat:	cat.o srt0.o /usr/lib/libsa.a
	ld -N srt0.o cat.o -lsa -lc
	cp a.out b.out; strip b.out; dd if=b.out of=cat ibs=32 skip=1; rm b.out

ls:	ls.o srt0.o /usr/lib/libsa.a
	ld -N srt0.o ls.o -lsa -lc
	cp a.out b.out; strip b.out; dd if=b.out of=ls ibs=32 skip=1; rm b.out

mkfs:	mkfs.o srt0.o /usr/lib/libsa.a
	ld -N srt0.o mkfs.o -lsa -lc
	cp a.out b.out; strip b.out; dd if=b.out of=mkfs ibs=32 skip=1; rm b.out

restor:	restor.o srt0.o /usr/lib/libsa.a
	ld -N srt0.o restor.o -lsa -lc
	cp a.out b.out; strip b.out; dd if=b.out of=restor ibs=32 skip=1; rm b.out

tpmkfs:	mkfs.o tpsrt0.o /usr/lib/libsa.a
	ld -N tpsrt0.o mkfs.o -lsa -lc
	cp a.out b.out; strip b.out; dd if=b.out of=tpmkfs ibs=32 skip=1; rm b.out

tprestor: restor.o tpsrt0.o /usr/lib/libsa.a
	ld -N tpsrt0.o restor.o -lsa -lc
	cp a.out b.out; strip b.out; dd if=b.out of=tprestor ibs=32 skip=1; rm b.out

srt0.o: rel.m ../h/mtpr.m ../h/mba.m ../h/uba.m srt0.s
	as -o srt0.o rel.m ../h/mtpr.m ../h/mba.m ../h/uba.m srt0.s

tpsrt0.o: rel.m ../h/mtpr.m ../h/mba.m ../h/uba.m tpsrt0.s
	as -o tpsrt0.o rel.m ../h/mtpr.m ../h/mba.m ../h/uba.m tpsrt0.s

relsrt0.o: rel.m ../h/mtpr.m ../h/mba.m ../h/uba.m relsrt0.s
	as -o relsrt0.o rel.m ../h/mtpr.m ../h/mba.m ../h/uba.m relsrt0.s

rpboot: rpboot.s
	as rpboot.s
	cp a.out b.out; strip b.out; dd if=b.out of=rpboot bs=32 skip=1; rm b.out
print:
	@pr makefile
	@/usr/ucb/ls -l | pr
	@pr *.h
	@pr *.m *.s
	@pr *.c

clean:
	rm -f *.o *.exe
	rm -f a.out b.out boot cat ls mkfs restor rpboot

install:
	cp tprestor ${DESTDIR}/sys/restor
	cp tpmkfs ${DESTDIR}/sys/mkfs
	cp boot ${DESTDIR}/sys/boot
	cp boot mkfs restor cat ls ../floppy
