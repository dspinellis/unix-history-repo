#	Makefile	4.7	81/03/22

DESTDIR=
CFLAGS=	-O -DSTANDALONE ${COPTS}
COPTS=	-DVAX780 -DVAX750 -DVAX730
RELOC=	70000
SRCS=	sys.c conf.c prf.c machdep.c \
	autoconf.c hp.c ht.c mba.c rk.c tm.c ts.c up.c uba.c
DRIVERS=autoconf.o hp.o ht.o mba.o rk.o tm.o ts.o up.o uba.o

all:	/usr/lib/libsa.a srt0.o boot cat ls icheck mkfs restor \
	tpicheck tpmkfs tprestor sboot

/usr/lib/libsa.a: sys.o conf.o ${DRIVERS} prf.o machdep.o
	ar crv ${DESTDIR}/usr/lib/libsa.a $?
	ranlib ${DESTDIR}/usr/lib/libsa.a

${DRIVERS}:
	cc -c -S ${COPTS} $*.c
	/lib/c2 -i $*.s | as -o $*.o
	rm $*.s

boot:	boot.o relsrt0.o ${DESTDIR}/usr/lib/libsa.a
	ld -N -T ${RELOC} relsrt0.o boot.o -lsa -lc
	cp a.out b.out; strip b.out; dd if=b.out of=boot ibs=32 skip=1; rm b.out

sboot:	boot.c relsrt0.o ${DESTDIR}/usr/lib/libsa.a
	cp boot.c sboot.c; chmod +w sboot.c
	cc -c -O -DJUSTASK sboot.c
	rm sboot.c
	ld -N -T ${RELOC} relsrt0.o sboot.o -lsa -lc
	cp a.out b.out; strip b.out; dd if=b.out of=sboot ibs=32 skip=1; rm b.out

cat:	cat.o srt0.o ${DESTDIR}/usr/lib/libsa.a
	ld -N srt0.o cat.o -lsa -lc
	cp a.out b.out; strip b.out; dd if=b.out of=cat ibs=32 skip=1; rm b.out

ls:	ls.o srt0.o ${DESTDIR}/usr/lib/libsa.a
	ld -N srt0.o ls.o -lsa -lc
	cp a.out b.out; strip b.out; dd if=b.out of=ls ibs=32 skip=1; rm b.out

mkfs.o:	/usr/src/cmd/mkfs.c
	cc ${CFLAGS} -c /usr/src/cmd/mkfs.c

mkfs:	mkfs.o srt0.o ${DESTDIR}/usr/lib/libsa.a
	ld -N srt0.o mkfs.o -lsa -lc
	cp a.out b.out; strip b.out; dd if=b.out of=mkfs ibs=32 skip=1; rm b.out

restor.o: /usr/src/cmd/restor.c
	cc ${CFLAGS} -c /usr/src/cmd/restor.c

restor:	restor.o srt0.o ${DESTDIR}/usr/lib/libsa.a
	ld -N srt0.o restor.o -lsa -lc
	cp a.out b.out; strip b.out; dd if=b.out of=restor ibs=32 skip=1; rm b.out

icheck.o: /usr/src/cmd/icheck.c
	cc ${CFLAGS} -c /usr/src/cmd/icheck.c

icheck: icheck.o srt0.o ${DESTDIR}/usr/lib/libsa.a
	ld -N srt0.o icheck.o -lsa -lc
	cp a.out b.out; strip b.out; dd if=b.out of=icheck ibs=32 skip=1; rm b.out

tpmkfs:	mkfs.o tpsrt0.o ${DESTDIR}/usr/lib/libsa.a
	ld -N tpsrt0.o mkfs.o -lsa -lc
	cp a.out b.out; strip b.out; dd if=b.out of=tpmkfs ibs=32 skip=1; rm b.out

tprestor: restor.o tpsrt0.o ${DESTDIR}/usr/lib/libsa.a
	ld -N tpsrt0.o restor.o -lsa -lc
	cp a.out b.out; strip b.out; dd if=b.out of=tprestor ibs=32 skip=1; rm b.out

tpicheck: icheck.o tpsrt0.o ${DESTDIR}/usr/lib/libsa.a
	ld -N tpsrt0.o icheck.o -lsa -lc
	cp a.out b.out; strip b.out; dd if=b.out of=tpicheck ibs=32 skip=1; rm b.out

srt0.o: srt0.c
	cc -E -DRELOC=0x${RELOC} ${COPTS} srt0.c | as -o srt0.o

tpsrt0.o: srt0.c
	cc -E -DRELOC=0x${RELOC} -DTP ${COPTS} srt0.c | as -o tpsrt0.o 

relsrt0.o: srt0.c
	cc -E -DRELOC=0x${RELOC} -DREL ${COPTS} srt0.c | as -o relsrt0.o

print:
	@pr -f makefile
	@/usr/ucb/ls -l | pr -f
	@pr -f *.h *.c

clean:
	rm -f *.o *.exe *.i
	rm -f a.out b.out boot cat ls icheck mkfs restor rpboot \
	    tpicheck tpmkfs tprestor sboot

lint:
	lint ${COPTS} -hxbn boot.c ${SRCS} | \
	    grep -v 'possible pointer alignment' | \
	    grep -v 'struct/union .* never defined'

install:
	cp tprestor ${DESTDIR}/tp/restor
	cp tpicheck ${DESTDIR}/tp/icheck
	cp tpmkfs ${DESTDIR}/tp/mkfs
	cp sboot ${DESTDIR}/tp/boot
	cp boot icheck mkfs restor cat ls ../floppy
