CFLAGS = -O

all:	/usr/lib/libsa.a srt0.o restor cat icheck mkfs mtboot maketape boot

cp cmp:
	@echo "standalone stuff-- examine this makefile before running"

xcp:	all
	mkdir /stand
	cp restor cat mkfs icheck /stand
	rm restor cat mkfs icheck
	cp boot /
	rm boot
	rm *.o

/usr/lib/libsa.a:	SYS.o conf.o hp.o ht.o prf.o rk.o rp.o tm.o 
	ar rv /usr/lib/libsa.a $?

restor:	srt0.o
	cc+ld-stand restor

cat:	srt0.o
	cc -c cat.c
	ld -s -o cat srt0.o cat.o -lsa -lc
	rm cat.o

mkfs:	srt0.o
	cc+ld-stand mkfs

icheck:	srt0.o
	cc+ld-stand icheck

mtboot:	mtboot.s
	as -o mtboot mtboot.s
	strip mtboot

maketape:	maketape.c
	cc -o maketape maketape.c

boot:	boot.o M.o
	ld -s -o boot M.o boot.o -lsa -lc

distr:	restor cat mkfs boot mtboot maketape
	maketape /dev/nrmt0 tapedir
	dump 0f /dev/nrmt0 /dev/rp11
	dump 0f /dev/rmt0 /dev/rp13

rkdistr:	restor cat mkfs boot mtboot maketape
	maketape /dev/nrmt0 rktapedir
	mv /v7/rp*unix /v7/hp*unix /v7/usr
	dump 0f /dev/nrmt0 /dev/rp11
	mv /v7/usr/*unix /v7
	cpgroup /v7/usr/src/cmd `cd /v7/usr/src/cmd; echo [a-m]*`
	dump 0f /dev/nrmt0 /dev/rrk2
	cpgroup /v7/usr/src/cmd `cd /v7/usr/src/cmd; echo [n-z]*`
	dump 0f /dev/nrmt0 /dev/rrk2
	cpgroup /v7/usr/src `cd /v7/usr/src/; echo [d-z]*`
	dump 0f /dev/nrmt0 /dev/rrk2
	cpgroup /v7/usr lib dmr games include mdec pub spool tmp
	dump 0f /dev/nrmt0 /dev/rrk2
	cpgroup /v7/usr doc
	dump 0f /dev/nrmt0 /dev/rrk2
	cpgroup /v7/usr dict sys man
	dump 0f /dev/rmt0 /dev/rrk2
