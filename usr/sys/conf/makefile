unix:	l.o mch.o c.o ../sys/LIB1 ../dev/LIB2
	ld -o unix -X -i l.o mch.o c.o ../sys/LIB1 ../dev/LIB2

all:
	cd ../sys; cc -c -O *.c; mklib; rm *.o
	cd ../dev; cc -c -O *.c; mklib; rm *.o

mch.o:	mch0.s mch.s
	as -o mch.o mch0.s mch.s

allsystems:
	mkconf <hphtconf
	make unix
	mv unix hphtunix
	mkconf <hptmconf
	make unix
	mv unix hptmunix
	mkconf <rptmconf
	make unix
	mv unix rptmunix
	mkconf <rphtconf
	make unix
	mv unix rphtunix
	mkconf <rktmconf
	make unix
	mv unix rktmunix
	mkconf <rkhtconf
	make unix
	mv unix rkhtunix
