
SCCSID = "@(#)makefile	4.1 (Berkeley) 3/1/81"
INSTALLDIR = ${DESTDIR}/usr/ucb

eyacc: ey0.o ey1.o ey2.o ey3.o ey4.o ey5.o
	cc -o eyacc -z ey0.o ey1.o ey2.o ey3.o ey4.o ey5.o

.c.o:
	cc -O -c $*.c

clean:
	-rm -f *.o eyacc

install: eyacc
	cp eyacc ${INSTALLDIR}/eyacc

print:
	@ls -l | pr
	@pr READ_ME makefile
	@pr ey.h ey0.c ey1.c ey2.c ey3.c ey4.c ey5.c

ey1.o:	ey.h
ey2.o:	ey.h
ey3.o:	ey.h
ey4.o:	ey.h
