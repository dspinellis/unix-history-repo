#	Makefile	4.4	86/02/12

DEBUG=
CFLAGS= -O  ${DEBUG}

OBJS= bsdtcp.o df2.o df12.o dk.o dn.o hys.o hysq.o mic.o nov.o pen.o pnet.o \
	rvmacs.o sy.o unet.o va212.o va811.o va820.o vad.o vent.o vmacs.o \
	hys24.o cds224.o att2224.o

aculib.a: ${OBJS}
	ar cr aculib.a ${OBJS}
	-ranlib aculib.a 2>&1 | sed '/no symbol table/d'

${OBJS}: ../uucp.h ../condevs.h

clean:
	rm -f *.o core errs  aculib.a
