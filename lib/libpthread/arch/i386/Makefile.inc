# Machine dependent sources
.PATH: ${.CURDIR}/arch/${MACHINE}

SRCS+= machdep.c syscall.S

# Necessary until someone puts gas-2.1.1 into the release.
syscall.o: syscall.S
	cpp ${CPPFLAGS} ${.CURDIR}/arch/${MACHINE}/syscall.S | as
	mv a.out syscall.o

syscall.so: syscall.S
	cpp -DPIC ${CPPFLAGS} ${.CURDIR}/arch/${MACHINE}/syscall.S | as 
	mv a.out syscall.so

syscall.po: syscall.S
	cpp -DPROFILED ${CPPFLAGS} ${.CURDIR}/arch/${MACHINE}/syscall.S | as
	mv a.out syscall.po

