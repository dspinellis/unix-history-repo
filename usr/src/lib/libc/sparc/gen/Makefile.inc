#	@(#)Makefile.inc	8.1 (Berkeley) 6/4/93

# machine-dependent gen sources
SRCS+=	_setjmp.s fabs.s fixunsdfsi.s modf.s mul.s saveregs.s setjmp.s umul.s
SRCS+=	frexp.c isinf.c isnan.c ldexp.c

# `source' files built from m4 source
# the name `div.o' is taken for the ANSI C `div' function, hence sdiv here
SRCS+=	rem.s sdiv.s udiv.s urem.s
CLEANFILES+=rem.s sdiv.s udiv.s urem.s

# these are copied down to the kernel library
KMSRCS+=mul.s rem.s sdiv.s udiv.s umul.s urem.s

# need DEFS.h too
libkern.sparc::
	cp -p ${.CURDIR}/sparc/DEFS.h /sys/libkern/sparc

sdiv.s: divrem.m4
	@echo 'building ${.TARGET} from ${.ALLSRC}'
	@(echo "define(NAME,\`.div')define(OP,\`div')define(S,\`true')"; \
	 cat ${.ALLSRC}) | m4 > ${.TARGET}
	@chmod 444 ${.TARGET}

udiv.s: divrem.m4
	@echo 'building ${.TARGET} from ${.ALLSRC}'
	@(echo "define(NAME,\`.udiv')define(OP,\`div')define(S,\`false')"; \
	 cat ${.ALLSRC}) | m4 > ${.TARGET}
	@chmod 444 ${.TARGET}

rem.s: divrem.m4
	@echo 'building ${.TARGET} from ${.ALLSRC}'
	@(echo "define(NAME,\`.rem')define(OP,\`rem')define(S,\`true')"; \
	 cat ${.ALLSRC}) | m4 > ${.TARGET}
	@chmod 444 ${.TARGET}

urem.s: divrem.m4
	@echo 'building ${.TARGET} from ${.ALLSRC}'
	@(echo "define(NAME,\`.urem')define(OP,\`rem')define(S,\`false')"; \
	 cat ${.ALLSRC}) | m4 > ${.TARGET}
	@chmod 444 ${.TARGET}
