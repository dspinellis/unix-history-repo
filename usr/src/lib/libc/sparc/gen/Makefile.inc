#	@(#)Makefile.inc	5.1 (Berkeley) %G%

# machine-dependent gen sources
SRCS+=	_setjmp.s fabs.s fixunsdfsi.s main.s modf.s mul.s saveregs.s \
	setjmp.s umul.s
SRCS+=	frexp.c isinf.c isnan.c ldexp.c

# `source' files built from m4 source
# the name `div.o' is taken for the ANSI C `div' function, hence sdiv here
SRCS+=	sdiv.s udiv.s rem.s urem.s
CLEANFILES+=sdiv.s udiv.s rem.s urem.s

sdiv.s: divrem.m4
	@echo 'building ${.TARGET} from ${.ALLSRC}'
	@(echo "define(NAME,\`.div')define(OP,\`div')define(S,\`true')"; \
	 cat ${.ALLSRC}) | m4 > ${.TARGET}

udiv.s: divrem.m4
	@echo 'building ${.TARGET} from ${.ALLSRC}'
	@(echo "define(NAME,\`.udiv')define(OP,\`div')define(S,\`false')"; \
	 cat ${.ALLSRC}) | m4 > ${.TARGET}

rem.s: divrem.m4
	@echo 'building ${.TARGET} from ${.ALLSRC}'
	@(echo "define(NAME,\`.rem')define(OP,\`rem')define(S,\`true')"; \
	 cat ${.ALLSRC}) | m4 > ${.TARGET}

urem.s: divrem.m4
	@echo 'building ${.TARGET} from ${.ALLSRC}'
	@(echo "define(NAME,\`.urem')define(OP,\`rem')define(S,\`false')"; \
	 cat ${.ALLSRC}) | m4 > ${.TARGET}
