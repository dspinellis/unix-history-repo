#  FP interpreter/compiler
#  Copyright (c) 1982  Scott B. Baden
#  Berkeley, California
#
#  Copyright (c) 1982 Regents of the University of California.
#  All rights reserved.  The Berkeley software License Agreement
#  specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	4.3 (Berkeley) %G%
#
DESTDIR =

FP_SYS = 	runFp.l fpMain.l\
		scanner.l parser.l codeGen.l\
		primFp.l utils.l fpPP.l handlers.l fpMeasures.l

FP_SYSO = 	runFp.o fpMain.o\
		scanner.o parser.o codeGen.o\
		primFp.o utils.o  fpPP.o handlers.o fpMeasures.o

.SUFFIXES:
.SUFFIXES:	.l .o
.l.o:
		liszt -axq $<


fp:		${FP_SYSO}
		/bin/rm -f fp
		echo "(progn (load 'runFp)\
			     (dumplisp fp))" | lisp > junk.fp
		chmod 755 fp

${FP_SYSO}:	specials.l fpMacs.o


install:
	install -s -o bin -g bin -m 755 fp ${DESTDIR}/usr/bin

clean:		
		rm -f core *.[ox] junk.fp


listings:	${FP_SYS}
		print runFp.l fpMain.l scanner.l parser.l codeGen.l \
		primFp.l utils.l fpPP.l handlers.l fpMeasures.l \
		specials.l fpMacs.l  &

depend:
