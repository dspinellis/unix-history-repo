#
# Copyright (c) 1980 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.5 (Berkeley) 6/2/87
#
#	as.h		Definitions for data structures
#	asnumber.h	Definitions for all numbers: byte .. G format float
#	asscan.h	Definitions for the character scanner
#	asscanl.h	Definitions for the character scanner;
#				local to asscan?.c
#	astokfix.awk
#	astoks.H	The defines for tokens that yacc produced
#				This is processed by astokfix.awk to yield:
#	astoks.h	Included implicitly in as.h
#	asexpr.h	The macros for parsing and expressions
#	assyms.h	Macros for dealing with the symbol table
#	instrs.h	Definitions to make instrs more readable
#
#	asscan1.c	buffer management, yylex, and buffer drainer
#	asscan2.c	character scanner, and buffer filler
#	asscan3.c	character sets definitions (initialized data)
#	asscan4.c	constructs normal integers; interfaces with bignum?.c
#
#	bignum1.c	constructs large integers; utility routines
#	bignum2.c	packs/unpacks floating point numbers
#	natof.c		constructs large floating point #'s, ala atof
#
#	asparse.c	parser
#	asexpr.c	parses expressions, constructs and evaluates
#				expression trees
#
#	asmain.c	main body
#	assyms.c	symbol table processing routines
#	asjxxx.c	Fixes jxxx instructions
#	ascode.c	Emits code
#	assizetab.c	Converts internal ordinal #'s into sizes, strings, etc
#	asio.c		Does block I/O and faster versions of fwrite
#
#	aspseudo.c	Symbol table definitions for reserved words
#	instrs		included in pseudo.c; instructions and semantic info
#				for each instructions
#
HDRS=	astoks.H astokfix.awk as.h asexpr.h asnumber.h asscan.h asscanl.h \
	assyms.h instrs.h
SRCS=	asscan1.c asscan2.c asscan3.c asscan4.c bignum1.c bignum2.c natof.c \
	floattab.c asmain.c asparse.c asexpr.c assyms.c asjxxx.c ascode.c \
	aspseudo.c assizetab.c asio.c
OBJS=	asscan1.o asscan2.o asscan3.o asscan4.o bignum1.o bignum2.o natof.o \
	floattab.o asparse.o asexpr.o asmain.o assyms.o asjxxx.o ascode.o \
	aspseudo.o assizetab.o asio.o
GRIND = astoks.h as.h asscan.h assyms.h asexpr.h instrs.h asnumber.h \
	asscanl.h asscan1.c asscan2.c asscan3.c asscan4.c bignum1.c \
	bignum2.c natof.c floattab.c asmain.c asscan.c asparse.c asexpr.c \
	assyms.c asjxxx.c ascode.c asio.c assizetab.c aspseudo.c

#	available flags:
#
#	AS		This is the assembler; always set
#  (UNIX and VMS are mutually exclusive.)
#	UNIX		Must be set if the assembler is to produce a.out
#			files for UNIX.
#
#	VMS		Must be set if the assembler is to produce executables
#			for VMS (Thanks to David Kashtan, SRI for these fixes)
#
#	if VMS is set, then these two flags are also valid:
#			(necessary to frob system calls and '$' conventions
#	VMSDEVEL	The assembler is being compiled under VMS
#	UNIXDEVEL	The assembler is being compiled under UNIX
#
#
#	DEBUG		print out various debugging information
#			in the first pass
#
#	FLEXNAMES	All names are stored internally as true character
#			strings, null terminated, and can be no more
#			than BUFSIZ long.
#			
AS=	/bin/as
DFLAGS=	-DAS
CFLAGS=	-O ${DFLAGS}

as: ${OBJS} ${LIBC}
	${CC} -o $@ ${CFLAGS} ${OBJS}

.c.o:	astoks.h ${HDRS}
	${CC} ${CFLAGS} -c $*.c

astoks.h: astoks.H astokfix.awk
	awk -f astokfix.awk < astoks.H > astoks.h

aspseudo.o: as.h astoks.h aspseudo.c instrs.h instrs.as
	${CC} -c -R ${DFLAGS} aspseudo.c

instrs.as: instrs
	(echo FLAVOR AS ; cat instrs) | awk -f instrs > instrs.as

lint: FRC
	lint ${CFLAGS} ${SRCS}

clean: FRC
	rm -f ${OBJS} instrs.as as core a.out errs

depend: instrs.as FRC
	mkdep ${CFLAGS} ${SRCS}

install: FRC
	install -s -o bin -g bin -m 755 as ${DESTDIR}/bin/as

tags: FRC
	ctags ${SRCS}

print:
	pr Makefile ${HDRS} ${SRCS}

FRC:
# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

asscan1.o: asscan1.c asscanl.h /usr/include/stdio.h as.h
asscan1.o: /usr/include/sys/types.h /usr/include/a.out.h
asscan1.o: /usr/include/sys/exec.h /usr/include/stab.h instrs.h astoks.h
asscan1.o: asnumber.h asscan.h
asscan2.o: asscan2.c asscanl.h /usr/include/stdio.h as.h
asscan2.o: /usr/include/sys/types.h /usr/include/a.out.h
asscan2.o: /usr/include/sys/exec.h /usr/include/stab.h instrs.h astoks.h
asscan2.o: asnumber.h asscan.h
asscan3.o: asscan3.c asscanl.h /usr/include/stdio.h as.h
asscan3.o: /usr/include/sys/types.h /usr/include/a.out.h
asscan3.o: /usr/include/sys/exec.h /usr/include/stab.h instrs.h astoks.h
asscan3.o: asnumber.h asscan.h
asscan4.o: asscan4.c asscanl.h /usr/include/stdio.h as.h
asscan4.o: /usr/include/sys/types.h /usr/include/a.out.h
asscan4.o: /usr/include/sys/exec.h /usr/include/stab.h instrs.h astoks.h
asscan4.o: asnumber.h asscan.h
bignum1.o: bignum1.c /usr/include/errno.h /usr/include/stdio.h as.h
bignum1.o: /usr/include/sys/types.h /usr/include/a.out.h
bignum1.o: /usr/include/sys/exec.h /usr/include/stab.h instrs.h astoks.h
bignum1.o: asnumber.h
bignum2.o: bignum2.c /usr/include/stdio.h as.h /usr/include/sys/types.h
bignum2.o: /usr/include/a.out.h /usr/include/sys/exec.h /usr/include/stab.h
bignum2.o: instrs.h astoks.h asnumber.h
natof.o: natof.c /usr/include/stdio.h /usr/include/ctype.h /usr/include/errno.h
natof.o: as.h /usr/include/sys/types.h /usr/include/a.out.h
natof.o: /usr/include/sys/exec.h /usr/include/stab.h instrs.h astoks.h
natof.o: asnumber.h
floattab.o: floattab.c /usr/include/stdio.h as.h /usr/include/sys/types.h
floattab.o: /usr/include/a.out.h /usr/include/sys/exec.h /usr/include/stab.h
floattab.o: instrs.h astoks.h asnumber.h
asmain.o: asmain.c /usr/include/stdio.h /usr/include/ctype.h
asmain.o: /usr/include/signal.h as.h /usr/include/sys/types.h
asmain.o: /usr/include/a.out.h /usr/include/sys/exec.h /usr/include/stab.h
asmain.o: instrs.h astoks.h asnumber.h assyms.h asscan.h asexpr.h
asmain.o: /usr/include/sys/stat.h
asparse.o: asparse.c /usr/include/stdio.h as.h /usr/include/sys/types.h
asparse.o: /usr/include/a.out.h /usr/include/sys/exec.h /usr/include/stab.h
asparse.o: instrs.h astoks.h asnumber.h asscan.h assyms.h asexpr.h
asexpr.o: asexpr.c /usr/include/stdio.h as.h /usr/include/sys/types.h
asexpr.o: /usr/include/a.out.h /usr/include/sys/exec.h /usr/include/stab.h
asexpr.o: instrs.h astoks.h asnumber.h asscan.h asexpr.h
assyms.o: assyms.c /usr/include/stdio.h /usr/include/ctype.h as.h
assyms.o: /usr/include/sys/types.h /usr/include/a.out.h /usr/include/sys/exec.h
assyms.o: /usr/include/stab.h instrs.h astoks.h asnumber.h asscan.h assyms.h
asjxxx.o: asjxxx.c /usr/include/stdio.h as.h /usr/include/sys/types.h
asjxxx.o: /usr/include/a.out.h /usr/include/sys/exec.h /usr/include/stab.h
asjxxx.o: instrs.h astoks.h asnumber.h assyms.h
ascode.o: ascode.c /usr/include/stdio.h as.h /usr/include/sys/types.h
ascode.o: /usr/include/a.out.h /usr/include/sys/exec.h /usr/include/stab.h
ascode.o: instrs.h astoks.h asnumber.h assyms.h
aspseudo.o: aspseudo.c /usr/include/stdio.h as.h /usr/include/sys/types.h
aspseudo.o: /usr/include/a.out.h /usr/include/sys/exec.h /usr/include/stab.h
aspseudo.o: instrs.h astoks.h asnumber.h instrs.as
assizetab.o: assizetab.c /usr/include/stdio.h as.h /usr/include/sys/types.h
assizetab.o: /usr/include/a.out.h /usr/include/sys/exec.h /usr/include/stab.h
assizetab.o: instrs.h astoks.h asnumber.h assyms.h
asio.o: asio.c /usr/include/stdio.h as.h /usr/include/sys/types.h
asio.o: /usr/include/a.out.h /usr/include/sys/exec.h /usr/include/stab.h
asio.o: instrs.h astoks.h asnumber.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
