#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	1.2	(Berkeley)	%G%
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
#	asscan4.c	constructs normal integers; interfaces with bignum.c
#
#	bignum.c	constructs large integers; utility routines
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
HDRS=	astoks.H astokfix.awk as.h asscan.h assyms.h asexpr.h 
SRCS=	asscan1.c asscan2.c asscan3.c asscan4.c bignum.c asmain.c asparse.c \
	asexpr.c assyms.c asjxxx.c ascode.c aspseudo.c assizetab.c asio.c
OBJS=	asscan1.o asscan2.o asscan3.o asscan4.o bignum.o asparse.o asexpr.o \
	asmain.o assyms.o asjxxx.o ascode.o aspseudo.o assizetab.o asio.o
GRIND = astoks.h as.h asscan.h assyms.h asexpr.h instrs.h asnumber.h \
	asscanl.h asscan1.c asscan2.c asscan3.c asscan4.c bignum.c \
	asmain.c asscan.c asparse.c asexpr.c assyms.c asjxxx.c ascode.c \
	asio.c assizetab.c aspseudo.c
#
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
DFLAGS=	-DAS
CFLAGS=	-O $(DFLAGS)

all: as

as: ${OBJS} ${LIBC}
	${CC} ${CFLAGS} ${OBJS} -o $@

astoks.h: astoks.H astokfix.awk
	awk -f astokfix.awk < astoks.H > astoks.h

aspseudo.o: as.h astoks.h aspseudo.c instrs.h instrs.as
	${CC} -c -R ${DFLAGS} aspseudo.c

instrs.as: instrs
	(echo FLAVOR AS ; cat instrs) | awk -f instrs > instrs.as

clean: FRC
	rm -f ${OBJS} core as

depend: FRC
	mkdep ${CFLAGS} ${SRCS}

install: FRC
	install -s -o bin -g bin as ${DESTDIR}/bin/as

lint: FRC
	lint ${CFLAGS} ${SRCS}

tags: FRC
	ctags ${SRCS}

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

asscan1.o: asscan1.c
asscan1.o:asscanl.h
asscan1.o: /usr/include/stdio.h
asscan1.o:as.h
asscan1.o: /usr/include/sys/types.h /usr/include/a.out.h
asscan1.o: /usr/include/sys/exec.h /usr/include/stab.h
asscan1.o:instrs.h
asscan1.o:astoks.h
asscan1.o:asnumber.h
asscan1.o:asscan.h
asscan2.o: asscan2.c
asscan2.o:asscanl.h
asscan2.o: /usr/include/stdio.h
asscan2.o:as.h
asscan2.o: /usr/include/sys/types.h /usr/include/a.out.h
asscan2.o: /usr/include/sys/exec.h /usr/include/stab.h
asscan2.o:instrs.h
asscan2.o:astoks.h
asscan2.o:asnumber.h
asscan2.o:asscan.h
asscan3.o: asscan3.c
asscan3.o:asscanl.h
asscan3.o: /usr/include/stdio.h
asscan3.o:as.h
asscan3.o: /usr/include/sys/types.h /usr/include/a.out.h
asscan3.o: /usr/include/sys/exec.h /usr/include/stab.h
asscan3.o:instrs.h
asscan3.o:astoks.h
asscan3.o:asnumber.h
asscan3.o:asscan.h
asscan4.o: asscan4.c
asscan4.o:asscanl.h
asscan4.o: /usr/include/stdio.h
asscan4.o:as.h
asscan4.o: /usr/include/sys/types.h /usr/include/a.out.h
asscan4.o: /usr/include/sys/exec.h /usr/include/stab.h
asscan4.o:instrs.h
asscan4.o:astoks.h
asscan4.o:asnumber.h
asscan4.o:asscan.h
bignum.o: bignum.c /usr/include/errno.h /usr/include/stdio.h
bignum.o:as.h
bignum.o: /usr/include/sys/types.h /usr/include/a.out.h /usr/include/sys/exec.h
bignum.o: /usr/include/stab.h
bignum.o:instrs.h
bignum.o:astoks.h
bignum.o:asnumber.h
asmain.o: asmain.c /usr/include/stdio.h /usr/include/ctype.h
asmain.o: /usr/include/signal.h
asmain.o:as.h
asmain.o: /usr/include/sys/types.h /usr/include/a.out.h /usr/include/sys/exec.h
asmain.o: /usr/include/stab.h
asmain.o:instrs.h
asmain.o:astoks.h
asmain.o:asnumber.h
asmain.o:assyms.h
asmain.o:asscan.h
asmain.o:asexpr.h
asparse.o: asparse.c /usr/include/stdio.h
asparse.o:as.h
asparse.o: /usr/include/sys/types.h /usr/include/a.out.h
asparse.o: /usr/include/sys/exec.h /usr/include/stab.h
asparse.o:instrs.h
asparse.o:astoks.h
asparse.o:asnumber.h
asparse.o:asscan.h
asparse.o:assyms.h
asparse.o:asexpr.h
asexpr.o: asexpr.c /usr/include/stdio.h
asexpr.o:as.h
asexpr.o: /usr/include/sys/types.h /usr/include/a.out.h /usr/include/sys/exec.h
asexpr.o: /usr/include/stab.h
asexpr.o:instrs.h
asexpr.o:astoks.h
asexpr.o:asnumber.h
asexpr.o:asscan.h
asexpr.o:asexpr.h
assyms.o: assyms.c /usr/include/stdio.h /usr/include/ctype.h
assyms.o:as.h
assyms.o: /usr/include/sys/types.h /usr/include/a.out.h /usr/include/sys/exec.h
assyms.o: /usr/include/stab.h
assyms.o:instrs.h
assyms.o:astoks.h
assyms.o:asnumber.h
assyms.o:asscan.h
assyms.o:assyms.h
asjxxx.o: asjxxx.c /usr/include/stdio.h
asjxxx.o:as.h
asjxxx.o: /usr/include/sys/types.h /usr/include/a.out.h /usr/include/sys/exec.h
asjxxx.o: /usr/include/stab.h
asjxxx.o:instrs.h
asjxxx.o:astoks.h
asjxxx.o:asnumber.h
asjxxx.o:assyms.h
ascode.o: ascode.c /usr/include/stdio.h
ascode.o:as.h
ascode.o: /usr/include/sys/types.h /usr/include/a.out.h /usr/include/sys/exec.h
ascode.o: /usr/include/stab.h
ascode.o:instrs.h
ascode.o:astoks.h
ascode.o:asnumber.h
ascode.o:assyms.h
aspseudo.o: aspseudo.c /usr/include/stdio.h
aspseudo.o:as.h
aspseudo.o: /usr/include/sys/types.h /usr/include/a.out.h
aspseudo.o: /usr/include/sys/exec.h /usr/include/stab.h
aspseudo.o:instrs.h
aspseudo.o:astoks.h
aspseudo.o:asnumber.h
aspseudo.o:instrs.as
assizetab.o: assizetab.c /usr/include/stdio.h
assizetab.o:as.h
assizetab.o: /usr/include/sys/types.h /usr/include/a.out.h
assizetab.o: /usr/include/sys/exec.h /usr/include/stab.h
assizetab.o:instrs.h
assizetab.o:astoks.h
assizetab.o:asnumber.h
assizetab.o:assyms.h
asio.o: asio.c /usr/include/stdio.h
asio.o:as.h
asio.o: /usr/include/sys/types.h /usr/include/a.out.h /usr/include/sys/exec.h
asio.o: /usr/include/stab.h
asio.o:instrs.h
asio.o:astoks.h
asio.o:asnumber.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
