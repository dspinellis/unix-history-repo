#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	1.3	(Berkeley)	%G%
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

clean:
	rm -f ${OBJS} core as

cleandir: clean
	rm -f ${MAN} tags .depend

depend: ${SRCS}
	mkdep ${CFLAGS} ${SRCS}

install: ${MAN}
	install -s -o bin -g bin as ${DESTDIR}/usr/bin

lint: ${SRCS}
	lint ${CFLAGS} ${SRCS}

tags: ${SRCS}
	ctags ${SRCS}
