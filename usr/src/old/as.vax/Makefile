#
# Copyright (c) 1980 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.4 (Berkeley) %G%
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
HDRS = astoks.H astokfix.awk as.h asexpr.h asnumber.h asscan.h asscanl.h \
	assyms.h instrs.h

SRCS = asscan1.c asscan2.c asscan3.c asscan4.c \
	bignum1.c bignum2.c natof.c floattab.c \
	asmain.c asparse.c \
	asexpr.c assyms.c \
	asjxxx.c ascode.c aspseudo.c \
	assizetab.c asio.c

OBJS = asscan1.o asscan2.o asscan3.o asscan4.o \
	bignum1.o bignum2.o natof.o floattab.o\
	asparse.o asexpr.o \
	asmain.o assyms.o \
	asjxxx.o ascode.o aspseudo.o \
	assizetab.o asio.o

GRIND = astoks.h as.h asscan.h assyms.h asexpr.h instrs.h asnumber.h \
	asscanl.h asscan1.c asscan2.c asscan3.c asscan4.c \
	bignum1.c bignum2.c natof.c floattab.c \
	asmain.c asscan.c asparse.c asexpr.c \
	assyms.c asjxxx.c ascode.c asio.c \
	assizetab.c aspseudo.c

DESTDIR = 

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

AS = /bin/as
LD = /bin/ld

DFLAGS= -DAS
CFLAGS = -O ${DFLAGS}

LDFLAGS = -O

as:	${OBJS}
	${CC} ${LDFLAGS} ${OBJS} -o as

.c.o:	astoks.h ${HDRS}
	${CC} ${CFLAGS} -c $*.c

astoks.h: astoks.H astokfix.awk
	awk -f astokfix.awk < astoks.H > astoks.h

aspseudo.o:	as.h astoks.h aspseudo.c instrs.h instrs.as
	${CC} -c -R ${DFLAGS} aspseudo.c

instrs.as: instrs
	(echo FLAVOR AS ; cat instrs) | awk -f instrs > instrs.as

lint:
	lint ${DFLAGS} ${SRCS}
clean:
	rm -f ${OBJS} instrs.as as core a.out errs

depend:

install:
	install -s as ${DESTDIR}/bin

print:
	pr Makefile ${HDRS} ${SRCS}
