#	@(#)Makefile	5.11 (Berkeley) 6/28/90
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
PROG=	as
CFLAGS+=-DAS -I.
SRCS=	asscan1.c asscan2.c asscan3.c asscan4.c bignum1.c bignum2.c natof.c \
	floattab.c asmain.c asparse.c asexpr.c assyms.c asjxxx.c ascode.c \
	aspseudo.c assizetab.c asio.c
CLEANFILES=instrs.as astoks.h

astoks.h: astoks.H astokfix.awk
	awk -f ${.CURDIR}/astokfix.awk < ${.CURDIR}/astoks.H > astoks.h

aspseudo.o:
	${CC} ${CFLAGS} -R -c ${.IMPSRC}

instrs.as: ${.CURDIR}/instrs
	(echo FLAVOR AS ; cat ${.CURDIR}/instrs) | \
	    awk -f ${.CURDIR}/instrs > instrs.as

.include <bsd.prog.mk>

${OBJS} .depend: instrs.as astoks.h


