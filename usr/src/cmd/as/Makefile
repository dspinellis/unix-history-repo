LOCAL = /usr/ucb/

#
#	as.h		Definitions for data structures
#	asscan.h	Definitions for the character scanner
#	astoks.h	The defines for tokens that yacc produced
#				included implicitly in as.h
#	asexpr.h	The macros for parsing and expressions
#	assyms.h	Macros for dealing with the symbol table
#
#	asscan.c	lexical analyzer and the character scanner
#	asparse.c	parser
#	asexpr.c	parses expressions, constructs and evaluates
#				expression trees
#
#	asmain.c	main body
#	assyms.c	symbol table processing routines
#	asjxxx.c	Fixes jxxx instructions
#	ascode.c	Emits code
#	asio.c		Does block I/O and faster versions of fwrite
#
#	aspseudo.c	Symbol table definitions for reserved words
#	instrs		included in pseudo.c; instructions and semantic info
#				for each instructions
#

HDRS = astoks.h as.h asscan.h assyms.h asexpr.h 

SRCS = asscan.c asmain.c asparse.c \
	asexpr.c assyms.c \
	asjxxx.c ascode.c aspseudo.c \
	asio.c

OBJS = asscan.o asparse.o asexpr.o \
	asmain.o assyms.o \
	asjxxx.o ascode.o aspseudo.o \
	asio.o

GRIND = astoks.h as.h asscan.h assyms.h asexpr.h \
	asmain.c asscan.c asparse.c asexpr.c \
	assyms.c asjxxx.c ascode.c asio.c

DESTDIR = 

#
#	available flags:
#
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

LD = /bin/ld

CFLAGS=  -DUNIX -DUNIXDEVEL -DFLEXNAMES -g

LDFLAGS = -g

LINTFLAGS = -DUNIX -DUNIXDEVEL -DFLEXNAMES

as:	$(OBJS)
	$(CC) $(LDFLAGS) $(OBJS) 
	mv a.out as

lint:
	lint $(LINTFLAGS) $(SRCS)

aspseudo.o :	as.h astoks.h aspseudo.c instrs
	$(CC) -S $(CFLAGS) aspseudo.c
	sh ./:rofix aspseudo.s
	$(AS) -o aspseudo.o aspseudo.s
	rm aspseudo.s

clean:
	rm -f $(OBJS) as
install:
	install -s as ${DESTDIR}/bin

print:
	pr Makefile $(HDRS) $(SRCS)

netprint:
	csh /usr/grad/henry/bin/:netprint Makefile $(HDRS) $(SRCS)

vgrind:
	vgrind $(GRIND)

sources: ${SRCS} ${HDRS}
${SRCS} ${HDRS}:
	sccs get $@
