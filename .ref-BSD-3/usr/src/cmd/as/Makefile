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
#
#	aspseudo.c	Symbol table definitions for reserved words
#	instrs		included in pseudo.c; instructions and semantic info
#				for each instructions
#

HDRS = astoks.h as.h asscan.h assyms.h asexpr.h 

SRCS = asscan.c asparse.c asexpr.c \
	asmain.c assyms.c \
	asjxxx.c ascode.c aspseudo.c

SRCS1= asscan.c asparse.c asexpr.c

SRCS2 = asmain.c assyms.c asjxxx.c ascode.c aspseudo.c instrs

OBJS = asscan.o asparse.o asexpr.o \
	asmain.o assyms.o \
	asjxxx.o ascode.o aspseudo.o

DESTDIR = 

#
#	available flags:
#
#	SORTEDOUTPUT	create the final a.out symbol table sorted by
#			(segment number, value) keys, instead of
#			by declaration order.  SDB currently requires
#			the symbol table to be in declaration order.
#	METRIC		print out number of times the hashing routine is called
#	DEBUG		print out various debugging information
#			in the first pass
#
#

CFLAGS= -O
LINTFLAGS =

as:	$(OBJS)
	$(CC) $(OBJS) 
	mv a.out as

lint:
	lint $(LINTFLAGS) $(SRCS)

asscan.o :	as.h astoks.h asexpr.h asscan.h asscan.c
	$(CC) $(CFLAGS) -c asscan.c

asparse.o:	as.h astoks.h asexpr.h asscan.h asparse.c
	$(CC) $(CFLAGS) -c asparse.c

asexpr.o:	as.h astoks.h asexpr.h asexpr.c
	$(CC) $(CFLAGS) -c asexpr.c

asmain.o :	as.h astoks.h asexpr.h assyms.h asmain.c
	$(CC) $(CFLAGS) -c asmain.c

assyms.o:	as.h astoks.h assyms.h assyms.c
	$(CC) $(CFLAGS) -c assyms.c

asjxxx.o:	as.h astoks.h assyms.h asjxxx.c
	$(CC) $(CFLAGS) -c asjxxx.c

ascode.o:	as.h astoks.h assyms.h ascode.c
	$(CC) $(CFLAGS) -c ascode.c

aspseudo.o :	as.h astoks.h aspseudo.c instrs
	$(CC) -S $(CFLAGS) aspseudo.c
	:rofix aspseudo.s
	$(AS) -o aspseudo.o aspseudo.s
	rm aspseudo.s

clean:
	rm -f $(OBJS) as

netprint:
	pr $(HDRS) $(SRCS1) > as1.pr
	pr $(SRCS2) > as2.pr
	netcp as1.pr Cory:netbin/as1.pr
	netcp as2.pr Cory:netbin/as2.pr
	rm as1.pr
	rm as2.pr

print:
	pr $(HDRS) $(SRCS)

vprint:
	vgrind $(HDRS) $(SRCS)

install:
	install -s as ${DESTDIR}/bin/as
