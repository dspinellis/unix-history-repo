# Specify the name of the program.
# All documentation and installation keys on this value.
# 
name=sc
NAME=SC

# This is where the install step puts it.
#EXDIR=/v/rgb/bin/psx
EXDIR=/usr/local/bin

# This is where the man page goes.
#MANDIR=/usr/man/man1
MANDIR=/usr/man/manl

# Set SIMPLE for lex.c if you don't want arrow keys or lex.c blows up
#SIMPLE=-DSIMPLE

# Set DOBACKUPS if you would like a backup copy of a source file on a save
DOBACKUPS=-DDOBACKUPS

# Set INTERNATIONAL if you need 8 bit characters.  You should
# not set this if you are running 5.3.0.  I think it is OK in 5.3.1.
#INTERNATIONAL=-DINTERNATIONAL
# Set SIGVOID if signal routines are type void.  System 5.3, VMS and ANSI C
# Compliant systems use this.  Most BSD systems do not.
#SIGVOID=-DSIGVOID

# Set IEEE_MATH if you need setsticky() calls in your signal handlers
#
#IEEE_MATH=-DIEEE_MATH

# This is the name of a pager like "more" If the line is commented out
# then "more" will be used. "pg" may be appropriate for SYSV
PAGER=-DDFLT_PAGER=\"less\"

# Use this for system V.2
#CFLAGS= -O -DSYSV2 
#LDFLAGS=
#LIB=-lm -lPW -lcurses

# Use this for system V.3
#CFLAGS= -g -DSYSV3
#LDFLAGS=
#LIB=-lm -lcurses -lPW

# Microport
#CFLAGS= -DSYSV2 -O -DUPORT -Ml
#LDFLAGS=-Ml
#LIB=-lm -lcurses -lPW

# Use this for BSD 4.2
#CFLAGS= -O -DBSD42
#LDFLAGS=
#LIB=-lm -lcurses -ltermcap

# Use this for Sequent boxes
#CC=atscc
#CFLAGS=-O -DBSD42
#LDFLAGS= -s
#LIB=-lm -lcurses -ltermcap
#PSCLIB=-lseq

# Use this for BSD 4.3
CFLAGS= -O -DBSD43
LDFLAGS=
LIB=-lm -lcurses -ltermcap

# Use this for system III (XENIX)
#CFLAGS= -O -DSYSIII
#LDFLAGS= -i
#LIB=-lm -lcurses -ltermcap

# Use this for VENIX
#CFLAGS= -DVENIX -DBSD42 -DV7
#LDFLAGS= -z -i 
#LIB=-lm -lcurses -ltermcap

# All of the source files
SRC=sc.h sc.c lex.c gram.y interp.c crypt.c xmalloc.c cmds.c range.c help.c \
	vi.c eres.sed sres.sed Makefile psc.c vmtbl.c version.c

# The objects
OBJS=sc.o interp.o cmds.o crypt.o xmalloc.o range.o help.o vi.o lex.o gram.o \
	vmtbl.o version.o

# The documents in the Archive
DOCS=README CHANGES sc.doc psc.doc tutorial.sc VMS_NOTES BSD_BUGS

$(name):$(PAR) 	$(OBJS)
	$(CC) ${CFLAGS} ${LDFLAGS} ${OBJS} ${LIB} -o $(name)

diff_to_sc:	diff_to_sc.c
	$(CC) ${CFLAGS} -o dtv diff_to_sc.c

p$(name):	psc.c pvmtbl.o
	$(CC) ${CFLAGS} -o p$(name) psc.c pvmtbl.o ${PSCLIB}
	cp p$(name) $(EXDIR)/p$(name)

pvmtbl.o: sc.h vmtbl.c
	$(CC) ${CFLAGS} -c -DPSC vmtbl.c
	mv vmtbl.o pvmtbl.o

lex.o:	sc.h y.tab.h gram.o lex.c
	$(CC) ${CFLAGS} ${SIMPLE} ${IEEE_MATH} ${INTERNATIONAL} ${SIGVOID} -c lex.c

sc.o:	sc.h sc.c
	$(CC) ${CFLAGS} ${INTERNATIONAL} ${PAGER} ${SIGVOID} -c sc.c

interp.o:	interp.c sc.h
	$(CC) ${CFLAGS} ${IEEE_MATH} ${SIGVOID} -c interp.c

gram.o:	sc.h y.tab.h

cmds.o: cmds.c sc.h
	$(CC) ${CFLAGS} ${DOBACKUPS} -c cmds.c

crypt.o: crypt.c sc.h
	$(CC) ${CFLAGS} ${DOBACKUPS} -c crypt.c

range.o: range.c sc.h

help.o: help.c sc.h

vi.o: vi.c sc.h

y.tab.h:	gram.y

gram.o:	sc.h y.tab.h gram.c
	$(CC) ${CFLAGS} -c gram.c
	sed<gram.y >experres.h -f eres.sed;sed < gram.y > statres.h -f sres.sed

gram.c:	gram.y
	$(YACC) -d gram.y; mv y.tab.c gram.c

clean:
	rm -f *.o *res.h y.tab.h $(name) p$(name) debug core gram.c $(name).1 \
	$(name).man p$(name).man p$(name).1 y.output

shar: ${SRC} ${DOCS}
	shar -c -m 64000 -f shar ${DOCS} ${SRC}

sshar: ${SRC}
	shar -c -m 1000000 -f shar ${SRC}

lint: sc.h sc.c lex.c gram.c interp.c cmds.c crypt.c
	lint ${CFLAGS} ${SIMPLE} sc.c lex.c gram.c interp.c cmds.c crypt.c \
	range.c xmalloc.c help.c vi.c -lcurses -lm

inspect: sc.h sc.c lex.c gram.c interp.c cmds.c crypt.c
	/bruces/ianj/bin/i386/inspect -abv -t 8 sc.c lex.c gram.c interp.c \
	cmds.c crypt.c range.c xmalloc.c help.c vi.c

print: sc.h gram.y sc.c lex.c interp.c cmds.c crypt.c 
	prc sc.h gram.y sc.c lex.c interp.c cmds.c crypt.c | lpr

$(name).1:	sc.doc
	sed -e s/pname/$(name)/g -e s/PNAME/$(NAME)/g sc.doc >  $(name).1

$(name).man:	$(name).1
	nroff -man $(name).1 > $(name).man

laser:	$(name).1
	itpf -x -Pim2 -man $(name).1

p$(name).1:	psc.doc
	sed -e s/pname/$(name)/g -e s/PNAME/$(NAME)/g psc.doc >  p$(name).1

p$(name).man:	p$(name).1
	nroff -man p$(name).1 > p$(name).man

install: $(EXDIR)/$(name)

inst-man: $(MANDIR)/$(name).1

$(EXDIR)/$(name): $(name)
	-mv $(EXDIR)/$(name) $(EXDIR)/$(name).old
	cp $(name) $(EXDIR)
	strip $(EXDIR)/$(name)

$(MANDIR)/$(name).1: $(name).1
	cp $(name).1 $(MANDIR)

diffs: ${SRC}
	for i in ${SRC} ${DOCS} ;\
		do \
		rcsdiff -c -r6.1 $$i ;\
		done

test: test.o vmtbl.o
	$(CC) ${CFLAGS} ${LDFLAGS} test.o vmtbl.o ${LIB} -o test
