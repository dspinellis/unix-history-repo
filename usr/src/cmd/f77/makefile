#	Makefile for the Fortran 77 compiler
#	running on the PDP11,
#	generating code for the PDP11,
#	using the Ritchie pass 2

all:	f77 f77pass1

cp:	all
	cp f77 /bin
	cp f77pass1 /lib
	rm *.o gram.c f77 f77pass1

cmp:	all
	cmp f77 /bin/f77
	cmp f77pass1 /lib/f77pass1
	rm *.o gram.c f77 f77pass1

CFL = -DTARGET=PDP11 -DFAMILY=DMR -DHERE=PDP11 -DOUTPUT=BINARY -DPOLISH=POSTFIX
CFLAGS = -O -s $(CFL)
LDFLAGS = -i -s

OBJECTS = main.o init.o gram.o lex.o proc.o \
	  equiv.o data.o expr.o exec.o intr.o io.o misc.o error.o\
	  put.o putdmr.o pdp11.o pdp11x.o

compiler :: f77 f77pass1
	touch compiler

compiler :: /lib/c1
	@echo "***Warning: /lib/c1 has changed"

f77 : driver.o pdp11x.o
	cc -n -s driver.o pdp11x.o -o f77
	@size f77
f77pass1 : $(OBJECTS)
	@echo LOAD
	@$(CC) $(LDFLAGS)  $(OBJECTS) -o f77pass1
	@size f77pass1

gram.c:	gram.head gram.dcl gram.expr gram.exec gram.io tokdefs
	( sed <tokdefs "s/#define/%token/" ;\
		cat gram.head gram.dcl gram.expr gram.exec gram.io ) >gram.in
	$(YACC) $(YFLAGS) gram.in
	mv y.tab.c gram.c
	rm gram.in

tokdefs: tokens
	grep -n . <tokens | sed "s/\([^:]*\):\(.*\)/#define \2 \1/" >tokdefs

lex.o : tokdefs
driver.o $(OBJECTS)  : defs defines locdefs ftypes
driver.o : drivedefs

locdefs : pdp11defs
	cp pdp11defs locdefs

put.o putdmr.o pdp11.o : dmrdefs
io.o : fio.h

lint:
	@echo "LINT DRIVER" >Lintout
	@lint -p $(CFL) driver.c pdp11x.c >>Lintout
	echo "LINT PASS1" >>Lintout
	@nohup lint -p $(CFL) error.c exec.c intr.c expr.c gram.c init.c io.c\
		lex.c main.c proc.c misc.c put.c putdmr.c pdp11.c pdp11x.c\
		equiv.c data.c >>Lintout&

cleanup:
	-rm f gram.c *.o
	du

install: /usr/bin/f77 /usr/lib/f77pass1

/usr/bin/f77 : f77
	strip f77
	@size f77 /usr/bin/f77
	cp f77 /usr/bin/f77

/usr/lib/f77pass1 : f77pass1
	strip f77pass1
	@size f77pass1 /usr/lib/f77pass1
	-chmod u-t /usr/lib/f77pass1
	@/usr/lib/f77pass1 2>/dev/null
	cp f77pass1 /usr/lib/f77pass1
	echo "***Now do a    chmod u+t /usr/lib/f77pass1    in su mode"
