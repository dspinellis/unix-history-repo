CFLAGS = -O -w
LIBES =
FILES1 =  Makefile efl.1 defs tokens main.c init.c tailor.c\
		gram.head gram.dcl gram.expr gram.exec\
		lex.l fixuplex symtab.c\
		dcl.c addr.c struct.c field.c misc.c alloc.c mk.c
FILES2 =  exec.c blklab.c simple.c print.c temp.c io.c error.c\
		pass2.c icfile.c free.c dclgen.c namgen.c
FILES = $(FILES1) $(FILES2)
P = detab 0 6 12 18 24 30 36 42 48 54 60 66 72 | und -3 | opr -r2

OBJECTS = main.o init.o tailor.o gram.o lex.o symtab.o\
	dcl.o addr.o struct.o field.o blklab.o\
	mk.o misc.o alloc.o print.o simple.o exec.o temp.o io.o error.o\
	pass2.o icfile.o free.o dclgen.o namgen.o

a.out : $(OBJECTS)
	@echo LOAD
	@$(CC) $(CFLAGS) $(LDFLAGS) $(OBJECTS) $(LIBES)
	@size

$(OBJECTS) :  defs
lex.o init.o : tokdefs

# gram.c can no longer be made on a pdp11 because of yacc limits
gram.c:	gram.head gram.dcl gram.expr gram.exec tokdefs
	( sed <tokdefs "s/#define/%token/" ;\
	cat gram.head gram.dcl gram.expr gram.exec) >gram.in
	$(YACC) $(YFLAGS) gram.in
	(echo "#define YYDEBUG"; cat y.tab.c) > gram.c
	rm -f gram.in y.tab.c

lex.c: fixuplex
	lex lex.l
	fixuplex
	mv lex.yy.c lex.c

tokdefs: tokens
	grep -n . <tokens | sed "s/\([^:]*\):\(.*\)/#define \2 \1/" >tokdefs

clean:
	-rm -f a.out *.o gram.c lex.c tokdefs 
	-du

printall:
	-pr $(FILES1) | $P
	-pr $(FILES2) | $P
	touch print

print:  $(FILES)
	-pr $? | $P
	touch print

lint: a.out
	lint -p *.c -lS

install: a.out
	strip a.out
	-@size a.out /usr/bin/efl
	cp a.out ${DESTDIR}/usr/bin/efl

test:	a.out
	a.out "system=gcos" efltest/Hard.e >z1 2>z2
	cmp z1 efltest/Hard.out
	a.out "system=gcos" efltest/Band.e >z1 2>z2
	cmp z1 efltest/Band.out
	a.out "system=gcos" efltest/Buram.e >z1 2>z2
	cmp z1 efltest/Buram.out
	a.out "system=gcos" efltest/Dgl.e >z1 2>z2
	cmp z1 efltest/Dgl.out
	rm -f z1 z2
	@echo TEST OK

tape: $(FILES) gram.c lex.c a.out
	tar cb $(FILES) gram.c lex.c a.out

