
#
# Evans Vax 11/780
# Caution: this make file will not work on unix TS systems!
#
HOLE=	2097152 #if you change this you must recompile rlc.c
TOBJS = crt0.o Orderit.o lisp.o eval.o eval2.o Talloc.o inits.o io.o \
	frame.o error.o sysat.o data.o\
	lam1.o lam2.o lam3.o lam4.o lam5.o lam6.o lam7.o lam8.o lamr.o lamnop.o\
	fex1.o fex2.o fex3.o fex4.o fexr.o\
	reset.o fpipe.o dmcount.o bigmath.o\
	subbig.o pbignum.o divbig.o inewint.o ffasl.o nfasl.o vsyscall.o\
	qfuncl.o trace.o evalf.o # backquote.o auxfns0.o

HOBJS = hcrt0.o rlc.o Orderit.o lisp.o eval.o eval2.o Salloc.o inits.o io.o \
	frame.o error.o sysat.o Sdata.o\
	lam1.o lam2.o lam3.o lam4.o lam5.o lam6.o lam7.o lam8.o lamr.o lamnop.o\
	fex1.o fex2.o fex3.o fex4.o fexr.o\
	reset.o fpipe.o bigmath.o dmcount.o\
	subbig.o pbignum.o divbig.o inewint.o nfasl.o ffasl.o vsyscall.o\
	qfuncl.o trace.o evalf.o 

GLOB = h/global.h h/sigtab.h
CFLAGS = -O -Ih #  -p # -g

T = $(DESTDIR)
LLIB = $T/usr/lib/lisp
LOBJS = $(LLIB)/auxfns0.o $(LLIB)/auxfns1.o $(LLIB)/backquote.o \
	$(LLIB)/toplevel.o $(LLIB)/tackon

.SUFFIXES : .c.l
.c.o :
	@echo cc -c -O $*.c;\
	/lib/cpp $< -Ih | /lib/ccom | fixmask | sed -f fixpbig.e |\
	/lib/c2 | as -o $*.o

.l.o :
	liszt $< > #resc
	@echo liszt $< done

tlisp: fixmask ${TOBJS} ${LLIB}/nld
	rm -f tlisp
	ld -x -o tlisp -e start ${TOBJS} -lm -lc 
	ls -l tlisp

hlisp: fixmask ${HOBJS} ${LLIB}/nld
	rm -f hlisp
	ld -x -H $(HOLE) -o hlisp -e hstart ${HOBJS} -lm -lc 
	ls -l hlisp

# ${OBJS}: ${GLOB}
data.o: h/sigtab.h

rlc.o: rlc.c 
	cc -c -O -DHOLE=${HOLE} rlc.c

Salloc.o: Talloc.c
	(echo "# define HOLE"; cat Talloc.c) > Zalloc.c ; make Zalloc.o;\
	rm Zalloc.c; mv Zalloc.o Salloc.o

Sdata.o: data.c
	(echo "# define HOLE"; cat data.c) > Zdata.c ; make Zdata.o;\
	rm Zdata.c; mv Zdata.o Sdata.o
	
fixmask: fixmask.c
	cc -O -o fixmask fixmask.c

clean:
	rm -f *.o tlisp hlisp map ?resc fixmask

print:
	@pr READ*
	@pr *akefile*
	@(size lisp ; size *.o) | pr -h sizes
	@ls -ls | pr
	@pr *.h *.c *.s

lint:
	lint *.h *.c

tape:
	tar crv Makefile [a-z]*.[cshxe] /usr/lib/lisp


install: mylisp 
	mv mylisp $T/usr/ucb/lisp
	chmod 1755 $T/usr/ucb/lisp
	@echo lisp installed

mylisp: tlisp $(LOBJS)
	-rm -f mylisp
	-echo "(fasl '$(LLIB)/auxfns0 'map)(sstatus appendmap t)\
	       (fasl '$(LLIB)/auxfns1 'map)\
	       (fasl '$(LLIB)/toplevel 'map)\
	       (sstatus appendmap nil)\
	       (dumplisp mylisp)" | tlisp
	$(LLIB)/tackon map mylisp
	@echo mylisp built


tags:  $(GLOB) $(TCFILES)
	ctags $(GLOB) $(TCFILES)

$(LLIB)/nld:
	cp /bin/ld $(LLIB)/nld

$(LLIB)/tackon: tackon.c
	$(CC) -O tackon.c -o $(LLIB)/tackon -s
