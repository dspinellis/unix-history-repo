#
# Evans Vax 11/780
#
GLOB = global.h sigtab.h
CFLAGS = -O #  -p # -g
D = /tmp
T = $(DESTDIR)
LLIB = /usr/lib/lisp
OBJS =	crt0.o lisp.o eval.o eval2.o Talloc.o inits.o io.o error.o sysat.o data.o\
	lam1.o lam2.o lam3.o lam4.o lam5.o lam6.o lam7.o lam8.o lamr.o\
	fex1.o fex2.o fex3.o fex4.o fexr.o\
	reset.o fpipe.o dmlad.o dsneg.o dodiv.o adbig.o \
	subbig.o pbignum.o divbig.o inewint.o rfasl.o ffasl.o fasl.o vsyscall.o\
	qfuncl.o bind.o vfork.o

.SUFFIXES : .c.l
.c.o :
	@echo cc -c $*.c;\
	/lib/cpp $< $D/$*.C;\
	/lib/ccom $D/$*.C | fixmask  > $D/$*.S;\
	sed -f fixpbig.e $D/$*.S > $D/$*.R;\
	/lib/c2 $D/$*.R > $D/$*.T;\
	as -o $*.o $D/$*.T;\
	rm $D/$*.[CRST]
.l.o :
	liszt $< > #resc
	@echo liszt $< done

all: tlisp $(LLIB) $(LLIB)/nld $(LLIB)/auxfns0.o $(LLIB)/auxfns1.o 

tlisp: fixmask ${OBJS}
	rm -f tlisp
	ld -X -o tlisp -e start ${OBJS} -lm -lc 
	ls -l tlisp

fixmask: fixmask.c
	cc -O -o fixmask fixmask.c

# ${OBJS}: ${GLOB}
data.o: sigtab.h

clean:
	rm -f *.o

print:
	@pr *akefile*
	@-(size lisp ; size *.o) | pr -h sizes
	@ls -ls | pr
	@pr *.h *.c *.s

lint:
	lint *.h *.c

install: $(LLIB)/auxfns0.o $(LLIB)/auxfns1.o
	-rm ${DESTDIR}/$(LLIB)/rlisp
	-ln tlisp ${DESTDIR}/$(LLIB)/rlisp
	-rm -f ${DESTDIR}/usr/ucb/lisp
	-echo "(fasl '\"$(LLIB)/auxfns0.o\")(fasl '\"$(LLIB)/auxfns1.o\")\
	(load 'lisprcfun)(dumplisp ${DESTDIR}/usr/ucb/lisp)" | tlisp
	@echo lisp installed

novminstall: $(LLIB)/auxfns0.fasl $(LLIB)/auxfns1.fasl
	-rm ${DESTDIR}/$(LLIB)/rlisp
	-ln tlisp ${DESTDIR}/$(LLIB)/rlisp
	-rm -f ${DESTDIR}/usr/ucb/lisp
	-echo "(fasl '\"$(LLIB)/auxfns0.o\")(fasl '\"$(LLIB)/auxfns1.o\")\
	(load 'lisprcfun)(sstatus dumpmode 410)\
	(dumplisp \"${DESTDIR}/usr/ucb/lisp.novm\")" | tlisp
	@echo lisp installed

nlisp:	tlisp auxfns0.o
	-rm -f nlisp
	echo "(fasl '\"auxfns0.o\")(dumplisp nlisp)"|tlisp
	echo nlisp done

$(LLIB):
	-if mkdir $(LLIB); then cp lib/* $(LLIB); fi

$(LLIB)/nld: nld.c
	cc -O -o ${DESTDIR}/usr/lib/lisp/nld nld.c

