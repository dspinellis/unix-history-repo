# 
#$Header: Makefile,v 1.15 84/02/29 17:19:21 sklower Exp $
#
#$Locker:  $
#
#  Franz Lisp C coded kernel 
#
#-- Default Paths:
#  see ../../ReadMe for a explaination of what LibDir and CopyTo mean
#  D is the directory used for holding intermediate files during 
#    compilation
#-- Options:
#  there is one compile time options which can be set in this file
#  * do profiling (ala the unix prof(1) command)
#
#  The selection of this options is made below
#  Other options can be selected by editing ../h/config.h or via
#  ../../lispconf
#
LibDir = /usr/lib/lisp
ObjDir = /usr/ucb
CopyTo = /dev/null
Liszt = liszt
Lisp = lisp
# if you change this you must recompile rlc.c 
# (and change the value in ../Makefile)
#
HOLE=	2097152 

.DEFAULT: nlisp

MipSrc1= ../low.c ../lowaux.s
MipSrc2= ../alloc.c ../data.c
MipSrc3= ../rlc.c
MipSrc4= ../lisp.c ../eval.c ../eval2.c ../inits.c ../io.c ../error.c \
	 ../sysat.c ../lam1.c ../lam2.c ../lam3.c ../lam4.c ../lam5.c\
	 ../lam6.c  ../lam7.c ../lam8.c ../lam9.c ../lamr.c ../lamp.c \
	 ../fex1.c ../fex2.c ../fex3.c ../fex4.c ../fexr.c\
	 ../fpipe.c \
	 ../subbig.c ../pbignum.c ../divbig.c \
	 ../ffasl.c ../fasl.c \
	 ../trace.c ../evalf.c ../frame.c ../lamgc.c

MipSrc = ${MipSrc1} ${MipSrc2} ${MipSrc3} ${MipSrc4}

MipObj1= ../lowaux.o ../low.o
MipObj2= ../alloc.o ../data.o
HoleMipObj2 = ../Salloc.o ../Sdata.o
HoleMipObj3 = ../rlc.o
MipObj4= ../lisp.o ../eval.o ../eval2.o ../inits.o ../io.o ../error.o \
	 ../sysat.o ../lam1.o ../lam2.o ../lam3.o ../lam4.o ../lam5.o\
	 ../lam6.o  ../lam7.o ../lam8.o ../lam9.o ../lamr.o ../lamp.o \
	 ../fex1.o ../fex2.o ../fex3.o ../fex4.o ../fexr.o\
	 ../fpipe.o \
	 ../subbig.o ../pbignum.o ../divbig.o \
	 ../ffasl.o ../fasl.o \
	 ../trace.o ../evalf.o ../frame.o ../lamgc.o


# The order of loading of certain files is important.
# lowaux.o must be first and low.o second.
# 
BottomObj = ${MipObj1}

# Different objects are required depending on whether there is to be
# a hole between text and data space.
#
NoHoleObj = /lib/crt0.o ${MipObj2}
HoleObj   = /lib/hcrt0.o ${HoleMipObj2} ${HoleMipObj3}

M68kASrc = callg.s dmlad.s ediv.s emul.s hack.s \
	qfuncl.c
M68kCSrc = 68k.c adbig.c calqhat.c dodiv.c dsmult.c \
	exarith.c fixbits.c inewint.c \
	mlsb.c mulbig.c nargs.c suncore.c
M68kSrc = ${M68kCSrc} ${M68kASrc}

M68kObj = 68k.o adbig.o callg.o calqhat.o dmlad.o dodiv.o dsmult.o \
	ediv.o emul.o exarith.o fixbits.o hack.o inewint.o \
	mlsb.o mulbig.o nargs.o qfuncl.o

AllSrc = $(M68kSrc) Makefile first8.c fixregs.sed

CFLAGS = -O -I../h

#ifdef unisoft
#CFLAGS = -I../h
#endif

#ifdef sun
Libs = -lcore
OsObjs = suncore.o
#endif

#ifdef sunII
Libs = -lcore -lsunwindow -lpixrect
#endif

#ifdef mc500
#Libs = -lBSD
#endif

#ifdef unisys3botch
#N=
#else
N=-n
#endif

# on non-ucb systems it might be more
# polite to use temporary files rather than pipes
#
#ifdef unisoft
#.SUFFIXES : .c.l.s
#.s.o:
#	./first8.out < $< > $*.t; as -o $*.o $*.t; rm $*.t
#else
.SUFFIXES : .c.l
#endif
.c.o :
#ifdef npinreg
#	/lib/cpp -I../h $*.c | /lib/xcomp |\
#	sed -f fixregs.sed > /tmp/x.s; as -o $*.o /tmp/x.s
#else
	cc -c $(CFLAGS) $*.c;\
	mv `basename $*.o` x; mv x $*.o
#endif

.l.o :
	liszt $< > #resc
	@echo liszt $< done


../rlc.o: ../rlc.c 
	cc -c $(CFLAGS) -DHOLE=${HOLE} ../rlc.c 
	mv rlc.o .. < /dev/null

../Salloc.o: ../alloc.c
	(echo "# define HOLE"; cat ../alloc.c) > Salloc.c;\
	make Salloc.o; mv Salloc.o .. < /dev/null ; rm Salloc.c
	
../Sdata.o: ../data.c
	(echo "# define HOLE"; cat ../data.c) > Sdata.c;\
	make Sdata.o; mv Sdata.o .. < /dev/null  ; rm Sdata.c

first8.out: first8.c
	cc -I../h first8.c -o first8.out

../low.o: ../low.c
	cc -S $(CFLAGS) ../low.c; sed 's/data$$/text/' < low.s > Low.s
	as -o ../low.o Low.s; rm Low.s low.s

#ifdef unisoft
#qfuncl.o: qfuncl.c first8.out
#	cc -E $(CFLAGS) ${ProfFlag2} qfuncl.c |\
#	./first8.out | sed 's/^#/|/' > qfuncl.s
#	as -o qfuncl.o qfuncl.s
#	rm qfuncl.s
#
#else
qfuncl.o: qfuncl.c
	cc -I../h -E ${ProfFlag2} qfuncl.c > x.s; as -o qfuncl.o x.s; rm x.s
#endif

# rawlisp is the standard raw lisp system.

rawlisp: ${BottomObj} ${NoHoleObj} ${MipObj4} ${M68kObj} ${OsObjs}
	rm -f rawlisp
	ld $N -x -o rawlisp -e start ${BottomObj} ${NoHoleObj} \
				${M68kObj} ${MipObj4} ${OsObjs} \
				${Libs} -ltermcap -lm -lc
	ls -l rawlisp


# hlisp is a raw lisp system with a hole between text and data

rawhlisp: ${BottomObj} ${HoleObj} ${MipObj4} ${M68kObj} ${OsObjs}
	rm -f rawhlisp
	ld -x -H ${HOLE} -o rawhlisp -e hstart ${BottomObj}  ${HoleObj} \
				${M68kObj} ${MipObj4} ${OsObjs} \
				${Libs} -ltermcap -lm -lc
	ls -l rawhlisp


clean:
	rm -f *.o rawlisp rawhlisp nlisp snlisp

lint:
	lint ../h/*.h *.c

install: nlisp 
	-rm -f ${ObjDir}/lisp
	mv nlisp ${ObjDir}/lisp
	@echo lisp installed

nlisp: rawlisp ${LibDir}
	-rm -f nlisp
	(cd ${LibDir} ; make Liszt=${Liszt} required)
	echo "(progn (setq build:map 'map \
			   build:lisp-type 'franz \
		           lisp-library-directory '${LibDir} \
			   build:dir '${LibDir} \
			   lisp-object-directory '${ObjDir}) \
		     (load '${LibDir}/buildlisp)\
	             (dumplisp nlisp))" | ./rawlisp
	${LibDir}/tackon map nlisp
	@echo nlisp built


donlisp:
	-rm -f nlisp
	make LibDir=${LibDir} Liszt=${Liszt} ObjDir=${ObjDir} nlisp

#--- snlisp: create a totally interpreted lisp.
#	dump as snlisp
snlisp: rawlisp
	echo "(progn (setq build:load t		\
			   build:lisp-type 'franz \
			   build:dir '${LibDir} \
			   lisp-object-directory '${ObjDir}\
			   lisp-library-directory '${LibDir})\
		     (load '${LibDir}/buildlisp)\
		     (dumplisp snlisp))" | rawlisp


tags:	tags ${M68kCSrc} ${MipSrc}
	ctags ../h/*.h $(M68kCSrc) ${MipSrc}

#--- copysource : copy source files to another directory
#  called via   make CopyTo=/xx/yyy/zz copysource
# 
copysource: ${AllSrc}
	(tar cf - ${AllSrc} | (cd ${CopyTo} ; tar xf -))

copyobjects: ${AllObj}
	(tar cf - ${AllObj} | (cd ${CopyTo} ; tar xf -))

scriptcatall: ${AllSrc}
	@../../scriptcat . franz/68k ${AllSrc} tags
