#
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
Liszt = ${ObjDir}/liszt
Lisp = ${ObjDir}/lisp
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

MipObj1= ../low.o ../lowaux.o
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

#------ Options

#--- profiling selection
# If the lisp system is to run with profiling, this must be done:
#  1) remove the # (comment character) from the ProfFlag and
#     ProfFlag2 definitions below (also do it in ../Makefile)
#  2) remove all .o files and do a make.
#
ProfFlag = # -XP
ProfFlag2 = # -DPROF


# The order of loading of certain files is important.
# low.o must be first and lowaux second.
# 
BottomObj = ${MipObj1}

# Different objects are required depending on whether there is to be
# a hole between text and data space.
#
NoHoleObj = crt0.o ${MipObj2}
HoleObj   = hcrt0.o ${HoleMipObj2} ${HoleMipObj3}

TahoeObj =  qfuncl.o tahoe.o malloc.o callg.o exarith.o inewint.o \
	    adbig.o calqhat.o dmlad.o prunei.o myfrexp.o mlsb.o \
	    dodiv.o dsmult.o mulbig.o retfrom.o pushframe.o
TahoeCSrc = qfuncl.c tahoe.c malloc.c callg.c exarith.c inewint.c \
	    adbig.c prunei.c myfrexp.c mlsb.c dodiv.c dsmult.c \
	    mulbig.c
TahoeASrc = crt0.s hcrt0.s calqhat.s dmlad.s pushframe.s retfrom.s
TahoeSrc =  ${TahoeASrc} ${TahoeCSrc}


AllSrc = Makefile fixmask.c fixpbig.e ${TahoeSrc}


.SUFFIXES : .c.l
# on non-ucb systems it might be more
# polite to use temporary files rather than pipes
#
.c.o :
	@csh -cfe "echo cc -c  $*.c;\
	rm -f $*.o;\
	/lib/cpp $< -I../h |\
	/lib/ccom ${ProfFlag} | fixmask |\
	sed -f fixpbig.e | \
	/lib/c2 | as -o $*.o"

.l.o :
	liszt $< > #resc
	@echo liszt $< done

# one special case:
#  add -DPROF if you want to profile the assembler code

qfuncl.o: qfuncl.c
	cc -I../h -E ${ProfFlag2} qfuncl.c | as -o qfuncl.o

#bigmath.o: bigmath.c
#	cc -I../h -E ${ProfFlag2} bigmath.c | as -o bigmath.o

../io.o: ../io.c
	@csh -cfe "echo cc -c  $*.c;\
	rm -f $*.o;\
	/lib/cpp $< -I../h |\
	/lib/ccom ${ProfFlag} | fixmask |\
	sed -f fixpbig.e | \
	/lib/c2 | as -J -o $*.o"

../rlc.o: ../rlc.c 
	cc -c -O -DHOLE=${HOLE} ../rlc.c 
	mv rlc.o .. < /dev/null

../low.o: ../low.c
	cc -I../h -R -c ../low.c  
	mv low.o .. < /dev/null

../Salloc.o: ../alloc.c
	(echo "# define HOLE ${HOLE}"; cat ../alloc.c) > Salloc.c;\
	make Salloc.o; mv Salloc.o .. < /dev/null ; rm Salloc.c
	
../Sdata.o: ../data.c
	(echo "# define HOLE ${HOLE}"; cat ../data.c) > Sdata.c;\
	make Sdata.o; mv Sdata.o .. < /dev/null  ; rm Sdata.c

fixmask: fixmask.c
	cc -O -o fixmask fixmask.c

# rawlisp is the standard raw lisp system.

rawlisp: fixmask fixpbig.e ${BottomObj} ${NoHoleObj} ${MipObj4} ${TahoeObj} 
	rm -f rawlisp
	ld -x -o rawlisp -e start ${BottomObj} ${NoHoleObj} \
				${TahoeObj} ${MipObj4}  -lm -lc -ltermlib
	ls -l rawlisp


# hlisp is a raw lisp system with a hole between text and data

rawhlisp: fixmask fixpbig.e ${BottomObj} ${HoleObj} ${MipObj4} ${TahoeObj} 
	rm -f rawhlisp
	ld -x -H ${HOLE} -o rawhlisp -e hstart ${BottomObj}  ${HoleObj} \
				${TahoeObj}  ${MipObj4} -lm -lc -ltermlib
	ls -l rawhlisp

clean:
	rm -f *.o rawlisp nlisp rawhlisp

lint:
	lint ../h/*.h *.c

tags:	tags ${TahoeSrc} ${MipSrc}
	ctags ../h/*.h ${TahoeCSrc} ${MipSrc}

install: nlisp 
	-rm -f ${ObjDir}/lisp
	install -c nlisp ${ObjDir}/lisp
	@echo lisp installed

nlisp: rawlisp ${LibDir}
	(cd ${LibDir} ; make Liszt=${Liszt} required)
	echo "(progn (setq build:map 'map \
			   build:lisp-type 'franz \
		           lisp-library-directory '${LibDir} \
			   build:dir '${LibDir} \
			   lisp-object-directory '${ObjDir}) \
		     (load '${LibDir}/buildlisp)\
		     (sstatus dumpcore nil)\
	             (dumplisp nlisp))" | rawlisp
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

#--- copysource : copy source files to another directory
#  called via   make CopyTo=/xx/yyy/zz copysource
# 
copysource: ${AllSrc}
	(tar cf - ${AllSrc} | (cd ${CopyTo} ; tar xf -))

copyobjects: ${AllObj}
	(tar cf - ${AllObj} | (cd ${CopyTo} ; tar xf -))

scriptcatall: ${AllSrc}
	@../../scriptcat . franz/tahoe ${AllSrc} tags
