#
# 
# $Header: /na/franz/franz/RCS/Makefile,v 1.4 83/03/04 12:26:13 jkf Exp $
#
#  Franz Lisp C coded kernel 
#
#-- Default Paths:
#  see ../ReadMe for a explaination of what LibDir and CopyTo mean
#  D is the directory used for holding intermediate files during 
#    compilation
#-- Options:
#  there are two compile time options which can be set in this file
#  * do profiling (ala the unix prof(1) command)
#  * conditionally compile for VMS (as opposed to Unix)
#
#  The selection of these options is made below
#  Other options can be selected by editing h/config.h.
#
LibDir = /usr/lib/lisp
ObjDir = /usr/ucb
CopyTo = /dev/null
Liszt = liszt
Lisp = lisp
D = /tmp
HOLE=	2097152 #if you change this you must recompile rlc.c

GenSrc = low.c lowaux.s lisp.c eval.c eval2.c inits.c io.c error.c sysat.c \
	lam1.c lam2.c lam3.c lam4.c lam5.c lam6.c lam7.c lam8.c lam9.c lamr.c \
	fex1.c fex2.c fex3.c fex4.c fexr.c\
	fpipe.c bigmath.s\
	subbig.c pbignum.c divbig.c inewint.s ffasl.c  fasl.c vsyscall.s\
	qfuncl.c trace.c evalf.c frame.c lamgc.c vax.c alloc.c data.c

GenObj = low.o lowaux.o lisp.o eval.o eval2.o inits.o io.o error.o sysat.o \
	lam1.o lam2.o lam3.o lam4.o lam5.o lam6.o lam7.o lam8.o lam9.o lamr.o \
	fex1.o fex2.o fex3.o fex4.o fexr.o\
	fpipe.o bigmath.o\
	subbig.o pbignum.o divbig.o inewint.o ffasl.o  fasl.o vsyscall.o\
	qfuncl.o trace.o evalf.o frame.o   lamgc.o vax.o

Includes = h/config.h 	h/global.h 	h/oaout.h 	h/vaxframe.h \
	h/catchfram.h 	h/dfuncs.h 	h/gtabs.h 	h/sigtab.h \
	h/chars.h 	h/frame.h 	h/lfuncs.h 	h/structs.h \
	h/chkrtab.h 	h/gc.h 		h/naout.h 	h/types.h \
	h/lconf.h	h/ltypes.h

# utilities
#
UtilSrc = fixmask.c fixpbig.e
UtilObj = fixmask

#------ Options

#--- profiling selection
# If the lisp system is to run with profiling, this must be done:
#  1) Replace NoProfObj with ProfObj in the defintion of TObj (or HObj)
#  2) remove the # (comment character) from the ProfFlag and
#     ProfFlag2 definitions below
#  3) remove all .o files and do a make.
#
ProfFlag = # -XP
ProfFlag2 = # -DPROF

#--- for vms
# In order to build a VMS version, remove the # from the ForVMS switch
# and 'make switchos' before doing a 'make tlisp'
ForVMS = # -DVMS

#-------- End Options  
NoProfSrc = lamnop.c dmcount.s
NoProfObj = lamnop.o dmcount.o

ProfSrc = lamp.c mcount.s mon.c
ProfObj = lamp.o mcount.o mon.o

#--- tlisp: the C coded lisp kernel

TStartupSrc = crt0.s
TStartupObj = crt0.o

TXtraSrc =
TXtraObj = alloc.o data.o

TSrc = ${GenSrc} ${TStartupSrc} ${TXtraSrc} ${NoProfSrc} ${Includes}
TObj = ${GenObj} ${TStartupObj} ${TXtraObj} ${NoProfObj}

#--- hlisp: like tlisp but with a large hole between text and data.
HStartupSrc = hcrt0.s
HStartupObj = hcrt0.o

HXtraSrc = rlc.c 
HXtraObj = rlc.o Salloc.o Sdata.o

HSrc = ${GenSrc} ${HStartupSrc} ${HXtraSrc} ${NoProfSrc} ${Includes}
HObj = ${GenObj} ${HStartupObj} ${HXtraObj} ${NoProfObj}

#--- all sources
AllSrc = Makefile ChangeLog ${GenSrc} ${TStartupSrc} ${HStartupSrc}\
	${TXtraSrc} ${HXtraSrc} ${NoProfSrc} ${ProfSrc} ${Includes} ${UtilSrc}

TagSrc = ${GenSrc} ${TStartupSrc} ${HStartupSrc}\
	${TXtraSrc} ${HXtraSrc} ${NoProfSrc} ${ProfSrc} ${Includes} ${UtilSrc}

#--- all object files
AllObj = ${GenObj} ${TStartupObj} ${HStartupObj}  ${TXtraObj} ${HXtraObj}\
	 ${NoProfObj} ${ProfObj} ${UtilObj}

#--- all files
AllFiles = ${AllSrc} ${AllObj}

CFLAGS = -O -Ih #  -p # -g


.SUFFIXES : .c.l
# on non-ucb systems it might be more
# polite to use temporary files rather than pipes
#
.c.o :
	@csh -cfe "echo cc -c  $*.c;\
	rm -f $*.o;\
	/lib/cpp $< -Ih ${ForVMS} |\
	/lib/ccom ${ProfFlag}  | ./fixmask  |\
	sed -f fixpbig.e |\
	/lib/c2 > $D/$*.s;\
	as -o $*.o $D/$*.s;\
	rm $D/$*.s"

.l.o :
	liszt $< > #resc
	@echo liszt $< done

# one special case:
#  add -DPROF if you want to profile the assembler code

qfuncl.o: qfuncl.c
	cc -Ih -E ${ProfFlag2} qfuncl.c | as -o qfuncl.o

# tlisp is the standard raw lisp system.

tlisp:  ${TSrc} fixmask fixpbig.e ${TObj} 
	rm -f tlisp
	ld -x -o tlisp -e start ${TObj} -lm -lc -ltermlib
	ls -l tlisp


# vlisp is just like tlisp except it should be built with sources compiled
# with the -DVMS switch set
vlisp:  ${USOURCES} fixmask ${TOBJS} 
	rm -f vlisp
	ld -x -o vlisp -e start ${TOBJS} -lm -lc -ltermlib
	ls -l vlisp

# hlisp is a raw lisp system with a hole between text and data

hlisp:  ${HSrc}  fixmask fixpbig.e ${HObj} 
	rm -f hlisp
	ld -x -H $(HOLE) -o hlisp -e hstart ${HObj} -lm -lc -ltermlib
	ls -l hlisp



# switchos is used to force all files dependent on the operating system
# to recompiled
switchos:
	rm -f alloc.o Salloc.o data.o fex3.o fpipe.o lam8.o

switchgcstrings:
	rm -f alloc.o lam8.o

data.o: h/sigtab.h

rlc.o: rlc.c 
	cc -c -O -DHOLE=${HOLE} rlc.c

low.o: low.c
	cc -Ih -R -c low.c

Salloc.o: alloc.c
	(echo "# define HOLE"; cat alloc.c) > Zalloc.c ; make Zalloc.o;\
	rm Zalloc.c; mv Zalloc.o Salloc.o
	
Sdata.o: data.c
	(echo "# define HOLE"; cat data.c) > Zdata.c ; make Zdata.o;\
	rm Zdata.c; mv Zdata.o Sdata.o

fixmask: fixmask.c
	cc -O -o fixmask fixmask.c

clean:
	rm -f *.o

print:
	@pr READ*
	@pr *akefile*
	@(size lisp ; size *.o) | pr -h sizes
	@ls -ls | pr
	@pr h/*.h *.c *.s

lint:
	lint h/*.h *.c

tape:
	tar crv Makefile [a-z]*.[cshxe] /usr/lib/lisp

arfile:
	ar cr arfile Makefile [A-Za-z]*.[cshe] h/*.h
	

${AllSrc}:
	sccs get $@

install: mylisp 
	-rm -f ${ObjDir}/lisp
	mv mylisp ${ObjDir}/lisp
	@echo lisp installed

mylisp: tlisp ${LibDir}
	-rm -f mylisp
	(cd ${LibDir} ; make Liszt=${Liszt} required)
	echo "(progn (setq build:map 'map \
			   build:lisp-type 'franz \
		           lisp-library-directory '${LibDir} \
			   build:dir '${LibDir} \
			   lisp-object-directory '${ObjDir}) \
		     (load '${LibDir}/buildlisp)\
	             (dumplisp mylisp))" | tlisp
	${LibDir}/tackon map mylisp
	@echo mylisp built

myzlisp: tlisp ${LibDir}
	make tlisp
	-rm -f myzlisp
	(cd ${LibDir} ; make Liszt=${Liszt} all)
	echo "(progn (setq build:map 'map \
			   build:lisp-type 'zlisp \
		           lisp-library-directory '${LibDir} \
			   build:dir '${LibDir} \
			   lisp-object-directory '${ObjDir}) \
		     (load '${LibDir}/buildlisp)\
	             (dumplisp myzlisp))" | tlisp
	${LibDir}/tackon map myzlisp
	@echo myzlisp built

domylisp:
	-rm -f mylisp
	make LibDir=${LibDir} Liszt=${Liszt} ObjDir=${ObjDir} mylisp

#--- slowmylisp: create a totally interpreted lisp.
#	dump as mylisp
slowmylisp: tlisp
	echo "(progn (setq build:load t		\
			   build:lisp-type 'franz \
			   build:dir '${LibDir} \
			   lisp-object-directory '${ObjDir}\
			   lisp-library-directory '${LibDir})\
		     (load '${LibDir}/buildlisp)\
		     (dumplisp mylisp))" | tlisp


tags:  $(TagSrc) 
	ctags $(TagSrc)

#--- copysource : copy source files to another directory
#  called via   make CopyTo=/xx/yyy/zz copysource
# 
copysource: ${AllSrc}
	(tar cf - ${AllSrc} | (cd ${CopyTo} ; tar xf -))

copyobjects: ${AllObj}
	(tar cf - ${AllObj} | (cd ${CopyTo} ; tar xf -))

scriptcatall: ${AllSrc}
	@(cd .. ; scriptcat franz franz ${AllSrc})
