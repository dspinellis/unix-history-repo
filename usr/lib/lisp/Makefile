


#	Makefile for /usr/lib/lisp
# this directory contains the lisp coded portion of the standard 
# lisp system and other useful lisp programs. 
# The command 'make all' insures that all source files are compiled
# The command 'make install' installs these files in the standard
# place (/usr/lib/lisp).  This is only useful of course if the current
# directory is not /usr/lib/lisp.
#
#--- Default paths and programs:
LibDir = /usr/lib/lisp
CopyTo = /dev/null
Liszt = liszt 

#--- ReqSrc: required source for building lisp system
#
ReqSrc = charmac.l common0.l common1.l common2.l toplevel.l \
	syntax.l  macros.l  vector.l array.l pp.l  version.l

#--- OtherSrc: other lisp coded library files
OtherSrc = machacks.l loop.l ucifnc.l  ucido.l jkfmacs.l trace.l syscall.l\
	cmumacs.l cmufncs.l fix.l step.l cmufile.l cmutpl.l cmuedit.l \
	structinit.l struct.l prof.l

#--- UtilSrc: source for utility programs
UtilSrc =  tackon.c

ReqObj = charmac.o common0.o common1.o common2.o toplevel.o \
	syntax.o  macros.o  vector.o array.o pp.o version.o

OtherObj = machacks.o loop.o ucifnc.o  ucido.o jkfmacs.o trace.o syscall.o\
	cmumacs.o cmufncs.o fix.o step.o cmufile.o cmutpl.o cmuedit.o \
	structinit.o struct.o prof.o

UtilObj = tackon

#--- AllSrc: all source files required for lisp system
AllSrc = Makefile ReadMe buildlisp.l cmuenv.l ${ReqSrc} ${OtherSrc} ${UtilSrc}

AllObj = ${ReqObj} ${OtherObj} ${UtilObj}
    
all: ${AllObj}

required: ${ReqObj} ${UtilObj}

.SUFFIXES: .l

.l.o:
	${Liszt}  -xaq $* 

machacks.o: machacks.l
	${Liszt} -xaq machacks 

loop.o: loop.l
	${Liszt} -mxaq loop  

structinit.o: structinit.l
	${Liszt} -mxaq structinit 

struct.o: struct.l
	${Liszt} -mxaq struct  
${AllSrc}:
	sccs get $@

sources: ${AllSrc}

xref:
	lxref ${AllSrc} > xref

tackon: tackon.c
	cc -O -o tackon tackon.c

echofiles:
	@echo ${ReqSrc} ${OtherSrc}

echorequired:
	@echo ${ReqSrc}
	
copysource: ${AllSrc}
	(tar cf - ${AllSrc} | (cd ${CopyTo} ; tar xf -))

copyobjects: ${AllObj}
	(tar cf - ${AllObj} | (cd ${CopyTo} ; tar xf -))

scriptcatall: ${AllSrc}
	@(X=`pwd` ; cd ${CdTo} ; scriptcat $$X lisplib ${AllSrc})

as:
	@echo "Grabbing as from /bin"
	cp /bin/as as

nld:
	@echo "Grabbing nld from /bin"
	cp /bin/ld nld

clean: 
	-rm -f *.o 
	-rm -f *.blat 
	-rm -f *.x
	
