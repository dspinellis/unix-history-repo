# $Header: /usr/lib/lisp/RCS/Makefile,v 1.1 83/01/29 18:43:41 jkf Exp $
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
	syntax.l  macros.l  vector.l array.l pp.l format.l version.l \
	tpl.l fcninfo.l

#--- OtherSrc: other lisp coded library files
OtherSrc = machacks.l loop.l ucifnc.l  ucido.l jkfmacs.l trace.l\
	syscall.l \
	cmumacs.l cmufncs.l fix.l step.l cmufile.l cmutpl.l cmuedit.l \
	structini.l struct.l prof.l hash.l flavorm.l lmhacks.l

LocalSrc = describe.l flavors.l vanilla.l

#--- UtilSrc: source for utility programs
UtilSrc =  tackon.c

ReqObj = charmac.o common0.o common1.o common2.o toplevel.o \
	syntax.o  macros.o  vector.o array.o pp.o format.o version.o \
	tpl.o fcninfo.o

OtherObj = machacks.o loop.o ucifnc.o  ucido.o jkfmacs.o trace.o\
	syscall.o\
	cmumacs.o cmufncs.o fix.o step.o cmufile.o cmutpl.o cmuedit.o \
	struct.o prof.o hash.o flavorm.o lmhacks.o

LocalObj =  describe.o flavors.o vanilla.o

UtilObj = tackon

#--- AllSrc: all source files required for lisp system
#		LocalSrc isn't included!
AllSrc = Makefile ReadMe buildlisp.l cmuenv.l fixit.ref \
	${ReqSrc} ${OtherSrc} ${UtilSrc}

AllObj = ${ReqObj} ${OtherObj} ${UtilObj}
    
all: ${AllObj}

local-all: ${AllObj} ${LocalObj}

required: ${ReqObj} ${UtilObj}

.SUFFIXES: .l

.l.o:
	${Liszt}  -xaq $* 

${AllSrc}:
	sccs get $@

## defstruct should be compiled with a compiled version of itself.
## When a compiled form doesn't exist, structini.l can be used to
## build a struct.o which is close but not exactly what you want.
## Recompiling struct will use struct.o and create the correct struct.o
## 
struct-again:
	${Liszt} -xaq struct

## this will only work if you have an up to date version of ctags which
## understands lisp files.

tags:    ${AllSrc}
	ctags ${AllSrc}

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

cleanreq:
	-rm -f ${ReqObj}

cleanall:
	-rm -f ${AllObj}

cleanother:
	-rm -f ${OtherObj}

clean: 
	-rm -f *.o 
	-rm -f *.blat 
	-rm -f *.x
