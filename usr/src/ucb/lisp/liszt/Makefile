
# SCCS id: @(#)Makefile	1.6	12/13/82

#	Makefile for liszt 

# Copyright (c) 1980 ,  The Regents of the University of California.
# the Copyright applies to all files referenced in this Makefile.
# All rights reserved.  
# author: j. foderaro

# this makefile creates these things:
#   nliszt - the lisp compiler.  We call it nliszt so we can test it out
#	     without having to say ./liszt
#   tags - a tags file for use by ex/vi
#
# CTE refers to compile time enviroment 
#
#--- Default Paths and programs
#
LibDir = /usr/lib/lisp
CopyTo = /dev/null
CcodeDir = ../franz
ObjDir = /usr/ucb
Liszt = ${ObjDir}/liszt
Lisp = ${ObjDir}/lisp

CTESrc = chead.l cmacros.l

CTEObj= cmacros.o

Src =  array.l datab.l decl.l 	expr.l	fixnum.l funa.l	funb.l func.l io.l \
	tlev.l 	util.l lversion.l

LxrefSrc = lxref.l

LxrefObj = lxref.o

AllSrc =  Makefile ChangeLog scmake.l cmake.l ${CTESrc} ${Src} ${LxrefSrc}

Obj = array.o datab.o decl.o 	expr.o	fixnum.o funa.o	funb.o func.o io.o \
	tlev.o 	util.o  lversion.o

AllObj =  ${CTEObj} ${Obj} ${LxrefObj}

.SUFFIXES: .l
.l.o:
	${Liszt} -qxa $* 

${Obj}: ${CTEObj}

${Src}:
	sccs get $@

nliszt : ${Obj} ${Lisp}
	echo "(load 'cmake.l)" | ${Lisp} 
	@echo nliszt done

#--- donliszt: force nliszt to be made
donliszt:
	-rm -f nliszt
	make Liszt=${Liszt} Lisp=${Lisp} nliszt

#--- generate an interpreted version
snliszt: ${Src}
	echo "(load 'scmake.l)" | ${Lisp}
	@echo snliszt done

lxref: lxref.o
	cp lxref.o lxref

lxref.o: lxref.l
	${Liszt} -xrq lxref

tags: /dev/tty  ${AllSrc}
	awk -f ltags ${AllSrc} | sort > tags


#
install: nliszt
	-rm -f ${ObjDir}/liszt
	mv nliszt ${ObjDir}/liszt

install-lxref: lxref
	-rm -f ${ObjDir}/lxref
	mv lxref ${ObjDir}/lxref

## this section potentially generates the system from scratch
## It takes a raw lisp system and builds the entire compiler
## interpreted and then dumps it out.  It will load in source
## files if object files are not available.
##

copysource: ${AllSrc}
	(tar cf - ${AllSrc} | (cd ${CopyTo} ; tar xf -))

copyobjects: ${AllObj}
	(tar cf - ${AllObj} | (cd ${CopyTo} ; tar xf -))

scriptcatall: ${AllSrc}
	@(cd .. ; scriptcat liszt liszt ${AllSrc})

cleanobj:	
	-rm -f \#* 
	-rm -f *.o 
	-rm -f map

clean:
	make cleanobj
	-rm -f nliszt



