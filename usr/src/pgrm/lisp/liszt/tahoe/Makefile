#$Header: /na/franz/liszt/vax/RCS/Makefile,v 1.6 83/08/15 19:27:49 layer Exp $
#
#	Makefile for liszt 
#
# Copyright (c) 1980, 1982, The Regents of the University of California.
# the Copyright applies to all files referenced in this Makefile.
# All rights reserved.  
# author: j. foderaro
#
# this makefile creates these things:
#   nliszt - the lisp compiler.  We call it nliszt so we can test it out
#	     without having to say ./liszt
#   tags - a tags file for use by ex/vi
#
# CTE refers to compile time enviroment 
#
#--- Default Paths and programs
#
.DEFAULT:nliszt

CopyTo = /dev/null
ObjDir = /usr/ucb
Liszt = liszt
Lisp = ../../franz/tahoe/nlisp

Flg = -qa

CTESrc = ../chead.l ../cmacros.l ../const.l

CTEObj= cmacros.o

Src =  ../array.l ../datab.l ../decl.l 	../expr.l ../fixnum.l ../funa.l	 \
	../funb.l ../func.l ../io.l ../tlev.l ../util.l ../lversion.l	\
	../vector.l ../instr.l

SharedSrc = ${CTESrc} ${Src} ../ChangeLog ../cmake.l

AllSrc =  Makefile lisprc.l lisztrc.l

Obj = array.o datab.o decl.o expr.o fixnum.o funa.o funb.o func.o io.o \
	tlev.o util.o lversion.o vector.o instr.o

AllObj =  ${CTEObj} ${Obj}

donliszt:
	rm -f nliszt
	make Liszt=${Liszt} Lisp=${Lisp} nliszt

nliszt: ${CTEObj} ${Obj} ${Lisp}
	echo "(load '../cmake.l)(genl nliszt)" | ${Lisp} 

#--- generate an interpreted version
snliszt: ${Src} ${Lisp}
	echo "(load '../cmake.l)(genl snliszt slow)" | ${Lisp}

array.o: ../array.l
	${Liszt} ${Flg} ../array.l -o array.o

vector.o: ../vector.l
	${Liszt} ${Flg} ../vector.l -o vector.o

instr.o: ../instr.l
	${Liszt} ${Flg} ../instr.l -o instr.o

datab.o: ../datab.l
	${Liszt} ${Flg} ../datab.l -o datab.o

decl.o: ../decl.l
	${Liszt} ${Flg} ../decl.l -o decl.o

expr.o: ../expr.l
	${Liszt} ${Flg} ../expr.l -o expr.o

fixnum.o: ../fixnum.l
	${Liszt} ${Flg} ../fixnum.l -o fixnum.o

funa.o: ../funa.l
	${Liszt} ${Flg} ../funa.l -o funa.o

funb.o: ../funb.l
	${Liszt} ${Flg} ../funb.l -o funb.o

func.o: ../func.l
	${Liszt} ${Flg} ../func.l -o func.o

io.o: ../io.l
	${Liszt} ${Flg} ../io.l -o io.o

tlev.o: ../tlev.l
	${Liszt} ${Flg} ../tlev.l -o tlev.o

util.o: ../util.l
	${Liszt} ${Flg} ../util.l -o util.o

lversion.o: ../lversion.l
	${Liszt} ${Flg} ../lversion.l -o lversion.o

cmacros.o: ../cmacros.l
	${Liszt} ${Flg} ../cmacros.l -o cmacros.o

tags:	../tags ${Src} ${CTESrc}
	awk -f ../ltags ${Src} ${CTESrc} | sort > ../tags

#
install: nliszt
	-rm -f ${ObjDir}/liszt
	install nliszt ${ObjDir}/liszt

copysource: ${AllSrc}
	(tar cf - ${AllSrc} | (cd ${CopyTo} ; tar xf -))

copyobjects: ${AllObj}
	(tar cf - ${AllObj} | (cd ${CopyTo} ; tar xf -))

scriptcatall: ${AllSrc}
	@../../scriptcat . liszt/vax ${AllSrc}

cleanobj:	
	rm -f \#* *.[xo] map

clean:
	make cleanobj
	rm -f nliszt snliszt
