#$Header: Makefile,v 1.18 83/09/12 15:27:18 layer Exp $
#
#				-[Sat Jul 30 15:47:44 PDT 1983 by layer]-
#
#	Makefile for liszt 
#
# Copyright (c) 1980, 1982,  The Regents of the University of California.
# the Copyright applies to all files referenced in this Makefile.
# All rights reserved.  
# Authors: John Foderaro (jkf@berkeley.ARPA)
#	   Kevin Layer (layer@berkeley.ARPA)
#
# this makefile creates these things:
#   liszt - the interface to the lisp compiler.  This is used only for
#	    non virtual memory systems, because the overhead for forking
#	    the assembler is too great.
#   nliszt - the lisp compiler. This is the default.
#   snliszt - the lisp compiler, but interpreted.
#   fromasm - used to build the compiler usually for the first
#		time from only .s files.  These files are assembled
#		and loaded into a lisp.
#   install - install the new version created
#   clean - remove all .o files and *nliszt's
#   tags - a tags file for use by ex/vi
#
# CTE refers to compile time enviroment 
#
#--- Default Paths and programs
#
.DEFAULT:nliszt
.SUFFIXES:
.SUFFIXES: .l.o

# DESTDIR is the relative offset of where the compiler goes
# (when making new distributions, the `root' is often at /nbsd).
# RootDir is the root directory of the franz lisp system
# (this is changed when the system is configured by ../../lispconf).
DESTDIR =

LibDir = ${DESTDIR}/usr/lib/lisp
ObjDir = ${DESTDIR}/usr/ucb
Liszt = ${ObjDir}/liszt
Lisp = ${ObjDir}/lisp
#ifdef swapper
#XLiszt = ${ObjDir}/xliszt
#endif

CFLAGS = -O # -Ddebug
Flg =  -xqa

CTESrc = ../chead.l ../cmacros.l ../const.l

CTEObj= cmacros.o

Src =  ../array.l ../datab.l ../decl.l ../expr.l ../fixnum.l ../funa.l\
       ../instr.l ../vector.l ../funb.l ../func.l ../io.l\
       ../tlev.l ../util.l ../lversion.l

SharedSrc = ${CTESrc} ${Src} ../ChangeLog ../cmake.l

AllSrc =  Makefile Makefile2 lisprc.l lisztrc.l liszt.c

Obj = array.o vector.o datab.o decl.o expr.o fixnum.o\
      instr.o funa.o funb.o func.o io.o tlev.o util.o lversion.o

AllObj =  ${CTEObj} ${Obj}

#liszt :: the user interface to xliszt
# (only for swapped based systems, right now dual/unisoft)
#ifdef swapper
#liszt:	liszt.c
#	cc $(CFLAGS) -DLISZT='"${XLiszt}"' -DAS='"${LibDir}/as"'\
#	   -o liszt liszt.c
#else
liszt:
#endif

donliszt:
	rm -f nliszt
	make Liszt=${Liszt} Lisp=${Lisp} nliszt

nliszt: ${CTEObj} ${Obj} liszt ${Lisp}
	rm -f nliszt
	echo "(load '../cmake.l)(genl nliszt)" | ${Lisp} 

#--- generate an interpreted version
snliszt: ${Src} ${Lisp}
	rm -f snliszt
	echo "(load '../cmake.l)(genl snliszt slow)" | ${Lisp}

# 'fromasm' is for making the compiler from
# .s files.  On 68k systems this is much faster than
# doing a 'make slow', then a 'make fast'.
fromasm:	assit load liszt
assit:
	for i in *.s; do echo $$i; as $$i; done

#--- load .o files into a lisp
load:
	rm -f nliszt
	echo "(load '../cmake.l)(genl nliszt)" | ${Lisp} 

# install nliszt, and if we are on a swap based system, then
#install nliszt as xliszt, and liszt (from liszt.c) as liszt.
install:
#ifdef swapper
#	mv nliszt ${XLiszt}
#	cp liszt ${Liszt}
#else
	mv nliszt ${Liszt}
#endif

clean:	cleanobj
	rm -f \#* *nliszt *.s

cleanobj:
	rm -f *.[ox]

#--- rules for each lisp file:
cmacros.o: ../cmacros.l
	${Liszt} ${Flg} ../cmacros.l -o cmacros.o

array.o: ../array.l
	${Liszt} ${Flg} ../array.l -o array.o

instr.o: ../instr.l
	${Liszt} ${Flg} ../instr.l -o instr.o

vector.o: ../vector.l
	${Liszt} ${Flg} ../vector.l -o vector.o

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

tags:	../tags ${Src} ${CTESrc}
	awk -f ../ltags ${Src} ${CTESrc} | sort > ../tags

print:
#	@pr README
	@ls -l | pr 
	@pr TODO Makefile* ../cmake.l lisztrc.l lisprc.l
	@pr -h "Liszt.c (for non-VMUNIX systems only)" liszt.c
	@/usr/local/slp -l ../lversion.l ../chead.l ../cmacros.l\
	   ../datab.l ../decl.l ../expr.l\
	   ../funa.l ../funb.l ../func.l\
	   ../fixnum.l ../array.l ../io.l ../tlev.l ../util.l

iprint:
	igrind -lsh Makefile*
	igrind -lc -h "Liszt.c (for non-VMUNIX systems only)" liszt.c
	vlp -p 10 ../lversion.l\
	   ../chead.l ../cmacros.l\
	   ../datab.l ../decl.l ../expr.l\
	   ../funa.l ../funb.l ../func.l\
	   ../fixnum.l ../array.l ../io.l ../tlev.l ../util.l\
	   ../cmake.l lisztrc.l lisprc.l > vlp.out
	itroff vlp.out
	rm vlp.out

scriptcatall: ${AllSrc}
	@../../scriptcat . liszt/68k ${AllSrc}

copysource: ${AllSrc}
	(tar cf - ${AllSrc} | (cd ${CopyTo} ; tar xf -))

copyobjects: ${AllObj}
	(tar cf - ${AllObj} | (cd ${CopyTo} ; tar xf -))
