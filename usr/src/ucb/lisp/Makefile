# $Header: Makefile,v 1.28 84/02/03 08:09:00 jkf Exp $
# $Locker:  $
#  Makefile for total Franz Lisp system.
#
# Read the file ReadMe for directions on how to build the system.
#
#    capabilities of this directory.
# copylibrary: copies distribution copy of lisp directory to LibDir
# fast: make a new lisp and liszt assuming that a liszt and lisp
#	already exist.  Results are franz/mylisp and liszt/nliszt.
#	Use 'make install' to install it.
# install: moves franz/mylisp to ObjDir/lisp  and moves
#	liszt/nliszt to ObjDir/liszt
#
#-- the rest of the capabilities are for use when making a distribution
#   directory.
# copyallsource: copies lisp distrib files in the current directory tree 
#       to the tree CopyTo.  
#	CopyTo should exist already, but the subdirectories
#	need not exist.
#
# Before doing one of the below, you should make sure that the on line
# manual is up to date.  Go to the doc subdirectory and type 
#	'make rall install'
#
# lispdist: makes a new distribution directory in LispDist.
#	LispDist should already exist.
#
# lispscriptdist: make a shell script lisp distribution.  The result is put
# 	in LispDist as a set of text files comprising a shell script.
#	The files are broken into a nice size for transport over the berknet.
#	The first thing that lispscriptdist does is to, 
#		'make prelispscriptdist'
#	Which insures that the files are ready to go.
#	Also, the value of Version should be set to the version number of
#	lisp you are making.
#
#--- Default paths and programs
DESTDIR =
.DEFAULT: all

# the following line is modifed by './lispconf', don't modify it by hand.
#ifdef vax
Mach = vax
#else
#Mach = 68k
#endif

RootDir = /usr/src/ucb/lisp
#ifdef ucbstd
LibDir = ${DESTDIR}/usr/lib/lisp
ObjDir = ${DESTDIR}/usr/ucb
#else
#LibDir = ${DESTDIR}${RootDir}/lisplib
#ObjDir = ${DESTDIR}${RootDir}/bin
#endif
LispDist = /usr/src/ucb/lispdist
CopyTo = /dev/null
Lisp = ${ObjDir}/lisp
Liszt = ${ObjDir}/liszt
Version = 38.89

# definitions that you shouldn't change
FranzD = franz/${Mach}
LisztD = liszt/${Mach}
CcodeDir = ../../${FranzD}

#--- this directory also has some sources
Src = Makefile ReadMe lispconf lispnews scriptcat Notice cvt.awk

#make as lisp and lisp assuming that there are .s files in the
#lisplib and liszt subdirs
fromasm:
#ifdef unisoft
#	(cd as68 ; make DESTDIR=${LibDir} install)
#endif
	(cd utils     ; make LibDir=${LibDir} all)
	(cd ${LibDir} ; make LibDir=${LibDir} as nld fromasm)
	(cd ${FranzD} ; make LibDir=${LibDir} ObjDir=${ObjDir} nlisp)
	(cd ${LisztD} ; make Lisp=${CcodeDir}/nlisp fromasm)
	(cd liszt     ; make Liszt=${Mach}/nliszt lxref)

## when the lisp system is rebuilt as part of the entire Nbsd distribution,
# three calls are made: 
#   first   'make'		 	to build lisp,liszt and lxref
#   next    'make DESTDIR=xxx install'	to install the new lisp
#   finally 'make clean'		to clean out objects
#
# the 'make all' is done when just a 'make' is done
all:
	(cd utils     ; make LibDir=${LibDir} all)
	(cd ${LibDir} ; make as nld)
	(cd ${FranzD} ; make LibDir=${LibDir} ObjDir=${ObjDir} donlisp)
	(cd ${LisztD} ; make Lisp=${CcodeDir}/nlisp nliszt)
	(cd liszt     ; make Liszt=${Mach}/nliszt lxref)

	
copylibrary: 
#ifdef ucbstd
	(cd lisplib ; make CopyTo=${LibDir} copysource)
	-mkdir ${LibDir}/manual
	(cd doc; make LibDir=${LibDir} FromDir=../lisplib \
				CopyTo=${LibDir}/manual copymanual)
#endif

fast:
	date
	(cd utils ; make LibDir=${LibDir} all)
	(cd ${LibDir}; make as nld tackon)
	(cd ${FranzD}; make Lisp=${Lisp} Liszt=${Liszt} LibDir=${LibDir}\
			        ObjDir=${ObjDir} donlisp)
	(cd ${LisztD}; make Lisp=${CcodeDir}/nlisp Liszt=${Liszt} donliszt)
	(X=`pwd` ; cd ${LibDir}; make Liszt=$$X/${LisztD}/nliszt clean all)
	date
	(cd ${FranzD}; make Liszt=../../${LisztD}/nliszt \
			ObjDir=${ObjDir} LibDir=${LibDir} donlisp)
	date
	(cd ${LisztD}; make Lisp=${CcodeDir}/nlisp \
			    Liszt=./nliszt cleanobj nliszt)
	(cd liszt ; make Liszt=${Mach}/nliszt lxref)
	date

slow:
	date
	(cd utils ; make LibDir=${LibDir} all)
	(cd ${LibDir}; make as nld)
	(cd ${FranzD}; make LibDir=${LibDir} ObjDir=${ObjDir} snlisp)
	date
	(cd ${LisztD}; make Lisp=${CcodeDir}/snlisp snliszt)
	(cd ${LisztD}; make Lisp=${CcodeDir}/snlisp Liszt=./snliszt nliszt)
	rm -f ${LisztD}/snliszt
	date
	rm -f ${FranzD}/snlisp
	(X=`pwd`; cd ${FranzD};make Liszt=$$X/${LisztD}/nliszt \
				    ObjDir=${ObjDir}\
				    LibDir=${LibDir} nlisp)
	(cd ${LisztD}; make Lisp=${CcodeDir}/nlisp Liszt=./nliszt nliszt)
	(X=`pwd`; cd ${LibDir} ; make Liszt=$$X/${LisztD}/nliszt all)
	(X=`pwd`; cd ${LibDir} ; make Liszt=$$X/${LisztD}/nliszt struct-again)
	(X=`pwd`; cd ${FranzD} ;  make Liszt=$$X/${LisztD}/nliszt \
				       ObjDir=${ObjDir}\
				       LibDir=${LibDir} donlisp)
	(cd ${LisztD}; make Lisp=${CcodeDir}/nlisp Liszt=./nliszt nliszt)
	(cd liszt ; make Liszt=${Mach}/nliszt lxref)
	date

install:
	(cd ${FranzD}; make ObjDir=${ObjDir} LibDir=${LibDir} install)
	(cd ${LisztD}; make ObjDir=${ObjDir} LibDir=${LibDir} install)
	(cd liszt; make ObjDir=${ObjDir} LibDir=${LibDir} install)


copyallsource:
	-mkdir ${CopyTo}/franz
	-mkdir ${CopyTo}/franz/vax
	-mkdir ${CopyTo}/franz/68k
	-mkdir ${CopyTo}/liszt
	-mkdir ${CopyTo}/liszt/vax
	-mkdir ${CopyTo}/liszt/68k
	-mkdir ${CopyTo}/doc
	-mkdir ${CopyTo}/utils
	-mkdir ${CopyTo}/lisplib
	-mkdir ${CopyTo}/lisplib/manual
	-mkdir ${CopyTo}/lisplib/autorun
	-mkdir ${CopyTo}/pearl
	cp ${Src} ${CopyTo}
	(cd franz; make   CopyTo=${CopyTo}/franz copysource)
	(cd franz/vax; make   CopyTo=${CopyTo}/franz/vax copysource)
	(cd franz/68k; make   CopyTo=${CopyTo}/franz/68k copysource)
	(cd liszt; make  CopyTo=${CopyTo}/liszt copysource)
	(cd liszt/vax; make  CopyTo=${CopyTo}/liszt/vax copysource)
	(cd liszt/68k; make  CopyTo=${CopyTo}/liszt/68k copysource)
	(cd ${LibDir} ; make  CopyTo=${CopyTo}/lisplib copysource)
	(cd doc; make CopyTo=${CopyTo}/doc copysource)
	(cd utils; make CopyTo=${CopyTo}/utils copysource)
	(cd doc; \
          make FromDir=${LibDir} CopyTo=${CopyTo}/lisplib/manual copymanual)
	(cd pearl; make CopyTo=${CopyTo}/pearl copysource)
  
lispdist:
	(cd ${LispDist}; rm -f -r *)
	make CopyTo=${LispDist} copyallsource

copyallobjects:
	(cd franz/vax; make   CopyTo=${CopyTo}/franz/vax copyobjects)
	(cd franz/68k; make   CopyTo=${CopyTo}/franz/68k copyobjects)
	(cd liszt/vax; make  CopyTo=${CopyTo}/liszt/vax copyobjects)
	(cd ${LibDir} ; make  CopyTo=${CopyTo}/lisplib copyobjects)

prelispscriptdist:
	(cd doc ; make all)

lispscriptdist:
	(cd ${LispDist}; rm -f -r opus*)
	make prelispscriptdist
	(make genlispscript | (cd ${LispDist} ; \
			       divide -500000 -popus${Version}. -l))

xtra: 
	(cd ${LispDist}; rm -f -r x${Mach}*)
	make Mach=${Mach} setupx
	(make Mach=${Mach} genxtra) | \
	     (cd ${LispDist} ; divide -500000 -px${Mach}.${Version}. -l)

lispas:
	make genas68 > ${LispDist}/lispas

setupx:
	(X=`pwd`; cd ${LibDir};\
	make Liszt="$$X/liszt/${Mach}/nliszt -e '(sstatus feature for-${Mach})'" xtra)
	(cd liszt/${Mach}; make  -f Makefile2 xtra)

genlispscript:
	@echo \# Franz Lisp distribution Opus ${Version}
	@echo mkdir franz
	@echo mkdir franz/h
	@echo mkdir franz/vax
	@echo mkdir franz/68k
	@echo mkdir liszt
	@echo mkdir liszt/vax
	@echo mkdir liszt/68k
	@echo mkdir doc
	@echo mkdir utils
	@echo mkdir pearl
	@echo mkdir lisplib
	@echo mkdir lisplib/manual
	@echo mkdir lisplib/autorun
	@scriptcat . . ${Src}
	@echo chmod 775 lispconf
	@(cd franz ; make scriptcatall)
	@(cd franz/vax ; make scriptcatall)
	@(cd franz/68k ; make scriptcatall)
	@(cd liszt ; make scriptcatall)
	@(cd liszt/vax ; make scriptcatall)
	@(cd liszt/68k ; make scriptcatall)
	@(cd doc ; make LibDir=${LibDir} scriptcatall)
	@(cd utils ; make scriptcatall )
	@(X=`pwd` ; cd pearl ; make CdTo=$$X scriptcatall)
	@(X=`pwd` ; cd ${LibDir} ;  make CdTo=$$X scriptcatall)
	@echo \# end of Franz Lisp Distribution

genas68:
	@(echo mkdir as68;cd as68; make scriptcat)

genxtra:
	@(X=`pwd` ; cd ${LibDir} ;  make CdTo=$$X scriptcatxtra)
	@(cd liszt/${Mach} ; make -f Makefile2 scriptcatxtra)

clean:
	cd franz    ; make clean
	cd franz/vax; make clean
	cd franz/68k; make clean
	cd liszt    ; make clean
	cd liszt/vax; make clean
	cd liszt/68k; make clean
	cd doc	    ; make clean

vdist-dim:
	rdist -v -d host=dim

dist-dim:
	rdist -d host=dim
