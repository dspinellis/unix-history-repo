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
LibDir = /usr/lib/lisp
LispDist = /usr/src/ucb/lispdist
CopyTo = /dev/null
CcodeDir = ../franz
ObjDir = /usr/ucb
DestObjDir = ${DESTDIR}/${ObjDir}
Lisp = ${ObjDir}/lisp
Liszt = ${ObjDir}/liszt
Version = 38.55

#--- this directory also has some sources
Src = Makefile ReadMe lispconf lispnews scriptcat Notice

## when the lisp system is rebuilt as part of the entire Nbsd distribution,
# three calls are made: 
#   first   'make'		 	to build lisp,liszt and lxref
#   next    'make DESTDIR=xxx install'	to install the new lisp
#   finally 'make clean'		to clean out objects
#
# the 'make all' is done when just a 'make' is done
all:
	(cd ${DESTDIR}/${LibDir} ; make as nld)
	(cd franz; make LibDir=${DESTDIR}/${LibDir} \
		        ObjDir=${DESTDIR}/${ObjDir}  domylisp)
	(cd liszt; make Lisp=../franz/mylisp nliszt)
	(cd liszt; make Liszt=./nliszt lxref)

	
copylibrary: 
	(cd lisplib ; make CopyTo=${LibDir} copysource)
	-mkdir ${LibDir}/manual
	(cd doc; \
          make LibDir=../lisplib CopyTo=${LibDir}/manual copymanual)

fast:
	date
	(cd ${LibDir}; make as nld tackon)
	(cd franz; make Lisp=${Lisp} Liszt=${Liszt} LibDir=${LibDir}\
			ObjDir=${DestObjDir} domylisp)
	(cd liszt; make Lisp=${CcodeDir}/mylisp Liszt=${Liszt} donliszt)
	(X=`pwd` ; cd ${LibDir}; make Liszt=$$X/liszt/nliszt clean all)
	date
	(cd franz; make Liszt=`pwd`/../liszt/nliszt \
			ObjDir=${DestObjDir} LibDir=${LibDir} domylisp)
	date
	(cd liszt; make Lisp=${CcodeDir}/mylisp \
			Liszt=./nliszt cleanobj nliszt)
	(cd liszt; make Liszt=./nliszt lxref)
	date

slow:
	date
	(cd ${LibDir}; make as nld tackon)
	(cd franz; make LibDir=${LibDir} ObjDir=${DestObjDir} slowmylisp)
	date
	(cd liszt; make Lisp=${CcodeDir}/mylisp snliszt)
	(cd liszt; make Lisp=${CcodeDir}/mylisp Liszt=./snliszt nliszt)
	(cd liszt; rm snliszt)
	date
	(X=`pwd`; cd franz;  make Liszt=$$X/liszt/nliszt \
				   ObjDir=${DestObjDir}\
				   LibDir=${LibDir} domylisp)
	(cd liszt; make Lisp=${CcodeDir}/mylisp Liszt=./nliszt nliszt)
	(X=`pwd`; cd ${LibDir} ; make Liszt=$$X/liszt/nliszt all)
	(X=`pwd`; cd ${LibDir} ; make Liszt=$$X/liszt/nliszt struct-again)
	(X=`pwd`; cd franz;  make Liszt=$$X/liszt/nliszt \
				   ObjDir=${DestObjDir}\
				   LibDir=${LibDir} domylisp)
	(cd liszt; make Lisp=${CcodeDir}/mylisp Liszt=./nliszt nliszt)
	(cd liszt; make Liszt=./nliszt lxref)
	date

install:
	(cd franz; make ObjDir=${DestObjDir} LibDir=${LibDir} install)
	(cd liszt; make ObjDir=${DestObjDir} LibDir=${LibDir} install)
	(cd liszt; make ObjDir=${DestObjDir} LibDir=${LibDir} install-lxref)


copyallsource:
	-mkdir ${CopyTo}/franz
	-mkdir ${CopyTo}/liszt
	-mkdir ${CopyTo}/doc
	-mkdir ${CopyTo}/lisplib
	-mkdir ${CopyTo}/lisplib/manual
	cp ${Src} ${CopyTo}
	(cd franz; make   CopyTo=${CopyTo}/franz copysource)
	(cd liszt; make  CopyTo=${CopyTo}/liszt copysource)
	(cd ${LibDir} ; make  CopyTo=${CopyTo}/lisplib copysource)
	(cd doc; make CopyTo=${CopyTo}/doc copysource)
	(cd doc; \
          make LibDir=${LibDir} CopyTo=${CopyTo}/lisplib/manual copymanual)
  
lispdist:
	(cd ${LispDist}; rm -f -r *)
	make CopyTo=${LispDist} copyallsource

copyallobjects:
	(cd franz; make   CopyTo=${CopyTo}/franz copyobjects)
	(cd liszt; make  CopyTo=${CopyTo}/liszt copyobjects)
	(cd ${LibDir} ; make  CopyTo=${CopyTo}/lisplib copyobjects)

prelispscriptdist:
	(cd doc ; make append all)

lispscriptdist:
	(cd ${LispDist}; rm -f -r *)
	make prelispscriptdist
	(make genlispscript | (cd ${LispDist} ; \
			       divide -500000 -popus${Version}. -l))


genlispscript:
	@echo \# Franz Lisp distribution Opus ${Version}
	@echo mkdir franz
	@echo mkdir franz/h
	@echo mkdir liszt
	@echo mkdir doc
	@echo mkdir lisplib
	@echo mkdir lisplib/manual
	@scriptcat . . ${Src}
	@echo chmod 775 lispconf
	@(cd franz ; make scriptcatall)
	@(cd liszt ; make scriptcatall)
	@(cd doc ; make LibDir=${LibDir} scriptcatall)
	@(X=`pwd` ; cd ${LibDir} ;  make CdTo=$$X scriptcatall)
	@echo \# end of Franz Lisp Distribution

clean:
	cd franz; make clean
	cd liszt; make clean
	cd doc; make clean
