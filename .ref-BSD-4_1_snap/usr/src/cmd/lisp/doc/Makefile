#  @(#)Makefile	34.10 5/18/82
#	makefile for the franz lisp manual
#
# sources: lmacs: macros for the franz documents
#	   ch1.n  intro and description of types
#	   ch2.n  data structure access
#	   ch3.n  on arithmetic functions.
#	   ch4.n  special functions.
#	   ch5.n  i/o
#	   ch6.n  system functions
#	   ch7.n  reader
#	   ch8.n  functions and macros 
#	   ch9.n  arrays 
#	   ch10.n exception handling 
#	   ch11.n trace package 
#	   ch12.n liszt 
#	   ch13.n cmu top level
#	   ch14.n stepper 
#	   ch15.n fixit package
#	   ch16.n lisp editor
#	   chb.n special symbols
#	   chc.n  short subjects


.SUFFIXES: .n .t .x .v .r .rx .q .qx .sp

# the syntax's have this meaning:
#  .n  nroff/troff source file
#  .t  troff output file, can be vpr -t 'ed 
#  .x  index file from a troff run, when collected and run through troff
#	again, an index is produced.
#  .v  this file never exists, but asking for it will cause a .t file to
#	be created and then vpr'ed.  the .t file will not be deleted.
#  .r  nroff output file.
#  .rx  special index output from nroff run.  These files should be catted
#       together and then left around for lisp to read when given the help
#	command.
#
#  .q  nroff output file compatible with model 37
#  .qx index file for .q files.
#
#  .sp spell errors
#
# make install will install the nroff versions of the manual in the
# directory (LMAN) where the auxfns0.l help command can find them.
#
LibDir = /usr/lib/lisp
CcodeDir = ../franz
CopyTo = /dev/null
TROFF=	/usr/ucb/vtroff
NROFF=  nroff
Rmt = 
O =

#--- Sources: 
#	We use the suffixes to tell make how to make a file.  Thus
#  we only specify the root and let the append function add the
#  appropriate suffix.

MacroSrc = lmacs

RootGenSrc = ch0 ch1 ch2 ch3 ch4 ch5 ch6 ch61 ch7 ch8 \
      ch9 ch10 ch11 ch12 ch13 ch14 ch15 ch16 chb chc

UtilSrc = Makefile indexsed mantags extrnames.awk fixmks.sed append.c \
	franz.n

#-- can't get a expression for all source at make read time. must use
#   append to add .n to RootGenSrc


all:
	make rall

rall: append
	make Rmt=${Rmt} O=${O} `append .r ${RootGenSrc}` helpindex

vall: append
	make Rmt=${Rmt} O=${O} `append .v ${RootGenSrc}` index.v

tall: append
	make Rmt=${Rmt} O=${O} `append .t ${RootGenSrc}` index.t

pall: append
	make Rmt=${Rmt} O=${O} `append .p ${RootGenSrc}` index.t
      
qall: append
	make Rmt=${Rmt} O=${O} `append .q ${RootGenSrc}` index.t

spall: append
	make Rmt=${Rmt} O=${O} `append .q ${RootGenSrc}` index.t

# only a few files describe functions which are indexed.

.t.v:
	vpr -t $*.t

.n.t:	
	tbl lmacs $*.n | ${Rmt} ${TROFF} -me ${O} -t 1> $*.t 2> $*.x

.n.p:
	tbl lmacs $*.n | csh /usr/ucb/nettroff ${O} -me 2> $*.x

.n.x:
	tbl lmacs $*.n | ${Rmt} ${TROFF} -me -z 2> $*.x

.n.r:
	tbl lmacs $*.n | ${Rmt} ${NROFF} -rb3 -me ${O} 1> $*.r 2> $*.rx

.n.rx:
	tbl lmacs $*.n | ${Rmt} ${NROFF} -rb3 -me 1> $*.r 2> $*.rx

.n.q:
	tbl lmacs $*.n | ${Rmt} ${NROFF} -me -T37 2> $*.qx | col > $*.q

.n.sp:
	spell $*.n > $*.sp
	
install: 
	make Rmt=${Rmt} O=${O} rall
	cp `append .r ${RootGenSrc}` helpindex ${LibDir}/manual

clean:
	-rm -f *.r 
	-rm -f *.rx 
	-rm -f helpindex 
	-rm -f *.t 
	-rm -f *.q 
	-rm -f *.x
	-rm -f append


findex:  append
	make `append .x ${RootGenSrc}`
	echo ".Ib" > index
	sort +3 -o index index `append .x ${RootGenSrc}`
	sed -f indexsed index > indexx 
	${Rmt} ${TROFF} -me lmacs indexx

index.t: 
	make `append .x ${RootGenSrc}`
	echo ".Ib" > index
	sort +3 -o index index `append .x ${RootGenSrc}`
	sed -f indexsed index > indexx
	${TROFF} -me -x -t lmacs indexx > index.t

pindex: append
	make `append .x ${RootGenSrc}`
	echo ".Ib" > index
	sort +3 -o index index `append .x ${RootGenSrc}`
	sed -f indexsed index > indexx
	csh /usr/ucb/nettroff -me lmacs indexx

helpindex: append
	make `append .rx ${RootGenSrc}`
	cat `append .rx ${RootGenSrc}` | tr '\227' ' ' > helpindex

tags:	/dev/tty append
	awk -f mantags `append .n ${RootGenSrc}` | sort > tags

append: append.c
	cc -o append append.c

# to create a database for lxref to use:
C-database: ${CcodeDir}/sysat.c
	grep "^	MK" ${CcodeDir}/sysat.c > mks
	sed -f fixmks.sed < mks > mks.fixed
	(echo "(Chome)" ; cat mks.fixed ) > C-database
	rm -f mks mks.fixed

doc-database: 
	awk -f extrnames.awk `append .n ${RootGenSrc}`\
 	   | sed -f fixmks.sed > doc-database


bigxref: C-database doc-database
	append -p ${LibDir}/ `(cd ${LibDir} ; make echorequired)` | \
		sed 's/\.l/.x/g' > lisplibfiles
	lxref doc-database  C-database  `cat lisplibfiles` > bigxref

# simple table of contents, just a listing of which function is
# documented in which chapter
tofc:  
	egrep "^.Lc|^.Lf|^.Lx|^.sh" `append .n  ${RootGenSrc}` > tofc


copysource:
	(tar cf - ${MacroSrc} `append .n ${RootGenSrc}` ${UtilSrc} | \
	 (cd ${CopyTo} ; tar xf -))

scriptcatall: ${AllSrc} append
	@(cd .. ; scriptcat doc doc ${MacroSrc} `doc/append .n ${RootGenSrc}` ${UtilSrc})
	@(X=`pwd` ; cd .. ; scriptcat ${LibDir}/manual lisplib/manual \
	      `$$X/append .r ${RootGenSrc}` helpindex)

copymanual: append
	(X=`pwd`; cd ${LibDir}/manual ; \
	cp `$$X/append .r ${RootGenSrc}` helpindex ${CopyTo})


