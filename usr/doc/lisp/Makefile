#  @(#)Makefile	34.3 7/9/81
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
#	   ...
#	   ch14.n stepper 
#	   ch15.n fixit package

TROFF=	/usr/ucb/vtroff
NROFF=  nroff

.SUFFIXES: .n .t .x .v .r .rx .q .qx

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
# make install will install the nroff versions of the manual in the
# directory (LMAN) where the auxfns0.l help command can find them.
#
LMAN = $(DESTDIR)/usr/lib/lisp/manual

SRC = lmacs ch0.n ch1.n ch2.n ch3.n ch4.n ch5.n ch6.n ch61.n ch7.n ch8.n \
      ch9.n ch10.n ch11.n ch12.n ch14.n ch15.n chb.n chc.n

INDX= ch2.x ch3.x ch4.x ch5.x ch6.x ch7.x

QINDX= ch2.qx ch3.qx ch4.qx ch5.qx ch6.qx ch7.qx

TGS= ch2.n ch3.n ch4.n ch5.n ch6.n ch61.n ch7.n

RXFILES= ch2.rx ch3.rx ch4.rx ch5.rx ch6.rx ch7.rx

RFILES= ch0.r ch1.r ch2.r ch3.r ch4.r ch5.r ch6.r ch7.r ch8.r ch9.r \
      ch10.r ch11.r ch12.r ch14.r ch15.r chb.r chc.r 

all: ${RFILES} helpindex

rall: ${RFILES}

vall: ch0.v ch1.v ch2.v ch3.v ch4.v ch5.v ch6.v ch7.v ch8.v ch9.v\
	 ch10.v ch11.v ch12.v ch14.v ch15.v chb.v chc.v index.v

tall: ch0.t ch1.t ch2.t ch3.t ch4.t ch5.t ch6.t ch7.t ch8.t ch9.t \
      ch10.t ch11.t ch12.t ch14.t ch15.t chb.t chc.t index.t

qall: ch0.q ch1.q ch2.q ch3.q ch4.q ch5.q ch6.q ch7.q ch8.q ch9.q \
      ch10.q ch11.q ch12.q ch14.q ch15.q chb.q chc.q 

# only a few files describe functions which are indexed.

.t.v:
	vpr -t $*.t

#.n.o:
#	tbl lmacs $*.n | ${TROFF} -me -t 1> $*.t 2> $*.x
#	vpr -t $*.t
	
.n.t:	
	tbl lmacs $*.n | ${TROFF} -me -t 1> $*.t 2> $*.x

.n.p:
	tbl lmacs $*.n | csh /usr/ucb/nettroff -me 2> $*.x

.n.x:
	tbl lmacs $*.n | ${TROFF} -me -z 2> $*.x

.n.r:
	tbl lmacs $*.n | ${NROFF} -me 1> $*.r 2> $*.rx

.n.rx:
	tbl lmacs $*.n | ${NROFF} -me 1> $*.r 2> $*.rx

.n.q:
	tbl lmacs $*.n | ${NROFF} -me -T37 2> $*.qx | col > $*.q

install: ${RFILES} helpindex
	cp ${RFILES} helpindex ${LMAN}

clean:
	rm -f *.r *.rx helpindex *.t *.q

findex: ${INDX}
	echo ".Ib" > index
	sort +3 -o index index ${INDX}
	sed -f indexsed index > indexx 
	${TROFF} -me lmacs indexx

index.t: ${INDX}
	echo ".Ib" > index
	sort +3 -o index index ${INDX}
	sed -f indexsed index > indexx
	${TROFF} -me -x -t lmacs indexx > index.t

pindex:
	echo ".Ib" > index
	sort +3 -o index index ${INDX}
	sed -f indexsed index > indexx
	csh /usr/ucb/nettroff -me -o5- lmacs indexx

index.q:
	echo ".Ib" > index
	cat index ${QINDX} | tr '\227' ' ' | sort +3 -o indexx
	nroff -me -T37 lmacs indexx | col > index.q

helpindex: ${RXFILES}
	cat ${RXFILES} | tr '\227' ' ' > helpindex

tags:	/dev/tty
	awk -f mantags ${TGS} | sort > tags

sources: ${SRC}
${SRC}:
	sccs get $@

clean:
	rm index *.x
