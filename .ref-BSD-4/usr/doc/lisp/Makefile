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

TROFF=	csh /usr/ucb/vtroff

.SUFFIXES: .n .t .x .o

INDX= ch2.x ch3.x ch4.x ch5.x ch6.x ch7.x

TGS= ch2.n ch3.n ch4.n ch5.n ch6.n ch61.n ch7.n

all: ch0.o ch1.o ch2.o ch3.o ch4.o ch5.o ch6.o ch7.o ch8.o ch9.o\
	 ch10.o ch11.o ch12.o chb.o chc.o index.o

tall: ch0.t ch1.t ch2.t ch3.t ch4.t ch5.t ch6.t ch7.t ch8.t ch9.t \
      ch10.t ch11.t ch12.t chb.t chc.t index.t



.t.o:
	vpr -t $*.t

#.n.o:
#	tbl lmacs $*.n | ${TROFF} -me -t 1> $*.t 2> $*.x
#	vpr -t $*.t
	
.n.t:	
	tbl lmacs $*.n | ${TROFF} -me -x -t 1> $*.t 2> $*.x

.n.p:
	tbl lmacs $*.n | csh /usr/ucb/nettroff -me 2> $*.x

.n.x:
	tbl lmacs $*.n | ${TROFF} -me -z 2> $*.x



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

tags:	/dev/tty
	awk -f mantags ${TGS} | sort > tags



clean:
	rm index *.x
