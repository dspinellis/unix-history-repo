#
# Copyright (c) 1988 Regents of the University of California.
# All rights reserved.
#
# Redistribution and use in source and binary forms are permitted
# provided that the above copyright notice and this paragraph are
# duplicated in all such forms and that any documentation,
# advertising materials, and other materials related to such
# distribution and use acknowledge that the software was developed
# by the University of California, Berkeley.  The name of the
# University may not be used to endorse or promote products derived
# from this software without specific prior written permission.
# THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
# IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
# WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
#
#	@(#)Makefile	5.10 (Berkeley) %G%
#
DEFS=	-DLIBC_SCCS
FLAGS=	-I.. ${DEFS}
CFLAGS=	-O ${FLAGS}
SRCS=	addd.s addf.s cmpd.s cmpf.s cvtdl.s cvtfl.s cvtld.s cvtlf.s cvtud.s \
	cvtuf.s divd.s divf.s fnorm.s fp_exp.s modf.s muld.s mulf.s negd.s \
	negf.s sfnorm.s sfp_exp.s subd.s subf.s
OBJS=	addd.o addf.o cmpd.o cmpf.o cvtdl.o cvtfl.o cvtld.o cvtlf.o cvtud.o \
	cvtuf.o divd.o divf.o fnorm.o fp_exp.o modf.o muld.o mulf.o negd.o \
	negf.o sfnorm.o sfp_exp.o subd.o subf.o
TAGSFILE=tags

.s.o:
	@${CPP} -E ${FLAGS} -DPROF $*.s | ${AS} -o $*.o
	@ld -X -o profiled/$*.o -r $*.o
	${CPP} -E ${FLAGS} $*.s | ${AS} -o $*.o
	@ld -x -r $*.o
	@mv a.out $*.o

all: ${OBJS}

link: ${OBJS}
	(cd ../../library; rm -f ${OBJS})
	(cd ../../profiled; rm -f ${OBJS})
	ln ${OBJS} ../../library
	(cd profiled; ln ${OBJS} ../../../profiled)

tags:
	echo "no tags here"

clean:
	rm -f *.o profiled/* a.out core ${TAGSFILE}

depend:
	mkdep ${CFLAGS} ${SRCS}
