#
# Copyright (c) 1988 Regents of the University of California.
# All rights reserved.
#
# Redistribution and use in source and binary forms are permitted
# provided that this notice is preserved and that due credit is given
# to the University of California at Berkeley. The name of the University
# may not be used to endorse or promote products derived from this
# software without specific prior written permission. This software
# is provided ``as is'' without express or implied warranty.
#
#	@(#)Makefile	5.7 (Berkeley) %G%
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
	@/lib/cpp -E ${FLAGS} -DPROF $*.s | ${AS} -o $*.o
	@ld -X -o profiled/$*.o -r $*.o
	/lib/cpp -E ${FLAGS} $*.s | ${AS} -o $*.o
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

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

addd.o: addd.s /usr/include/tahoemath/fp.h ../DEFS.h
addf.o: addf.s /usr/include/tahoemath//fp.h ../DEFS.h
cmpd.o: cmpd.s ../DEFS.h
cmpf.o: cmpf.s ../DEFS.h
cvtdl.o: cvtdl.s /usr/include/tahoemath/fp.h ../DEFS.h
cvtfl.o: cvtfl.s /usr/include/tahoemath/fp.h ../DEFS.h
cvtld.o: cvtld.s /usr/include/tahoemath/fp.h ../DEFS.h
cvtlf.o: cvtlf.s /usr/include/tahoemath/fp.h ../DEFS.h
cvtud.o: cvtud.s /usr/include/tahoemath/fp.h ../DEFS.h
cvtuf.o: cvtuf.s /usr/include/tahoemath/fp.h ../DEFS.h
divd.o: divd.s /usr/include/tahoemath/fp.h ../DEFS.h
divf.o: divf.s /usr/include/tahoemath/fp.h ../DEFS.h
fnorm.o: fnorm.s /usr/include/tahoemath/fp.h ../DEFS.h
fp_exp.o: fp_exp.s /usr/include/tahoemath/fp.h ../DEFS.h
modf.o: modf.s /usr/include/tahoemath/fp.h ../DEFS.h
muld.o: muld.s /usr/include/tahoemath/fp.h ../DEFS.h
mulf.o: mulf.s /usr/include/tahoemath/fp.h ../DEFS.h
negd.o: negd.s /usr/include/tahoemath/fp.h ../DEFS.h
negf.o: negf.s /usr/include/tahoemath/fp.h ../DEFS.h
sfnorm.o: sfnorm.s /usr/include/tahoemath/fp.h ../DEFS.h
sfp_exp.o: sfp_exp.s /usr/include/tahoemath/fp.h ../DEFS.h
subd.o: subd.s /usr/include/tahoemath/fp.h ../DEFS.h
subf.o: subf.s /usr/include/tahoemath/fp.h ../DEFS.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
