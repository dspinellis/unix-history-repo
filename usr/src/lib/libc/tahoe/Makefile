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
#	%W% (Berkeley) %G%
#
DEFS=	-DLIBC_SCCS -DSYSLIBC_SCCS
TAGSFILE=tags

# include this for floating point emulation code
#FPE=	fpe
ALL=	sys ${FPE}

all: ${ALL}
	for i in ${ALL}; do (cd $$i; make ${MFLAGS} DEFS=${DEFS}); done

clean:
	for i in ${ALL} fpe; do (cd $$i; make ${MFLAGS} clean); done

depend:
	for i in ${ALL}; do \
		(cd $$i; make ${MFLAGS} DEFS=${DEFS} depend); done

tags:
	for i in ${ALL}; do \
		(cd $$i; make ${MFLAGS} TAGSFILE=../${TAGSFILE} tags); \
	done

FRC:

