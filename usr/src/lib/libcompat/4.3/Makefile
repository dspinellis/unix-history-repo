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
#	@(#)Makefile	5.1 (Berkeley) %G%
#
CFLAGS=	-O ${DEFS}
SRCS=
OBJS=
TAGSFILE=tags

.c.o:
	${CC} -p ${CFLAGS} -c $*.c
	-ld -X -r $*.o
	mv a.out profiled/$*.o
	${CC} ${CFLAGS} -c $*.c
	-ld -x -r $*.o
	mv a.out $*.o

compat-4.3lib compat-4.3lib_p: ${OBJS}
	@echo "building profiled compat-4.3lib"
	@cd profiled; ar cru ../compat-4.3lib_p ${OBJS}
	@echo "building normal compat-4.3lib"
	@ar cru compat-4.3lib ${OBJS}

tags:
	cwd=`pwd`; \
	for i in ${SRCS}; do \
		ctags -a -f ${TAGSFILE} $$cwd/$$i; \
	done

clean:
	rm -f compat-4.3lib compat-4.3lib_p *.o profiled/*.o tags

depend:
	mkdep ${CFLAGS} ${SRCS}

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

