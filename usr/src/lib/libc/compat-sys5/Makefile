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
#	@(#)Makefile	5.10.1.1 (Berkeley) 6/9/88
#
# Many of these routines have been rewritten in assembly.  The assembly
# source can be found in the directory stdio.${MACHINE}.  The macro
# ${STDSRC} lists the files needed by all current implementations.  The
# macros ${VAXSRC} and ${TAHOESRC} are the supplemental files needed by
# the VAX and the TAHOE.  ${PORTSRC} is a list of all of the supplemental
# files.
DEFS=	-DLIBC_SCCS
CFLAGS=	-O ${DEFS}
STDSRC=	getopt.c memccpy.c memchr.c memcmp.c memcpy.c memset.c strcspn.c \
	strpbrk.c strspn.c strtok.c tmpnam.c strcatn.c strcmpn.c strcpyn.c
STDOBJ=	getopt.o memccpy.o memchr.o memcmp.o memcpy.o memset.o strcspn.o \
	strpbrk.o strspn.o strtok.o tmpnam.o strcatn.o strcmpn.o strcpyn.o
VAXSRC=
VAXOBJ=
TAHOESRC=	strchr.c strrchr.c
TAHOEOBJ=	strchr.o strrchr.o
PORTSRC=	strchr.c strrchr.c
PORTOBJ=	strchr.o strrchr.o
TAGSFILE=tags

.c.o:
	@${CC} -p ${CFLAGS} -c $*.c
	@-ld -X -o profiled/$*.o -r $*.o
	${CC} ${CFLAGS} -c $*.c
	@-ld -x -r $*.o
	@mv a.out $*.o

all: lib.${MACHINE}

link: lib.${MACHINE} link.${MACHINE}

link.vax:
	(cd ../library; rm -f ${STDOBJ} ${VAXOBJ})
	(cd ../profiled; rm -f ${STDOBJ} ${VAXOBJ})
	ln ${STDOBJ} ${VAXOBJ} ../library
	(cd profiled; ln ${STDOBJ} ${VAXOBJ} ../../profiled)

link.tahoe:
	(cd ../library; rm -f ${STDOBJ} ${TAHOEOBJ})
	(cd ../profiled; rm -f ${STDOBJ} ${TAHOEOBJ})
	ln ${STDOBJ} ${TAHOEOBJ} ../library
	(cd profiled; ln ${STDOBJ} ${TAHOEOBJ} ../../profiled)

lib.vax: ${STDOBJ} ${VAXOBJ}

lib.tahoe: ${STDOBJ} ${TAHOEOBJ}

tags: tags.${MACHINE}

tags.vax:
	cwd=`pwd`; \
	for i in ${STDSRC} ${VAXSRC}; do \
		ctags -a -f ${TAGSFILE} $$cwd/$$i; \
	done

tags.tahoe:
	cwd=`pwd`; \
	for i in ${STDSRC} ${TAHOESRC}; do \
		ctags -a -f ${TAGSFILE} $$cwd/$$i; \
	done

clean:
	rm -f *.o profiled/* a.out core ${TAGSFILE}

depend: depend.${MACHINE}

depend.vax: ${STDSRC} ${VAXSRC}
	mkdep ${CFLAGS} ${STDSRC} ${VAXSRC}

depend.tahoe: ${STDSRC} ${TAHOESRC}
	mkdep ${CFLAGS} ${STDSRC} ${TAHOESRC}

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

getopt.o: getopt.c /usr/include/stdio.h
memccpy.o: memccpy.c
memchr.o: memchr.c
memcmp.o: memcmp.c
memcpy.o: memcpy.c
memset.o: memset.c
strcspn.o: strcspn.c
strpbrk.o: strpbrk.c
strspn.o: strspn.c
strtok.o: strtok.c
tmpnam.o: tmpnam.c /usr/include/sys/param.h /usr/include/sys/types.h
tmpnam.o: /usr/include/signal.h /usr/include/machine/trap.h
tmpnam.o: /usr/include/machine/machparam.h /usr/include/machine/endian.h
tmpnam.o: /usr/include/sys/stat.h /usr/include/sys/file.h /usr/include/stdio.h
strcatn.o: strcatn.c
strcmpn.o: strcmpn.c
strcpyn.o: strcpyn.c
strchr.o: strchr.c /usr/include/stdio.h
strrchr.o: strrchr.c /usr/include/stdio.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
