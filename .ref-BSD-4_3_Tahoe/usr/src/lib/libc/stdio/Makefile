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
#	@(#)Makefile	5.9 (Berkeley) 6/27/88
#
# Many of these routines have been rewritten in assembly.  The assembly
# source can be found in the directory stdio.${MACHINE}.  The macro
# ${STDSRC} lists the files needed by all current implementations.  The
# macros ${VAXSRC} and ${TAHOESRC} are the supplemental files needed by
# the VAX and the TAHOE.  ${PORTSRC} is a list of all of the supplemental
# files.

DEFS=	-DLIBC_SCCS
CFLAGS=	-O ${DEFS}
STDSRC=	clrerr.c doprnt.c doscan.c exit.c fdopen.c fgetc.c filbuf.c \
	findiop.c flsbuf.c fopen.c fprintf.c fputc.c fread.c freopen.c \
	fseek.c ftell.c fwrite.c getchar.c getw.c printf.c putchar.c \
	putw.c rew.c scanf.c setbuf.c setbuffer.c sprintf.c ungetc.c \
	vfprintf.c vprintf.c vsprintf.c
STDOBJ=	clrerr.o doprnt.o doscan.o exit.o fdopen.o fgetc.o filbuf.o \
	findiop.o flsbuf.o fopen.o fprintf.o fputc.o fread.o freopen.o \
	fseek.o ftell.o fwrite.o getchar.o getw.o printf.o putchar.o \
	putw.o rew.o scanf.o setbuf.o setbuffer.o sprintf.o ungetc.o \
	vfprintf.o vprintf.o vsprintf.o
VAXSRC=
VAXOBJ=
TAHOESRC=	fgets.c fputs.c gets.c puts.c
TAHOEOBJ=	fgets.o fputs.o gets.o puts.o
PORTSRC=	fgets.c fputs.c gets.c puts.c
PORTOBJ=	fgets.o fputs.o gets.o puts.o
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
	(cd ../library; rm -f  ${STDOBJ} ${VAXOBJ})
	(cd ../profiled; rm -f  ${STDOBJ} ${VAXOBJ})
	ln  ${STDOBJ} ${VAXOBJ} ../library
	(cd profiled; ln  ${STDOBJ} ${VAXOBJ} ../../profiled)

link.tahoe:
	(cd ../library; rm -f  ${STDOBJ} ${TAHOEOBJ})
	(cd ../profiled; rm -f  ${STDOBJ} ${TAHOEOBJ})
	ln  ${STDOBJ} ${TAHOEOBJ} ../library
	(cd profiled; ln  ${STDOBJ} ${TAHOEOBJ} ../../profiled)


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

clrerr.o: clrerr.c /usr/include/stdio.h
doprnt.o: doprnt.c /usr/include/sys/types.h /usr/include/varargs.h
doprnt.o: /usr/include/stdio.h /usr/include/ctype.h
doscan.o: doscan.c /usr/include/stdio.h /usr/include/ctype.h
exit.o: exit.c
fdopen.o: fdopen.c /usr/include/sys/types.h /usr/include/sys/file.h
fdopen.o: /usr/include/stdio.h
fgetc.o: fgetc.c /usr/include/stdio.h
filbuf.o: filbuf.c /usr/include/stdio.h /usr/include/sys/types.h
filbuf.o: /usr/include/sys/stat.h
findiop.o: findiop.c /usr/include/stdio.h /usr/include/errno.h
flsbuf.o: flsbuf.c /usr/include/stdio.h /usr/include/sys/types.h
flsbuf.o: /usr/include/sys/stat.h
fopen.o: fopen.c /usr/include/sys/types.h /usr/include/sys/file.h
fopen.o: /usr/include/stdio.h
fprintf.o: fprintf.c /usr/include/stdio.h
fputc.o: fputc.c /usr/include/stdio.h
fread.o: fread.c /usr/include/stdio.h
freopen.o: freopen.c /usr/include/sys/types.h /usr/include/sys/file.h
freopen.o: /usr/include/stdio.h
fseek.o: fseek.c /usr/include/stdio.h
ftell.o: ftell.c /usr/include/stdio.h
fwrite.o: fwrite.c /usr/include/stdio.h
getchar.o: getchar.c /usr/include/stdio.h
getw.o: getw.c /usr/include/stdio.h
printf.o: printf.c /usr/include/stdio.h
putchar.o: putchar.c /usr/include/stdio.h
putw.o: putw.c /usr/include/stdio.h
rew.o: rew.c /usr/include/sys/types.h /usr/include/sys/file.h
rew.o: /usr/include/stdio.h
scanf.o: scanf.c /usr/include/stdio.h
setbuf.o: setbuf.c /usr/include/stdio.h
setbuffer.o: setbuffer.c /usr/include/stdio.h
sprintf.o: sprintf.c /usr/include/stdio.h
ungetc.o: ungetc.c /usr/include/stdio.h
vfprintf.o: vfprintf.c /usr/include/stdio.h /usr/include/varargs.h
vprintf.o: vprintf.c /usr/include/stdio.h /usr/include/varargs.h
vsprintf.o: vsprintf.c /usr/include/stdio.h /usr/include/varargs.h
fgets.o: fgets.c /usr/include/stdio.h
fputs.o: fputs.c /usr/include/stdio.h
gets.o: gets.c /usr/include/stdio.h
puts.o: puts.c /usr/include/stdio.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
