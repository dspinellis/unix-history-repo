#
# Copyright (c) 1980 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.4 (Berkeley) 9/5/85
#
# Several routines have been rewritten in assembly language for the VAX.
# If you are not running on a VAX, you should use the original C sources
# which are included in this directory. Regretably we do not have a C
# version of doprnt().
#
#ifndef vax
#SRCS=	${STDSRC} ${VAXSRC}
#OBJS=	${STDOBJ} ${VAXOBJ}
#else
SRCS=	${STDSRC}
OBJS=	${STDOBJ}
#endif not vax

CFLAGS=	-O ${DEFS}

STDSRC=	clrerr.c doscan.c \
	exit.c fdopen.c fgetc.c filbuf.c findiop.c flsbuf.c fopen.c \
	fprintf.c fputc.c fread.c freopen.c fseek.c ftell.c \
	fwrite.c getchar.c getw.c printf.c putchar.c \
	putw.c rew.c scanf.c setbuf.c setbuffer.c sibuf.c sobuf.c \
	sprintf.c strout.c ungetc.c
VAXSRC=	fgets.c fputs.c gets.c puts.c
STDOBJ=	clrerr.o doscan.o \
	exit.o fdopen.o fgetc.o filbuf.o findiop.o flsbuf.o fopen.o \
	fprintf.o fputc.o fread.o freopen.o fseek.o ftell.o \
	fwrite.o getchar.o getw.o printf.o putchar.o \
	putw.o rew.o scanf.o setbuf.o setbuffer.o sibuf.o sobuf.o \
	sprintf.o strout.o ungetc.o
VAXOBJ=	fgets.o fputs.o gets.o puts.o
TAGSFILE=tags

.c.o:
	${CC} -p ${CFLAGS} -c $*.c
	-ld -X -r $*.o
	mv a.out profiled/$*.o
	${CC} ${CFLAGS} -c $*.c
	-ld -x -r $*.o
	mv a.out $*.o

stdiolib stdiolib_p: ${OBJS}
	@echo "building profiled stdiolib"
	@cd profiled; ar cru ../stdiolib_p ${OBJS}
	@echo "building normal stdiolib"
	@ar cru stdiolib ${OBJS}

tags:
	cwd=`pwd`; \
	for i in ${SRCS}; do \
		ctags -a -f ${TAGSFILE} $$cwd/$$i; \
	done

clean:
	rm -f stdiolib stdiolib_p *.o profiled/*.o tags Makefile.bak

depend:
	for i in ${SRCS}; do \
	    cc -M ${CFLAGS} $$i | awk ' { if ($$1 != prev) \
		{ if (rec != "") print rec; rec = $$0; prev = $$1; } \
		else { if (length(rec $$2) > 78) { print rec; rec = $$0; } \
		else rec = rec " " $$2 } } \
		END { print rec } ' >> makedep; done
	echo '/^# DO NOT DELETE THIS LINE/+2,$$d' >eddep
	echo '$$r makedep' >>eddep
	echo 'w' >>eddep
	cp Makefile Makefile.bak
	ed - Makefile < eddep
	rm eddep makedep
	echo '# DEPENDENCIES MUST END AT END OF FILE' >> Makefile
	echo '# IF YOU PUT STUFF HERE IT WILL GO AWAY' >> Makefile
	echo '# see make depend above' >> Makefile

# DO NOT DELETE THIS LINE -- make depend uses it

clrerr.o: clrerr.c /usr/include/stdio.h
doscan.o: doscan.c /usr/include/stdio.h /usr/include/ctype.h
exit.o: exit.c
fdopen.o: fdopen.c /usr/include/sys/types.h /usr/include/sys/file.h
fdopen.o: /usr/include/stdio.h
fgetc.o: fgetc.c /usr/include/stdio.h
filbuf.o: filbuf.c /usr/include/stdio.h /usr/include/sys/types.h
filbuf.o: /usr/include/sys/stat.h
findiop.o: findiop.c /usr/include/stdio.h
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
rew.o: rew.c /usr/include/stdio.h
scanf.o: scanf.c /usr/include/stdio.h
setbuf.o: setbuf.c /usr/include/stdio.h
setbuffer.o: setbuffer.c /usr/include/stdio.h
sibuf.o: sibuf.c /usr/include/stdio.h
sobuf.o: sobuf.c /usr/include/stdio.h
sprintf.o: sprintf.c /usr/include/stdio.h
strout.o: strout.c /usr/include/stdio.h
ungetc.o: ungetc.c /usr/include/stdio.h
# DEPENDENCIES MUST END AT END OF FILE
# IF YOU PUT STUFF HERE IT WILL GO AWAY
# see make depend above
