#
# Copyright (c) 1980 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.4 (Berkeley) 9/5/85
#
# printf & co are here because they are machine dependent
#
CFLAGS=	${DEFS} -I../gen
OBJS=	doprnt.o fputs.o fgets.o puts.o gets.o
SRCS=	doprnt.c fputs.c fgets.c puts.c gets.c
TAGSFILE=tags

.c.o:
	cc -E -DPROF ${CFLAGS} $*.c | as -o $*.o
	-ld -x -r $*.o
	mv a.out profiled/$*.o
	cc -E ${CFLAGS} $*.c | as -o $*.o
	-ld -x -r $*.o
	mv a.out $*.o

stdiolib stdiolib_p: ${OBJS}
	@echo "building profiled stdiolib"
	@cd profiled; ar cru ../stdiolib_p ${OBJS}
	@echo "building normal stdiolib"
	@ar cru stdiolib ${OBJS}

tags:

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

doprnt.o: doprnt.c ../gen/DEFS.h
fputs.o: fputs.c ../gen/DEFS.h
fgets.o: fgets.c ../gen/DEFS.h
puts.o: puts.c ../gen/DEFS.h
gets.o: gets.c ../gen/DEFS.h
# DEPENDENCIES MUST END AT END OF FILE
# IF YOU PUT STUFF HERE IT WILL GO AWAY
# see make depend above
