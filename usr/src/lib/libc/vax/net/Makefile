#
# Copyright (c) 1983 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.4 (Berkeley) 9/5/85
#
SRCS=	htonl.c htons.c ntohl.c ntohs.c
OBJS=	htonl.o htons.o ntohl.o ntohs.o
CFLAGS=	${DEFS}
TAGSFILE=tags

.c.o:
	/lib/cpp -E ${CFLAGS} -DPROF $*.c | ${AS} -o $*.o
	-ld -x -r $*.o
	mv a.out profiled/$*.o
	/lib/cpp -E ${CFLAGS} $*.c | ${AS} -o $*.o
	-ld -x -r $*.o
	mv a.out $*.o

netlib netlib_p: ${OBJS}
	@echo "building profiled netlib"
	@cd profiled; ar cru ../netlib_p ${OBJS}
	@echo "building normal netlib"
	@ar cru netlib ${OBJS}

tags:
#	cwd=`pwd`; \
#	for i in ${SRCS}; do \
#		ctags -a -f ${TAGSFILE} $$cwd/$$i; \
#	done

clean:
	rm -f *.o profiled/*.o errs a.out core netlib netlib_p \
		tags Makefile.bak

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

htonl.o: htonl.c ./DEFS.h
htons.o: htons.c ./DEFS.h
ntohl.o: ntohl.c ./DEFS.h
ntohs.o: ntohs.c ./DEFS.h
# DEPENDENCIES MUST END AT END OF FILE
# IF YOU PUT STUFF HERE IT WILL GO AWAY
# see make depend above
