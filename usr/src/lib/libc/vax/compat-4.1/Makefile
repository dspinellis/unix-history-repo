#
# Copyright (c) 1980 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.5 (Berkeley) 9/6/85
#
SRCS=	reset.c
OBJS=	reset.o
CFLAGS= ${DEFS}
TAGSFILE=tags

.c.o:
	/lib/cpp -E ${CFLAGS} -DPROF $*.c | ${AS} -o $*.o
	-ld -X -r $*.o
	mv a.out profiled/$*.o
	/lib/cpp -E ${CFLAGS} $*.c | ${AS} -o $*.o
	-ld -x -r $*.o
	mv a.out $*.o

compat-4.1lib compat-4.1lib_p: ${OBJS}
	@echo "building profiled compat-4.1"
	@cd profiled; ar cru ../compat-4.1lib_p ${OBJS}
	@echo "building normal compat-4.1"
	@ar cru compat-4.1lib ${OBJS}

tags:

clean:
	rm -f *.o profiled/*.o errs a.out core \
	    compat-4.1lib compat-4.1lib_p tags

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

reset.o: reset.c ./DEFS.h
# DEPENDENCIES MUST END AT END OF FILE
# IF YOU PUT STUFF HERE IT WILL GO AWAY
# see make depend above
