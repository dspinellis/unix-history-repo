#
# Makefile for NNTP server support programs
#
# %W%	(Berkeley) %G%
#

OBJS	=	mkgrdates.o

SRCS	=	mkgrdates.c

HFILES	=

CFLAGS= -O

# Where these support programs will live

LIBDIR	= /usr/spool/news/lib

all:	mkgrdates

mkgrdates: mkgrdates.o
	cc ${CFLAGS} -o mkgrdates mkgrdates.o

install: mkgrdates
	cp mkgrdates ${LIBDIR}/mkgrdates
	chmod 755 ${LIBDIR}/mkgrdates

lint:
	lint mkgrdates.c

clean:
	rm -f *.o mkgrdates

tags:	${SRCS} ${HFILES}
	ctags ${SRCS} ${HFILES}

depend:
	cc -M ${CFLAGS} ${SRCS} | \
	awk ' { if ($$1 != prev) { print rec; rec = $$0; prev = $$1; } \
		else { if (length(rec $$2) > 78) { print rec; rec = $$0; } \
		       else rec = rec " " $$2 } } \
	      END { print rec } ' > makedep
	echo '/^# DO NOT DELETE THIS LINE/+2,$$d' >eddep
	echo '$$r makedep' >>eddep
	echo 'w' >>eddep
	cp Makefile Makefile.bak
	ed - Makefile < eddep
	rm eddep makedep
	echo '# DEPENDENCIES MUST END AT END OF FILE' >> Makefile
	echo '# IF YOU PUT STUFF HERE IT WILL GO AWAY' >> Makefile
	echo '# see make depend above' >> Makefile

# DO NOT DELETE THIS LINE
# DEPENDENCIES MUST END AT END OF FILE

mkgrdates.o: mkgrdates.c /usr/include/stdio.h /usr/include/sys/types.h
mkgrdates.o: /usr/include/sys/time.h /usr/include/time.h
mkgrdates.o: /usr/include/sys/stat.h ./../common/conf.h
# DEPENDENCIES MUST END AT END OF FILE
# IF YOU PUT STUFF HERE IT WILL GO AWAY
# see make depend above
