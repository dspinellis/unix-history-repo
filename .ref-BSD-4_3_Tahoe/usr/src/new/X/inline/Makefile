#	@(#)Makefile	1.3	(Berkeley)	84/08/20
#
DESTDIR=
CFLAGS=	-O
LFLAGS=	

# The program itself
#
PROG=	inline

# Sources
#
SRCS=	main.c machdep.c machpats.c libcpats.c langpats.c

# Objects
#
OBJS=	main.o machdep.o machpats.o libcpats.o langpats.o

# Header files
#
HDRS=	inline.h

all: ${PROG}

${PROG}: ${OBJS} ${HDRS}
	cc ${LFLAGS} -o ${PROG} ${OBJS}

${OBJS}:
	cc ${CFLAGS} -c $*.c

install: 

clean:
	rm -f a.out core ${OBJS} ${PROG} *.bak *.BAK *~ #*

print:
	@pr ${HDRS} ${SRCS}

lint:
	lint ${SRCS}

depend:
	cc -M ${SRCS} | \
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

# DO NOT DELETE THIS LINE -- make depend uses it


main.o: main.c /usr/include/stdio.h /usr/include/ctype.h ./inline.h
machdep.o: machdep.c /usr/include/stdio.h /usr/include/ctype.h
machpats.o: machpats.c ./inline.h
libcpats.o: libcpats.c ./inline.h
langpats.o: langpats.c ./inline.h
# DEPENDENCIES MUST END AT END OF FILE
# IF YOU PUT STUFF HERE IT WILL GO AWAY
# see make depend above
