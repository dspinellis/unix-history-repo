#
DESTDIR=
CFLAGS=	-O

# Programs that live in subdirectories, and have makefiles of their own.
#
SUBDIR=

# C programs that live in the current directory and do not need
# explicit make lines.
#
STD=

# C programs that live in the current directory and need explicit make lines.
#
NSTD=

# Shell scripts
#
SHELL=

all:	${SUBDIR} ${STD} ${NSTD}

#${SUBDIR}: /tmp
#	cd $@; make ${MFLAGS}
#
#${STD}:
#	cc ${CFLAGS} -o $@ $@.c
#
# Files listed in ${NSTD} have explicit make lines given below.

install:
	-for i in ${SUBDIR}; do \
		(cd $$i; make ${MFLAGS} DESTDIR=${DESTDIR} install); done
	-for i in ${STD} ${NSTD}; do \
		(install $$i ${DESTDIR}/usr/local/$$i); done
	-for i in ${SHELL}; do \
		(install -c -m 755 $$i.sh ${DESTDIR}/usr/local/$$i); done

clean:
	rm -f a.out core *.s *.o
	for i in ${SUBDIR}; do (cd $$i; make ${MFLAGS} clean); done
	rm -f ${STD} ${NSTD}

depend:
	cat </dev/null >x.c
	for i in ${STD} ${NSTD}; do \
		(echo $$i: $$i.c >>makedep; \
		/bin/grep '^#[ 	]*include' x.c $$i.c | sed \
			-e 's,<\(.*\)>,"/usr/include/\1",' \
			-e 's/:[^"]*"\([^"]*\)".*/: \1/' \
			-e 's/\.c//' >>makedep); done
	echo '/^# DO NOT DELETE THIS LINE/+2,$$d' >eddep
	echo '$$r makedep' >>eddep
	echo 'w' >>eddep
	cp Makefile Makefile.bak
	ed - Makefile < eddep
	rm eddep makedep x.c
	echo '# DEPENDENCIES MUST END AT END OF FILE' >> Makefile
	echo '# IF YOU PUT STUFF HERE IT WILL GO AWAY' >> Makefile
	echo '# see make depend above' >> Makefile

# DO NOT DELETE THIS LINE -- make depend uses it

# DEPENDENCIES MUST END AT END OF FILE
# IF YOU PUT STUFF HERE IT WILL GO AWAY
# see make depend above
