#	@(#)Makefile	1.2 (Berkeley) %G%

CFLAGS=	-O
LIBC=	/usr/lib/libc.a

# Programs that live in subdirectories, and have makefiles of their own.
#
SUBDIR=	cmd.${MACHINE}

# Shell scripts that need only be installed and are never removed.
#
SCRIPT= Correct Spell difftc itroff

# C sources that live in the current directory
#
SRCS=	20b.c chgbars.c doorcard.c la.c rinfo.c sdiff.c showtc.c

# C programs that live in the current directory and do not need
# explicit make lines.
#
STD=	20b chgbars doorcard rinfo sdiff

# C programs that live in the current directory and need explicit make lines.
#
NSTD=	showtc

# Programs that get installed with special permissions
#
PERM=	la

all:	${SUBDIR} ${STD} ${PERM} ${NSTD}

${SUBDIR}: FRC
	cd $@; make ${MFLAGS}

${STD}: ${LIBC}
	${CC} ${CFLAGS} -o $@ $@.c

clean cleandir: FRC
	-for i in ${SUBDIR} cmd.vax cmd.tahoe; do \
		(cd $$i; make ${MFLAGS} clean); \
	done
	rm -f a.out core *.s *.o ${STD} ${PERM} ${NSTD}

depend: ldepend
	for i in ${SUBDIR}; do (cd $$i; make ${MFLAGS} depend); done

ldepend: FRC
	mkdep -p ${CFLAGS} ${SRCS}

install: FRC
	-for i in ${SUBDIR}; do \
		(cd $$i; make ${MFLAGS} DESTDIR=${DESTDIR} install); done
	-for i in ${SCRIPT}; do \
		(install -c -o bin -g bin -m 755 $$i.sh ${DESTDIR}/usr/local/bin/$$i); \
	done
	install -s -o bin -g bin -m 755 ${STD} ${NSTD} ${DESTDIR}/usr/local/bin
# programs that run with special permissions
	install -s -o bin -g kmem -m 2755 la ${DESTDIR}/usr/local/bin/la

FRC:

# Files listed in ${NSTD} and ${OPTIONAL} have explicit make lines given below.

showtc dis:
	${CC} ${CFLAGS} -o $@ $@.c -ltermcap

la:
	${CC} ${CFLAGS} -o $@ $@.c -lutil
