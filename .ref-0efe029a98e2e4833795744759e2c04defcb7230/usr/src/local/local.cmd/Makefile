CFLAGS=	-O
LIBC=	/lib/libc.a

# Programs that live in subdirectories, and have makefiles of their own.
#
SUBDIR=	cmd.${MACHINE}

# Shell scripts that need only be installed and are never removed.
#
SCRIPT= Correct Spell difftc itroff

# C sources that live in the current directory
#
SRCS=	20b.c 2621.c chgbars.c cpr.c dis.c dl.c \
	doorcard.c la.c rinfo.c sdiff.c showtc.c yank.c

# C programs that live in the current directory and do not need
# explicit make lines.
#
STD=	20b 2621 chgbars cpr dl doorcard rinfo sdiff yank

# C programs that live in the current directory and need explicit make lines.
#
NSTD=	dis showtc

# Programs that get installed with special permissions
#
PERM=	la

all:	${SUBDIR} ${STD} ${PERM} ${NSTD}

${SUBDIR}: FRC
	cd $@; make ${MFLAGS}

${STD} ${PERM}: ${LIBC}
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
		(install -c -o bin -g bin -m 755 $$i.sh ${DESTDIR}/usr/local/$$i); \
	done
	install -s -o bin -g bin -m 755 ${STD} ${NSTD} ${DESTDIR}/usr/local
# programs that run with special permissions
	install -s -o bin -g kmem -m 2755 la ${DESTDIR}/usr/local/la

FRC:

# Files listed in ${NSTD} and ${OPTIONAL} have explicit make lines given below.

showtc dis:
	${CC} ${CFLAGS} -o $@ $@.c -ltermcap

