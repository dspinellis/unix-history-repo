#
#	@(#)Makefile	1.4	(CWI)	85/10/25
#
CFLAGS= -DINCORE -g
DESTDIR=
BINDIR=/usr/local
SUBDIR= utilities devhar eqn pic ideal grap tbl
TFILES=n1.o n2.o n3.o n4.o n5.o t6.o n7.o n8.o n9.o t10.o ni.o nii.o hytab.o suftab.o

all:	ditroff ${SUBDIR}

ditroff:	${TFILES}
	${CC} -o ditroff -n ${CFLAGS} ${TFILES} 

${SUBDIR}: /tmp
	cd $@; make ${MFLAGS}

n1.o:	tdef.h tw.h ext.h ni.o
n2.o:	tdef.h tw.h
n3.o:	tdef.h tw.h
n4.o:	tdef.h tw.h
n5.o:	tdef.h tw.h
n6.o:	tdef.h tw.h
t6.o:	tdef.h tw.h dev.h ni.o
n7.o:	tdef.h tw.h
n8.o:	tdef.h tw.h
n9.o:	tdef.h tw.h
n10.o:	tdef.h tw.h
t10.o:	tdef.h tw.h dev.h ni.o
ni.o:	tdef.h tw.h
nii.o:	tdef.h tw.h

install:	all
	install -s ditroff ${DESTDIR}${BINDIR}/ditroff
	install -c -m 644 ditroff.1 ${DESTDIR}/usr/man/manl/ditroff.1
	for i in ${SUBDIR}; do \
		(cd $$i; make ${MFLAGS} DESTDIR=${DESTDIR} install); done

clean:
	rm -f make.out *.o ditroff
	for i in ${SUBDIR}; do (cd $$i; make ${MFLAGS} clean); done
