# @(#)Makefile	4.8 (Berkeley) 3/21/83

DESTDIR	=
CFLAGS=	-O

OBJS=	closedir.o opendir.o readdir.o seekdir.o telldir.o
DIST=	README Makefile dir.h closedir.c opendir.c readdir.c \
	seekdir.c telldir.c directory.3s

.c.o:
	${CC} ${CFLAGS} -I. -c $*.c
	-ld -x -r $*.o
	mv a.out $*.o

libndir: ${OBJS}
	ar cru libndir ${OBJS}
	ranlib libndir

install: libndir
	cp dir.h ${DESTDIR}/usr/include/dir.h
	cp libndir ${DESTDIR}/usr/lib/libndir.a
	ranlib ${DESTDIR}/usr/lib/libndir.a
	cp directory.3s /usr/man/man3/directory.3s

clean:
	rm -f libndir ${OBJS}

dist: ${DIST}
	echo "#!/bin/csh" >dist
	echo "# Run this file as shell script" >> dist
	echo "mkdir libndir" >> dist
	echo "chdir libndir" >> dist
	for i in ${DIST}; do ( \
		echo "echo extracting $$i" >> dist; \
		echo "cat > $$i <<'EOF'" >> dist; \
		cat $$i >> dist; \
		echo "'EOF'" >> dist); done
	chmod 775 dist
