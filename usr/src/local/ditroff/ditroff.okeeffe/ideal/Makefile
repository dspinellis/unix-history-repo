#
#	Makefile	(CWI)	1.1	85/03/01
#
# BINDIR is the place where ideal will be installed
BINDIR = /usr/local
# DESTDIR is the destination where we want to move the thing (temporarily)
DESTDIR =
# LIBDIR is the place where we expect the the things we need and is
# is therefore hardwired in. Since it is all local, we use BINDIR as well
LIBDIR = ${BINDIR}/lib/ideal/lib/
CFLAGS = -O -DLIBDIR=\"${LIBDIR}\"

SOURCES = ideal.h ideal.c util.c memut.c bldds.c simul.c exprn.c\
	action.c piece.c opaque.c inter.c opqpoly.c idlex.l idyac.y
OBJECTS = y.tab.o lex.yy.o ideal.o util.o memut.o bldds.o simul.o\
	exprn.o action.o piece.o opaque.o opqpoly.o inter.o
ADMIXTURE = y.tab.c lex.yy.c ideal.c util.c memut.c bldds.c simul.c\
	exprn.c action.c piece.c opaque.c opqpoly.c inter.c

SUBDIR = idfilt lib

all: ideal ideal.sh ${SUBDIR}

${SUBDIR}: /tmp
	cd $@; make ${MFLAGS} DESTDIR=${DESTDIR}

# ideal is a shell file. It needs to know where it can find the real binary
ideal.sh: ideal.cmd
	sed "s#IDDIR=.*#IDDIR=${LIBDIR}#" < ideal.cmd > ideal.sh

ideal:	$(OBJECTS)
	cc -o ideal $(OBJECTS) -ll -lm

$(OBJECTS):	ideal.h

ideal.h:	stdas.h

lex.yy.c:	idlex.l
	lex idlex.l

y.tab.c:	idyac.y
	yacc -d idyac.y

install: all $(LIBDIR)
	install -s ideal ${DESTDIR}/${LIBDIR}/ideal
	install -m 755 ideal.sh ${DESTDIR}/${BINDIR}/ideal
	install -c -m 644 ideal.1 ${DESTDIR}/usr/man/manl/ideal.1
	cd idfilt; make DESTDIR=${DESTDIR} install
	cd lib; make DESTDIR=${DESTDIR} install

$(LIBDIR):
	mkdir ${BINDIR}/lib/ideal
	mkdir${BINDIR}/lib/ideal/lib
list:
	pr $(SOURCES)

lint:
	lint $(ADMIXTURE) -lm

clean:
	rm -f *.o y.tab.c lex.yy.c y.tab.h
	for i in ${SUBDIR}; do (cd $$i; make ${MFLAGS} clean); done

