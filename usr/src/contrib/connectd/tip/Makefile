CFLAGS=	-I../include -DDEFBR=1200 -DDEFFS=BUFSIZ -DPRISTINE
LDFLAGS=
OBJS=	cmds.o cmdtab.o partab.o tip.o tipout.o value.o vars.o
SOURCES=cmds.c cmdtab.c partab.c tip.c tipout.c value.c vars.c
CXREF=	/usr/ucb/ctags -xw

tip:	${OBJS}
	${CC} -o tip ${LDFLAGS} ${OBJS}  ../libc/libc.a

${OBJS}: tip.h


install: tip
	install -s -m 4711 -o ${OWNER} -g ${GROUP} tip ${DESTDIR}/usr/bin/tip

tags:	${SOURCES} tip.h
	ctags -w ${SOURCES} tip.h

clean:
	rm -f tip *.o

print:
	@pr README TODO makefile
	@size *.o | pr -h SIZES
	@${CXREF} tip.h ${SOURCES} | pr -h XREF
	@ls -l | pr 
	@pr tip.h ${SOURCES}

lint:
	-lint -hbacvx ${CFLAGS} ${SOURCES}
