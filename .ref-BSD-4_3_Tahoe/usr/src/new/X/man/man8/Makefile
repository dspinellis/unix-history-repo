.SUFFIXES: .8 .0

.8.0:
	/usr/man/manroff $*.8 > $*.0

OBJ8=	X.0 X.0 Xapollo.0 Xis.0 Xnest.0 Xqvss.0 Xsun.0 Xvs100.0

all:;

install: ${OBJ8}
	install -c -o bin -g bin -m 444 ${OBJ8} ${DESTDIR}/usr/new/man/cat8

clean:
	rm -f *~ \#*

paper:
	for i in *.? *.[0-9]?; do \
		echo -n "Troffing ";\
		echo -n $$i;\
		echo '....';\
		${TROFF} -man $$i;\
	done
