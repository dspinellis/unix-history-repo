.SUFFIXES: .3 .0

.3.0:
	/usr/man/manroff $*.3 > $*.0

OBJ3=	XMenu.0 Xlib.0 xtext.0 xtty.0

all:;

install: ${OBJ3}
	install -c -o bin -g bin -m 444 ${OBJ3} ${DESTDIR}/usr/new/man/cat3

clean:
	rm -f *~ \#*

paper:
	for i in *.? *.[0-9]?; do \
		echo -n "Troffing ";\
		echo -n $$i;\
		echo '....';\
		${TROFF} -man $$i;\
	done
