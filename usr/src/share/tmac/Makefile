CFLAGS=	-O
LIBC=	/lib/libc.a
SRCS=	tmac.an tmac.an.new tmac.an6n tmac.an6t

all clean cleandir depend lint tags:

install:
	for i in ${SRCS}; do \
		(install -c -o bin -g bin -m 644 $$i \
		    ${DESTDIR}/usr/share/tmac/$$i); done

