#	Makefile	1.3	87/05/30
#
DESTDIR=
CFLAGS=	-O
LDFLAGS=

c2:	c20.o c21.o c22.o c23.o
	$(CC) $(LDFLAGS) -o c2 c2?.o

c20.o:	c20.c c2.h
c21.o:	c21.c c2.h
c22.o:  c22.c c2.h
c23.o:  c23.c c2.h
	$(CC) $(CFLAGS) -R -c c23.c

c2.h:	tokens
	grep -n '^' tokens>toks
	( echo '/tokens/+1;/^$$/-1d'; \
	echo '/tokens/r toks'; \
	echo '/tokens/+1;/^$$/-1s/^\([0-9]*\):\([A-Z][A-Z0-9]*\)/#define \2	\1/'; \
	echo 'w'; \
	echo 'q'; \
	) | ed - c2.h
	rm toks

clean:
	rm -f *.o *.s 

install: c2
	install -s c2 ${DESTDIR}/lib/c2
