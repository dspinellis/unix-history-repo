CFLAGS= -g

all: test-distrib etags ctags loadst make-docfile digest-doc \
   movemail cvtmail fakemail yow

test-distrib: test-distrib.c
	cc -o test-distrib test-distrib.c
	./test-distrib

etags: etags.c
	cc -o etags ${CFLAGS} -DETAGS etags.c

ctags: etags.c
	cc -o ctags ${CFLAGS} -DCTAGS etags.c

loadst: loadst.c
	cc -o loadst ${CFLAGS} loadst.c

make-docfile: make-docfile.c
	cc -o make-docfile ${CFLAGS} make-docfile.c

digest-doc: digest-doc.c
	cc -o digest-doc ${CFLAGS} digest-doc.c

movemail: movemail.c
	cc -o movemail ${CFLAGS} movemail.c

cvtmail: cvtmail.c
	cc -o cvtmail ${CFLAGS} cvtmail.c

fakemail: fakemail.c
	cc -o fakemail ${CFLAGS} fakemail.c

yow: yow.c
	cc -o yow ${CFLAGS} yow.c
