# Lisp compiler `liszt' makefile. Opus 31g.
# source is in complra.l complrb.l complrc.l complrd.l
FILES= compmacs.l complra.l complrb.l complrc.l complrd.l
LISP=	/usr/ucb/lisp
LISZT=	/usr/ucb/liszt

liszt: complra.o complrb.o complrc.o complrd.o ${LISP}
	${LISP} < loadcomps.l
	@echo Compiler built

install:
	mv liszt ${DESTDIR}/usr/ucb/liszt

clean:
	rm -f *.s *.o *.fasl *.msg *.map liszt uctolc

.SUFFIXES: .l
.l.o : 	
	${LISZT} $<  < /dev/null

tags: /dev/tty uctolc
	awk -f ltags ${FILES} | uctolc | sort > tags

uctolc: uctolc.c
	cc -O uctolc.c -o uctolc
