#	Makefile	4.2	87/05/31
#
CFLAGS=-O
DESTDIR=

graph: graph.c
	${CC} $(CFLAGS) -o graph graph.c -lplot -lm

install: graph
	install -s graph ${DESTDIR}/usr/bin/graph

clean:
	rm -f *.o graph

depend:
