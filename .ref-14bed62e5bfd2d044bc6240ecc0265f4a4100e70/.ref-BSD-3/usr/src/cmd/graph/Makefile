CFLAGS=-O

graph: graph.c
	cc $(CFLAGS) -o graph graph.c /usr/lib/libplot.a -lm

install: graph
	install -s graph $(DESTDIR)/usr/bin
clean:
	rm -f *.o graph
