# If you don't want to plot, take out the -Dplot and the ref. to plot.a
PLOT = /usr/lib/libplot.a -Dplot
CFLAGS=-O
prof:	prof.o
	$(CC) -o prof prof.o $(PLOT)

install:
	install -s prof $(DESTDIR)/usr/bin

clean :
	rm -f *.o
