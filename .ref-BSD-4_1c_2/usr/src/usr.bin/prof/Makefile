# If you don't want to plot, take out the -Dplot and the ref. to plot.a
PLOT = -lplot -Dplot
CFLAGS=-O 
prof:	prof.o
	$(CC) -o prof prof.o $(PLOT)

prof.o: prof.c
	cc -c $(CFLAGS) $(PLOT) prof.c

install:
	install -s prof $(DESTDIR)/usr/bin

clean :
	rm -f *.o prof
