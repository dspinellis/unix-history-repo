all: plot.a t4014.a t300.a t300s.a t450.a

plot.a:	plot.c.a
	-rm -r xplot;mkdir xplot
	cd xplot;ar x ../plot.c.a
	cd xplot;cc -c -O *.c
	cd xplot;ar rc plot.a *.o
	mv xplot/plot.a .
	rm -r xplot
t4014.a:	t4014.c.a
	-rm -r xplot;mkdir xplot
	cd xplot;ar x ../t4014.c.a
	cd xplot;cc -c -O *.c
	cd xplot;ar rc t4014.a *.o
	mv xplot/t4014.a .
	rm -r xplot
t300.a:	t300.c.a
	-rm -r xplot;mkdir xplot
	cd xplot;ar x ../t300.c.a
	cd xplot;cc -c -O *.c
	cd xplot;ar rc t300.a *.o
	mv xplot/t300.a .
	rm -r xplot
t300s.a:	t300s.c.a
	-rm -r xplot;mkdir xplot
	cd xplot;ar x ../t300s.c.a
	cd xplot;cc -c -O *.c
	cd xplot;ar rc t300s.a *.o
	mv xplot/t300s.a .
	rm -r xplot
t450.a:	t450.c.a
	-rm -r xplot;mkdir xplot
	cd xplot;ar x ../t450.c.a
	cd xplot;cc -c -O *.c
	cd xplot;ar rc t450.a *.o
	mv xplot/t450.a .
	rm -r xplot

install: all
	install plot.a $(DESTDIR)/usr/lib/libplot.a
	ranlib $(DESTDIR)/usr/lib/libplot.a
	install t4014.a $(DESTDIR)/usr/lib/lib4014.a
	ranlib $(DESTDIR)/usr/lib/lib4014.a
	install t300.a $(DESTDIR)/usr/lib/lib300.a
	ranlib $(DESTDIR)/usr/lib/lib300.a
	install t300s.a $(DESTDIR)/usr/lib/lib300s.a
	ranlib $(DESTDIR)/usr/lib/lib300s.a
	install t450.a $(DESTDIR)/usr/lib/lib450.a
	ranlib $(DESTDIR)/usr/lib/lib450.a

clean:
	rm -f plot.a t4014.a t300.a t300s.a t450.a
	-rmdir xplot
