all:	libplot.a libt4014.a libt300.a libt300s.a libt450.a

cp:	all
	cp lib*.a /lib
libplot.a:	plot.c.a
	ar x plot.c.a
	cc -c -O *.c
	ar rc libplot.a *.o
	rm *.c *.o
libt4014.a:	t4014.c.a
	ar x t4014.c.a
	cc -c -O *.c
	ar rc libt4014.a *.o
	rm *.c *.o
libt300.a:	t300.c.a
	ar x t300.c.a
	cc -c -O *.c
	ar rc libt300.a *.o
	rm *.c *.o
libt300s.a:	t300s.c.a
	ar x t300s.c.a
	cc -c -O *.c
	ar rc libt300s.a *.o
	rm *.c *.o
libt450.a:	t450.c.a
	ar x t450.c.a
	cc -c -O *.c
	ar rc libt450.a *.o
	rm *.c *.o
