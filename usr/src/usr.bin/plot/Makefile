#
#	Makefile	4.2	83/03/24
#
all: tek t300 t300s t450 vplot

tek:	driver.o
	cc -o tek  driver.o -l4014 -lm
t300:	driver.o 
	cc -o t300 driver.o -l300 -lm
t300s:	driver.o 
	cc -o t300s driver.o -l300s -lm
t450:	driver.o 
	cc -o t450 driver.o -l450 -lm
vplot:	vplot.c
	cc -o vplot vplot.c

install: all
	install -s tek ${DESTDIR}/usr/bin
	install -s t300 ${DESTDIR}/usr/bin
	install -s t300s ${DESTDIR}/usr/bin
	install -s t450 ${DESTDIR}/usr/bin
	install -s vplot ${DESTDIR}/usr/bin
	install -c plot.sh ${DESTDIR}/usr/bin/plot
clean:
	rm -f *.o tek t300 t300s t450 vplot
