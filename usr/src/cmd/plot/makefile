CFLAGS = -n -O -s

all:	tek t300 t300s t450 vplot
	:

cp:	all
	cp tek t300 t300s t450 vplot /bin
	rm tek t300 t300s t450 vplot driver.o

cmp:	all
	cmp tek /bin/tek
	rm tek
	cmp t300 /bin/t300
	rm t300
	cmp t300s /bin/t300s
	rm t300s
	cmp t450 /bin/t450
	rm t450
	cmp vplot /bin/vplot
	rm vplot
	rm -f driver.o

tek:	driver.o
	cc -n -s -o tek  driver.o -lt4014 -lm
t300:	driver.o 
	cc -n -s -o t300 driver.o -lt300 -lm
t300s:	driver.o 
	cc -n -s -o t300s driver.o -lt300s -lm
t450:	driver.o 
	cc -n -s -o t450 driver.o -lt450 -lm

vplot:	chrtab.o vplot.o
	cc -n -s -o vplot vplot.o chrtab.o
	rm vplot.o chrtab.o
