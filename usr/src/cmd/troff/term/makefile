cp:	all
	for i in *.o; do cp $$i /usr/lib/term/`basename $$i .o`; done
	rm *.o

cmp:	all
	for i in *.o; do cmp $$i /usr/lib/term/`basename $$i .o`; done
	rm *.o

all:	tab300-12.o tab300.o tab300s-12.o tab300s.o
all:	tab37.o tab450-12-8.o tab450-12.o tab450.o tab832.o taba1.o tablp.o tabtn300.o
	:

.c.o:
	cc -c $<; strip $@
