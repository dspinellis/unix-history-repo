#	Makefile	1.3	83/03/30

CFLAGS = -O
LDFLAGS = -lm
OBJS = hdb.o hpoint.o hgraph.o main.o

gprint:  ${OBJS}
	cc -o gprint ${OBJS} ${LDFLAGS}

${OBJS}: gprint.h

clean:
	rm -f *.o
