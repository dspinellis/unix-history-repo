
OBJ= hdb.o hpoint.o hgraph.o main.o getlogin.o

all:   $(OBJ) gprint

hdb.o:   hdb.c 
	cc -c -g hdb.c 

hgraph.o:	hgraph.c
	cc -c -g hgraph.c

hpoint.o:	hpoint.c
	cc -c -g hpoint.c


main.o:   main.c 
	cc -c -g main.c 

gprint:  $(OBJ)
	cc -o gprint -g $(OBJ) -lm 

gdump: gdump.o getlogin.o
	cc -o gdump -g gdump.o getlogin.o
