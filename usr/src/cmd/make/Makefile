# Description file for the Make command
# Makefile 4.1 81/02/28

P = und -3 | opr -r2
T =
FILES = Makefile ident.c defs main.c doname.c misc.c files.c dosys.c\
	gram.y gcos.c
OBJECTS = ident.o main.o doname.o misc.o files.o dosys.o gram.o 
LIBES= 
LINT = lint -ps
CFLAGS = -O -DASCARCH

GCOSFILES = defs ident.c main.c doname.c misc.c gram.c gcos.c

all:	make

cmp:	make
	cmp make /bin/make
	rm *.o gram.c make

cp:	make
	cp make /bin/make
	rm *.o gram.c make

make:  $(OBJECTS)
	$(CC) -n -s $(CFLAGS) $(OBJECTS) $(LIBES) -o make

$(OBJECTS):  defs

clean:
	-rm *.o gram.c

install:
	install make $(DESTDIR)/bin/make

printall: # Print files off line.
	-pr $(FILES) | $P
	touch print

print:  $(FILES)	# print recently changed files
	-pr $? | $P
	touch print

save: # Write files on Spider store.
	-nfs -ucv make $(FILES)

test: 1zap 2zap
	diff 1zap 2zap
	rm 1zap 2zap

1zap:
	./make -dp | grep -v TIME >1zap

2zap:
	/bin/make -dp | grep -v TIME >2zap

time:  time1 time2 ;

time1 time2 :
	time ./make $T
	time /bin/make $T

lint :  dosys.c doname.c files.c main.c misc.c ident.c gram.c
	$(LINT) dosys.c doname.c files.c main.c misc.c ident.c gram.c
	rm gram.c

src:
	cp $(FILES) /usr/src/cmd/make

gcos: $(GCOSFILES)
	fsend -c -u sif $?
	touch gcos
