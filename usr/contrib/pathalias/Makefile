#!/bin/make -f
# pathalias -- by steve bellovin, as told to peter honeyman

# if you can't or don't intend to use dbm files, don't bother with makedb
DBM = -ldbm
# or if you roll your own ...
# DBM = dbm.o

CC = cc
CFLAGS = -O
LDFLAGS = -s
YFLAGS = -d

OBJ = addlink.o addnode.o extern.o local.o main.o mapit.o mapaux.o mem.o parse.o printit.o
HDRS = def.h config.h
CSRC = addlink.c addnode.c extern.c local.c main.c mapit.c mapaux.c mem.c printit.c
SRC = $(CSRC) parse.y
LSRC = $(CSRC) parse.c

all: pathalias makedb

pathalias: $(OBJ)
	$(CC) $(LDFLAGS) $(OBJ) -o pathalias

$(OBJ):	$(HDRS)

parse.c: parse.y $(HDRS)
	$(YACC) $(YFLAGS) parse.y
	mv y.tab.c parse.c

makedb: makedb.o
	$(CC) $(LDFLAGS) makedb.o $(DBM) -o makedb

makedb.o: config.h

clean:
	rm -f *.o y.tab.? parse.c errs makedb pathalias

clobber: clean
	rm -f pathalias makedb

tags: $(SRC) $(HDRS) makedb.c
	ctags -w $(HDRS) $(SRC)

bundle:
	@bundle README CHANGES pathalias.1 Makefile ${HDRS} ${SRC} makedb.c

lint:	$(LSRC)
	lint $(CFLAGS) $(LSRC)
	lint makedb.c

install:
	@echo "install pathalias according to local conventions"
