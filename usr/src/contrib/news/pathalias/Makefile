#!/bin/make -f
# pathalias -- by steve bellovin, as told to peter honeyman

### begin configuration section
###
# if you can't or don't intend to use dbm files,
# don't bother with DBM or makedb
DBM = -ldbm
# or if you roll your own ...
# DBM = dbm.o
###
# where is getopt (if not in the c library)?
# GETOPT = getopt.o
###
# internet domain name resolver library
# enable RESOLVER in config.h
#RESOLVER = -lresolv
### end of configuration section 


CC = cc
CFLAGS = -O
LDFLAGS = -s $(GETOPT) $(DBM) $(RESOLVER)
YFLAGS = -d

OBJ = addlink.o addnode.o domain.o local.o main.o mapit.o mapaux.o mem.o parse.o printit.o
HDRS = def.h config.h
CSRC = addlink.c addnode.c domain.c local.c main.c mapit.c mapaux.c mem.c printit.c
LSRC = $(CSRC) parse.c
SRC = $(CSRC) parse.y makedb.c

pathalias: $(OBJ)
	$(CC) $(OBJ) $(LDFLAGS) -o pathalias

all: pathalias makedb

$(OBJ):	$(HDRS)

parse.c: parse.y $(HDRS)
	$(YACC) $(YFLAGS) parse.y
	mv y.tab.c parse.c

makedb: makedb.o
	$(CC) makedb.o $(LDFLAGS) $(DBM) -o makedb

makedb.o: config.h

clean:
	rm -f *.o y.tab.? parse.c

clobber: clean
	rm -f pathalias makedb

tags: $(SRC) $(HDRS)
	ctags -w $(HDRS) $(SRC)

bundle:
	@bundle README CHANGES pathalias.1 Makefile ${HDRS} ${SRC}

lint:	$(LSRC)
	lint $(CFLAGS) $(LSRC)
	lint makedb.c

install:
	@echo "install pathalias, makedb, and pathalias.8"
	@echo "according to local conventions"
