
# See bog.h for configuration

OBJS=bog.o help.o mach.o prtable.o timer.o word.o

CC=gcc
CFLAGS=-O
LIBS=-lcurses -ltermlib

# DICT is the dictionary that will be used to construct the word list used
# by bog
DICT=/usr/dict/words

bog: $(OBJS)
	$(CC) $(CFLAGS) -s -o bog $(OBJS) $(LIBS)

all: bog mkdict mkindex dict

mkdict: mkdict.c
	$(CC) -s -O -o mkdict mkdict.c

mkindex: mkindex.c
	$(CC) -s -O -o mkindex mkindex.c

showdict: showdict.o word.o
	$(CC) -s -O -o showdict showdict.o word.o
	rm -f showdict.o

dict: mkdict mkindex
	./mkdict < $(DICT) > dict
	./mkindex < dict > dict.ind

$(OBJS): bog.h

install: all
	@echo "Move dict, dict.ind, and the helpfile to where you specified in bog.h"
	@echo "Move bog.man to where you want the manual page to go"

clean:
	rm -f *.o bog

lint:
	lint -abchx bog.c help.c mach.c prtable.c timer.c word.c

