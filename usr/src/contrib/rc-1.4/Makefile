# Makefile for rc.

SHELL=/bin/sh

# Use an ANSI compiler (or at least one that groks prototypes and void *):
CC=cc -g -O -Wall
CFLAGS=
LDFLAGS=

OBJS=	builtins.o except.o exec.o fn.o footobar.o getopt.o \
	glob.o glom.o hash.o heredoc.o input.o lex.o list.o main.o match.o \
	nalloc.o open.o print.o redir.o sigmsgs.o signal.o status.o tree.o \
	utils.o var.o version.o wait.o walk.o which.o y.tab.o

rc: $(OBJS)
	$(CC) -o $@ $(OBJS)

sigmsgs.c: mksignal
	sh mksignal /usr/include/sys/signal.h

y.tab.c: parse.y
	yacc -d parse.y

trip: rc
	./rc -p < trip.rc

clean: force
	rm -f *.o *.tab.* sigmsgs.*

history: force
	cd history; make CC="$(CC)" $(HISTORYMAKEFLAGS)

force:

# dependencies:

sigmsgs.h: sigmsgs.c
lex.o y.tab.o: y.tab.c
builtins.c fn.c status.c hash.c: sigmsgs.h
