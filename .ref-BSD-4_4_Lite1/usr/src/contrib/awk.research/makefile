# /****************************************************************
# Copyright (C) AT&T 1993
# All Rights Reserved
# 
# Permission to use, copy, modify, and distribute this software and
# its documentation for any purpose and without fee is hereby
# granted, provided that the above copyright notice appear in all
# copies and that both that the copyright notice and this
# permission notice and warranty disclaimer appear in supporting
# documentation, and that the name of AT&T or any of its entities
# not be used in advertising or publicity pertaining to
# distribution of the software without specific, written prior
# permission.
# 
# AT&T DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
# INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.
# IN NO EVENT SHALL AT&T OR ANY OF ITS ENTITIES BE LIABLE FOR ANY
# SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
# WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER
# IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
# ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF
# THIS SOFTWARE.
# ****************************************************************/

YFLAGS = -d
CFLAGS = -g
CC = cc

OFILES = b.o main.o parse.o proctab.o tran.o lib.o run.o 

SOURCE = awk.h proto.h awk.g.y awk.lx.l b.c main.c maketab.c parse.c lib.c run.c tran.c
CSOURCE = y.tab.c lex.yy.c b.c main.c proctab.c parse.c lib.c run.c tran.c

a.out:	awk.g.o awk.lx.o $(OFILES) $(ALLOC) awk.h
	$(CC) $(CFLAGS) awk.g.o awk.lx.o $(OFILES) $(ALLOC)  -lm

$(OFILES) awk.lx.o:	awk.h prevy.tab.h proto.h

awk.g.o:	awk.h proto.h

prevy.tab.h:	y.tab.h
	-cmp -s y.tab.h prevy.tab.h || (cp y.tab.h prevy.tab.h; echo change maketab)

proctab.c:	maketab
	./maketab >proctab.c

maketab:	prevy.tab.h maketab.c
	$(CC) $(CFLAGS) maketab.c -o maketab

bundle:
	@bundle README FIXES $(SOURCE) makefile awk.1

bowell:	$(SOURCE) makefile NEW FIXES awktest.a
	push bowell $? /usr/src/cmd/awk
	touch bowell

profile:
	make 'CFLAGS=-p'

install:	a.out
	cp a.out /usr/bin/awk
	strip /usr/bin/awk

clean:
	rm -f a.out *.o t.* *temp* *.out *junk* y.tab.* lex.yy.c prevy.tab.h maketab proctab.c foo* glop*
