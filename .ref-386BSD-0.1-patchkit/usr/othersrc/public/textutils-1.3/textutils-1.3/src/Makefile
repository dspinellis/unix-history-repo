# Generated automatically from Makefile.in by configure.
# Makefile for GNU textutils programs.
# Do not use this makefile directly, but only from `../Makefile'.
# Copyright (C) 1991 Free Software Foundation, Inc.

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

SHELL = /bin/sh

srcdir = .


SOURCES = cat.c cmp.c comm.c csplit.c cut.c \
expand.c fold.c head.c join.c nl.c \
paste.c pr.c sort.c split.c sum.c tac.c \
tail.c tr.c unexpand.c uniq.c wc.c version.c

DISTFILES = Makefile.in system.h $(SOURCES)

PROGS = cat cmp comm csplit cut expand fold head join nl \
paste pr sort split sum tac tail tr unexpand uniq wc

all: $(PROGS) $(LIBPROGS)
.PHONY: all

install: all
	for f in $(PROGS); do $(INSTALL) $$f $(bindir)/$(binprefix)$$f; done
.PHONY: install

tags: $(SOURCES)
	ctags $(SOURCES)

TAGS: $(SOURCES)
	etags $(SOURCES)

mostlyclean:
	rm -f $(PROGS) $(LIBPROGS) *.o core
.PHONY: mostlyclean

clean: mostlyclean
	rm -f tags TAGS Makefile
.PHONY: clean

realclean: clean
.PHONY: realclean

dist:
	ln $(DISTFILES) ../`cat ../.fname`/src
.PHONY: dist

# Linking rules.

$(PROGS): ../lib/libtu.a version.o

cat: cat.o
	$(CC) $(LDFLAGS) -o $@ cat.o ../lib/libtu.a version.o $(LIBS)

cmp: cmp.o
	$(CC) $(LDFLAGS) -o $@ cmp.o ../lib/libtu.a version.o $(LIBS)

comm: comm.o
	$(CC) $(LDFLAGS) -o $@ comm.o ../lib/libtu.a version.o $(LIBS)

csplit: csplit.o
	$(CC) $(LDFLAGS) -o $@ csplit.o ../lib/libtu.a version.o $(LIBS)

cut: cut.o
	$(CC) $(LDFLAGS) -o $@ cut.o ../lib/libtu.a version.o $(LIBS)

expand: expand.o
	$(CC) $(LDFLAGS) -o $@ expand.o ../lib/libtu.a version.o $(LIBS)

fold: fold.o
	$(CC) $(LDFLAGS) -o $@ fold.o ../lib/libtu.a version.o $(LIBS)

head: head.o
	$(CC) $(LDFLAGS) -o $@ head.o ../lib/libtu.a version.o $(LIBS)

join: join.o
	$(CC) $(LDFLAGS) -o $@ join.o ../lib/libtu.a version.o $(LIBS)

nl: nl.o
	$(CC) $(LDFLAGS) -o $@ nl.o ../lib/libtu.a version.o $(LIBS)

paste: paste.o
	$(CC) $(LDFLAGS) -o $@ paste.o ../lib/libtu.a version.o $(LIBS)

pr: pr.o
	$(CC) $(LDFLAGS) -o $@ pr.o ../lib/libtu.a version.o $(LIBS)

sort: sort.o
	$(CC) $(LDFLAGS) -o $@ sort.o ../lib/libtu.a version.o $(LIBS)

split: split.o
	$(CC) $(LDFLAGS) -o $@ split.o ../lib/libtu.a version.o $(LIBS)

sum: sum.o
	$(CC) $(LDFLAGS) -o $@ sum.o ../lib/libtu.a version.o $(LIBS)

tac: tac.o
	$(CC) $(LDFLAGS) -o $@ tac.o ../lib/libtu.a version.o $(LIBS)

tail: tail.o
	$(CC) $(LDFLAGS) -o $@ tail.o ../lib/libtu.a version.o $(LIBS)

tr: tr.o
	$(CC) $(LDFLAGS) -o $@ tr.o ../lib/libtu.a version.o $(LIBS)

unexpand: unexpand.o
	$(CC) $(LDFLAGS) -o $@ unexpand.o ../lib/libtu.a version.o $(LIBS)

uniq: uniq.o
	$(CC) $(LDFLAGS) -o $@ uniq.o ../lib/libtu.a version.o $(LIBS)

wc: wc.o
	$(CC) $(LDFLAGS) -o $@ wc.o ../lib/libtu.a version.o $(LIBS)

# Compilation rules.

nl.o tac.o: ../lib/regex.h
comm.o nl.o uniq.o: ../lib/linebuffer.h
cat.o cmp.o comm.o csplit.o cut.o expand.o fold.o head.o: system.h
join.o nl.o paste.o pr.o sort.o split.o sum.o tac.o: system.h
tail.o tr.o unexpand.o uniq.o wc.o: system.h
