# Generated automatically from Makefile.in by configure.
# Makefile for GNU fileutils programs.
# Do not use this makefile directly, but only from `../Makefile'.
# Copyright (C) 1990, 1991, 1992 Free Software Foundation, Inc.

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


SOURCES = chmod.c chgrp.c chown.c cp-aux.c cp-hash.c cp.c \
dd.c df.c du.c install.c ln.c ls.c mkdir.c mkfifo.c mknod.c mv.c mvdir.c \
rm.c rmdir.c touch.c version.c

DISTFILES = Makefile.in cp.h $(SOURCES)

PROGS = chgrp chown chmod cp dd du ginstall ln dir vdir ls mkdir \
mkfifo mknod mv rm rmdir touch  df

all: $(PROGS) $(LIBPROGS)
.PHONY: all

install: all
	[ "x$(LIBPROGS)" = x ] || ./ginstall -o root -m 4755 $(LIBPROGS) $(libdir)
	for f in $(PROGS); do ./ginstall $$f $(bindir)/$(binprefix)$$f; done
	cd $(bindir); ./ln -f dir d; ./ln -f vdir v; ./mv -f ginstall install
.PHONY: install

tags: $(SOURCES)
	ctags $(SOURCES)

TAGS: $(SOURCES)
	etags $(SOURCES)

# Use /bin/rm instead of ./rm in case ./rm is compiled for a different
# architecture.
mostlyclean:
	/bin/rm -f $(PROGS) $(LIBPROGS) *.o core
.PHONY: mostlyclean

clean: mostlyclean
	rm -f tags TAGS Makefile dir.c vdir.c
.PHONY: clean

realclean: clean
.PHONY: realclean

dist:
	ln $(DISTFILES) ../`cat ../.fname`/src
.PHONY: dist

# Linking rules.

$(PROGS): ../lib/libfu.a version.o

chmod: chmod.o
	$(CC) $(LDFLAGS) -o $@ chmod.o ../lib/libfu.a version.o $(LIBS)

chgrp: chgrp.o
	$(CC) $(LDFLAGS) -o $@ chgrp.o ../lib/libfu.a version.o $(LIBS)

chown: chown.o
	$(CC) $(LDFLAGS) -o $@ chown.o ../lib/libfu.a version.o $(LIBS)

cp: cp.o cp-hash.o cp-aux.o
	$(CC) $(LDFLAGS) -o $@ cp.o cp-hash.o cp-aux.o ../lib/libfu.a version.o $(LIBS)

dd: dd.o
	$(CC) $(LDFLAGS) -o $@ dd.o ../lib/libfu.a version.o $(LIBS)

df: df.o
	$(CC) $(LDFLAGS) -o $@ df.o ../lib/libfu.a version.o $(LIBS)

dir: dir.o
	$(CC) $(LDFLAGS) -o $@ dir.o ../lib/libfu.a version.o $(LIBS)

du: du.o
	$(CC) $(LDFLAGS) -o $@ du.o ../lib/libfu.a version.o $(LIBS)

ginstall: install.o
	$(CC) $(LDFLAGS) -o $@ install.o ../lib/libfu.a version.o $(LIBS)

ln: ln.o
	$(CC) $(LDFLAGS) -o $@ ln.o ../lib/libfu.a version.o $(LIBS)

ls: ls.o
	$(CC) $(LDFLAGS) -o $@ ls.o ../lib/libfu.a version.o $(LIBS)

mkdir: mkdir.o
	$(CC) $(LDFLAGS) -o $@ mkdir.o ../lib/libfu.a version.o $(LIBS)

mkfifo: mkfifo.o
	$(CC) $(LDFLAGS) -o $@ mkfifo.o ../lib/libfu.a version.o $(LIBS)

mknod: mknod.o
	$(CC) $(LDFLAGS) -o $@ mknod.o ../lib/libfu.a version.o $(LIBS)

mv: mv.o
	$(CC) $(LDFLAGS) -o $@ mv.o ../lib/libfu.a version.o $(LIBS)

mvdir: mvdir.o
	$(CC) $(LDFLAGS) -o $@ mvdir.o ../lib/libfu.a version.o $(LIBS)

rm: rm.o
	$(CC) $(LDFLAGS) -o $@ rm.o ../lib/libfu.a version.o $(LIBS)

rmdir: rmdir.o
	$(CC) $(LDFLAGS) -o $@ rmdir.o ../lib/libfu.a version.o $(LIBS)

touch: touch.o
	$(CC) $(LDFLAGS) -o $@ touch.o ../lib/libfu.a version.o $(LIBS)

vdir: vdir.o
	$(CC) $(LDFLAGS) -o $@ vdir.o ../lib/libfu.a version.o $(LIBS)

# Compilation rules.

chmod.o install.o mkdir.o mkfifo.o mknod.o: ../lib/modechange.h
cp.o mv.o ln.o: ../lib/backupfile.h
cp.o cp-hash.o cp-aux.o: cp.h
df.o: ../lib/mountlist.h ../lib/fsusage.h
chgrp.o chmod.o chown.o cp.o cp-aux.o cp-hash.o dd.o du.o: ../lib/system.h
install.o ln.o ls.o mkdir.o mkfifo.o mknod.o mv.o mvdir.o: ../lib/system.h
rm.o rmdir.o touch.o: ../lib/system.h

# These rules are workarounds for C compilers that don't accept -c and
# -o at the same time.
dir.c:	ls.c
	rm -f $@; cp ls.c $@

dir.o:		dir.c
	$(CC) $(CFLAGS) -DMULTI_COL -c dir.c

vdir.c:	ls.c
	rm -f $@; cp ls.c $@

vdir.o:		vdir.c
	$(CC) $(CFLAGS) -DLONG_FORMAT -c vdir.c
