# Copyright (C) 1989 Free Software Foundation, Inc.
# written by Douglas C. Schmidt (schmidt@ics.uci.edu)
# 
# This file is part of GNU GPERF.
# 
# GNU GPERF is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 1, or (at your option)
# any later version.
# 
# GNU GPERF is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with GNU GPERF; see the file COPYING.  If not, write to
# the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA. 

prefix=/usr/gnu
BINDIR=$(prefix)/bin
LIBDIR=$(prefix)/lib
MANDIR=$(prefix)/man
INSTALL=install
  
all: src-dir
  
src-dir: 
	cd src; $(MAKE) LIBDIR=$(LIBDIR)
  
install:
	cd src; $(MAKE) BINDIR=$(BINDIR) MANDIR=$(MANDIR) INSTALL="$(INSTALL)" install
	$(INSTALL) gperf.1 $(MANDIR)/man1/gperf.1
  
run_tests: gperf
	-cd tests; $(MAKE) GPERF=../src/gperf

distrib: 
	cd ..; rm -f gperf.tar.Z; tar cvf gperf.tar gperf; compress gperf.tar; uuencode gperf.tar.Z < gperf.tar.Z > GSHAR
  
shar:
	cd ..; makekit -ngperf ./gperf ./gperf/* ./gperf/src/* ./gperf/tests/*; 
  
clean: 
	cd src; $(MAKE) clean
	cd tests; $(MAKE) clean
	-rm -f *~ 
  
realclean: clean
	-rm -f gperf.info* gperf.?? gperf.??s gperf.log gperf.toc \
          gperf.*aux *inset.c *out gperf
