#Copyright (C) 1990, 1991 Free Software Foundation, Inc.
#     Written by James Clark (jjc@jclark.uucp)
#
#This file is part of groff.
#
#groff is free software; you can redistribute it and/or modify it under
#the terms of the GNU General Public License as published by the Free
#Software Foundation; either version 1, or (at your option) any later
#version.
#
#groff is distributed in the hope that it will be useful, but WITHOUT ANY
#WARRANTY; without even the implied warranty of MERCHANTABILITY or
#FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
#for more details.
#
#You should have received a copy of the GNU General Public License along
#with groff; see the file LICENSE.  If not, write to the Free Software
#Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

TROFF=../troff/troff
FFLAG=-F../ps
GROPS=../ps/grops
DOCS=chars.PS meref.PS meintro.PS
MEMACROS=../macros/tmac.e
PSMACROS=../ps/tmac.ps
SOELIM=../etc/soelim

all: $(DOCS)

.SUFFIXES: .tr .me .PS .dit

.dit.PS:
	$(GROPS) $(FFLAG) $< >$@

.me.dit:
	$(SOELIM) $< \
	| sed -e "s;@VERSION@;`cat ../VERSION`;" \
	| $(TROFF) -i -Tps $(FFLAG) $(MEMACROS) $(PSMACROS) >$@

.tr.dit:
	$(TROFF) -Tps $(FFLAG) $(PSMACROS) $< >$@

chars.PS: chars.dit
meref.PS: meref.dit
meintro.PS: meintro.dit

install:
	
clean:
	-rm -f *.PS *.dit core

distclean: clean
realclean: distclean
