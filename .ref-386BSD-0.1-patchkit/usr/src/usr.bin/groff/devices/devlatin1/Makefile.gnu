#Copyright (C) 1989, 1990 Free Software Foundation, Inc.
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

FONTDIR=/usr/local/lib/groff/font
DEVICEDIR=$(FONTDIR)/devlatin1
# resolution in units per inch
RES=240
# characters per inch
CPI=10
# lines per inch
LPI=6
FONTS=R I B BI

all: $(FONTS) DESC

$(FONTS): R.proto
	@echo Making $@
	@(charwidth=`expr $(RES) / $(CPI)` ; \
 	 sed -e "s/^name [A-Z]*$$/name $@/" \
	     -e "s/^\\([^	]*\\)	[0-9]+	/\\1	$$charwidth	/" \
	     -e "s/^spacewidth [0-9]+$$/spacewidth $$charwidth/" \
	     -e "s/^internalname .*$$/internalname $@/" \
	     -e "/^internalname/s/BI/3/" \
	     -e "/^internalname/s/B/2/" \
	     -e "/^internalname/s/I/1/" \
	     -e "/^internalname .*[^ 0-9]/d" \
	     R.proto >$@)

DESC: DESC.proto
	@echo Making $@
	@sed -e "s/^res .*$$/res $(RES)/" \
	    -e "s/^hor .*$$/hor `expr $(RES) / $(CPI)`/" \
	    -e "s/^vert .*$$/vert `expr $(RES) / $(LPI)`/" \
	    -e "s/^fonts .*$$/fonts `set $(FONTS); echo $$#` $(FONTS)/" \
	    DESC.proto >$@

install.nobin: all
	-[ -d $(FONTDIR) ] || mkdir $(FONTDIR)
	-[ -d $(DEVICEDIR) ] || mkdir $(DEVICEDIR)
	cp $(FONTS) DESC $(DEVICEDIR)

install.bin:

install: install.bin install.nobin

clean:
	-rm -f $(FONTS) DESC

distclean: clean

realclean: distclean

TAGS:
