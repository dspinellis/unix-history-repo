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

#PAGE=letter
PAGE=A4
BINDIR=/usr/local/bin
FONTDIR=/usr/local/lib/groff/font
DEVICEDIR=$(FONTDIR)/devps
FONTS = AB ABI AI AR \
	BMB BMBI BMI BMR \
	CB CBI CI CR \
	HB HBI HI HR \
	HNB HNBI HNI HNR \
	NB NBI NI NR \
	PB PBI PI PR \
	TB TBI TI TR \
	ZCMI S SS ZD ZDR

DOWNLOAD=symbolsl.ps zapfdr.ps

DEVICEFILES=$(FONTS) $(DOWNLOAD) text.enc prologue eqnchar download textmap

all: $(DEVICEFILES)

install.nobin: $(DEVICEFILES)
	-[ -d $(FONTDIR) ] || mkdir $(FONTDIR)
	-[ -d $(DEVICEDIR) ] || mkdir $(DEVICEDIR)
	cp $(DEVICEFILES) $(DEVICEDIR)
	cp DESC-$(PAGE) $(DEVICEDIR)/DESC
	-[ -d $(BINDIR) ] || mkdir $(BINDIR)
	cp afmtodit $(BINDIR)

install.bin:

install: install.bin install.nobin

clean:

realclean: clean

distclean: clean

fonts: FORCE DESC
	$(MAKE) -f FontMakefile
TAGS:

FORCE:
