# transcript/lib/troff.font/Makefile.bsd
#
# Copyright (C) 1985 Adobe Systems Incorporated
#
# Makefile for troff width tables and pscat correspondence tables
#
# See pscatmap(8) and the .map files for more information.
#
# If you add a font family to your PostScript/troff database,
# add it to the FAMILIES line in this makefile, then do a "make".

.SUFFIXES: .ct .map

#######################################################################
# Here are the family names, the face names are gotten from the .map files

FAMILIES = Times.ct Helvetica.ct
FILES=	Times.head Times.ct ftR ftI ftB ftS \
	Helvetica.head Helvetica.ct ftH ftHI ftHB ftHS

#######################################################################

install: ${FAMILIES}
	-mkdir $${TROFFFONTDIR?undefined}
	cp -p ${FILES} $${TROFFFONTDIR?undefined}

.map.ct:
	$${PSLIBDIR-/usr/local/lib/ps}/pscatmap $*.map
	make `awk -f doto.awk $*.map`
	awk -f head.awk $*.map >$*.head

# If you want to be able to check the ".c" files, remove the last 
# rule line (the "rm").
.c.o:
	cc -c $*.c
	mv $*.o $*
	strip $*
	rm $*.c

clean:
	rm -f - *.ct *.head *.c *.o ft? ft?? *BAK *CKP .emacs_[0-9]*
