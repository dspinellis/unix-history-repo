#	@(#)Makefile	1.2 (Berkeley) %G%
# lib/ditroff.font/Makefile
#
# Copyright (c) 1984 Adobe Systems Incorporated. All Rights Reserved.
# PostScript is a trademark of Adobe Systems Incorporated.
# RCSID: $Header: Makefile,v 2.1 85/11/24 12:24:55 shore Rel $

# see README for more information
# makedev is the ditroff program that builds device descriptions

# Berkeley changes:
# 17 February, 1987.  Edward Wang (edward@arpa)
#	added stipple fonts and avoid forking new makes.

LOADFONTS = R I B BI H HB C CB S SS
LOADF = R.aux I.aux B.aux BI.aux H.aux HB.aux C.aux CB.aux S.aux SS.aux
MOREFONTS = HO HD CO CD A AO AB AD \
	K KI KB KD NB ND NI NR \
	PB PD PI PR V VB VD VO ZC ZD
MOREF = HO.aux HD.aux CO.aux CD.aux A.aux AO.aux AB.aux AD.aux \
	K.aux KI.aux KB.aux KD.aux NB.aux ND.aux NI.aux NR.aux \
	PB.aux PD.aux PI.aux PR.aux V.aux VB.aux VD.aux VO.aux ZC.aux ZD.aux
MOREOUT = HO.out HD.out CO.out CD.out A.out AO.out AB.out AD.out \
	K.out KI.out KB.out KD.out NB.out ND.out NI.out NR.out \
	PB.out PD.out PI.out PR.out V.out VB.out VD.out VO.out ZC.out ZD.out
STIPPLES = cf ug mg
ALLFONTS = $(LOADF) $(MOREF)

.SUFFIXES: .out .aux .map

# how to make a .aux (and the file with the same name with no extension)
# from a .map (this is input to makedev)

.map.aux:
	./afmdit $* ..

.aux.out:
	$$MAKEDEV $*

# how to make a .out (and a .aux) from a .map
# just do the above two steps

.map.out:
	./afmdit $* ..
	$$MAKEDEV $*

all: makedev DESC.out moreout

programs:

install: all
	rm -rf $$DITDIR/devpsc
	mkdir $$DITDIR/devpsc
	-chmod 755 $$DITDIR/devpsc
	-chown $$OWNER.$$GROUP $$DITDIR/devpsc
	install -c -m 644 -o $$OWNER -g $$GROUP *.aux *.map *.out DESC \
		$$DITDIR/devpsc
	cd $$DITDIR/devpsc; ln C.map CW.map; ln C.out CW.out; ln C.aux CW.aux

makedev:
	test -r $$MAKEDEV
	-chmod +x afmdit

DESC.out: DESC $(LOADF)
	$$MAKEDEV DESC

moreout: $(MOREOUT)

allfonts: $(ALLFONTS)

$(ALLFONTS): afmdit.awk

DESC: afmdit.awk Makefile charset devspecs
	rm -f DESC
	echo "# ditroff device description for PostScript" >> DESC
	echo "# PostScript is a trademark of Adobe Systems Incorporated">>DESC
	echo fonts `echo $(LOADFONTS) | wc -w` $(LOADFONTS) >> DESC
	echo stipples `echo $(STIPPLES) | wc -w` $(STIPPLES) >> DESC
	cat devspecs >> DESC
	echo "charset" >> DESC
	cat charset >> DESC

clean:
	rm -f $(LOADFONTS) $(MOREFONTS) core DESC *.out *.aux temp.* \
		*.CKP *.BAK

