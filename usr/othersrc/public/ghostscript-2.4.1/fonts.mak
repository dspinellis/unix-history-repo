#    Copyright (C) 1991 Aladdin Enterprises.  All rights reserved.
#    Distributed by Free Software Foundation, Inc.
#
# This file is part of Ghostscript.
#
# Ghostscript is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY.  No author or distributor accepts responsibility
# to anyone for the consequences of using it or for whether it serves any
# particular purpose or works at all, unless he says so in writing.  Refer
# to the Ghostscript General Public License for full details.
#
# Everyone is granted permission to copy, modify and redistribute
# Ghostscript, but only under the conditions described in the Ghostscript
# General Public License.  A copy of this license is supposed to have been
# given to you along with Ghostscript so you can know your rights and
# responsibilities.  It should be in a file named COPYING.  Among other
# things, the copyright notice and this notice must be preserved on all
# copies.

# Makefile for Ghostscript fonts.
# For more information about Ghostscript fonts, consult the Fontmap file.

AFM=afm
BDF=bdf
FONTS=fonts

fonts: AvantGarde Bookman Charter Courier Helvetica NewCenturySchlbk \
	Palatino Symbol TimesRoman Ugly Utopia ZapfChancery ZapfDingbats

# ----------------------------------------------------------------

# Each Ghostscript font has a uniqueID (an integer).  This is used
# to identify distinct fonts within the Ghostscript font machinery.
# since some P*stScr*pt programs assume that adding a small integer
# to a uniqueID produces a new, distinct, unused uniqueID,
# the uniqueID values in this file are all multiples of 10.
# To avoid some conflicts with Adobe's numbering scheme, the uniqueID
# values in this file all lie between 4000000 and 4999999.
# The uniqueID is also used only when converting the font.
# The algorithm for computing the UniqueID is given below.

# A UniqueID for a Ghostscript font looks like:
#
# 4TTWVE0
#
# where TT is a two-digit number representing the typeface,
# W represents the weight (normal, bold, ...),
# V represents the variant (normal, italic, oblique, ...), and
# E represents the expansion (normal, condensed, ...).
# This scheme will not work forever.  As soon there are more 99
# typefaces, or more than 9 weights or variants, we will have to do
# something else. But it suffices for the near future.
#
# The filename for a font is constructed in a somewhat similar way:
#
# FTTWVVVE.gsf
#
# where F is the foundry, TT a two-letter abbreviation for the
# typeface, and W, V, and E the weight, variant, and expansion.  Since a
# font can have multiple variants, we allocate three letters to that
# (for example, Lucida Regular Sans Typewriter Italic).  If a font has
# four variants, you're on your own.  If a font does have multiple
# variants, it's best to add the expansion letter `r', so that it is
# clear which letters are variants and which the expansion.
#
# This scheme is very close to the one proposed in `Filenames for
# fonts', published in the first 1990 issue of TUGboat (the
# journal of the TeX Users Group).
#
# In the following tables, we made no attempt to be exhaustive.
# Instead, we have simply allocated entries for those things that we needed
# for the fonts that we are actually distributing.
#
#
# foundries:
# ----------------------------------------------------------------
# b = Bitstream
# p = Adobe (`p' for PostScript)
#
#
#
# typefaces:
# id   name			  filename prefix
# ----------------------------------------------------------------
# 08 = Avant Garde		= pag		(Adobe)
# 11 = Bookman			= pbk		(Adobe)
# 01 = Charter			= bch		(Bitstream)
# 02 = Courier			= pcr		(Adobe)
# 03 = Helvetica		= phv		(Adobe)
# 04 = New Century Schoolbook	= pnc		(Adobe)
# 09 = Palatino			= ppl		(Adobe)
# 05 = Symbol			= psy		(Adobe)
# 06 = Times			= ptm		(Adobe)
# 00 = Ugly			= ugly		(public domain)
# 07 = Zapf Chancery		= zc		(public domain)
# 10 = Zapf Dingbats		= pzd		(Adobe)
#
# 90 = Hershey Gothic English	= hrge
# 91 = Hershey Gothic Italian	= hrit
# 92 = Hershey Gothic German	= hrgr
# 93 = Hershey Greek		= hrgk
# 94 = Hershey Plain		= hrpl
# 95 = Hershey Script		= hrsc
# 96 = Hershey Symbol		= hrsy
#
#
# weights:
# 0 = normal			= r
# 1 = bold			= b
# 2 = book			= k
# 3 = demi			= d
# 4 = light			= l
#
#
# variants:
# 0 = normal			= r (omitted when the weight is normal)
# 1 = italic			= i
# 2 = oblique			= o
#
#
# expansions:
# 0 = normal			= r (omitted when the weight and variant
#                                    are normal)
# 1 = narrow			= n
#
#

# ----------------------------------------------------------------

# The bdftops conversion program takes the following arguments:
#
#	 bdftops xx.bdf [yy1.afm ...] zz.gsf fontname uniqueID [encodingname]
#
# These arguments have the following meanings:
#
#	xx.bdf - the input bitmap file, a BDF file
#	yy*.afm - the AFM files giving the metrics (optional)
#	zz.gsf - the output file
#	fontname - the font name
#	uniqueID - the uniqueID, as described above
#	encodingname - the encoding for the font
#
# Currently, the only defined encodings are StandardEncoding,
# ISOLatin1Encoding, and SymbolEncoding. If the encoding is omitted,
# StandardEncoding is assumed.

# ----------------------------------------------------------------

# ---------------- Avant Garde ----------------

AvantGarde: $(FONTS)/pagk.gsf $(FONTS)/pagko.gsf $(FONTS)/pagd.gsf \
	$(FONTS)/pagdo.gsf

$(FONTS)/pagk.gsf: $(BDF)/avt18.bdf $(AFM)/avantgarde_book.afm
	bdftops $(BDF)/avt18.bdf $(AFM)/avantgarde_book.afm \
		$(FONTS)/pagk.gsf AvantGarde-Book 4082000

$(FONTS)/pagko.gsf: $(BDF)/pagko.bdf $(AFM)/avantgarde_bookoblique.afm
	bdftops $(BDF)/pagko.bdf $(AFM)/avantgarde_bookoblique.afm \
		$(FONTS)/pagko.gsf AvantGarde-BookOblique 4082200

$(FONTS)/pagd.gsf: $(BDF)/pagd.bdf $(AFM)/avantgarde_demi.afm
	bdftops $(BDF)/pagd.bdf $(AFM)/avantgarde_demi.afm \
		$(FONTS)/pagd.gsf AvantGarde-Demi 4083000

$(FONTS)/pagdo.gsf: $(BDF)/pagdo.bdf $(AFM)/avantgarde_demioblique.afm
	bdftops $(BDF)/pagdo.bdf $(AFM)/avantgarde_demioblique.afm \
		$(FONTS)/pagdo.gsf AvantGarde-DemiOblique 4083200

# ---------------- Bookman ----------------

Bookman: $(FONTS)/pbkl.gsf $(FONTS)/pbkli.gsf $(FONTS)/pbkd.gsf \
	$(FONTS)/pbkdi.gsf

$(FONTS)/pbkl.gsf: $(BDF)/pbkl.bdf $(AFM)/Bookman-Light.afm
	bdftops $(BDF)/pbkl.bdf $(AFM)/Bookman-Light.afm \
		$(FONTS)/pbkl.gsf Bookman-Light 4114000

$(FONTS)/pbkli.gsf: $(BDF)/pbkli.bdf $(AFM)/Bookman-LightItalic.afm
	bdftops $(BDF)/pbkli.bdf $(AFM)/Bookman-LightItalic.afm \
		$(FONTS)/pbkli.gsf Bookman-LightItalic 4114100

$(FONTS)/pbkd.gsf: $(BDF)/pbkd.bdf $(AFM)/Bookman-Demi.afm
	bdftops $(BDF)/pbkd.bdf $(AFM)/Bookman-Demi.afm \
		$(FONTS)/pbkd.gsf Bookman-Demi 4113000

$(FONTS)/pbkdi.gsf: $(BDF)/pbkdi.bdf $(AFM)/Bookman-DemiItalic.afm
	bdftops $(BDF)/pbkdi.bdf $(AFM)/Bookman-DemiItalic.afm \
		$(FONTS)/pbkdi.gsf Bookman-DemiItalic 4113100

# ---------------- Charter ----------------
# (No separate metrics for this font.)

Charter: $(FONTS)/bchr.gsf $(FONTS)/bchri.gsf $(FONTS)/bchb.gsf \
	$(FONTS)/bchbi.gsf

$(FONTS)/bchr.gsf: $(BDF)/charR24.bdf
	bdftops $(BDF)/charR24.bdf \
		$(FONTS)/bchr.gsf Charter-Roman 4010000

$(FONTS)/bchri.gsf: $(BDF)/charI24.bdf
	bdftops $(BDF)/charI24.bdf \
		$(FONTS)/bchri.gsf Charter-Italic 4010100

$(FONTS)/bchb.gsf: $(BDF)/charB24.bdf
	bdftops $(BDF)/charB24.bdf \
		$(FONTS)/bchb.gsf Charter-Bold 4011000
		
$(FONTS)/bchbi.gsf: $(BDF)/charBI24.bdf
	bdftops $(BDF)/charBI24.bdf \
		$(FONTS)/bchbi.gsf Charter-BoldItalic 4011100

# ---------------- Courier ----------------

# Ghostscript has two sets of Courier fonts, a lower-quality set derived
# from the X11R4 bitmaps, and a higher-quality set contributed by IBM
# to X11R5 in Type 1 form.  Unfortunately, the two sets don't contain
# the same variants, and some PostScript files use Courier-Oblique rather
# than Courier-Italic.  For this reason, we keep the X11R4 Oblique fonts.

Courier: $(FONTS)/cour.pfa $(FONTS)/couri.pfa $(FONTS)/courb.pfa \
	$(FONTS)/courbi.pfa $(FONTS)/pcrro.gsf $(FONTS)/pcrbo.gsf
#	$(FONTS)/pcrr.gsf $(FONTS)/pcrb.gsf

# Old Courier, longer used.
$(FONTS)/pcrr.gsf: $(BDF)/courR24.bdf $(AFM)/courier.afm
	bdftops $(BDF)/courR24.bdf $(AFM)/courier.afm \
		$(FONTS)/pcrr.gsf Courier 4020000

$(FONTS)/pcrro.gsf: $(BDF)/courO24.bdf $(AFM)/courier_oblique.afm
	bdftops $(BDF)/courO24.bdf $(AFM)/courier_oblique.afm \
		$(FONTS)/pcrro.gsf Courier-Oblique 4020200

# Old Courier-Bold, no longer used.
$(FONTS)/pcrb.gsf: $(BDF)/courB24.bdf $(AFM)/courier_bold.afm
	bdftops $(BDF)/courB24.bdf $(AFM)/courier_bold.afm \
		$(FONTS)/pcrb.gsf Courier-Bold 4021000

$(FONTS)/pcrbo.gsf: $(BDF)/courBO24.bdf $(AFM)/courier_boldoblique.afm
	bdftops $(BDF)/courBO24.bdf $(AFM)/courier_boldoblique.afm \
		$(FONTS)/pcrbo.gsf Courier-BoldOblique 4021200

# ---------------- Helvetica ----------------

Helvetica: $(FONTS)/phvr.gsf $(FONTS)/phvro.gsf $(FONTS)/phvrrn.gsf \
	 $(FONTS)/phvb.gsf $(FONTS)/phvbo.gsf

$(FONTS)/phvr.gsf: $(BDF)/helvR24.bdf $(AFM)/helvetica.afm
	bdftops $(BDF)/helvR24.bdf $(AFM)/helvetica.afm \
		$(FONTS)/phvr.gsf Helvetica 4030000

$(FONTS)/phvro.gsf: $(BDF)/helvO24.bdf $(AFM)/helvetica_oblique.afm
	bdftops $(BDF)/helvO24.bdf $(AFM)/helvetica_oblique.afm \
		$(FONTS)/phvro.gsf Helvetica-Oblique 4030200

$(FONTS)/phvrrn.gsf: $(BDF)/hvmrc14.bdf $(AFM)/Helvetica-Narrow.afm
	bdftops $(BDF)/hvmrc14.bdf $(AFM)/Helvetica-Narrow.afm \
		$(FONTS)/phvrrn.gsf Helvetica-Narrow 4030310

$(FONTS)/phvb.gsf: $(BDF)/helvB24.bdf $(AFM)/helvetica_bold.afm
	bdftops $(BDF)/helvB24.bdf $(AFM)/helvetica_bold.afm \
		$(FONTS)/phvb.gsf Helvetica-Bold 4031000

$(FONTS)/phvbo.gsf: $(BDF)/helvBO24.bdf $(AFM)/helvetica_boldoblique.afm
	bdftops $(BDF)/helvBO24.bdf $(AFM)/helvetica_boldoblique.afm \
		$(FONTS)/phvbo.gsf Helvetica-BoldOblique 4031200

# ---------------- New Century Schoolbook ----------------

NewCenturySchlbk: $(FONTS)/pncr.gsf $(FONTS)/pncri.gsf $(FONTS)/pncb.gsf \
	$(FONTS)/pncbi.gsf

$(FONTS)/pncr.gsf: $(BDF)/ncenR24.bdf $(AFM)/newcenturyschlbk_roman.afm
	bdftops $(BDF)/ncenR24.bdf $(AFM)/newcenturyschlbk_roman.afm \
		$(FONTS)/pncr.gsf NewCenturySchlbk-Roman 4040000

$(FONTS)/pncri.gsf: $(BDF)/ncenI24.bdf $(AFM)/newcenturyschlbk_italic.afm
	bdftops $(BDF)/ncenI24.bdf $(AFM)/newcenturyschlbk_italic.afm \
		$(FONTS)/pncri.gsf NewCenturySchlbk-Italic 4040100

$(FONTS)/pncb.gsf: $(BDF)/ncenB24.bdf $(AFM)/newcenturyschlbk_bold.afm
	bdftops $(BDF)/ncenB24.bdf $(AFM)/newcenturyschlbk_bold.afm \
		$(FONTS)/pncb.gsf NewCenturySchlbk-Bold 4041000

$(FONTS)/pncbi.gsf: $(BDF)/ncenBI24.bdf $(AFM)/newcenturyschlbk_bolditalic.afm
	bdftops $(BDF)/ncenBI24.bdf $(AFM)/newcenturyschlbk_bolditalic.afm \
		$(FONTS)/pncbi.gsf NewCenturySchlbk-BoldItalic 4041100

# ---------------- Palatino ----------------

Palatino: $(FONTS)/pplr.gsf $(FONTS)/pplri.gsf $(FONTS)/pplb.gsf \
	$(FONTS)/pplbi.gsf

$(FONTS)/pplr.gsf: $(BDF)/pal18.bdf $(AFM)/Palatino-Roman.afm
	bdftops $(BDF)/pal18.bdf $(AFM)/Palatino-Roman.afm \
		$(FONTS)/pplr.gsf Palatino-Roman 4090000

$(FONTS)/pplri.gsf: $(BDF)/pplri.bdf $(AFM)/Palatino-Italic.afm
	bdftops $(BDF)/pplri.bdf $(AFM)/Palatino-Italic.afm \
		$(FONTS)/pplri.gsf Palatino-Italic 4090100

$(FONTS)/pplb.gsf: $(BDF)/pplb.bdf $(AFM)/Palatino-Bold.afm
	bdftops $(BDF)/pplb.bdf $(AFM)/Palatino-Bold.afm \
		$(FONTS)/pplb.gsf Palatino-Bold 4091000

$(FONTS)/pplbi.gsf: $(BDF)/pplbi.bdf $(AFM)/Palatino-BoldItalic.afm
	bdftops $(BDF)/pplbi.bdf $(AFM)/Palatino-BoldItalic.afm \
		$(FONTS)/pplbi.gsf Palatino-BoldItalic 4091100

# ---------------- Symbol ----------------

Symbol: $(FONTS)/psyr.gsf

$(FONTS)/psyr.gsf: $(BDF)/symb24.bdf $(AFM)/symbol.afm
	bdftops $(BDF)/symb24.bdf $(AFM)/symbol.afm \
		$(FONTS)/psyr.gsf Symbol 4050000 SymbolEncoding

# ---------------- Times Roman ----------------

TimesRoman: $(FONTS)/ptmr.gsf $(FONTS)/ptmri.gsf $(FONTS)/ptmb.gsf \
	$(FONTS)/ptmbi.gsf

$(FONTS)/ptmr.gsf: $(BDF)/timR24.bdf $(AFM)/times_roman.afm
	bdftops $(BDF)/timR24.bdf $(AFM)/times_roman.afm \
		$(FONTS)/ptmr.gsf Times-Roman 4060000

$(FONTS)/ptmri.gsf: $(BDF)/timI24.bdf $(AFM)/times_italic.afm
	bdftops $(BDF)/timI24.bdf $(AFM)/times_italic.afm \
		$(FONTS)/ptmri.gsf Times-Italic 4060100

$(FONTS)/ptmb.gsf: $(BDF)/timB24.bdf $(AFM)/times_bold.afm
	bdftops $(BDF)/timB24.bdf $(AFM)/times_bold.afm \
		$(FONTS)/ptmb.gsf Times-Bold 4061000

$(FONTS)/ptmbi.gsf: $(BDF)/timBI24.bdf $(AFM)/times_bolditalic.afm
	bdftops $(BDF)/timBI24.bdf $(AFM)/times_bolditalic.afm \
		$(FONTS)/ptmbi.gsf Times-BoldItalic 4061100

# ---------------- Ugly ----------------

# This font, and only this font, is stored in the main executable
# directory for Ghostscript, not the fonts directory.

Ugly: uglyr.gsf

uglyr.gsf: ugly10.bdf
	bdftops ugly10.bdf uglyr.gsf Ugly 4000000

# ---------------- Utopia ----------------
# (Already in Type 1 form.)

Utopia: $(FONTS)/utrg.pfa $(FONTS)/uti.pfa $(FONTS)/utb.pfa \
	$(FONTS)/utbi.pfa

# ---------------- Zapf Chancery ----------------

ZapfChancery: $(FONTS)/zcr.gsf $(FONTS)/zcri.gsf $(FONTS)/zcro.gsf \
	$(FONTS)/zcb.gsf

$(FONTS)/zcr.gsf: $(BDF)/zcr24.bdf $(AFM)/ZapfChancery-Roman.afm
	bdftops $(BDF)/zcr24.bdf $(AFM)/ZapfChancery-Roman.afm \
		$(FONTS)/zcr.gsf ZapfChancery 4070000

# We fake italic with oblique, but use the italic metrics.
$(FONTS)/zcri.gsf: $(BDF)/zcro24.bdf $(AFM)/ZapfChancery-MediumItalic.afm
	bdftops $(BDF)/zcro24.bdf $(AFM)/ZapfChancery-MediumItalic.afm \
		$(FONTS)/zcro.gsf ZapfChancery-MediumItalic 4070100

$(FONTS)/zcro.gsf: $(BDF)/zcro24.bdf $(AFM)/ZapfChancery-MediumItalic.afm
	bdftops $(BDF)/zcro24.bdf $(AFM)/ZapfChancery-MediumItalic.afm \
		$(FONTS)/zcro.gsf ZapfChancery-Oblique 4070200

$(FONTS)/zcb.gsf: $(BDF)/zcb30.bdf $(AFM)/ZapfChancery-Bold.afm
	bdftops $(BDF)/zcb30.bdf $(AFM)/ZapfChancery-Bold.afm \
		$(FONTS)/zcb.gsf ZapfChancery-Bold 4071000

# ---------------- Zapf Dingbats ----------------

ZapfDingbats: $(FONTS)/pzdr.gsf

$(FONTS)/pzdr.gsf: $(BDF)/pzdr.bdf $(AFM)/ZapfDingbats.afm
	bdftops $(BDF)/pzdr.bdf $(AFM)/ZapfDingbats.afm \
		$(FONTS)/pzdr.gsf ZapfDingbats 4100000
