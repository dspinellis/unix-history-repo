# lib/ditroff.font/afmdit.awk
#
# Copyright (c) 1984, 1985 Adobe Systems Incorporated. All Rights Reserved.
#
# RCSID: $Header: afmdit.awk,v 2.1 85/11/24 12:25:05 shore Rel $
#
# This awk(1) program helps build ditroff font description
# files (for input to makedev/devconfig).  It parses
# Adobe's AFM PostScript font metrics files.
# 
#
# The awk program presented here is rather hairy, but documented
# throughout.  It started out as a simple idea but got a little
# out of hand, a redesign is in order, but this version works
# just fine so far.
#
# It works on canonical AFM files, home brew ones are not guarenteed,
# as there is little hope for error detection and recovery.
#
# Lots of the extra character definitions (2 letter codes) come from
#	"Adventures with Typesetter-Independent Troff"
#	by Mark Kahrs and Lee Moore, Department of Computer Science
#	Technical Report TR 159, June 1985
#	University of Rochester, Rochester NY  14627
# I thank them for their efforts.
#
# PostScript is a trademark of Adobe Systems Incorporated.
#
# last edit: Fri Nov 15 10:44:34 1985
#
# RCSLOG:
# $Log:	afmdit.awk,v $
# Revision 2.1  85/11/24  12:25:05  shore
# Product Release 2.0
# 
# 

BEGIN {
	MetricsVersion = 1.0	# afm format version to match
	SCALE = 5.0		# font width scale factor to 
				# achieve proper dynamic range
				# (ditroff widths are <= 256)
	fudge = 10		# ascender/descender fudge factor
				# a character is judged to be an 
				# ascender (descender) if its bounding
				# box is within fudge of the published
				# ascender (descender) numbers for this
				# font

	# file names for pieces. These are cat'ed together in a shell script.
	header = "temp.header"	# comments and ligature list
	spaces = "temp.spaces"	# space widths and "charset" header
	trailer = "temp.trailer"# actual character data
	aux = "temp.aux" 	# PS width aux file for psdit

	isspecial = 0		# font is the FIRST special font (S)
	isfixedpitch = 0	# font is a typewriter font
	istext = 1		# font is a text font (not special)

# ditmap is a mapping from non-ascii PostScript character names
#	(plus a few special ones) to DITROFF \( special codes.
#	Note that some chars have more than one code (separated by spaces).
#	If we ever determine some extra mappings, or actually digitize
#	some full ditroff fonts, this will have to change.
#	Play with it if you like, but the results may be gruesome.

	ditmap[".ditspace"] = "\\^ \\| \\&"
	ditmap["AE"] = "AE"
	ditmap["Alpha"] = "*A"
	ditmap["Beta"] = "*B"
	ditmap["Chi"] = "*X"
	ditmap["Delta"] = "*D"
	ditmap["Epsilon"] = "*E"
	ditmap["Eta"] = "*Y"
	ditmap["Gamma"] = "*G"
	ditmap["Iota"] = "*I"
	ditmap["Kappa"] = "*K"
	ditmap["Lambda"] = "*L"
	ditmap["Lslash"] = "PL"
	ditmap["Mu"] = "*M"
	ditmap["Nu"] = "*N"
	ditmap["OE"] = "OE"
	ditmap["Omega"] = "*W"
	ditmap["Omicron"] = "*O"
	ditmap["Oslash"] = "O/"
	ditmap["Phi"] = "*F"
	ditmap["Pi"] = "*P"
	ditmap["Psi"] = "*Q"
	ditmap["Rho"] = "*R"
	ditmap["Sigma"] = "*S"
	ditmap["Tau"] = "*T"
	ditmap["Theta"] = "*H"
	ditmap["Upsilon"] = "*U"
	ditmap["Xi"] = "*C"
	ditmap["Zeta"] = "*Z"
	ditmap["acute"] = "aa \\'"
	ditmap["ae"] = "ae"
	ditmap["aleph"] = "al"
	ditmap["alpha"] = "*a"
	ditmap["angleleft"] = "l<"
	ditmap["angleright"] = "r>"
	ditmap["approxequal"] = "~="
	ditmap["arrowboth"] = "<>"
	ditmap["arrowdblboth"] = "io"
	ditmap["arrowdblleft"] = "<: lh"	# left double arrow (& hand)
	ditmap["arrowdblright"] = ":> im rh"	# right double arrow (& hand)
	ditmap["arrowdown"] = "da"
#	ditmap["arrowleft"] = "<-"	# see procs
#	ditmap["arrowright"] = "->"	# see procs
	ditmap["arrowup"] = "ua"
	ditmap["asteriskmath"] = "**"
	ditmap["bar"] = "or"
	ditmap["beta"] = "*b"
	ditmap["breve"] = "be"
	ditmap["caron"] = "hc"
	ditmap["carriagereturn"] = "cr"
	ditmap["cedilla"] = "cd"
	ditmap["cent"] = "ct"
	ditmap["chi"] = "*x"
	ditmap["circlemultiply"] = "ax"
	ditmap["circleplus"] = "a+"
	ditmap["circumflex"] = "^"	# see ascii
	ditmap["copyrightserif"] = "co"
	ditmap["dagger"] = "dg"
	ditmap["daggerdbl"] = "dd"
	ditmap["degree"] = "de"
	ditmap["delta"] = "*d"
	ditmap["diamond"] = "dm"
	ditmap["dieresis"] = "um .."	# umlaut
	ditmap["divide"] = "di"
	ditmap["dotaccent"] = "dt"
	ditmap["dotlessi"] = "ui"
	ditmap["dotmath"] = "m."
	ditmap["element"] = "mo cm"
	ditmap["emdash"] = "em"
	ditmap["emptyset"] = "es"
	ditmap["endash"] = "en"
	ditmap["epsilon"] = "*e"
	ditmap["equal"] = "eq"	;	mathonly["eq"] = "equal"
#	ditmap["equivalence"] = "=="	# see procs
	ditmap["eta"] = "*y"
	ditmap["exclamdown"] = "!! I!"
	ditmap["existential"] = "te"
	ditmap["ff"] = "ff"
	ditmap["ffi"] = "Fi"
	ditmap["ffl"] = "Fl"
	ditmap["fi"] = "fi"
	ditmap["fl"] = "fl"
	ditmap["florin"] = "$D"
	ditmap["gamma"] = "*g"
	ditmap["germandbls"] = "ss"
	ditmap["gradient"] = "gr"
	ditmap["grave"] = "ga \\`"
	ditmap["greaterequal"] = ">="
	ditmap["guillemotleft"] = "d<"
	ditmap["guillemotright"] = "d>"
	ditmap["heart"] = "bs"		# bell system logo
	ditmap["hyphen"] = "hy"
	ditmap["infinity"] = "if"
#	ditmap["integral"] = "is"	# see procs
	ditmap["intersection"] = "ca"
	ditmap["iota"] = "*i"
	ditmap["kappa"] = "*k"
	ditmap["lambda"] = "*l"
	ditmap["lessequal"] = "<="
	ditmap["logicaland"] = "an la"
	ditmap["logicalnot"] = "no"
	ditmap["logicalor"] = "lo"
	ditmap["lslash"] = "Pl"
	ditmap["macron"] = "mc ma"
	ditmap["minus"] = "\\- mi"
	ditmap["minute"] = "fm mt"
	ditmap["mu"] = "*m"
	ditmap["multiply"] = "mu"
	ditmap["notelement"] = "!m"
	ditmap["notequal"] = "!="
	ditmap["notsubset"] = "!s"
	ditmap["nu"] = "*n"
	ditmap["oe"] = "oe"
	ditmap["ogonek"] = "og"
	ditmap["omega"] = "*w"
	ditmap["omicron"] = "*o"
	ditmap["oslash"] = "o/"
	ditmap["paragraph"] = "pp"
	ditmap["partialdiff"] = "pd"
	ditmap["perpendicular"] = "bt"
	ditmap["perthousand"] = "pm"
	ditmap["phi"] = "*f"
	ditmap["pi"] = "*p"
	ditmap["plus"] = "pl"	;	mathonly["pl"] = "plus"
	ditmap["plusminus"] = "+-"
	ditmap["propersubset"] = "sb"
	ditmap["propersuperset"] = "sp"
	ditmap["proportional"] = "pt"
	ditmap["psi"] = "*q"
	ditmap["questiondown"] = "?? I?"
	ditmap["quotedblleft"] = "lq"
	ditmap["quotedblright"] = "rq"
	ditmap["quotesingle"] = "n'"
#	ditmap["radical"] = "sr"	# see procs
	ditmap["reflexsubset"] = "ib"
	ditmap["reflexsuperset"] = "ip"
	ditmap["registerserif"] = "rg"
	ditmap["rho"] = "*r"
	ditmap["ring"] = "ri"
	ditmap["second"] = "sd"
	ditmap["section"] = "sc"
	ditmap["sigma"] = "*s"
	ditmap["sigma1"] = "ts"
	ditmap["similar"] = "ap"
	ditmap["slash"] = "sl"
	ditmap["sterling"] = "ps po"
	ditmap["tau"] = "*t"
	ditmap["therefore"] = "tf"
	ditmap["theta"] = "*h"
	ditmap["tilde"] = "~"		# see ascii
	ditmap["trademarkserif"] = "tm"
	ditmap["underscore"] = "\\_"
	ditmap["union"] = "cu"
	ditmap["universal"] = "fa"
	ditmap["upsilon"] = "*u"
	ditmap["xi"] = "*c"
	ditmap["yen"] = "yi yn $J"
	ditmap["zeta"] = "*z"

# hack font, chars have their troff names

	ditmap["br"] = "br"	# box rule
	ditmap["bu"] = "bu"	# bullet
	ditmap["bv"] = "bv"	# bold vertical
	ditmap["bx"] = "bx"	# box
	ditmap["ci"] = "ci"	# circle
	ditmap["lb"] = "lb"	# left bot curly
	ditmap["lc"] = "lc"	# left ceil
	ditmap["lf"] = "lf"	# left floor
	ditmap["lk"] = "lk"	# left center curly
	ditmap["lt"] = "lt"	# left top curly
	ditmap["ob"] = "ob"	# outline bullet
	ditmap["rb"] = "rb"	# right bot curly
	ditmap["rc"] = "rc"	# right ceil
	ditmap["rf"] = "rf"	# right floor
	ditmap["rk"] = "rk"	# right center curly
	ditmap["rn"] = "rn"	# root extender
	ditmap["rt"] = "rt"	# rith top curly
	ditmap["ru"] = "ru"	# rule
	ditmap["sq"] = "sq"	# square
	ditmap["ul"] = "ul"	# under rule
	ditmap["vr"] = "vr"	# vertical rule
	
	

# >>>>> IMPORTANT NOTE! <<<<<
# if you edit these, make sure you supply char codes and widths
# below (in proc[]) and make sure you define the proc in the 
# PostScript prolog

	ditmap[".proctext"] = "14 12 34 18 38 58 78 13 23"
	ditmap[".procspecial"] = "is sr -> <- =="

	# character-code and width info for synthetic characters

	cc = 129	# manufacture unique character codes
	proc["14"] = cc++ " 0 833"	# 1/4
	proc["12"] = cc++ " 0 833"	# 1/2
	proc["34"] = cc++ " 0 833"	# 3/4
	proc["18"] = cc++ " 0 833"	# 1/8
	proc["38"] = cc++ " 0 833"	# 3/8
	proc["58"] = cc++ " 0 833"	# 5/8
	proc["78"] = cc++ " 0 833"	# 7/8
	proc["13"] = cc++ " 0 833"	# 1/3
	proc["23"] = cc++ " 0 833"	# 2/3

#	proc["mi"] = cc++ " 0 549"	# minus

	proc["sr"] = "214 0 549"	# square root 
	proc["is"] = "242 3 274"	# integral
	proc["->"] = "174 0 987"        # arrow right
	proc["<-"] = "172 0 987"        # arrow left
	proc["=="] = "186 0 549"        # equivalence

# L. and l. are used for line drawing on systems without graphics

# ascii is a mapping which contains the PostScript character names
#	for the (printable) ascii characters.  The values are the ascii
#	character codes (not used in this program).  We just test to
#	see if a name is in the table.
#	ditroff (unlike Adobe) thinks that the ascii ^ and ~ are the accents
#	hence we must leave out asciicircum and asciitilde

	ascii["space"] = 32
	ascii["exclam"] = 33
	ascii["quotedbl"] = 34
	ascii["numbersign"] = 35
	ascii["dollar"] = 36
	ascii["percent"] = 37
	ascii["ampersand"] = 38
	ascii["quoteright"] = 39
	ascii["parenleft"] = 40
	ascii["parenright"] = 41
	ascii["asterisk"] = 42
	ascii["plus"] = 43
	ascii["comma"] = 44
	ascii["hyphen"] = 45
	ascii["period"] = 46
	ascii["slash"] = 47
	ascii["zero"] = 48
	ascii["one"] = 49
	ascii["two"] = 50
	ascii["three"] = 51
	ascii["four"] = 52
	ascii["five"] = 53
	ascii["six"] = 54
	ascii["seven"] = 55
	ascii["eight"] = 56
	ascii["nine"] = 57
	ascii["colon"] = 58
	ascii["semicolon"] = 59
	ascii["less"] = 60
	ascii["equal"] = 61
	ascii["greater"] = 62
	ascii["question"] = 63
	ascii["at"] = 64
	ascii["A"] = 65
	ascii["B"] = 66
	ascii["C"] = 67
	ascii["D"] = 68
	ascii["E"] = 69
	ascii["F"] = 70
	ascii["G"] = 71
	ascii["H"] = 72
	ascii["I"] = 73
	ascii["J"] = 74
	ascii["K"] = 75
	ascii["L"] = 76
	ascii["M"] = 77
	ascii["N"] = 78
	ascii["O"] = 79
	ascii["P"] = 80
	ascii["Q"] = 81
	ascii["R"] = 82
	ascii["S"] = 83
	ascii["T"] = 84
	ascii["U"] = 85
	ascii["V"] = 86
	ascii["W"] = 87
	ascii["X"] = 88
	ascii["Y"] = 89
	ascii["Z"] = 90
	ascii["bracketleft"] = 91
	ascii["backslash"] = 92
	ascii["bracketright"] = 93
#	ascii["asciicircum"] = 94	# 195 see ditmap, should be ascii!
	ascii["underscore"] = 95
	ascii["quoteleft"] = 96
	ascii["a"] = 97
	ascii["b"] = 98
	ascii["c"] = 99
	ascii["d"] = 100
	ascii["e"] = 101
	ascii["f"] = 102
	ascii["g"] = 103
	ascii["h"] = 104
	ascii["i"] = 105
	ascii["j"] = 106
	ascii["k"] = 107
	ascii["l"] = 108
	ascii["m"] = 109
	ascii["n"] = 110
	ascii["o"] = 111
	ascii["p"] = 112
	ascii["q"] = 113
	ascii["r"] = 114
	ascii["s"] = 115
	ascii["t"] = 116
	ascii["u"] = 117
	ascii["v"] = 118
	ascii["w"] = 119
	ascii["x"] = 120
	ascii["y"] = 121
	ascii["z"] = 122
	ascii["braceleft"] = 123
	ascii["bar"] = 124
	ascii["braceright"] = 125
#	ascii["asciitilde"] = 126	# 196 see ditmap, should be ascii !
	} # BEGIN

# look at .map files to learn about "special" (sigh)

FILENAME ~ /^S\.map$/ {
	isspecial = 1
	istext = 0
	next
	}

FILENAME ~ /^SS\.map$/ {
	isspecial = 0
	istext = 0
	next
	}

FILENAME ~ /.*\.map$/ {next}


# Now define the actions on the fields in real AFM format
#	Note that we generate 3 files which are cat'ed together
#	outside this program.  This is to avoid scanning the
#	afm file more than once.  Most of the header stuff is ignored.

/^StartFontMetrics /	{if ($2 != MetricsVersion) {
				print "ERROR! Metrics Version mismatch"
				exit 2 # does this get passed out ?
				}
			next
			}

/^Comment Copyright/	{print "# " $0 > header
			next
			}

/^Comment /	{next}

/^FontName /	{fontname = $2
		print "# PostScript " fontname " from " FILENAME > header
		print "# PostScript is a trademark of Adobe Systems Incorporated" > header
		next
		}

/^FullName /	{next}

/^FamilyName /	{next}

/^Weight /	{next}

/^ItalicAngle /	{next}

/^IsFixedPitch /{if ($2 == "true") {isfixedpitch = 1}
		else {isfixedpitch = 0}
		next
		}

/^UnderlinePosition /{next}

/^UnderlineThickness /{next}

/^Version /	{next}

/^Notice /	{print "# " $0 >header
		next
		}

/^FontBBox /	{next}

/^CapHeight /	{capheight = $2
		next
		}

/^XHeight /	{xheight = $2
		next
		}

/^Descender /	{descender = $2
		next
		}

/^Ascender /	{ascender = $2
		next
		}

/^EncodingScheme /{next}

/^StartCharMetrics/	{printf "ligatures " > header
			if (capheight < ascender) {
				ascender = capheight
				}
			next}

/^C -1 ;/	{next}			# ignore unencoded chars

# now the hard part: the pattern for encoded characters.
# note the dependence on canonical form
# a more detailed parse of split($0,,";") might be better

# 1 2	   3 4  5      6 7 8  9 0 11     12     13     14     15   ???
/^C [0-9]* ; WX [0-9]* ; N .* ; B [-0-9]* [-0-9]* [-0-9]* [-0-9]* ;/ {
	charcode = $2
	width = $5
	charname = $8
	bblly = $12
	bbury = $14

#	parse ligatures
	n = split($0,line,";")
	for (i = 5; i < n ; i++) {
		if (line[i] ~ / L .* .* /) {
			d = split(line[i],ligs," ")
			printf "%s ", ligs[d] > header
			}
		}

#	compute width
	scaledwidth = int(0.5 + (width / SCALE))
	if ((scaledwidth < 0) || (scaledwidth > 256)) {
		print "ERROR! Width out of range!"
		exit 2
		}

#	handle space
	if (charname == "space") {		# special char widths
		spacewidth = scaledwidth
		if (isfixedpitch == 0) {
			em6 =  int (0.5 + (1000.0/6.0) / SCALE)
			em12 = int (0.5 + (1000.0/12.0) / SCALE)
		}
		else {
			em6 = spacewidth
			em12 = spacewidth
		}
		printf "spacewidth %d\n", spacewidth > spaces
		print "charset" > spaces
		printf "\\|\t%d 0 000\t1/6 em space\n", em6 >trailer
		printf "\\^\t%d 0 000\t1/12 em space\n", em12 > trailer
		printf "\\&\t00 0 000\tno space\n" > trailer
		next
		}

#	figure out ascender/descender flags (hack!?)
	ad = 0
	if (bblly - fudge <= descender) ad += 1
	if (bbury + fudge >= ascender) ad += 2

#	dump the description line
	if (length(ascii[charname]) > 0) {
		printf "%c\t%d %d 0%o\t%s\n", charcode, scaledwidth, ad, charcode, charname  > trailer
		# look for ditmap synonyms
		if (length(ditmap[charname]) > 0) {
			n = split(ditmap[charname],others," ")
			for (i = 1; i <= n; i++) {
				oth = others[i];
				if ((length(mathonly[oth]) > 0) && (isspecial != 1)) continue;
				printf "%s\t\"\n", others[i]  > trailer
				}
			}
		}
	else if (length(ditmap[charname]) > 0) {
		# not a printable ascii character
		n = split(ditmap[charname],others," ")
		printf "%s\t%d %d 0%o\t%s\n", others[1], scaledwidth, ad, charcode, charname  > trailer
		for (i = 2; i <= n; i++) {
			printf "%s\t\"\n", others[i]  > trailer
			}
		}

# dump a line for PS specific actual width/hack table

	printf "%d %d %d\n", charcode, width, specialflag > aux

	}

/^EndCharMetrics/{
	printf "0\n" > header

	# dump the "fudged" characters.
	for (i in proc) {
		p = proc[i]
		split(p,vals," ")
		scaledwidth = int(0.5 + (vals[3] / SCALE))
		if (((istext == 1) && (index(ditmap[".proctext"],i) != 0)) || ((isspecial == 1) && (index(ditmap[".procspecial"],i) != 0))) {
			printf "%s\t%d %d 0%o\tfudgedproc!\n", i, scaledwidth, vals[2], vals[1] > trailer

			printf "%d %d %d\n", vals[1], vals[3], 1 > aux
			}
		}
	next
	}

/^EndFontMetrics/{next}

END	{}
