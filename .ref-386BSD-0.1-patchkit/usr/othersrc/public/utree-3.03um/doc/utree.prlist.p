%!PS-Adobe-3.0
%%Creator: groff version 1.03
%%DocumentNeededResources: font Times-Roman
%%+ font Times-Bold
%%+ font Times-Italic
%%DocumentSuppliedResources: procset grops 1.03 0
%%Pages: 1
%%PageOrder: Ascend
%%Orientation: Portrait
%%EndComments
%%BeginProlog
%%BeginResource: procset grops 1.03 0

/setpacking where {
	pop
	currentpacking
	true setpacking
} if

/grops 120 dict dup begin 

% The ASCII code of the space character.
/SC 32 def

/A /show load def
/B { 0 SC 3 -1 roll widthshow } bind def
/C { 0 exch ashow } bind def
/D { 0 exch 0 SC 5 2 roll awidthshow } bind def
/E { 0 rmoveto show } bind def
/F { 0 rmoveto 0 SC 3 -1 roll widthshow } bind def
/G { 0 rmoveto 0 exch ashow } bind def
/H { 0 rmoveto 0 exch 0 SC 5 2 roll awidthshow } bind def
/I { 0 exch rmoveto show } bind def
/J { 0 exch rmoveto 0 SC 3 -1 roll widthshow } bind def
/K { 0 exch rmoveto 0 exch ashow } bind def
/L { 0 exch rmoveto 0 exch 0 SC 5 2 roll awidthshow } bind def
/M { rmoveto show } bind def
/N { rmoveto 0 SC 3 -1 roll widthshow } bind def
/O { rmoveto 0 exch ashow } bind def
/P { rmoveto 0 exch 0 SC 5 2 roll awidthshow } bind def
/Q { moveto show } bind def 
/R { moveto 0 SC 3 -1 roll widthshow } bind def
/S { moveto 0 exch ashow } bind def
/T { moveto 0 exch 0 SC 5 2 roll awidthshow } bind def

% name size font SF -

/SF {
	findfont exch
	[ exch dup 0 exch 0 exch neg 0 0 ] makefont
	dup setfont
	[ exch /setfont cvx ] cvx bind def
} bind def

% name a c d font MF -

/MF {
	findfont
	[ 5 2 roll
	0 3 1 roll % b
	neg 0 0 ] makefont
	dup setfont
	[ exch /setfont cvx ] cvx bind def
} bind def

/level0 0 def
/RES 0 def
/PL 0 def
/LS 0 def

% BP -

/BP {
	/level0 save def
	1 setlinecap
	1 setlinejoin
	72 RES div dup scale
	LS {
		90 rotate
	} {
		0 PL translate
	} ifelse
	1 -1 scale
} bind def

/EP {
	level0 restore
	showpage
} bind def


% centerx centery radius startangle endangle DA -

/DA {
	newpath arcn stroke
} bind def

% x y SN - x' y'
% round a position to nearest (pixel + (.25,.25))

/SN {
	transform 
	.25 sub exch .25 sub exch
	round .25 add exch round .25 add exch
	itransform
} bind def
	
% endx endy startx starty DL -
% we round the endpoints of the line, so that parallel horizontal
% and vertical lines will appear even

/DL {
	SN
	moveto
	SN
	lineto stroke
} bind def

% centerx centery radius DC -

/DC {
	newpath 0 360 arc closepath
} bind def


/TM matrix def

%  width height centerx centery DE -

/DE {
	TM currentmatrix pop
	translate scale newpath 0 0 .5 0 360 arc closepath
	TM setmatrix
} bind def

% these are for splines

/RC /rcurveto load def
/RL /rlineto load def
/ST /stroke load def
/MT /moveto load def
/CL /closepath load def

% fill the last path

% amount FL -

/FL {
	currentgray exch setgray fill setgray
} bind def

% fill with the ``current color''

/BL /fill load def

/LW /setlinewidth load def
% new_font_name encoding_vector old_font_name RE -

/RE {
	findfont
	dup maxlength dict begin
	{
		1 index /FID ne { def } { pop pop } ifelse
	} forall
	/Encoding exch def
	dup /FontName exch def
	currentdict end definefont pop
} bind def

/DEFS 0 def

% hpos vpos EBEGIN -

/EBEGIN {
	moveto
	DEFS begin
} bind def

/EEND /end load def

/CNT 0 def
/level1 0 def

% llx lly newwid wid newht ht newllx newlly PBEGIN -

/PBEGIN {
	/level1 save def
	translate
	div 3 1 roll div exch scale
	neg exch neg exch translate
	% set the graphics state to default values
	0 setgray
	0 setlinecap
	1 setlinewidth
	0 setlinejoin
	10 setmiterlimit
	[] 0 setdash
	/setstrokeadjust where {
		pop
		false setstrokeadjust
	} if
	/setoverprint where {
		pop
		false setoverprint
	} if
	newpath
	/CNT countdictstack def
	/showpage {} def
} bind def

/PEND {
	clear
	countdictstack CNT sub { end } repeat
	level1 restore
} bind def

end def

/setpacking where {
	pop
	setpacking
} if
%%EndResource
%%IncludeResource: font Times-Roman
%%IncludeResource: font Times-Bold
%%IncludeResource: font Times-Italic
grops begin/DEFS 1 dict def DEFS begin/u{.001 mul}bind def end/RES 72 def/PL
841.89 def/LS false def/ENC0[/asciicircum/asciitilde/Scaron/Zcaron/scaron
/zcaron/Ydieresis/trademark/.notdef/.notdef/.notdef/.notdef/.notdef/.notdef
/.notdef/.notdef/.notdef/.notdef/.notdef/.notdef/.notdef/.notdef/.notdef
/.notdef/.notdef/.notdef/.notdef/.notdef/.notdef/.notdef/.notdef/.notdef/space
/exclam/quotedbl/numbersign/dollar/percent/ampersand/quoteright/parenleft
/parenright/asterisk/plus/comma/hyphen/period/slash/zero/one/two/three/four
/five/six/seven/eight/nine/colon/semicolon/less/equal/greater/question/at/A/B/C
/D/E/F/G/H/I/J/K/L/M/N/O/P/Q/R/S/T/U/V/W/X/Y/Z/bracketleft/backslash
/bracketright/circumflex/underscore/quoteleft/a/b/c/d/e/f/g/h/i/j/k/l/m/n/o/p/q
/r/s/t/u/v/w/x/y/z/braceleft/bar/braceright/tilde/.notdef/quotesinglbase
/guillemotleft/guillemotright/bullet/florin/fraction/perthousand/dagger
/daggerdbl/endash/emdash/ff/fi/fl/ffi/ffl/dotlessi/dotlessj/grave/hungarumlaut
/dotaccent/breve/caron/ring/ogonek/quotedblleft/quotedblright/oe/lslash
/quotedblbase/OE/Lslash/.notdef/exclamdown/cent/sterling/currency/yen/brokenbar
/section/dieresis/copyright/ordfeminine/guilsinglleft/logicalnot/minus
/registered/macron/degree/plusminus/twosuperior/threesuperior/acute/mu
/paragraph/periodcentered/cedilla/onesuperior/ordmasculine/guilsinglright
/onequarter/onehalf/threequarters/questiondown/Agrave/Aacute/Acircumflex/Atilde
/Adieresis/Aring/AE/Ccedilla/Egrave/Eacute/Ecircumflex/Edieresis/Igrave/Iacute
/Icircumflex/Idieresis/Eth/Ntilde/Ograve/Oacute/Ocircumflex/Otilde/Odieresis
/multiply/Oslash/Ugrave/Uacute/Ucircumflex/Udieresis/Yacute/Thorn/germandbls
/agrave/aacute/acircumflex/atilde/adieresis/aring/ae/ccedilla/egrave/eacute
/ecircumflex/edieresis/igrave/iacute/icircumflex/idieresis/eth/ntilde/ograve
/oacute/ocircumflex/otilde/odieresis/divide/oslash/ugrave/uacute/ucircumflex
/udieresis/yacute/thorn/ydieresis]def/Times-Italic@0 ENC0/Times-Italic RE
/Times-Bold@0 ENC0/Times-Bold RE/Times-Roman@0 ENC0/Times-Roman RE
%%EndProlog
%%Page: 1 1
%%BeginPageSetup
BP
%%EndPageSetup
/F0 10/Times-Roman@0 SF 293.84(UTREE.PRLIST\(1L\) UTREE.PRLIST\(1L\))72 48 R/F1
9/Times-Bold@0 SF(NAME)72 84 Q/F2 10/Times-Bold@0 SF(utr)108 96 Q(ee.prlist)
-.18 E F0 2.5<ad46>2.5 G(ilter)171.51 96 Q F2(utr)2.5 E(ee)-.18 E F0
(tree list \214les)2.5 E F1(SYNOPSIS)72 112.8 Q F0
(utree.prlist [options] list\214le)108 124.8 Q F1(DESCRIPTION)72 141.6 Q F2
(utr)108 153.6 Q(ee.prlist)-.18 E F0 .193
(reads a formatted directory tree list \214le)2.694 F F2(list\214le)2.693 E F0
.193(previously created from within)2.693 F F2(utr)2.693 E(ee)-.18 E F0 .193
(on the)2.693 F/F3 10/Times-Italic@0 SF(tr)2.693 E(ee)-.37 E(scr)108 165.6 Q
(een)-.37 E F0 .943(with the out-command \()3.443 F F2(o)A F0 3.443(\)a)C .943
(nd the list-option \()252.595 165.6 R F2(l)A F0 3.444(\). The)B .944
(tree list can be converted to various output)3.444 F(formats for dif)108 177.6
Q(ferent output devices and printers \(see below\).)-.18 E
(The converted tree list is written to stdout.)5 E F1(OPTIONS)72 194.4 Q F0
(The following command line options are interpreted by)108 206.4 Q F2(utr)2.5 E
(ee.prlist)-.18 E F0(:)A F2(-T dev)108 235.2 Q F0(Create output for device)188
235.2 Q F2(dev)2.5 E F0 5(.C)C(urrently supported devices see below)316.59
235.2 Q(.)-.65 E F2(-V)108 252 Q F0(Display program version and exit.)188 252 Q
F2(-d dev)108 268.8 Q F0(Create output for device)188 268.8 Q F2(dev)2.5 E F0 5
(.C)C(urrently supported devices see below)316.59 268.8 Q(.)-.65 E F2(-f fnt)
108 285.6 Q F0(Use font)188 285.6 Q F2(fnt)2.5 E F0 5(.T)C
(his option is meaningful only for postscript \(default: Courier)250.49 285.6 Q
(-Bold\).)-.2 E F2(-h)108 302.4 Q F0
(Display some help about usage and options.)188 302.4 Q F2(-i ind)108 319.2 Q
F0(Set tree indention to)188 319.2 Q F2(ind)2.5 E F0
(columns \(3 .. 9, default: 6\).)2.5 E F2(-s siz)108 336 Q F0(Use font size)188
336 Q F2(siz)2.5 E F0 5(.T)C
(his option is meaningful only for postscript \(default: 10\).)267.43 336 Q F1
(OUTPUT FORMA)72 352.8 Q(TS)-.666 E F0
(Currently supported output formats or devices are:)108 364.8 Q F2(ascii)108
393.6 Q F0(ASCII using graphical meta characters \(-|+\).)188 393.6 Q F2(850)
108 410.4 Q F0(Printers supporting the IBM international character set PC850.)
188 410.4 Q F2(ps)108 427.2 Q F0(ADOBE POSTSCRIPT printers or previewers.)188
427.2 Q F2(term)108 444 Q F0 -.7(Te)188 444 S
(rminals using graphical characters if de\214ned \(default\).).7 E F1(SEE ALSO)
72 465.6 Q F0(utree\(1L\))108 477.6 Q F1(AUTHOR)72 494.4 Q F0(Peter Klingebiel)
108 506.4 Q F1(COPYRIGHT)72 523.2 Q F0 2.5<a931>108 535.2 S
(991/92 Peter Klingebiel & UNIX Magazin Munich)123.1 535.2 Q .204
(Permission is granted to copy and distribute)108 559.2 R F2(utr)2.703 E(ee)
-.18 E F0 .203(in modi\214ed or unmodi\214ed form, for noncommercial use,)2.703
F .594(provided \(a\) this copyright notice is preserved, \(b\) no attempt is \
made to restrict redistribution of this \214le,)108 571.2 R .021(and \(c\) thi\
s \214le is not distributed as part of any collection whose redistribution is \
restricted by a compilation)108 583.2 R(copyright.)108 595.2 Q(March 6 1992)72
768 Q(UTREE V)255.31 768 Q(ersion 3.03-um)-1.11 E(1)535 768 Q EP
%%Trailer
end
%%EOF
