%!PS-Adobe-3.0
%%Creator: groff version 1.03
%%DocumentNeededResources: font Times-Roman
%%+ font Times-Bold
%%+ font Times-Italic
%%DocumentSuppliedResources: procset grops 1.03 0
%%Pages: 14
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
/F0 10/Times-Roman@0 SF 365.52(UTREE\(1L\) UTREE\(1L\))72 48 R/F1 9
/Times-Bold@0 SF(NAME)72 84 Q/F2 10/Times-Bold@0 SF(utr)108 96 Q(ee)-.18 E F0
2.5<ad53>2.5 G(creen oriented \214lesystem browser and utility)146.23 96 Q F1
(SYNOPSIS)72 112.8 Q F0(utree [options] [rootdirectory])108 124.8 Q F1
(DESCRIPTION)72 141.6 Q F0 1.155
(The screen oriented \214lesystem browser and utility)108 153.6 R F2(utr)3.654
E(ee)-.18 E F0 1.154(descends the directory hierarchy rooted in your)3.654 F
1.42(home directory)108 165.6 R 3.92(,d)-.65 G 1.42
(e\214ned in a tree list \214le if given, or rooted in)181.01 165.6 R F2 -.18
(ro)3.921 G(otdir).18 E(ectory)-.18 E F0 1.421(if given, and displays the)3.921
F 1.076(directory tree on the so called)108 177.6 R/F3 10/Times-Italic@0 SF(tr)
3.576 E 1.076(ee scr)-.37 F(een)-.37 E F0 6.076(.O)C 3.576(nt)295.818 177.6 S
(he)307.174 177.6 Q F3(tr)3.576 E 1.076(ee scr)-.37 F(een)-.37 E F2(utr)3.576 E
(ee)-.18 E F0 1.075(lets you walk thru the tree, execute)3.576 F(some default \
and user de\214ned commands on it and change to any directory in the tree.)108
189.6 Q .492(If you have changed to a directory)108 201.6 R F2(utr)2.992 E(ee)
-.18 E F0 .493(displays all \214les in this directory on the so called)2.992 F
F3 .493(\214le scr)2.993 F(een)-.37 E F0 5.493(.O)C(n)535 201.6 Q(the)108 213.6
Q F3 .153(\214le scr)2.653 F(een)-.37 E F2(utr)2.653 E(ee)-.18 E F0 .153(lets \
you walk thru the \214le list and execute some default and user de\214ned comm\
ands on)2.653 F(\214les.)108 225.6 Q F1(OPTIONS)72 242.4 Q F0
(The following command line options are interpreted by)108 254.4 Q F2(utr)2.5 E
(ee)-.18 E F0(:)A F2(-L)108 283.2 Q F0(Follow symbolic links to directories)188
283.2 Q F2(-S)108 300 Q F0
(Ignore default minimal screen size of 80 columns and 24 lines)188 300 Q F2(-V)
108 316.8 Q F0(Display)188 316.8 Q F2(utr)2.5 E(ee)-.18 E F0
(version and copyright notice)2.5 E F2(-a)108 333.6 Q F0 1.316(Read in all dir\
ectories, including those that begin with a dot \(.\), which are normally)188
333.6 R(skipped)188 345.6 Q F2(-b)108 362.4 Q F0
(Suppress ringing of the bell \(useful for visual bells\))188 362.4 Q F2(-c)108
379.2 Q F0(Don')188 379.2 Q 2.5(td)-.18 G
(isplay and update a clock every second)218.65 379.2 Q F2(-d var=[val])108 396
Q F0(Set the variable)188 396 Q F2(var)2.5 E F0(to value)2.5 E F2(val)2.5 E F0
(or unset the variable)2.5 E F2(var)2.5 E(-d typ:[cmd])108 412.8 Q F0 .147
(De\214ne the command)188 412.8 R F2(cmd)2.647 E F0 .147(for the \214letype)
2.647 F F2(typ)2.646 E F0 .146(or unde\214ne any command for the \214letype)
2.646 F F2(typ)188 424.8 Q(-f lst)108 441.6 Q F0
(Build directory tree from list \214le)188 441.6 Q F2(lst)2.5 E(-g)108 458.4 Q
F0(Don')188 458.4 Q 2.5(tu)-.18 G(se graphical characters)218.65 458.4 Q F2(-h)
108 475.2 Q F0(Display usage and some help about options)188 475.2 Q F2(-i ind)
108 492 Q F0(Set the tree level indention to)188 492 Q F2(ind)2.5 E F0
(columns \(3..9\))2.5 E F2(-n)108 508.8 Q F0 -.74(Av)188 508.8 S
(oid scanning the tree for changes i.e. after shell escape).74 E F2(-l lev)108
525.6 Q F0(Build the tree up to level)188 525.6 Q F2(lev)2.5 E F0
(for quicker startup)2.5 E F2(-o)108 542.4 Q F0 .841(Omit saving changes in va\
riables and \214letype command de\214nitions, command history)188 542.4 R
(and key bindings)188 554.4 Q F2(-p lin)108 571.2 Q F0(Use)188 571.2 Q F2(lin)
5.13 E F0 2.629
(lines for displaying \214lenames of the current directory on the)5.13 F F3(tr)
5.129 E 2.629(ee scr)-.37 F(een)-.37 E F0(\(default: 3\))188 583.2 Q F2(-q)108
600 Q F0(Build the tree up to level 2 \(like -l 2\))188 600 Q F2(-r)108 616.8 Q
F0(Build the tree scanning and reading the \214le system instead of reading a \
tree list \214le)188 616.8 Q F2(-s)108 633.6 Q F0(Don')188 633.6 Q 2.5(tu)-.18
G(se hardware scrolling)218.65 633.6 Q F2(-u)108 650.4 Q F0(Read and update al\
l \214le lists after building the tree from a tree list \214le)188 650.4 Q F2
(-v mod)108 667.2 Q F0 .314(Set using video attributes to mode)188 667.2 R F2
(mod)2.815 E F0(.)A F2(mod)5.315 E F0 .315(may be)2.815 F F2(2)2.815 E F0 .315
(for all possible,)2.815 F F2(1)2.815 E F0 .315(for bold and)2.815 F
(underline and)188 679.2 Q F2(0)2.5 E F0(for none video attributes.)2.5 E F2
(-w)108 696 Q F0(Suppress warnings about unreadable directories)188 696 Q F2
(-x cmd)108 712.8 Q F0 .752(Use and execute the string)188 712.8 R F2(cmd)3.252
E F0 .752(as initial input at startup.)3.252 F .752(The string)5.752 F F2(cmd)
3.252 E F0 .751(is a simple)3.252 F(sequence of)188 724.8 Q F2(utr)2.5 E(ee)
-.18 E F0(commands)2.5 E(April 2 1992)72 768 Q(UTREE V)255.31 768 Q
(ersion 3.03-um)-1.11 E(1)535 768 Q EP
%%Page: 2 2
%%BeginPageSetup
BP
%%EndPageSetup
/F0 10/Times-Roman@0 SF 365.52(UTREE\(1L\) UTREE\(1L\))72 48 R .613
(The boolean options)108 84 R/F1 10/Times-Bold@0 SF(bcgnosw)3.113 E F0 .614
(and the numerical options)3.113 F F1(ipv)3.114 E F0 .614
(may also be pre-set in the environment vari-)3.114 F(able)108 96 Q F1(UTREE)
2.768 E F0 5.268(.E)C .268(.g. if you want to suppress ringing of the bell and\
 displaying and updating the clock and if)175.756 96 R
(you prefer a tree indention of 4 columns set the environment variable)108 108
Q F1(UTREE)2.5 E F0(to ')2.5 E F1(bci4)A F0 2.5('f)C(or this reason.)464.89 108
Q .192(Most of the options correspond to)108 120 R F1(utr)2.692 E(ee)-.18 E F0
.192(variables and therefore they may also be set or unset with the vari-)2.692
F(able command \()108 132 Q F1(=)A F0 2.5(\). See)B(the sections)2.5 E/F2 10
/Times-Italic@0 SF(global commands)2.5 E F0(and)2.5 E F2(variables)2.5 E F0
(below)2.5 E(.)-.65 E/F3 9/Times-Bold@0 SF(UTREE ST)72 148.8 Q(AR)-.666 E(TUP)
-.315 E F0(If)108 160.8 Q F1(utr)2.942 E(ee)-.18 E F0 .441
(is called without the optional command line parameter)2.942 F F1 -.18(ro)2.941
G(otdir).18 E(ectory)-.18 E F0 .441(or this directory is rooted in)2.941 F .737
(your home directory)108 172.8 R F1(utr)3.237 E(ee)-.18 E F0 .738
(tries to build up the directory tree reading a \214le)3.237 F F1(.utr)3.238 E
(eelist)-.18 E F0 .738(in your home direc-)3.238 F(tory)108 184.8 Q 3.48(,w)
-.65 G .98(hich contains a list of your directory tree created by a previous)
136.66 184.8 R F1(utr)3.479 E(ee)-.18 E F0 .979(session or by the additional)
3.479 F .675(shell script)108 196.8 R F1(utr)3.175 E(ee.mklist)-.18 E F0 .675
(called before.)3.175 F .675(If the command line option)5.675 F F1 .675(-f lst)
3.175 F F0 .675(is given)3.175 F F1(utr)3.175 E(ee)-.18 E F0 .675
(builds up the tree)3.175 F .942(from this list \214le)108 208.8 R F1(lst)3.442
E F0 .942(which may be created with the denoted shell script)3.442 F F1(utr)
3.441 E(ee.mklist)-.18 E F0 .941(or a command like)3.441 F F1(\214nd)108 220.8
Q F0 5.74(.E)C .74(ntries in such list \214les have to start with the director\
y separator / \(slash\) in the \214rst column of the)139.03 220.8 R 2.5
(line. All)108 232.8 R(other entries are ignored and skipped.)2.5 E .262(If bu\
ilding the tree from a list \214le the \214le lists of directories are created\
 and read in on demand only)108 256.8 R 2.761(,w)-.65 G(hich)522.78 256.8 Q
.483(means when a directory becomes the current directory by moving the cursor\
 to this directory)108 268.8 R 5.484(.T)-.65 G .484(his speeds)498.686 268.8 R
1.196(up the start of)108 280.8 R F1(utr)3.696 E(ee)-.18 E F0 1.196(because th\
ere is no need for scanning and reading the \214lesystem for subdirectories)
3.696 F(what may take some time for lar)108 292.8 Q(ger \214lesystems.)-.18 E
.981
(In all others cases, if no list \214le is given with the command line option)
108 316.8 R F1 .981(-f lst)3.481 F F0(,)A F1 -.18(ro)3.481 G(otdir).18 E
(ectory)-.18 E F0 .981(is rooted in)3.481 F .112
(your home directory and the \214le)108 328.8 R F1(.utr)2.612 E(eelist)-.18 E
F0 .112(is not found, or the command line option)2.612 F F1(-r)2.612 E F0 .112
(is given)2.612 F F1(utr)2.612 E(ee)-.18 E F0(builds)2.612 E .855
(up the tree by scanning and reading the \214lesystem recursively)108 340.8 R
5.855(.Y)-.65 G .855(ou can speed up the start with the com-)376.52 340.8 R
.687(mandline option)108 352.8 R F1 .687(-l lev)3.187 F F0 .687(which causes)
3.187 F F1(utr)3.187 E(ee)-.18 E F0 .687
(to build the initial directory tree only up to level)3.187 F F1(lev)3.186 E F0
.686(similar to)3.186 F .691(the option)108 364.8 R F1(-l)3.191 E F0 .691
(of the well known)3.191 F F1(\214nd)3.191 E F0 3.191(command. At)3.191 F .691
(runtime some commands let you expand the directory)3.191 F(tree later and ins\
pect directories and subtrees not visible at startup \(see below\).)108 376.8 Q
F3(SCREEN LA)72 393.6 Q(YOUT)-.828 E F0 .372
(The screen is divided into three regions.)108 405.6 R .372
(The top screen line, the so called)5.372 F F2 .372(echo line)2.872 F F0 .372
(is for displaying mes-)2.872 F .971(sages and entering input.)108 417.6 R .971
(The second screen line, the so called)5.971 F F2 .971(help line)3.471 F F0
.972(is for displaying help messages)3.471 F(and the)108 429.6 Q F1(utr)2.5 E
(ee)-.18 E F0(default or user de\214ned menu lines.)2.5 E .434
(The rest of the screen forms a window on the directory tree on the)108 441.6 R
F2(tr)2.934 E .434(ee scr)-.37 F(een)-.37 E F0 .434
(or on the \214le list of the cur)2.934 F(-)-.2 E .927(rent directory on the)
108 453.6 R F2 .927(\214le scr)3.427 F(een)-.37 E F0 5.927(.O)C 3.428(rh)
252.602 453.6 S 3.428(ei)264.36 453.6 S 3.428(su)275.008 453.6 S .928
(sed for displaying variable or \214letype commands settings, for)287.326 453.6
R(displaying help pages and displaying command outputs.)108 465.6 Q F3
(UTREE SCREENS AND MENUS)72 482.4 Q F1(Utr)108 494.4 Q(ee)-.18 E F0
(knows the following screens and menus:)2.5 E F1(tr)108 523.2 Q(ee scr)-.18 E
(een)-.18 E F0 .13(This is the initial screen displayed when)208 523.2 R F1
(utr)2.63 E(ee)-.18 E F0 .13(is started and has built up the direc-)2.63 F
1.905(tory tree.)208 535.2 R(The)6.905 E F2(tr)4.405 E 1.905(ee scr)-.37 F(een)
-.37 E F0 1.905(forms a window on the directory tree.)4.405 F 1.906
(The current)6.906 F .402(directory on which most commands are working is high\
lighted and the last screen)208 547.2 R .647(lines are used to display the \
\214rst \214les of the \214le list of the current directory)208 559.2 R 5.647
(.Y)-.65 G(ou)530 559.2 Q .473
(may move the window over the directory tree, enlar)208 571.2 R .473
(ge or shrink the directory tree)-.18 F .481(window or walk thru the directory\
 tree and execute default and user de\214ned com-)208 583.2 R .077
(mands on directories and subtrees.)208 595.2 R .076
(On the help line the most important commands)5.077 F .109
(available for directories or subtrees on the)208 607.2 R F2(tr)2.609 E .109
(ee scr)-.37 F(een)-.37 E F0 .11(are shown in the default)2.609 F F2(tr)2.61 E
(ee)-.37 E(menu)208 619.2 Q F0 6.968(.T)C 1.968
(he tree menu shows the names of the the)245.238 619.2 R F2(tr)4.467 E 1.967
(ee scr)-.37 F(een)-.37 E F0 1.967(commands, the)4.467 F 1.819
(keystrokes to invoke a command are marked with uppercase letters.)208 631.2 R
-1(Yo)6.819 G 4.32(um)1 G(ay)530.56 631.2 Q .05(switch the menu to a user de\
\214ned menu line displaying user de\214ned commands for)208 643.2 R(the)208
655.2 Q F2(tr)3.675 E 1.175(ee scr)-.37 F(een)-.37 E F0 6.175(.F)C 1.175
(or further information about all)282.715 655.2 R F2(tr)3.675 E 1.175(ee scr)
-.37 F(een)-.37 E F0 1.175(commands see the)3.675 F(sections)208 667.2 Q F2
1.639(global commands)4.139 F F0(,)A F2(tr)4.139 E 1.639(ee scr)-.37 F 1.639
(een commands)-.37 F F0(and)4.139 E F2(variables)4.139 E F0 1.639
(of this manual)4.139 F(page.)208 679.2 Q F1(\214le scr)108 708 Q(een)-.18 E F0
(The)208 708 Q F2 .975(\214le scr)3.475 F(een)-.37 E F0 .975
(forms a window on all \214les of the current directory)3.475 F 5.975(.T)-.65 G
.975(he current)498.765 708 R 1.779
(\214le on which most commands are working is highlighted.)208 720 R -1(Yo)
6.779 G 4.279(ym)1 G 1.779(ay move the)487.562 720 R(April 2 1992)72 768 Q
(UTREE V)255.31 768 Q(ersion 3.03-um)-1.11 E(2)535 768 Q EP
%%Page: 3 3
%%BeginPageSetup
BP
%%EndPageSetup
/F0 10/Times-Roman@0 SF 365.52(UTREE\(1L\) UTREE\(1L\))72 48 R 1.488(window ov\
er the \214le list or walk thru the \214le list and execute default and user)
208 84 R 1.28(de\214ned commands on \214les.)208 96 R 1.279
(On the help line the most important commands for)6.279 F .402
(\214les available on the)208 108 R/F1 10/Times-Italic@0 SF .402(\214le scr)
2.902 F(een)-.37 E F0 .403(are shown in the default)2.903 F F1 .403
(\214le menu)2.903 F F0 5.403(.T)C .403(he \214le menu)489.754 108 R 1.383
(displays the names of the the)208 120 R F1 1.382(\214le scr)3.882 F(een)-.37 E
F0 1.382(commands, the keystrokes to invoke a)3.882 F .599
(command are marked with uppercase letters.)208 132 R -1(Yo)5.599 G 3.099(um)1
G .599(ay switch the menu to a user)422.533 132 R .109
(de\214ne menu line displaying user de\214ned commands for the)208 144 R F1
.109(\214le scr)2.609 F(een)-.37 E F0 5.109(.F)C .109(or further)501.851 144 R
.32(information about all)208 156 R F1 .32(\214le scr)2.82 F(een)-.37 E F0 .32
(commands see the sections)2.82 F F1 .32(global commands)2.82 F F0(,)A F1
(\214le)2.82 E(scr)208 168 Q(een commands)-.37 E F0(and)2.5 E F1(variables)2.5
E F0(of this manual page.)2.5 E/F2 10/Times-Bold@0 SF(help scr)108 196.8 Q(een)
-.18 E F0 1.143(If you have switched to the)208 196.8 R F1 1.143(help scr)3.643
F(een)-.37 E F0 1.143(with the help command \()3.643 F F2(h)A F0 3.642(\)o)C
3.642(nt)498.276 196.8 S 1.142(he help)509.698 196.8 R .84(line the)208 208.8 R
F1 .84(help menu)3.34 F F0 .84
(is displayed and you may select help about interesting topics.)3.34 F 1.012(T\
he help menu displays the names of all available help topics, the keystrokes t\
o)208 220.8 R .357
(select help about a topic are marked with uppercase letters.)208 232.8 R .358
(For more information)5.358 F(see the section)208 244.8 Q F1(help pages)2.5 E
F0(below)2.5 E(.)-.65 E F2(variables scr)108 273.6 Q(een)-.18 E F0 .981
(If you have switched to the)208 273.6 R F1(variables)3.481 E F0 .981
(screen with the variables command \()3.481 F F2(=)A F0 3.48(\)a)C(ll)534.44
273.6 Q .23(variables and their settings are displayed and you may set or unse\
t any of the vari-)208 285.6 R 2.521(ables. On)208 297.6 R .021
(the help line is shown how to set or unset a variable in a short form.)2.521 F
(All)5.02 E .343(changes in variables de\214nitions are saved to the \214le)208
309.6 R F2(.utr)2.843 E(ee)-.18 E F0 .343(in your home directory)2.843 F .2
(if the variable)208 321.6 R F2(AUT)2.699 E(OSA)-.18 E(VE)-1.29 E F0 .199
(is set.)2.699 F .199(For more information see the section)5.199 F F1
(variables)2.699 E F0(below)208 333.6 Q(.)-.65 E F2(commands scr)108 362.4 Q
(een)-.18 E F0 .34(If you have switched to the \214letype)208 362.4 R F1 .34
(commands scr)2.84 F(een)-.37 E F0 .34(with the \214letype commands)2.84 F .726
(command \()208 374.4 R F2(:)A F0 3.226(\)a)C .726
(ll \214letype commands and their settings are displayed and you may)268.322
374.4 R .103(set or unset any of the \214letype commands.)208 386.4 R .103
(On the help line is shown how to set or)5.103 F .563
(unset a \214letype command in a short form.)208 398.4 R .562
(All changes in \214letype command de\214-)5.562 F 3.462
(nitions are saved to the \214le)208 410.4 R F2(.utr)5.962 E(ee)-.18 E F0 3.463
(in your home directory if the variable)5.962 F F2(AUT)208 422.4 Q(OSA)-.18 E
(VE)-1.29 E F0 2.35(is set.)4.85 F 2.35(For more information see the section)
7.35 F F1 2.35(\214letype commands)4.85 F F0(below)208 434.4 Q(.)-.65 E F2
(shell scr)108 463.2 Q(een)-.18 E F0 .53(If you have switched to the)208 463.2
R F1 .531(shell scr)3.031 F(een)-.37 E F0 .531(with the shell command \()3.031
F F2(!)A F0 3.031(\)f)C .531(or executing)489.759 463.2 R .112
(commands not supported directly by)208 475.2 R F2(utr)2.612 E(ee)-.18 E F0
.112(all previously entered commands saved)2.612 F .106
(in a so called history list are displayed on the)208 487.2 R F1 .106
(shell scr)2.606 F(een)-.37 E F0 5.106(.T)C .106(he last executed com-)452.762
487.2 R .322(mand is marked with)208 499.2 R F2(->)2.822 E F0 5.322(.Y)C .321
(ou may get any command from the history list into the)319.02 499.2 R .317
(line editor for editing and execution or enter and execute a new command.)208
511.2 R 1.718 -.7(To g)5.318 H(et).7 E 3.114(ac)208 523.2 S .613
(ommand from the history list into the line editor you can use the keys)219.994
523.2 R F2(C-p)3.113 E F0(for)3.113 E .756(the previous command or)208 535.2 R
F2(C-n)3.256 E F0 .756(for the next command in the history list.)3.256 F 2.156
-.7(To g)5.756 H .756(et a).7 F .887(command by number enter)208 547.2 R F1
(!number)3.386 E F0 3.386(,t)C 3.386(og)364.072 547.2 S .886
(et a command by a search pattern enter)377.458 547.2 R F1(!pattern)208 559.2 Q
F0 6.078(.A)C 1.078
(ll commands up to a maximal number de\214ned in the variable)256.018 559.2 R
F2(HIST)3.579 E(-)-.92 E(SIZE)208 571.2 Q F0 1.089
(are saved in the history list.)3.59 F 1.089(Before leaving)6.089 F F2(utr)
3.589 E(ee)-.18 E F0 1.089(all saved commands in)3.589 F .777
(the history list are saved to a history \214le)208 583.2 R F2(.utr)3.277 E
(eehist)-.18 E F0 .778(in your home directory if the)3.277 F(variable)208 595.2
Q F2(AUT)4.259 E(OSA)-.18 E(VE)-1.29 E F0 1.759(is set.)4.259 F 1.758
(At startup this \214le is searched for and read in if)6.759 F 2.5(found. For)
208 607.2 R(more information see also the section)2.5 E F1(variables)2.5 E F0
(below)2.5 E(.)-.65 E F2(bindings scr)108 636 Q(een)-.18 E F0 .148
(If you have switched to the)208 636 R F1 .149(bindings scr)2.648 F(een)-.37 E
F0 .149(with the bindings command \()2.649 F F2(|)A F0 2.649(\)a)C .149(ll cur)
515.891 636 R(-)-.2 E .093(rently de\214ned key bindings and their meaning are\
 displayed and you may bind any)208 648 R .352
(key to an appropriate utree command or to insert a string into the input buf)
208 660 R(fer)-.18 E 5.352(.A)-.55 G(t)537.22 660 Q 1.511(startup a \214le)208
672 R F2(.utr)4.011 E(ee-TERM)-.18 E F0 1.511
(in your home directory or a \214le)4.011 F F2(utr)4.01 E(ee-TERM)-.18 E F0
1.51(in a)4.01 F .135(global startup directory is read in if found containing \
key bindings for the terminal)208 684 R .077
(type de\214ned in the environment variable)208 696 R F2(TERM)2.577 E F0 5.077
(.A)C .076(ll changes in key bindings you)417.679 696 R .008(have done on the)
208 708 R F1 .008(bindings scr)2.508 F(een)-.37 E F0 .009
(at runtime are saved to)2.508 F F2(.utr)2.509 E(ee-TERM)-.18 E F0 .009
(if the vari-)2.509 F(able)208 720 Q F2(AUT)2.975 E(OSA)-.18 E(VE)-1.29 E F0
.474(is set.)2.975 F .474(For more information see also the section)5.474 F F2
.474(key bindings)2.974 F F0(April 2 1992)72 768 Q(UTREE V)255.31 768 Q
(ersion 3.03-um)-1.11 E(3)535 768 Q EP
%%Page: 4 4
%%BeginPageSetup
BP
%%EndPageSetup
/F0 10/Times-Roman@0 SF 365.52(UTREE\(1L\) UTREE\(1L\))72 48 R(below)208 84 Q
(.)-.65 E/F1 10/Times-Bold@0 SF(status scr)108 112.8 Q(een)-.18 E F0 .309
(The status command \()208 112.8 R F1(s)A F0 2.809(\)d)C .309
(isplays all information available about a \214le or directory)312.556 112.8 R
.689(on the)208 124.8 R/F2 10/Times-Italic@0 SF .689(status scr)3.189 F(een)
-.37 E F0 5.689(.H)C .688
(ere you may change the ownership, group membership or)304.266 124.8 R .087
(access rights of a \214le or directory)208 136.8 R 5.087(.O)-.65 G 2.587(nB)
355.979 136.8 S .088(SD systems not all of the denoted changes)370.236 136.8 R
(may be allowed for normal users.)208 148.8 Q/F3 9/Times-Bold@0 SF
(KEY NAMING CONVENTIONS AND DEF)72 165.6 Q(AUL)-.666 E 2.25(TK)-.828 G
(EY BINDINGS)276.255 165.6 Q F0(All)108 177.6 Q F1(utr)3.151 E(ee)-.18 E F0
.651(commands are simple single letter commands or control sequences.)3.151 F
.65(The default or user de\214ned)5.65 F .922(commands therefore are invoked w\
ith a single keystroke or a combination of the <)108 189.6 R F2(CONTROL)A F0
3.422(>k)C .922(ey with)509.358 189.6 R .508(another key)108 201.6 R 5.508(.T)
-.65 G .507(he naming conventions in the following manual sections for)168.906
201.6 R F1(utr)3.007 E(ee)-.18 E F0 .507(commands invoked by a)3.007 F
(keystroke are:)108 213.6 Q F1(key)108 242.4 Q F0(means hit this <)208 242.4 Q
F2(key)A F0 2.5(>o)C(nly)298.16 242.4 Q F1(C-key)108 259.2 Q F0
(means hold down the <)208 259.2 Q F2(CONTROL)A F0(>-key and hit <)A F2(key)A
F0(>)A 1.466 -.7(To p)108 276 T .067
(ermit rebinding of pre-de\214ned keys or binding functions keys to).7 F F1
(utr)2.567 E(ee)-.18 E F0 .067(commands all control sequences)2.567 F
(have special names.)108 288 Q(All key or functions names and their default bi\
ndings are list in the table below)5 E F1(SELECT)108 304.8 Q F0(CR, NL)208
304.8 Q F1(FOR)108 321.6 Q -1.11(WA)-.35 G(RD)1.11 E F0(C-f)208 321.6 Q F1
(BACKW)108 338.4 Q(ARD)-1.11 E F0(C-b)208 338.4 Q F1(NEXT)108 355.2 Q F0(C-n)
208 355.2 Q F1(PREVIOUS)108 372 Q F0(C-p)208 372 Q F1(NEXTP)108 388.8 Q(AGE)
-.74 E F0(C-v)208 388.8 Q F1(PREVP)108 405.6 Q(AGE)-.74 E F0(C-w)208 405.6 Q F1
(BEGIN)108 422.4 Q F0(C-a)208 422.4 Q F1(END)108 439.2 Q F0(C-e)208 439.2 Q F1
(UP)108 456 Q F0(C-u)208 456 Q F1(DOWN)108 472.8 Q F0(C-d)208 472.8 Q F1(INSER)
108 489.6 Q(T)-.35 E F0(C-o)208 489.6 Q F1(DELETE)108 506.4 Q F0(BS)208 506.4 Q
F1(KILL)108 523.2 Q F0(C-k)208 523.2 Q F1(SETMARK)108 540 Q F0(C-@)208 540 Q F1
(GOT)108 556.8 Q(OMARK)-.18 E F0(C-g)208 556.8 Q F1(GOT)108 573.6 Q(OT)-.18 E
(AG)-.74 E F0(C-t,T)208 573.6 Q(AB)-.8 E F1(HELP)108 590.4 Q F0(C-r)208 590.4 Q
F1(REFRESH)108 607.2 Q F0(C-l)208 607.2 Q F1(CANCEL)108 624 Q F0(C-x)208 624 Q
F1(BREAK)108 640.8 Q F0(C-c,C-y)208 640.8 Q F1(EXIT)108 657.6 Q F0(C-z)208
657.6 Q 1.226(In the following sections of this manual page the default key bi\
ndings are used instead of the names or)108 674.4 R(functions.)108 686.4 Q .157
(Some function keys are supported by)108 703.2 R F1(utr)2.658 E(ee)-.18 E F0
2.658(,i)C .158
(.e. the four arrow or cursor keys, and are bound to appropriate)290.002 703.2
R 2.5(functions. See)108 715.2 R(the sections)2.5 E F2(key bindings)2.5 E F0
(and)2.5 E F2(function keys)2.5 E F0(below for more details.)2.5 E
(April 2 1992)72 768 Q(UTREE V)255.31 768 Q(ersion 3.03-um)-1.11 E(4)535 768 Q
EP
%%Page: 5 5
%%BeginPageSetup
BP
%%EndPageSetup
/F0 10/Times-Roman@0 SF 365.52(UTREE\(1L\) UTREE\(1L\))72 48 R/F1 9
/Times-Bold@0 SF(HELP P)72 84 Q(AGES)-.666 E F0 .214(If the)108 96 R/F2 10
/Times-Bold@0 SF(utr)2.714 E(ee)-.18 E F0 .214
(help pages contained in the \214le)2.714 F F2(utr)2.713 E(ee.help)-.18 E F0
.213(are accessible in a directory de\214ned at compile time)2.713 F .064
(or de\214ned in the environment variable)108 108 R F2(UTLIB)2.564 E F0 .064
(you can get help on all screens or from within the line editor)2.564 F .022
(with the help command \()108 120 R F2(h)A F0(or)2.522 E F2(?)2.522 E F0 2.522
(\)o)C 2.522(rt)242.864 120 S .022(he help key \()251.496 120 R F2(C-r)A F0
2.521(\). After)B .021(displaying help about your current context, i.e.)2.521 F
.821(help about tree commands if you are on the)108 132 R/F3 10/Times-Italic@0
SF(tr)3.321 E .821(ee scr)-.37 F(een)-.37 E F0 3.322(,y)C .822
(ou can switch to the help menu and select help)346.242 132 R .196
(about all topics with a single keystroke.)108 144 R(All)5.196 E F3 .196
(help scr)2.696 F(een)-.37 E F0 .195
(commmands and the menu items of the help menu)2.696 F
(displayed on the help line are:)108 156 Q F2(h)108 184.8 Q F0
(\(Help\) About the help pages and the help menu)188 184.8 Q F2(a)108 201.6 Q
F0(\(About\) Information about)188 201.6 Q F2(utr)2.5 E(ee)-.18 E F0
(and key naming conventions)2.5 E F2(u)108 218.4 Q F0(\(Usage\) Description of)
188 218.4 Q F2(utr)2.5 E(ee)-.18 E F0(usage and commandline options)2.5 E F2(g)
108 235.2 Q F0(\(Global\) Global commands common for the)188 235.2 Q F3(tr)2.5
E(ee)-.37 E F0(and the)2.5 E F3(\214le scr)2.5 E(een)-.37 E F2(t)108 252 Q F0
(\(T)188 252 Q(ree\) Commands for the)-.35 E F3(tr)2.5 E(ee scr)-.37 E(een)-.37
E F2(f)108 268.8 Q F0(\(File\) Commands for the)188 268.8 Q F3(\214le scr)2.5 E
(een)-.37 E F2(e)108 285.6 Q F0(\(Edit\) Commands of the builtin line editor)
188 285.6 Q F2(v)108 302.4 Q F0(\(V)188 302.4 Q(ars\))-1.11 E F2(Utr)2.5 E(ee)
-.18 E F0(variables and variable de\214nition)2.5 E F2(c)108 319.2 Q F0
(\(Cmds\) Filetype commands and \214letype command de\214nition)188 319.2 Q F2
(l)108 336 Q F0
(\(Line\) Line format for user de\214ned tree, \214le and \214letype commands.)
188 336 Q F2(k)108 352.8 Q F0(\(Keys\) Function keys used by)188 352.8 Q F2
(utr)2.5 E(ee)-.18 E(p)108 369.6 Q F0(\(Patterns\) File pattern matching, \214\
lename, modi\214cation time and \214le size patterns)188 369.6 Q F2(q)108 386.4
Q F0(\(Quit\) Leave)188 386.4 Q F3(help scr)2.5 E(een)-.37 E F0(The help pages\
 contain in short form most information given in the sections of this manual p\
age.)108 403.2 Q F1(GLOBAL COMMANDS)72 420 Q F0 .138
(The following commands are common for the)108 432 R F3(tr)2.638 E(ee)-.37 E F0
(and)2.638 E F3 .138(\214le scr)2.638 F(een)-.37 E F0 .139
(They can be given in lowercase or upper)2.638 F(-)-.2 E(case letters:)108 444
Q F2(C-z)108 472.8 Q F0(Exit)208 472.8 Q F2(utr)2.5 E(ee)-.18 E F0
(from all screens)2.5 E F2(C-c,C-y)108 489.6 Q F0
(Cancel or break current command or input)208 489.6 Q F2(C-l)108 506.4 Q F0
(Redisplay the current screen or the input line)208 506.4 Q F2 -.74(TA)108
523.2 S(B,C-t).74 E F0(Move to the next tagged \214le or the next directory co\
ntaining tagged \214les)208 523.2 Q F2(h,?,C-r)108 540 Q F0
(Display help pages and switch to the)208 540 Q F3(help scr)2.5 E(een)-.37 E F0
(and the help menu)2.5 E F2(@,C-@)108 556.8 Q F0
(Mark the current directory or \214le)208 556.8 Q F2(#,C-g)108 573.6 Q F0
(Goto to a previously marked directory or \214le)208 573.6 Q F2(a)108 590.4 Q
F0(Display)208 590.4 Q F2(utr)2.5 E(ee)-.18 E F0(version and copyright notice)
2.5 E F2(d)108 607.2 Q F0(Display current date and time)208 607.2 Q F2(j)108
624 Q F0(Move to the next directory or \214le \(for vi fans\))208 624 Q F2(k)
108 640.8 Q F0(Move to the previous directory or \214le \(for vi fans\))208
640.8 Q F2(n)108 657.6 Q F0 .052(Change sort criteria from lexical order to mo\
di\214cation time order or vice versa and)208 657.6 R
(resort \214les in the \214le list)208 669.6 Q F2(t)108 686.4 Q F0 -.7(Ta)208
686.4 S 2.5<678c>.7 G(les matching a \214le pattern for further processing)
230.91 686.4 Q F2(u)108 703.2 Q F0(Untag \214les)208 703.2 Q(April 2 1992)72
768 Q(UTREE V)255.31 768 Q(ersion 3.03-um)-1.11 E(5)535 768 Q EP
%%Page: 6 6
%%BeginPageSetup
BP
%%EndPageSetup
/F0 10/Times-Roman@0 SF 365.52(UTREE\(1L\) UTREE\(1L\))72 48 R/F1 10
/Times-Bold@0 SF(w)108 84 Q F0(Display full pathname of the current directory)
208 84 Q F1(z)108 100.8 Q F0
(Zoom \214les from \214lelist matching a \214le pattern)208 100.8 Q F1(=)108
117.6 Q F0(Switch to the)208 117.6 Q/F2 10/Times-Italic@0 SF(variables scr)2.5
E(een)-.37 E F0 2.5(,d)C(isplay and set or unset variables)338.73 117.6 Q F1(:)
108 134.4 Q F0(Switch to the)208 134.4 Q F2(commands scr)2.5 E(een)-.37 E F0
2.5(,d)C(isplay and set or unset \214letype commands)344.28 134.4 Q F1(|)108
151.2 Q F0(Switch to the)208 151.2 Q F2(bindings scr)2.5 E(een)-.37 E F0 2.5
(,d)C(isplay all key bindings and bind or rebind keys)335.96 151.2 Q F1(!)108
168 Q F0 .592(Switch to the)208 168 R F2 .592(shell scr)3.092 F(een)-.37 E F0
3.092(,d)C .593(isplay all commands from the shell command history)323.36 168 R
1.871(list, enter and execute commands not supported directly from)208 180 R F1
(utr)4.371 E(ee)-.18 E F0 6.871(.B)C 1.87(efore a)510.65 180 R 1.328
(given command is executed the command line is searched for some sprintf like)
208 192 R .508
(format characters lead in by a percent sign \(%\) which are expanded.)208 204
R .507(See the sec-)5.508 F(tion)208 216 Q F2(line formats)2.5 E F0
(for more information)2.5 E F1($)108 232.8 Q F0(Escape to an interactive shell)
208 232.8 Q/F3 9/Times-Bold@0 SF(TREE SCREEN COMMANDS)72 249.6 Q F0 .477
(All commands on the)108 261.6 R F2(tr)2.977 E .477(ee scr)-.37 F(een)-.37 E F0
.477(can be given in lowercase or uppercase letters with the meaning denoted)
2.977 F(below)108 273.6 Q 5.388(.C)-.65 G .387
(ommands given in lowercase letters af)146.348 273.6 R .387
(fect the current directory only)-.18 F 5.387(.C)-.65 G .387
(ommands given in upper)436.549 273.6 R(-)-.2 E .22
(case letters indicated by an uppercase letter in the table below af)108 285.6
R .221(fect the subtree rooted in the current direc-)-.18 F .656
(tory or all tagged \214les in the subtree rooted in the current directory)108
297.6 R 5.655(.T)-.65 G(he)396.105 297.6 Q F2(tr)3.155 E .655(ee scr)-.37 F
(een)-.37 E F0 .655(commmands and the)3.155 F
(menu items of the default tree menu displayed on the help line are:)108 309.6
Q F1(>,CR,NL,SP)108 338.4 Q(,>)-.92 E F0(Change to the)208 338.4 Q F2
(\214le scr)2.5 E(een)-.37 E F0(of the current directory)2.5 E F1(<)108 355.2 Q
F0(Change to the)208 355.2 Q F2(\214le scr)2.5 E(een)-.37 E F0
(of the parent directory)2.5 E F1(C-n)108 372 Q F0(Move to the next directory)
208 372 Q F1(C-p)108 388.8 Q F0(Move to the previous directory)208 388.8 Q F1
(C-f)108 405.6 Q F0
(Move to the next directory on same level as the current directory)208 405.6 Q
F1(C-b)108 422.4 Q F0
(Move to the previous directory on same level as the current directory)208
422.4 Q F1(C-v)108 439.2 Q F0(Move one page forward)208 439.2 Q F1(C-w)108 456
Q F0(Move one page backward)208 456 Q F1(C-a)108 472.8 Q F0
(Move to the beginning of the directory tree)208 472.8 Q F1(C-e)108 489.6 Q F0
(Move to the end of the directory tree)208 489.6 Q F1 -.74(TA)108 506.4 S
(B,C-t).74 E F0(Move to next the directory containing tagged \214les)208 506.4
Q F1(C-u)108 523.2 Q F0(Scroll up one line the directory tree)208 523.2 Q F1
(C-d)108 540 Q F0(Scroll down one line the directory tree)208 540 Q F1(@,C-@)
108 556.8 Q F0(Mark the current directory)208 556.8 Q F1(#,C-g)108 573.6 Q F0
(Move to a previously marked directory)208 573.6 Q F1(h,?)108 590.4 Q F0
(\(Help\) Help about)208 590.4 Q F2(tr)2.5 E(ee scr)-.37 E(een)-.37 E F0
(commands)2.5 E F1(b,B)108 607.2 Q F0 .109(\(Backup\) Backup the current direc\
tory or tree or backup all tagged \214les in the sub-)208 607.2 R(tree)208
619.2 Q F1(c,C)108 636 Q F0 1.516(\(Chdir\) Move to a directory or copy all ta\
gged \214les in the subtree. Before you)208 636 R 2.382
(move to a directory you are requested for the name of this directory before.)
208 648 R .213(Instead entering a name you can select a directory from the tre\
e with C-n and C-p.)208 660 R .898(Instead of entering a name of a directory w\
here to copy the tagged \214les you can)208 672 R .095
(select a directory from the tree with C-n and C-p or directly from)208 684 R
F2(tr)2.594 E .094(ee scr)-.37 F(een)-.37 E F0(using)2.594 E(CR.)208 696 Q F1
(f,F)108 712.8 Q F0 1.588(\(Find\) Find \214les in the current directory or su\
btree matching a \214le pattern you)208 712.8 R .164(have to enter before.)208
724.8 R .163(If a \214le matching the given pattern is found you may tag this)
5.163 F(April 2 1992)72 768 Q(UTREE V)255.31 768 Q(ersion 3.03-um)-1.11 E(6)535
768 Q EP
%%Page: 7 7
%%BeginPageSetup
BP
%%EndPageSetup
/F0 10/Times-Roman@0 SF 365.52(UTREE\(1L\) UTREE\(1L\))72 48 R(\214le, change \
to the directory containing the found \214le or continue \214nd)208 84 Q/F1 10
/Times-Bold@0 SF(g,G)108 100.8 Q F0 2.054
(\(Grep\) Search for pattern in \214les in the current directory or subtree.)
208 100.8 R -1(Yo)7.054 G 4.554(ua)1 G(re)532.23 100.8 Q 1.121
(requested for a \214le and a search pattern.)208 112.8 R 1.12
(If a \214le matching the search pattern is)6.121 F 1.157(found you may tag th\
is \214le, change to the directory containing the found \214le or)208 124.8 R
(continue search)208 136.8 Q F1(i)108 153.6 Q F0 .827
(\(Info\) Display some short information about the current directory)208 153.6
R 5.827(.D)-.65 G .827(isplayed are)491.133 153.6 R .286(access rights, modi\
\214cation time and the disk usage of the current directory or sub-)208 165.6 R
(tree)208 177.6 Q F1(l,L)108 194.4 Q F0 .821(\(List\) List \214les in the curr\
ent directory or subtree matching a \214le pattern you are)208 194.4 R
(requested before or list all tagged \214les in the subtree)208 206.4 Q F1(m,M)
108 223.2 Q F0 .566(\(Mkdir\) Create a new directory rooted in the current dir\
ectory or move all tagged)208 223.2 R 1.661(\214les to a destination directory)
208 235.2 R 6.661(.I)-.65 G 1.661
(nstead of entering the directory name you can)345.365 235.2 R 1.404
(select this name from the directory tree with C-n and C-p or directly from)208
247.2 R/F2 10/Times-Italic@0 SF(tr)3.904 E(ee)-.37 E(scr)208 259.2 Q(een)-.37 E
F0(using CR.)2.5 E F1(n,N)108 276 Q F0 .223(Change the sort criteria from lexi\
cal order to modi\214cation time order or vice versa)208 276 R
(and resort the \214le list of the current directory or subtree)208 288 Q F1(o)
108 304.8 Q F0 .274(\(Out\) W)208 304.8 R .274(rite a list of directories, \
\214les, tagged \214les, \214les matching a \214le pattern or a)-.4 F .51
(formatted tree list to a list \214le.)208 316.8 R 3.009(Af)5.509 G .509
(ormatted tree list \214le can later be displayed on)350.958 316.8 R
(the screen or send to a printer using the additional \214lter command)208
328.8 Q F1(utr)2.5 E(ee.prlist)-.18 E(q)108 345.6 Q F0(\(Quit\) Leave the)208
345.6 Q F2(tr)2.5 E(ee scr)-.37 E(een)-.37 E F0(and exit)2.5 E F1(utr)2.5 E(ee)
-.18 E -.92(r,)108 362.4 S(R).92 E F0 .864
(\(Rmdir\) Remove the current directory or all tagged \214les in the subtree.)
208 362.4 R 3.364(Ad)5.864 G(irec-)521.68 362.4 Q
(tory to be removed may not contain any subdirectories.)208 374.4 Q F1(s)108
391.2 Q F0 1.89(\(Stat\) Switch to the)208 391.2 R F2 1.889(status scr)4.389 F
(een)-.37 E F0 4.389(,d)C 1.889(isplay all status information of the current)
360.906 391.2 R(directory and change owner)208 403.2 Q 2.5(,g)-.4 G
(roup and access rights of the current directory)328.95 403.2 Q F1(t,T)108 420
Q F0(\(T)208 420 Q .922(ag\) T)-.7 F .923(ag \214les in the current directory \
or subtree matching a \214le pattern you are)-.7 F(requested before)208 432 Q
F1(u,U)108 448.8 Q F0
(\(Untag\) Untag \214les in the current directory or subtree)208 448.8 Q F1
(z,Z)108 465.6 Q F0 1.328
(Zoom \214les matching a \214le pattern in the current directory or subtree.)
208 465.6 R(Zooming)6.327 E .776(means that only those \214les matching the \
\214le pattern are displayed and visible for)208 477.6 R(further processing.)
208 489.6 Q F1(+)108 506.4 Q F0(Enlar)208 506.4 Q(ge the tree window)-.18 E 2.5
(,s)-.65 G(hrink the \214le window one line)314.09 506.4 Q F1(-)108 523.2 Q F0
(Shrink the tree window)208 523.2 Q 2.5(,e)-.65 G(nlar)310.39 523.2 Q
(ge the \214le window one line)-.18 E F1(/)108 540 Q F0 1.192(Scan the current\
 directory or tree and rebuild directories if they need rebuilding)208 540 R
(\(i.e. if they are not yet read in or have changed\))208 552 Q F1(\\)108 568.8
Q F0 1.283(Scan the current directory for subdirectories and build up and inse\
rt the subtree)208 568.8 R(into the directory tree.)208 580.8 Q -1(Yo)5 G 2.5
(ua)1 G(re requested for the maximal tree level to build up)325.03 580.8 Q F1
(0)108 597.6 Q F0 1.162(Switch the tree menuline from the default to the user \
de\214ned tree commands or)208 597.6 R(vice versa)208 609.6 Q F1(1..9)108 626.4
Q F0(Execute the user de\214ned tree command 1 .. 9)208 626.4 Q .709(For furth\
er information about \214le patterns for the commands \214nd, grep, list, tag \
and untag see the section)108 643.2 R F2(\214le patterns)108 655.2 Q F0(below)
2.5 E 5(.F)-.65 G(or user de\214ned tree commands see the section)194.85 655.2
Q F2(variables)2.5 E F0(.)A/F3 9/Times-Bold@0 SF(FILE SCREEN COMMANDS)72 672 Q
F0 .652(All commands on the)108 684 R F2 .651(\214le scr)3.151 F(een)-.37 E F0
.651(can be given in lowercase or uppercase letters with the meaning denoted)
3.151 F(below)108 696 Q 5.868(.C)-.65 G .868
(ommands given in lowercase letters)146.828 696 R(af)5.868 E .869
(fect the current \214le only)-.18 F 5.869(.C)-.65 G .869
(ommands given in uppercase)421.023 696 R .312
(letters indicated by an uppercase letter in the table below af)108 708 R .312
(fect all tagged \(selected\) \214les if \214les are tagged)-.18 F .4
(or the current \214le if no \214les are tagged.)108 720 R .4(The \214le)5.4 F
F2(scr)2.9 E .4(een commmands)-.37 F F0 .4
(and the menu items of the default \214le)2.9 F(April 2 1992)72 768 Q(UTREE V)
255.31 768 Q(ersion 3.03-um)-1.11 E(7)535 768 Q EP
%%Page: 8 8
%%BeginPageSetup
BP
%%EndPageSetup
/F0 10/Times-Roman@0 SF 365.52(UTREE\(1L\) UTREE\(1L\))72 48 R
(menu displayed on the help line are:)108 84 Q/F1 10/Times-Bold@0 SF
(q,CR,NL,SP)108 112.8 Q F0(\(Quit\) Leave the)208 112.8 Q/F2 10/Times-Italic@0
SF(\214le scr)2.5 E(een)-.37 E F0(and change back to the)2.5 E F2(tr)2.5 E
(ee scr)-.37 E(een)-.37 E F1(C-f)108 129.6 Q F0(Move to the next \214le)208
129.6 Q F1(C-b)108 146.4 Q F0(Move to the previous \214le)208 146.4 Q F1(C-n)
108 163.2 Q F0(Move to the \214le on the next line)208 163.2 Q F1(C-p)108 180 Q
F0(Move to the \214le on the previous line)208 180 Q F1(C-v)108 196.8 Q F0
(Move one page forward)208 196.8 Q F1(C-w)108 213.6 Q F0
(Move one page backward)208 213.6 Q F1(C-a)108 230.4 Q F0
(Move to the beginning of the \214le list)208 230.4 Q F1(C-e)108 247.2 Q F0
(Move to the end of the \214le list)208 247.2 Q F1 -.74(TA)108 264 S(B,C-t).74
E F0(Move to the next tagged \214le)208 264 Q F1(C-u)108 280.8 Q F0
(Scroll up one line the)208 280.8 Q F2(\214le scr)2.5 E(een)-.37 E F1(C-d)108
297.6 Q F0(Scroll down one line the)208 297.6 Q F2(\214le scr)2.5 E(een)-.37 E
F1(@,C-@)108 314.4 Q F0(Mark the current \214le)208 314.4 Q F1(#,C-g)108 331.2
Q F0(Move to a previously marked \214le)208 331.2 Q F1(h,?)108 348 Q F0
(\(Help\) Help about)208 348 Q F2(\214le scr)2.5 E(een)-.37 E F0
(commands and switch to the)2.5 E F2(help scr)2.5 E(een)-.37 E F1(c,C)108 364.8
Q F0 .983(\(Copy\) Copy the current \214le or tagged \214les.)208 364.8 R -1
(Yo)5.983 G 3.483(ua)1 G .983(re requested for a destination)419.988 364.8 R
.416(\214le or directory where to copy the \214le or tagged \214les.)208 376.8
R .416(Instead of entering a direc-)5.416 F 2.188
(tory name you can select a destination directory using C-n and C-p or select)
208 388.8 R(directly on the)208 400.8 Q F2(tr)2.5 E(ee scr)-.37 E(een)-.37 E F0
(with CR)2.5 E F1(e,E)108 417.6 Q F0
(\(Edit\) Edit the current \214le or tagged \214les)208 417.6 Q F1(f)108 434.4
Q F0(\(Find\) Find \214les matching a \214le pattern you are requested before)
208 434.4 Q F1(g,G)108 451.2 Q F0 .983
(\(Grep\) Search for a pattern in the current \214le or tagged \214les.)208
451.2 R .983(Before search you)5.983 F
(are requested for a \214le pattern and the search pattern to search for)208
463.2 Q F1(i,I)108 480 Q F0 1.119
(Display some short information about the current \214le or tagged \214les.)208
480 R(Displayed)6.119 E 2.071(are the access rights, the size and the modi\214\
cation time of the current \214le or)208 492 R(tagged \214les)208 504 Q F1(l,L)
108 520.8 Q F0(\(List\) List \214les matching a \214le pattern you are request\
ed before or all tagged \214les)208 520.8 Q F1(m,M)108 537.6 Q F0 .379
(\(Move\) Move or rename the current \214le or tagged \214les.)208 537.6 R -1
(Yo)5.379 G 2.879(ua)1 G .379(re requested for the)461.393 537.6 R .743(new \
\214le name or a destination directory where to move the current \214le or tag\
ged)208 549.6 R .307(\214les Instead of entering the name of a destination dir\
ectory you can select a direc-)208 561.6 R
(tory using C-n and C-p or directly on the)208 573.6 Q F2(tr)2.5 E(ee scr)-.37
E(een)-.37 E F0(with CR)2.5 E F1(n)108 590.4 Q F0 .223(Change the sort criteri\
a from lexical order to modi\214cation time order or vice versa)208 590.4 R
(and resort the \214le list)208 602.4 Q F1(p,P)108 619.2 Q F0
(\(Print\) Print out the current \214le or tagged \214les)208 619.2 Q F1 -.92
(r,)108 636 S(R).92 E F0 .265
(\(Remove\) Remove the current \214le or tagged \214les.)208 636 R .265
(Before removing you are asked)5.265 F
(if you really want to remove the current \214le or tagged \214les)208 648 Q F1
(s,S)108 664.8 Q F0 1.103(\(Stat\) Switch to the)208 664.8 R F2 1.103
(status scr)3.603 F(een)-.37 E F0 1.103
(display all status information of the current or)3.603 F .078
(tagged \214les and change owner)208 676.8 R 2.578(,g)-.4 G .078
(roup and access rights of the current \214le or tagged)339.07 676.8 R(\214les)
208 688.8 Q F1(t,T)108 705.6 Q F0(\(T)208 705.6 Q .467(ag\) T)-.7 F 2.967
(ag the)-.7 F .467
(current \214le or \214les matching a \214le pattern you are requested before)
2.967 F(for further processing)208 717.6 Q(April 2 1992)72 768 Q(UTREE V)255.31
768 Q(ersion 3.03-um)-1.11 E(8)535 768 Q EP
%%Page: 9 9
%%BeginPageSetup
BP
%%EndPageSetup
/F0 10/Times-Roman@0 SF 365.52(UTREE\(1L\) UTREE\(1L\))72 48 R/F1 10
/Times-Bold@0 SF(u,U)108 84 Q F0 1.377(\(Untag\) Untag the current \214le or \
\214les matching a \214le pattern you are requested)208 84 R(before)208 96 Q F1
-.55(v,)108 112.8 S(V).55 E F0(\(V)208 112.8 Q(iew\) V)-.6 E
(iew the current \214le or tagged \214les)-.6 E F1(x,X)108 129.6 Q F0 .86
(Execute the current \214le or tagged \214les.)208 129.6 R .861
(If a \214letype command is de\214ned for this)5.861 F 1.908(\214le you can ex\
ecute this \214letype command, otherwise you are requested for a)208 141.6 R
(command or for parameters to execute)208 153.6 Q F1(z)108 170.4 Q F0
(Zoom \214les matching a \214le pattern)208 170.4 Q F1(>)108 187.2 Q F0
(If the current \214le is a directory change to the)208 187.2 Q/F2 10
/Times-Italic@0 SF(\214le scr)2.5 E(een)-.37 E F0(of this directory)2.5 E F1(<)
108 204 Q F0(Change back to the)208 204 Q F2(\214le scr)2.5 E(een)-.37 E F0
(of the parent directory)2.5 E F1(/)108 220.8 Q F0
(Rebuild the \214le list \(i.e. after shell escape\))208 220.8 Q F1(0)108 237.6
Q F0 1.204(Switch the menuline from the default to the user de\214ned \214le c\
ommands or vice)208 237.6 R(versa)208 249.6 Q F1(1..9)108 266.4 Q F0
(Execute the user de\214ned \214le commands 1 .. 9)208 266.4 Q .709(For furthe\
r information about \214le patterns for the commands \214nd, grep, list, tag a\
nd untag see the section)108 283.2 R F2 .568(\214le patterns)108 295.2 R F0
(below)3.068 E 5.568(.F)-.65 G .569
(or user de\214ned \214le commands see the section)196.554 295.2 R F2
(variables)3.069 E F0 5.569(.F)C .569(or \214letype command exe-)435.813 295.2
R(cution invoked with the command)108 307.2 Q F1(x)2.5 E F0(see the section)2.5
E F2(\214letype commands)2.5 E F0(below)2.5 E(.)-.65 E/F3 9/Times-Bold@0 SF
(LINE EDIT)72 324 Q(OR COMMANDS)-.162 E F0(Many)108 336 Q F1(utr)2.583 E(ee)
-.18 E F0 .082(commands need some user input for further processing which is d\
one with a builtin simple line)2.583 F(editor)108 348 Q 5.03(.M)-.55 G .03
(any commands pre-set the input buf)147.2 348 R .031
(fer with a default input line if this default is known.)-.18 F .031(For some)
5.031 F .185(commands you can use the keys C-n and C-p to scroll in already ex\
isting input lists and select an input line)108 360 R .753
(for editing or processing without entering the line completely)108 372 R 5.753
(.T)-.65 G .754(he line editor knows about the following)373.267 372 R
(functions:)108 384 Q F1(CR,NL)108 412.8 Q F0(Accept and send the input line)
208 412.8 Q F1(C-c,C-y)108 429.6 Q F0(Cancel input and leave the line editor)
208 429.6 Q F1(C-o)108 446.4 Q F0
(Switch from overwrite-mode to insert-mode or vice versa)208 446.4 Q F1(C-l)108
463.2 Q F0(Redisplay the input line)208 463.2 Q F1(C-f)108 480 Q F0
(Move the cursor one character forward)208 480 Q F1(C-b)108 496.8 Q F0
(Move the cursor one character backward)208 496.8 Q F1(C-a)108 513.6 Q F0
(Move the cursor to the beginning of the input line)208 513.6 Q F1(C-e)108
530.4 Q F0(Move the cursor to the end of the input line)208 530.4 Q F1(C-v)108
547.2 Q F0(Scroll horizontally forward the input line)208 547.2 Q F1(C-w)108
564 Q F0(Scroll horizontally backward the input line)208 564 Q F1(C-d)108 580.8
Q F0(Delete one character under the cursor)208 580.8 Q F1(C-h,DEL)108 597.6 Q
F0(Delete one character left from the cursor)208 597.6 Q F1(C-x)108 614.4 Q F0
(Delete the input line completely)208 614.4 Q F1(C-k)108 631.2 Q F0
(Delete the input line from the cursor position to the end)208 631.2 Q F1(C-t)
108 648 Q F0 -.35(Tr)208 648 S
(anspose two characters under and left from the cursor).35 E F1(C-r)108 664.8 Q
F0(Display help pages and switch to the)208 664.8 Q F2(help scr)2.5 E(een)-.37
E F1(C-@)108 681.6 Q F0(Set a mark at the current cursor position)208 681.6 Q
F1(C-g)108 698.4 Q F0(Move the cursor to the previously marked position)208
698.4 Q F1(C-n)108 715.2 Q F0(Get the next entry into the line editor)208 715.2
Q(April 2 1992)72 768 Q(UTREE V)255.31 768 Q(ersion 3.03-um)-1.11 E(9)535 768 Q
EP
%%Page: 10 10
%%BeginPageSetup
BP
%%EndPageSetup
/F0 10/Times-Roman@0 SF 365.52(UTREE\(1L\) UTREE\(1L\))72 48 R/F1 10
/Times-Bold@0 SF(C-p)108 84 Q F0(Get the previous entry into the line editor)
208 84 Q .189(All other printable characters are appended at the end of input \
line, inserted in insert-mode or overwrite the)108 100.8 R .94
(character under the cursor in overwrite-mode.)108 112.8 R .941
(The current mode is displayed at the end of the help line.)5.94 F(Most line e\
ditor commands are also available with function keys, see the section)108 124.8
Q/F2 10/Times-Italic@0 SF(function keys)2.5 E F0(below)2.5 E(.)-.65 E/F3 9
/Times-Bold@0 SF -1.161(VA)72 141.6 S(RIABLES)1.161 E F0 .639(Utree knows abou\
t and uses the following variables which may be set or unset at startup in the\
 startup\214le)108 153.6 R F1($HOME/.utr)108 165.6 Q(ee)-.18 E F0 2.629(,w)C
.129(ith some commandline options \(see)184.329 165.6 R F2(options)2.629 E F0
.129(above\), or the variables command \()2.629 F F1(=)A F0 2.63(\)o)C 2.63(nt)
520.15 165.6 S(he)530.56 165.6 Q F2(variables scr)108 177.6 Q(een)-.37 E F0(:)A
F1(BELL)108 206.4 Q F0(or)208 206.4 Q F1(BL)2.5 E F0 2.5(:A)C
(llow ringing of the bell if set)244.67 206.4 Q F1(CLOCK)108 223.2 Q F0(or)208
223.2 Q F1(CL)2.5 E F0 2.5(:S)C(how and update clock every second if set)243.56
223.2 Q F1(GRAPHCHARS)108 240 Q F0(or)208 240 Q F1(GC)2.689 E F0 2.689(:U)C
.189(se the graphical character set if set.)246.708 240 R .188
(Not all terminal database termcap or)5.188 F
(terminfo de\214nitions of the graphical character set are correct)208 252 Q F1
(TERMSCROLL)108 268.8 Q F0(or)208 268.8 Q F1(TS)3.558 E F0 3.558(:U)C 1.058
(se hardware terminal scrolling if set.)245.676 268.8 R 1.059
(On some terminals \(i.e. on the X)6.059 F
(terminal emulator xterm\) redrawing the screen may be faster than scrolling)
208 280.8 Q F1(SCANTREE)108 297.6 Q F0(or)208 297.6 Q F1(ST)4.474 E F0 4.474
(:A)C 1.974(llow scanning the tree for changes if set.)247.508 297.6 R 1.973
(Many commands scan the)6.974 F .313
(directory tree after execution what may take some time.)208 309.6 R .314
(Prohibiting tree scanning)5.314 F(therefore may speed up)208 321.6 Q F1(utr)
2.5 E(ee)-.18 E F0 2.5(al)2.5 G(ittle bit)338.33 321.6 Q F1 -1.11(WA)108 338.4
S(RNDIRS)1.11 E F0(or)208 338.4 Q F1(WD)2.5 E F0 2.5(:A)C
(llow warnings and requests about unreadable directories if set)248.55 338.4 Q
F1(LEXSOR)108 355.2 Q(T)-.35 E F0(or)208 355.2 Q F1(LS)3.338 E F0 3.338(:S)C
.838(ort \214lenames in lexical order if set, in order of modi\214cation times\
 if not)243.576 355.2 R(set.)208 367.2 Q F1(AUT)108 384 Q(OSA)-.18 E(VE)-1.29 E
F0(or)208 384 Q F1(AS)3.39 E F0 3.391(:S)C .891
(ave changes in variables or \214letype commands de\214nitions, key bindings)
244.231 384 R(and history list to appropriate \214les in the home directory)208
396 Q F1(TREEINDENT)108 412.8 Q F0(or)208 412.8 Q F1(TI)3.469 E F0 3.469(:S)C
.969(et the tree level indention column \(3 .. 9\) if possible.)242.168 412.8 R
.968(Normally the tree)5.968 F .767(level indention column is calculated depen\
dent on the number of screen columns)208 424.8 R
(and the maximal \214lesystem depth automatically)208 436.8 Q F1(VIDEOMODE)108
453.6 Q F0(or)208 453.6 Q F1(VM)2.883 E F0 2.883(:S)C .383
(et using of video attributes.)247.096 453.6 R 2.883(2m)5.383 G .383
(eans use all possible attributes and their)378.834 453.6 R 4.184
(combinations. 1)208 465.6 R 1.685
(means use the attributes reverse and underline only)4.184 F 6.685(.0)-.65 G
(means)514.45 465.6 Q(don')208 477.6 Q 2.5(tu)-.18 G(se any video attribute.)
236.43 477.6 Q F1(FILELINES)108 494.4 Q F0(or)208 494.4 Q F1(FL)2.5 E F0 2.5
(:N)C(umber of lines of the \214le window on the)244.11 494.4 Q F2(tr)2.5 E
(ee scr)-.37 E(een)-.37 E F0(\(1 .. 9, default 3\))2.5 E F1(HISTSIZE)108 511.2
Q F0(or)208 511.2 Q F1(HS)2.592 E F0 2.592(:M)C .091
(aximal number of shell commands which are hold in the shell commands)246.524
511.2 R(history list \(6 .. 99, default: 22\))208 523.2 Q F1(EDIT)108 540 Q(OR)
-.18 E F0(or)208 540 Q F1(ED)2.885 E F0 2.885(:P)C .386
(rogram for editing \214les.)244.33 540 R .386
(When rede\214ning the editor variable don')5.386 F 2.886(tf)-.18 G(or)519.63
540 Q(get)-.18 E
(to check and set or unset the editopts variable for editor options)208 552 Q
F1(EDIT)108 568.8 Q(OPTS)-.18 E F0(or)208 568.8 Q F1(EO)2.5 E F0 2.5(:F)C
(ile editor options)244.12 568.8 Q F1 -.74(PA)108 585.6 S(GER).74 E F0(or)208
585.6 Q F1(PG)2.61 E F0 2.61(:P)C .109(rogram for viewing \214les.)243.78 585.6
R .109(When rede\214ning the pager variable don')5.109 F 2.609(tf)-.18 G(or)
519.63 585.6 Q(get)-.18 E
(to check and set or unset the pageopts variable for pager options)208 597.6 Q
F1 -.74(PA)108 614.4 S(GEOPTS).74 E F0(or)208 614.4 Q F1(PO)2.5 E F0 2.5(:F)C
(ile pager options)243.56 614.4 Q F1(XDUMPER)108 631.2 Q F0(or)208 631.2 Q F1
(XD)3.483 E F0 3.483(:P)C .983(rogram for hexdumping \214les.)246.076 631.2 R
.983(When rede\214ning the hexdumper variable)5.983 F(don')208 643.2 Q 4.646
(tf)-.18 G(or)236.906 643.2 Q 2.146
(get to check and set or unset the xdumpopts variable for hexdumper)-.18 F
(options)208 655.2 Q F1(XDUMPOPTS)108 672 Q F0(or)208 672 Q F1(XO)2.5 E F0 2.5
(:F)C(ile hexdumper options)244.67 672 Q F1(LPRINTER)108 688.8 Q F0(or)208
688.8 Q F1(LP)3.551 E F0 3.551(:P)C 1.052(rogram for printing \214les or sendi\
ng \214les to the printer spooling system.)244.552 688.8 R .465
(When rede\214ning the lineprinter variable don')208 700.8 R 2.965(tf)-.18 G
(or)400.57 700.8 Q .464(get to check and set or unset the)-.18 F
(lprintopts variable for lineprinter options)208 712.8 Q(April 2 1992)72 768 Q
(UTREE V)255.31 768 Q(ersion 3.03-um)-1.11 E(10)530 768 Q EP
%%Page: 11 11
%%BeginPageSetup
BP
%%EndPageSetup
/F0 10/Times-Roman@0 SF 365.52(UTREE\(1L\) UTREE\(1L\))72 48 R/F1 10
/Times-Bold@0 SF(LPRINT)108 84 Q(OPTS)-.18 E F0(or)208 84 Q F1(LO)2.5 E F0 2.5
(:P)C(rinter options)244.12 84 Q F1(BACKUP)108 100.8 Q F0(or)208 100.8 Q F1(BK)
2.5 E F0 2.5(:P)C(rogram or shell script for backing up a directory or tree)
244.12 100.8 Q F1(BACKUPOPTS)108 117.6 Q F0(or)208 117.6 Q F1(BO)2.5 E F0 2.5
(:B)C(ackup options)245.23 117.6 Q F1(SHELL)108 134.4 Q F0(or)208 134.4 Q F1
(SH)2.5 E F0 2.5(:I)C(nteractive shell for shell escape)240.78 134.4 Q F1
(TREECMD1..9)108 151.2 Q F0(or)208 151.2 Q F1(T1..9)2.5 E F0 2.5(:U)C
(ser de\214ned)253 151.2 Q/F2 10/Times-Italic@0 SF(tr)2.5 E(ee scr)-.37 E(een)
-.37 E F0(commands 1 .. 9)2.5 E F1(FILECMD1..9)108 168 Q F0(or)208 168 Q F1
(F1..9)2.5 E F0 2.5(:U)C(ser de\214ned)252.44 168 Q F2(\214le scr)2.5 E(een)
-.37 E F0(commands 1 .. 9)2.5 E -1.11(Va)108 184.8 S .041
(riables are set with a line ')1.11 F F1(variable=value)A F0 2.541('o)C 2.541
(r')298.677 184.8 S F1(shorthand=value)307.878 184.8 Q F0 2.542('\()C .042
(i.e. ')389.46 184.8 R F1(tr)A .042(eecmd1=ps -ef)-.18 F F0 2.542('o)C 2.542
(r')487.016 184.8 S F1 .042(t1=ps -ef)496.218 184.8 R F0('\))A .623
(and unset with a line ')108 196.8 R F1(variable=)A F0 3.123('o)C 3.123(r')
251.868 196.8 S F1(shorthand=)261.651 196.8 Q F0 3.123('\()C .622(i.e. ')
321.034 196.8 R F1(t1=)A F0 3.122('\). When)B .622
(de\214ning user tree or \214le commands)3.122 F .303
(some sprintf like format characters lead in by a percent sign \()108 208.8 R
F1(%)A F0 2.803(\)h)C .303(ave a special meaning and are expanded)377.682 208.8
R .861(before the command is executed.)108 220.8 R .861
(For further information about the command line format see the section)5.861 F
F2(line formats)108 232.8 Q F0(below)2.5 E(.)-.65 E .598
(The last sharp sign \()108 244.8 R F1(#)A F0 3.098(\)i)C 3.098(nav)205.7 244.8
S .599(ariable de\214nition is used as leadin for a menu item of the de\214ned\
 user \214le or)226.336 244.8 R .073(tree command.)108 256.8 R .073
(Example: the variable de\214nition ')5.073 F F1 .073(fc1=wc -l %F #Count)B F0
2.573('f)C .072(or the user de\214ned \214le command)408.27 256.8 R 3.354(1i)
108 268.8 S 3.354(se)119.134 268.8 S .854(xpanded to ')130.818 268.8 R F1 .854
(wc -l \214lename)B F0 3.354('a)C .855(nd in the user command \214le menu ')
254.228 268.8 R F1(Count)A F0 3.355('i)C 3.355(sd)438.275 268.8 S .855
(isplayed behind menu)450.52 268.8 R(item 1.)108 280.8 Q/F3 9/Times-Bold@0 SF
(FILETYPE COMMANDS)72 297.6 Q F0(On)108 309.6 Q F2 1.016(\214le scr)3.516 F
(een)-.37 E F0 1.016(you can execute a \214le or a command on it with the)3.516
F F1(utr)3.516 E(ee)-.18 E F0 1.016(execute command \()3.516 F F1(x)A F0 3.516
(\). Y)B 1.016(ou are)-1 F .063(requested for parameters if the current \214le\
 is executable, for a command to execute on the current \214le if it is)108
321.6 R .126(not executable.)108 333.6 R .125
(For a type of \214le you can de\214ne so called)5.126 F F2 .125
(\214letype commands)2.625 F F0 .125(which are called if the current)2.625 F
(\214le matches a given \214le pattern.)108 345.6 Q .203
(Filetype commands can be set and unset at startup in the startup\214le)108
357.6 R F1($HOME/.utr)2.704 E(ee)-.18 E F0 2.704(,w)C .204(ith the commandline)
457.372 357.6 R(option)108 369.6 Q F1(-d)2.732 E F0 .232
(or the \214letype command \()2.732 F F1(:)A F0 2.731(\)s)C .231
(imilar to setting and unsetting variables.)265.441 369.6 R .231
(Filetype commands are set)5.231 F .225(with a line like ')108 381.6 R F1
(\214letype:command)A F0 2.725('\()C .225(i.e. ')260.495 381.6 R F1 .225
(*.c:cc -c -O)B F0 2.726('\). The)B .226(command \(i.e. ')2.726 F F1 .226
(cc -c -O)B F0 .226('\) is then executed if)B .388
(the current \214le matches the given \214le pattern \(i.e. ')108 393.6 R F1
(*.c)A F0 2.887('f)C .387(or a C source \214le ending with ')335.437 393.6 R F1
(.c)A F0 2.887('\). Filetype)B(com-)2.887 E(mands are unset with a line ')108
405.6 Q F1(\214letype:)A F0 2.5('\()C(i.e. ')266.58 405.6 Q F1(*.c:)A F0('\).)A
.302(When de\214ning \214letype commands some sprintf like format characters l\
ead in by a percent sign \()108 417.6 R F1(%)A F0 2.803(\)h)C .303(ave a)
518.877 417.6 R .436(special meaning and are expanded before the command is ex\
ecuted. For further information about \214le pat-)108 429.6 R
(terns and the format line characters and her meaning see the sections)108
441.6 Q F2(\214le patterns)2.5 E F0(and)2.5 E F2(line formats)2.5 E F0(below)
2.5 E(.)-.65 E F3(LINE FORMA)72 458.4 Q(TS)-.666 E F0 .564(When de\214ning a u\
ser tree or \214le command or a \214letype command some sprintf like format li\
ne characters)108 470.4 R .308
(are known and expanded before the command is executed.)108 482.4 R .308
(These format line characters and their meaning)5.308 F(are:)108 494.4 Q F1(%B)
108 523.2 Q F0(or)2.5 E F1(%b)2.5 E F0 1.467(is expanded to the basename \(\
\214lename without extension\) of the current \214le or)208 523.2 R(directory)
208 535.2 Q F1(%D)108 552 Q F0(or)2.5 E F1(%d)2.5 E F0
(is expanded to the full pathname of the current directory)208 552 Q F1(%F)108
568.8 Q F0(or)2.5 E F1(%f)2.5 E F0
(is expanded to the \214lename of the current \214le or directory)208 568.8 Q
F1(%H)108 585.6 Q F0(or)2.5 E F1(%h)2.5 E F0
(is expanded to the pathname of your home directory)208 585.6 Q F1(%P)108 602.4
Q F0(or)2.5 E F1(%p)2.5 E F0
(is expanded to the full pathname of the current \214le or directory)208 602.4
Q F1(%R)108 619.2 Q F0(or)2.5 E F1(%r)2.5 E F0
(is expanded to the pathname of the root directory from where)208 619.2 Q F1
(utr)2.5 E(ee)-.18 E F0(was started)2.5 E F1(%S)108 636 Q F0(or)2.5 E F1(%s)2.5
E F0 .468(is expanded to additional parameter\(s\) for a command which are req\
uested before)208 636 R(the command is executed)208 648 Q .157
(The command line ')108 664.8 R F1 .157(command %s %f >%b.out)B F0 2.657('i)C
.157(.e. is expanded before execution to ')313.119 664.8 R F1 .158
(command parame-)B .18(ters \214lename >basename.out)108 676.8 R F0 2.68('w)C
.18(ith \214lename of the current \214le or directory \()243.11 676.8 R F1(%f)A
F0 .18(\), basename.out of the cur)B(-)-.2 E .012(rent \214le or directory \()
108 688.8 R F1(%b.out)A F0 2.512(\)a)C .012(nd additional parameters \()236.37
688.8 R F1(%s)A F0 2.512(\)w)C .012(hich are requested before command execu-)
368.058 688.8 R(tion.)108 700.8 Q .545(For further information about tree, \
\214le and \214letype commands see the sections)108 712.8 R F2(variables)3.045
E F0(and)3.045 E F2 .545(\214letype com-)3.045 F(mands)108 724.8 Q F0(.)A
(April 2 1992)72 768 Q(UTREE V)255.31 768 Q(ersion 3.03-um)-1.11 E -.37(11)
530.37 768 S EP
%%Page: 12 12
%%BeginPageSetup
BP
%%EndPageSetup
/F0 10/Times-Roman@0 SF 365.52(UTREE\(1L\) UTREE\(1L\))72 48 R/F1 9
/Times-Bold@0 SF(FILE P)72 84 Q -.666(AT)-.666 G(TERNS).666 E F0 .022(Some com\
mands \(list, \214nd, grep, tag or untag\) require \214le patterns for matchin\
g \214les using some special \(or)108 96 R(meta\) characters.)108 108 Q(Shell \
like \214lename pattern matching interprets the following meta characters:)108
120 Q/F2 10/Times-Bold@0 SF(*)108 148.8 Q F0
(matches all characters in a \214lename)208 148.8 Q F2(?)108 165.6 Q F0
(matches one character in a \214lename)208 165.6 Q F2([class])108 182.4 Q F0
.519(matches one character from a character class.)208 182.4 R 3.018(Ac)5.518 G
.518(haracter class includes all char)412.618 182.4 R(-)-.2 E .943
(acters enclosed between the opening and closing brackets \()208 194.4 R F2([)A
F0(and)3.443 E F2(])3.443 E F0 3.443(\). If)B .943(in a class)3.443 F 1.077
(de\214nition a minus sign \()208 206.4 R F2(-)A F0 3.576(\)i)C 3.576(sf)
322.552 206.4 S 1.076(ound between two other characters this means the)333.348
206.4 R 1.208
(range from the character before and the character behind the minus sign.)208
218.4 R 1.209(If the)6.209 F .692
(\214rst character of a class de\214nition is a exclamation mark \()208 230.4 R
F2(!)A F0 3.192(\)t)C .692(his means matching)459.176 230.4 R
(of all characters excluded those de\214ned in the class, i.e.)208 242.4 Q
76.96([abc] matches)108 259.2 R(the characters 'a', 'b' and 'c')2.5 E 73.63
([a-z_] matches)108 276 R(the characters from 'a' to)2.5 E('z' and '_')5 E 70.3
([!a-z_] matches)108 292.8 R(all characters except 'a' to 'z' and '_')2.5 E
(File size pattern matching interprets the following meta characters:)108 309.6
Q F2(=size)108 338.4 Q F0(matches all \214les of size)208 338.4 Q F2(size)2.5 E
(!size)108 355.2 Q F0(matches all \214les not of size)208 355.2 Q F2(size)2.5 E
(>size)108 372 Q F0(matches all \214les lar)208 372 Q(ger than)-.18 E F2(size)
2.5 E(<size)108 388.8 Q F0(matches all \214les smaller than)208 388.8 Q F2
(size)2.5 E F0 1.006(Size may be speci\214ed in bytes \()108 405.6 R F2(b)A F0
3.507(,d)C 1.007(efault\), kilo bytes \()257.256 405.6 R F2(k)A F0 3.507(\)o)C
3.507(rm)353.774 405.6 S 1.007(ega bytes \()368.391 405.6 R F2(m)A F0 1.007
(\), i.e. ')B F2(>2k)A F0 3.507('m)C 1.007(atches all \214les)481.326 405.6 R
(lar)108 417.6 Q(ger than 2 kilo bytes or 2048 bytes.)-.18 E(The additional \
\214le time pattern matching interprets the following meta characters:)108
446.4 Q F2(\)time)108 475.2 Q F0(matches all \214les modi\214ed within)208
475.2 Q F2(time)2.5 E(\(time)108 492 Q F0
(matches all \214les not modi\214ed within)208 492 Q F2(time)2.5 E F0 -.35(Ti)
108 508.8 S .259(me may be speci\214ed in minutes \().35 F F2(m)A F0 .259
(\), hours \()B F2(h)A F0 2.759(,d)C .259(efault\), days \()311.351 508.8 R F2
(d)A F0 2.759(\)o)C 2.759(rw)383.778 508.8 S .259(eeks \()397.087 508.8 R F2(w)
A F0 .259(\), i.e. ')B F2(\)2d)A F0 2.759('m)C .259(atches all \214les)482.823
508.8 R(modi\214ed within last 2 days.)108 520.8 Q 1.4 -.7(To c)108 544.8 T(om\
bine shell like \214lename patterns and/or additional \214le size and modi\214\
cation time patterns use).7 E F2(&)108 573.6 Q F0(for)208 573.6 Q F2(AND)2.5 E
F0(ing of patterns)A F2(|)108 590.4 Q F0(for)208 590.4 Q F2(OR)2.5 E F0
(ing of patterns)A .827(If a character is preceeded by a backslash or enclosed\
 in quotes his interpretation is suppressed and he is)108 607.2 R
(used as he is.)108 619.2 Q F1(KEY BINDINGS)72 636 Q F0 .528
(All defaults key bindings are listed in the section)108 648 R/F3 10
/Times-Italic@0 SF .527(key naming conventions and default key bindings)3.027 F
F0(above.)3.027 E .511(All supported function keys if de\214ned in the termcap\
 or terminfo terminal database and their default bind-)108 660 R .515
(ings are listed in the next section.)108 672 R .514
(Rebindings of default keys or additional bindings of other keys may be)5.515 F
.677(done in terminal dependent startup\214les)108 684 R F2(utr)3.178 E
(ee-TERM)-.18 E F0 .678(in a global directory containing)3.178 F F2(utr)3.178 E
(ee)-.18 E F0 .678(startup\214les or)3.178 F 1.234(in \214les)108 696 R F2
(.utr)3.734 E(ee-TERM)-.18 E F0 1.234(in your home directory where)3.734 F F2
(TERM)3.734 E F0 1.233(denotes the terminal type as de\214ned in the)3.734 F
.961(environment variable)108 708 R F2(TERM)3.461 E F0 5.961(.T)C .962
(hese startup \214les are built from lines like)242.253 708 R F2
('key_sequence=utr)3.462 E(ee_key')-.18 E F0(or)3.462 E F2
('key_sequence="string"')108 720 Q F0 6.407(.K)C 1.406
(ey_sequence describes the function key string, utree_key the)230.917 720 R F2
(utr)3.906 E(ee)-.18 E F0 1.406(key or)3.906 F(April 2 1992)72 768 Q(UTREE V)
255.31 768 Q(ersion 3.03-um)-1.11 E(12)530 768 Q EP
%%Page: 13 13
%%BeginPageSetup
BP
%%EndPageSetup
/F0 10/Times-Roman@0 SF 365.52(UTREE\(1L\) UTREE\(1L\))72 48 R .57(function na\
me, a string enclosed in braces a string to insert into the input buf)108 84 R
(fer)-.18 E 5.57(.A)-.55 G .57(comment lead in by a)451.62 84 R .182
(sharp sign \(#\) should contain the name of the bound key)108 96 R 5.182(.F)
-.65 G .181(or de\214ning key sequences of function keys con-)347.382 96 R .506
(trol keys are de\214ned with a leading caret \(^, i.e. ^x, ^? means DEL\) and\
 some other special characters may)108 108 R .459
(be de\214ned lead in by a backslash \(\\\).)108 120 R .458
(These escaped characters and their meaning are: b backspace \(^h\),)5.458 F(f)
5.458 E .112(formfeed \(^l\), n newline \(^j\), r return \(^m\), t tab \(^i\),\
 e or E escape \(^[ or ESC\) and s space.)108 132 R .113(If a backslash is)
5.112 F 1.483
(followed by up to three digits this de\214nes an octal given character)108 144
R 6.483(.I)-.55 G 1.483(.e. the de\214nition '\\e^O\\003=END')399.651 144 R
(binds the keystring <ESCAPE> <CONTROL-O> <ASCII-3> to the)108 156 Q/F1 10
/Times-Bold@0 SF(utr)2.5 E(ee)-.18 E F0(key or function END.)2.5 E .42
(The simplest way to de\214ne keys is to switch to the)108 172.8 R/F2 10
/Times-Italic@0 SF .42(bindings scr)2.92 F(een)-.37 E F0 .42
(and there to do all bindings.)2.92 F -1(Yo)5.42 G 2.92(uh)1 G(ave)526.12 172.8
Q .984
(only to hit the key to bind and terminate the key sequence with CR or NL.)108
184.8 R .984(Therefore CR or NL or key)5.984 F .238
(sequences containing CR or NL cannot be bound on the)108 196.8 R F2 .238
(bindings scr)2.738 F(een)-.37 E F0 5.238(.T)C .239(hen you have to enter the)
413.226 196.8 R F1(utr)2.739 E(ee)-.18 E F0 .341
(name where the key is to bind to.)108 208.8 R -1(Yo)5.341 G 2.841(uc)1 G .341
(an use C-n and C-p to select the wanted name.)271.989 208.8 R .34
(At last you should)5.341 F .159
(give a short comment to the bound key\(i.e. the key name\) for documentation.)
108 220.8 R .159(If you want to bind a key for)5.159 F .619(insertion of a str\
ing \(and so to bind function keys to simple letter commands\) you have to ent\
er the string)108 232.8 R .26(enclosed in double quotes.)108 244.8 R .26
(I.e. the binding)5.26 F F1('\\eh="h"')2.76 E F0 .261
(binds the key <ESCAPE> <h> to the string <h> and)2.761 F .809
(the so called help.)108 256.8 R .809(Any bindings you have done on the)5.809 F
F2 .809(bindings scr)3.309 F(een)-.37 E F0 .809(are saved to a \214le)3.309 F
F1(.utr)3.308 E(ee-TERM)-.18 E F0(into your home directory if the variable AUT)
108 268.8 Q(OSA)-.18 E(VE is set.)-1.29 E/F3 9/Times-Bold@0 SF(FUNCTION KEYS)72
285.6 Q F0 1.206(The following function keys are supported by)108 297.6 R F1
(utr)3.706 E(ee)-.18 E F0 1.206
(and pre-bound at startup to appropriate functions if)3.706 F
(they are de\214ned in your system')108 309.6 Q 2.5(st)-.55 G
(ermcap or terminfo database:)245.21 309.6 Q F1(CursorRight)108 338.4 Q F0
(Move forward \(FOR)208 338.4 Q -1.11(WA)-.55 G(RD,)1.11 E F1(C-f)2.5 E F0(\))A
F1(CursorLeft)108 355.2 Q F0(Move backward \(BACKW)208 355.2 Q(ARD,)-1.11 E F1
(C-b)2.5 E F0(\))A F1(CursorUp)108 372 Q F0(Move up \(PREVIOUS,)208 372 Q F1
(C-p)2.5 E F0(\))A F1(CursorDown)108 388.8 Q F0(Move down \(NEXT)208 388.8 Q(,)
-.74 E F1(C-n)2.5 E F0(\))A F1(Home/Begin)108 405.6 Q F0
(Move to beginning \(BEGIN,)208 405.6 Q F1(C-a)2.5 E F0(\))A F1(End)108 422.4 Q
F0(Move to end \(END,)208 422.4 Q F1(C-e)2.5 E F0(\))A F1(NextPage/PageDown)108
439.2 Q F0(Move one page down \(NEXTP)208 439.2 Q(AGE,)-.92 E F1(C-v)2.5 E F0
(\))A F1(Pr)108 456 Q(evPage/PageUp)-.18 E F0(Move one page up \(PREVP)208 456
Q(AGE,)-.92 E F1(C-w)2.5 E F0(\))A F1(Scr)108 472.8 Q(ollUp)-.18 E F0
(Scroll one line up \(UP)208 472.8 Q(,)-1.11 E F1(C-u)2.5 E F0(\))A F1(Scr)108
489.6 Q(ollDown)-.18 E F0(Scroll one line down or delecte character \(DOWN,)208
489.6 Q F1(C-d)2.5 E F0(\))A F1(Insert)108 506.4 Q F0(Change to directory \()
208 506.4 Q F1(>)A F0 2.5(\)o)C 2.5(rs)309.79 506.4 S
(witch insert/overwrite mode \(INSER)319.51 506.4 Q -.74(T,)-.6 G F1(C-o)3.24 E
F0(\))A F1(Delete)108 523.2 Q F0 .372(Change to parent directory \()208 523.2 R
F1(<)A F0 2.872(\)o)C 2.872(rd)339.14 523.2 S .371
(elete character under cursor like \(DELETE,)350.342 523.2 R F1(C-)2.871 E(h)
208 535.2 Q F0(\))A F1(Clear)108 552 Q F0
(Refresh screen or input line \(REFRESH,)208 552 Q F1(C-l)2.5 E F0(\))A F1
(Help)108 568.8 Q F0(Call the help menu \(HELP)208 568.8 Q(,)-1.11 E F1(C-r)2.5
E F0(\))A F1(Select)108 585.6 Q F0(Select and/or accept \(SELECT)208 585.6 Q(,)
-.74 E F1(CR,NL)2.5 E F0(\))A F1(Do/Command)108 602.4 Q F0
(Select and/or accept \(SELECT)208 602.4 Q(,)-.74 E F1(CR,NL)2.5 E F0(\))A F1
(Mark)108 619.2 Q F0(Set a mark \(SETMARK,)208 619.2 Q F1(C-@)2.5 E F0(\))A F1
(Enter)108 636 Q F0(Select and/or accept \(SELECT)208 636 Q(,)-.74 E F1(CR,NL)
2.5 E F0(\))A .227
(This function keys may be rebound or other function keys may be bound on the)
108 652.8 R F2 .228(bindings scr)2.728 F(een)-.37 E F0 5.228(.F)C .228(or more)
508.392 652.8 R(information about key bindings see the section)108 664.8 Q F1
(key bindings)2.5 E F0(above.)2.5 E F3(ENVIRONMENT)72 681.6 Q F1(UTREE)108
693.6 Q F0(Some boolean and numerical settings for)208 693.6 Q F1(utr)2.5 E(ee)
-.18 E(UTLIB)108 710.4 Q F0(Directory for)208 710.4 Q F1(utr)2.5 E(ee)-.18 E F0
(help pages and startup \214les)2.5 E(April 2 1992)72 768 Q(UTREE V)255.31 768
Q(ersion 3.03-um)-1.11 E(13)530 768 Q EP
%%Page: 14 14
%%BeginPageSetup
BP
%%EndPageSetup
/F0 10/Times-Roman@0 SF 365.52(UTREE\(1L\) UTREE\(1L\))72 48 R/F1 10
/Times-Bold@0 SF(HOME)108 84 Q F0 -2.13(User ')208 84 R 2.5(sh)-.55 G
(ome directory)241.42 84 Q F1(TERM)108 100.8 Q F0 -.7(Te)208 100.8 S
(rminal type).7 E F1(EDIT)108 117.6 Q(OR)-.18 E F0(File editor)208 117.6 Q F1
-.74(PA)108 134.4 S(GER).74 E F0(File pager/viewer)208 134.4 Q F1(SHELL)108
151.2 Q F0(Interactive shell for shell escape)208 151.2 Q/F2 9/Times-Bold@0 SF
(FILES)72 172.8 Q F1(HOME/.utr)108 184.8 Q(eelist)-.18 E F0 -.35(Tr)208 184.8 S
(ee list \214le of user).35 E 1.1 -.55('s \214).37 H(lesystem).55 E F1
(HOME/.utr)108 201.6 Q(eehist)-.18 E F0(History list of shell commands)208
201.6 Q F1(HOME/.utr)108 218.4 Q(ee)-.18 E F0 -2.13(User ')208 218.4 R(s)-.55 E
F1(utr)2.5 E(ee)-.18 E F0
(startup \214le containing variables and \214letype commands de\214nitions)2.5
E F1(UTLIB/utr)108 235.2 Q(ee)-.18 E F0(Global)208 235.2 Q F1(utr)2.5 E(ee)-.18
E F0(startup \214le containing variables and \214letype commands de\214nitions)
2.5 E F1(UTLIB/utr)108 252 Q(ee-TERM)-.18 E F0
(Global key bindings for terminal type TERM)208 252 Q F1(HOME/.utr)108 268.8 Q
(ee-TERM)-.18 E F0 -2.13(User ')7.69 F 2.5(sk)-.55 G
(ey bindings for terminal type TERM)241.42 268.8 Q F1(UTLIB/utr)108 285.6 Q
20.73(ee.help Utr)-.18 F(ee)-.18 E F0(help pages)2.5 E F1(BIN/utr)108 302.4 Q
(ee.backup)-.18 E F0(Backup shell script or program)208 302.4 Q F1(BIN/utr)108
319.2 Q(ee.mklist)-.18 E F0(Create directory tree list shell script)208 319.2 Q
F1(BIN/utr)108 336 Q(ee.prlist)-.18 E F0
(Display or print a formatted tree list \214le)208 336 Q F1(UTLIB)108 352.8 Q
F0 .155(can be a system default directory containing library \214les \(i.e. /u\
sr/local/lib\) de\214ned at compile time)2.655 F .156
(or a directory de\214ned in the environment variable)108 364.8 R F1(UTLIB)
2.656 E F0(.)A F1(BIN)5.156 E F0 .156
(is a public directory containing executable)2.656 F(\214les \(i.e. /usr/local\
/bin\) and should be included in the command search path environment variable)
108 376.8 Q F1 -.74(PAT)2.5 G(H).74 E F0(.)A F2(SEE ALSO)72 393.6 Q F0
(utree.prlist\(1L\))108 405.6 Q(cp\(1\) grep\(1\) ls\(1\) mv\(1\) rm\(1\))108
417.6 Q(du\(1\) mkdir\(1\) rmdir\(1\))108 429.6 Q(\214nd\(1\) sh\(1\))108 441.6
Q F2(BUGS)72 458.4 Q F0(Changes in \214lesystem after shell escape or editor s\
ession are not always detected.)108 470.4 Q
(Directory tree depth >32 may be confusing.)108 482.4 Q
(Screen sizes smaller than 80x24 may be confusing.)108 494.4 Q
(Symbolic links to directories may be confusing.)108 506.4 Q(On most BSD syste\
ms changing owner and/or group of \214les for normal users is not allowed.)108
518.4 Q F2(AUTHOR)72 535.2 Q F0(Peter Klingebiel)108 547.2 Q F2(COPYRIGHT)72
564 Q F0 2.5<a931>108 576 S(991/1992 Peter Klingebiel & UNIX Magazin Munich)
123.1 576 Q .204(Permission is granted to copy and distribute)108 600 R F1(utr)
2.703 E(ee)-.18 E F0 .203
(in modi\214ed or unmodi\214ed form, for noncommercial use,)2.703 F .594(provi\
ded \(a\) this copyright notice is preserved, \(b\) no attempt is made to rest\
rict redistribution of this \214le,)108 612 R .021(and \(c\) this \214le is no\
t distributed as part of any collection whose redistribution is restricted by \
a compilation)108 624 R(copyright.)108 636 Q(April 2 1992)72 768 Q(UTREE V)
255.31 768 Q(ersion 3.03-um)-1.11 E(14)530 768 Q EP
%%Trailer
end
%%EOF
