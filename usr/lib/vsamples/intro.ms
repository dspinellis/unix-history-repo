.SH
Introduction
.PP
This catalog gives samples of the various fonts available at
Berkeley using vtroff on our Versatec and Varian.
We have them working 4 pages across in a 36 inch Versatec,
and rotated 90 degrees on a Benson-Varian 11 inch plotter.
The same software should be adaptable to an 11 inch Versatec,
and in fact is running at several other sites,
however, not having one here, it isn't part of this distribution.
Such a driver is available from Tom Ferrin at UCSF.
.PP
To use these fonts:
.IP (1)
Hershey.  This is the default font.
The Hershey font is currently the
.I only
complete font, with all 16 point sizes
and all the special characters troff knows about.
To get it, use vtroff directly.
To illustrate this with the \-ms macro package:
.DS
.B "vtroff \-ms paper.nr"
.DE
.IP (2)
Fonts with roman, italic, and bold, such as nonie.
You can load all three fonts with, for example:
.DS
.B "vtroff \-F nonie \-ms paper.nr"
.DE
.IP
To get just one of these fonts, use (3) below, appending .r, .i, or .b
to the font name to specify which font you want mounted, e.g.,
to get italics in delegate,
.DS
.B "vtroff \-2 delegate.i \-ms paper.nr"
.DE
.IP (3)
To get a font without a complete set,
choose which font (1, 2, or 3) you want replaced by the chosen font.
For example, to use bocklin as though it were bold, since font 3 is bold, use:
.DS
.B "vtroff \-3 bocklin \-ms paper.nr"
.DE
.PP
To switch between fonts in troff, use
.DS
.B "\&.ft 3"
.DE
to switch to font 3, for example, or
use
.DS
.B "\ef3word\ef1"
.DE
to switch within a line.
For more information see the Nroff/Troff Users Manual.
.PP
Special note:
.I troff
thinks it is talking to a CAT phototypesetter.
Thus, it does all sorts of strange things,
such as enforcing restrictions like 7.54 inches maximum
width, 4 fonts, a certain 16 point sizes, proportional
spacing by point size, etc.
.PP
In particular, the following glyphs will
.I always
be taken from the special font, no matter what font you
are using at the time:
.DS
@, #, ", \', \`, <, >, \e, {, }, ~, ^, and _.
.DE
This may explain what are otherwise surprising results
in some of the subsequent pages.
.PP
In addition, the following Greek letters have been decreed
by
.I troff
as looking so much like their Roman counterparts
that the Roman version (font 1) is always printed, no matter what
font is mounted on font 1 at the time:
.DS
A, B, E, Z, H, I, K, M, N, O, P, T, X.
.DE
(See table II in the back of the Nroff/Troff Users's Manual
for details about what glyphs are in each font and how to
generate the special glyphs.)
