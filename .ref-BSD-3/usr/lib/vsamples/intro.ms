.SH
Introduction
.PP
This catalog gives samples of the various fonts available at
Berleley on Ernie Co-Vax using vtroff on the Versatec Printer/Plotter.
These have NOT been extensively tested, and no guarantee of any kind
is made concerning their reliability.
.PP
To use these fonts:
.IP (1)
Hershey. This is the default font. To get it, use vtroff directly.
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
