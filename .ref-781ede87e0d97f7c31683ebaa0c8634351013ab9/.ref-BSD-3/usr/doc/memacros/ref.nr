.nr MO 25			\" mod number
.de TL			\" *** title line
.lp
.di XX
..
.de DE			\" *** description
\\\\h'|\\n(DIu'\\\\c
.br
.di
.in +\\n(DIu
.ti 0
.cu 1000
.XX
.rm XX
.cu 0
..
.ds N \s-1NROFF\s0
.ds T \s-1TROFF\s0
.nr DI 1.5i
.he '\-ME REFERENCE MANUAL''%'
.de NR
.b "\en\\$1" "\\$2"
..
.de ST
.b "\e*\\$1" "\\$2"
..
.sc
.+c
.ce 20
.sz 14
.b "\-ME REFERENCE MANUAL"
.sz
.sp
.i "Release 1.1/\n(MO"
.sp 2
.ul
Eric P. Allman
.sp
Electronics Research Laboratory
University of California, Berkeley
Berkeley, California  94720
.ce 0
.sp 4
.pp
This document describes
in extremely terse form
the features
of the
.b \-me
macro package
for version seven \*N/\*T\*(dg.
.(f
\(dg\*N and \*T are Trademarks of Bell Laboratories.
.)f
Some familiarity is assumed
with
those programs,
specifically,
the reader should understand
breaks,
fonts,
pointsizes,
the use and definition of number registers
and strings,
how to define macros,
and scaling factors for ens, points,
.b v 's
(vertical line spaces),
etc.
.pp
For a more casual introduction
to text processing
using \*N,
refer to the document
.ul
Writing Papers with \*N using \-me.
.pp
There are a number of macro parameters
that may be adjusted.
Fonts may be set to a font number only.
In \*N font 8
is underlined,
and is set in bold font in \*T
(although font
3,
bold in \*T,
is not underlined in \*N).
Font 0 is no font change;
the font of the surrounding text
is used instead.
Notice that fonts 0 and 8 are
.q pseudo-fonts ;
that is,
they are simulated by the macros.
This means that although it is legal to set a font register
to zero or eight,
it is not legal to use the escape character form,
such as:
.(b
\ef8
.)b
.pp
All distances
are in basic units,
so it is nearly always necessary
to use a scaling factor.
For example,
the request
to set the paragraph indent
to eight one-en spaces is:
.(b
\&.nr pi 8n
.)b
and not
.(b
\&.nr pi 8
.)b
which would set the paragraph indent to eight basic units,
or about 0.02 inch.
Default parameter values are given in brackets
in the remainder of this document.
.pp
Registers and strings
of the form
.b $ \c
.i x
may be used in expressions
but should not be changed.
Macros of the form
.b $ \c
.i x
perform some function
(as described)
and may be redefined
to change this function.
This may be a sensitive operation;
look at the body of the original macro
before changing it.
.pp
All names in \-me
follow a rigid naming convention.
The user may define number registers,
strings,
and macros,
provided that s/he
uses single character upper case names
or double character names
consisting of letters and digits,
with at least one upper case letter.
In no case should special characters
be used in user-defined names.
.pp
On daisy wheel type printers
in twelve pitch,
the
.b \-rx1
flag can be stated to make lines default to
one eighth inch
(the normal spacing for a newline in twelve-pitch).
This is normally too small for easy readability,
so the default is to space one sixth inch.
.pp
This documentation was
.if n \*N'ed
.if t \*T'ed
on \*(td
and applies to version
1.1/\n(MO
of the \-me macros.
.sh 1 "Paragraphing"
.pp
These macros are used
to begin paragraphs.
The standard paragraph macro
is
.b .pp ;
the others are all variants
to be used for special purposes.
.pp
The first call to one of the paragraphing macros
defined in this section
or the
.b .sh
macro
(defined in the next session)
.i initializes
the macro processor.
After initialization
it is not possible to use any of the following requests:
.b .sc ,
.b .lo ,
.b .th ,
or
.b .ac .
Also,
the effects of changing parameters
which will have a global effect
on the format of the page
(notably page length and header and footer margins)
are not well defined
and should be avoided.
.TL
.b .lp
.DE
Begin left-justified paragraph.
Centering and underlining
are turned off if they were on,
the font is set to
.NR (pf
[1]
the type size
is set to
.NR (pp
[10p],
and a
.NR (ps
space is inserted
before the paragraph
[0.35v in \*T, 1v or 0.5v in \*N
depending on device resolution].
The indent is reset
to
.NR ($i
[0]
plus
.NR (po
[0]
unless the paragraph
is inside a display.
(see
.b .ba ).
At least
the first two lines
of the paragraph
are kept together
on a page.
.TL
.b .pp
.DE
Like
.b .lp ,
except that it puts
.NR (pi
[5n]
units of indent.
This is the standard paragraph macro.
.TL
.b .ip
.i T
.i I
.DE
Indented paragraph
with hanging tag.
The body of the following paragraph
is indented
.i I
spaces
(or
.NR (ii
[5n]
spaces
if
.i I
is not specified)
more than a non-indented paragraph
(such as with
.b .pp )
is.
The title
.i T
is exdented (opposite of indented).
The result is a paragraph
with an even left edge
and
.i T
printed in the margin.
Any spaces in
.i T
must be unpaddable.
If
.i T
will not fit in the space provided,
.b .ip
will start a new line.
.TL
.b .np
.DE
A variant of .ip which numbers paragraphs.
Numbering is reset
after a
.b .lp ,
.b .pp ,
or
.b .sh .
The current paragraph number
is in
.NR ($p .
.sh 1 "Section Headings"
.pp
Numbered sections
are similiar to paragraphs
except that a
section number
is automatically
generated for each one.
The section numbers are of the form
.b 1.2.3 .
The
.i depth
of the section
is the count of numbers
(separated by decimal points)
in the section number.
.pp
Unnumbered section headings are similar,
except that no number is attached
to the heading.
.TL
.b .sh
.i +N
.i T
.i "a b c d e f"
.DE
Begin numbered section
of depth
.i N .
If
.i N
is missing
the current depth
(maintained in
the number register
.NR ($0 )
is used.
The values of
the individual parts of the section number
are maintained in
.NR ($1
through
.NR ($6 .
There is a
.NR (ss
[1v]
space before the section.
.i T
is printed
as a section title
in font
.NR (sf
[8]
and size
.NR (sp
[10p].
The
.q name
of the section may be accessed via
.ST ($n .
If
.NR (si
is non-zero,
the base indent
is set to
.NR (si
times the section depth,
and the section title
is exdented.
(See
.b .ba .)
Also,
an additional indent of
.NR (so
[0]
is added to the section title
(but not to the body of the section).
The font is then set
to the paragraph font,
so that more information may occur
on the line
with the section number
and title.
.b .sh
insures that there is enough room
to print the section head
plus the beginning of a paragraph
(about 3 lines total).
If
.i a
through
.i f
are specified,
the section number is set to that number
rather than incremented automatically.
If any of
.i a
through
.i f
are a hyphen
that number is not reset.
If
.i T
is a single underscore
(\c
.q _ )
then the section depth and numbering is reset,
but the base indent is not reset
and nothing is printed out.
This is useful to automatically
coordinate section numbers with
chapter numbers.
.TL
.b .sx
.i +N
.DE
Go to section depth
.i N
[\c
.b \-1 ],
but do not print the number
and title,
and do not increment the section number
at level
.i N .
This has the effect
of starting a new paragraph
at level
.i N .
.TL
.b .uh
.i T
.DE
Unnumbered section heading.
The title
.i T
is printed
with the same rules for spacing,
font, etc.,
as for
.b .sh .
.TL
.b .$p
.i T
.i B
.i N
.DE
Print section heading.
May be redefined
to get fancier headings.
.i T
is the title passed on the
.b .sh
or
.b .uh
line;
.i B
is the section number for this section,
and
.i N
is the depth of this section.
These parameters are not always present;
in particular,
.b .sh
passes all three,
.b .uh
passes only the first,
and
.b .sx
passes three,
but the first two
are null strings.
Care should be taken if this macro
is redefined;
it is quite complex and subtle.
.TL
.b .$0
.i T
.i B
.i N
.DE
This macro is called automatically
after every call to
.b .$p .
It is normally undefined,
but may be used
to automatically put
every section title
into the table of contents
or for some similiar function.
.i T
is the section title
for the section title which was just printed,
.i B
is the section number,
and
.i N
is the section depth.
.TL
.b .$1
\-
.b .$6
.DE
Traps called just before printing that depth section.
May be defined to
(for example)
give variable spacing
before sections.
These macros are called from
.b .$p ,
so if you redefine that macro
you may lose this feature.
.sh 1 "Headers and Footers"
.ds TP \fI\(aal\|\(aam\^\(aar\^\(aa\fP
.pp
Headers and footers
are put at the top and bottom
of every page
automatically.
They are set in font
.NR (tf
[3]
and size
.NR (tp
[10p].
Each of the definitions
apply as of the
.i next
page.
Three-part titles
must be quoted
if there are two blanks adjacent
anywhere in the title
or more than eight blanks total.
.pp
The spacing
of headers and footers
are controlled by three number registers.
.NR (hm
[4v]
is the distance from the top of the page
to the top of the header,
.NR (fm
[3v]
is the distance from the bottom of the page
to the bottom of the footer,
.NR (tm
[7v]
is the distance from the top of the page
to the top of the text,
and
.NR (bm
[6v]
is the distance from the bottom of the page
to the bottom of the text
(nominal).
The macros
.b .m1 ,
.b .m2 ,
.b .m3 ,
and
.b .m4
are also supplied for compatibility
with
\s-1ROFF\s0 documents.
.TL
.b .he
\*(TP
.DE
Define three-part header,
to be printed on the top
of every page.
.TL
.b .fo
\*(TP
.DE
Define footer,
to be printed at the bottom
of every page.
.TL
.b .eh
\*(TP
.DE
Define header,
to be printed at the top of every
even-numbered page.
.TL
.b .oh
\*(TP
.DE
Define header,
to be printed at the top of every
odd-numbered page.
.TL
.b .ef
\*(TP
.DE
Define footer,
to be printed at the bottom
of every even-numbered page.
.TL
.b .of
\*(TP
.DE
Define footer,
to be printed at the bottom
of every odd-numbered page.
.TL
.b .hx
.DE
Suppress headers and footers
on the next page.
.TL
.b .m1
.i +N
.DE
Set the space between the top of the page
and the header
[4v].
.TL
.b .m2
.i +N
.DE
Set the space between the header
and the first line of text
[2v].
.TL
.b .m3
.i +N
.DE
Set the space
between the bottom of the text
and the footer
[2v].
.TL
.b .m4
.i +N
.DE
Set the space
between the footer
and the bottom of the page
[4v].
.TL
.b .ep
.DE
End this page,
but do not begin the next page.
Useful for forcing out footnotes,
but other than
that hardly every used.
Must be followed by a
.b .bp
or the end of input.
.TL
.b .$h
.DE
Called at every page
to print the header.
May be redefined
to provide fancy
(e.g.,
multi-line)
headers,
but doing so
loses the function of the
.b .he ,
.b .fo ,
.b .eh ,
.b .oh ,
.b .ef ,
and
.b .of
requests,
as well as the chapter-style title feature
of
.b .+c .
.TL
.b .$f
.DE
Print footer;
same comments apply
as in
.b .$h .
.TL
.b .$H
.DE
A normally undefined macro
which is called
at the top of each page
(after outputing
the header,
initial saved floating keeps,
etc.);
in other words,
this macro is called immediately before
printing text
on a page.
It can be used for column headings
and the like.
.sh 1 "Displays"
.pp
All displays except centered blocks
and block quotes
are preceeded and followed
by an extra
.NR (bs
[same as
.NR (ps ]
space.
Quote spacing is stored in a separate register;
centered blocks have no default initial or trailing space.
The vertical spacing of all displays except quotes
and centered blocks
is stored in register
.NR ($R
instead of
.NR ($r .
.TL
.b .(l
.i m
.i f
.DE
Begin list.
Lists are single spaced,
unfilled text.
If
.i f
is
.b F ,
the list will be filled.
If
.i m
[\c
.b I ]
is
.b I
the list is indented by
.NR (bi
[4n];
if
.b M
the list is indented to the left margin;
if
.b L
the list is left justified with respect to the text
(different from
.b M
only if the base indent
(stored in
.NR ($i
and set with
.b .ba )
is not zero);
and if
.b C
the list is centered on a line-by-line basis.
The list is set in font
.NR (df
[0].
Must be matched by a
.b .)l .
This macro is almost like
.b .(b
except that no attempt is made
to keep the display on one page.
.TL
.b .)l
.DE
End list.
.TL
.b .(q
.DE
Begin major quote.
These are single spaced,
filled,
moved in from the text
on both sides
by
.NR (qi
[4n],
preceeded and followed
by
.NR (qs
[same as
.NR (bs ]
space,
and are set in point size
.NR (qp
[one point smaller than surrounding text].
.TL
.b .)q
.DE
End major quote.
.TL
.b .(b
.i m
.i f
.DE
Begin block.
Blocks are a form of
.i keep ,
where the text of a keep
is kept together on one page
if possible
(keeps are useful
for tables and figures
which should not be broken
over a page).
If the block will not fit
on the current page
a new page is begun,
.i unless
that would leave more than
.NR (bt
[0]
white space
at the bottom of the text.
If
.NR (bt
is zero, the threshold feature
is turned off.
Blocks are not filled
unless
.i f
is
.b F ,
when they are filled.
The block will be left-justified
if
.i m
is
.b L ,
indented by
.NR (bi
[4n]
if
.i m
is
.b I
or absent,
centered
(line-for-line)
if
.i m
is
.b C ,
and left justified to the margin
(not to the base indent)
if
.i m
is
.b M .
The block is set in font
.NR (df
[0].
.TL
.b .)b
.DE
End block.
.TL
.b .(z
.i m
.i f
.DE
Begin floating keep.
Like
.b .(b
except that the keep is
.i floated
to the bottom of the page
or the top of the next page.
Therefore,
its position relative to the text changes.
The floating keep is preceeded and followed
by
.NR (zs
[1v]
space.
Also,
it defaults to mode
.b M .
.TL
.b .)z
.DE
End floating keep.
.TL
.b .(c
.DE
Begin centered block.
The next keep
is centered as a block,
rather than on a line-by-line basis
as with
.b ".(b C" .
This call may be nested
inside keeps.
.TL
.b .)c
.DE
End centered block.
.sh 1 Annotations
.TL
.b .(d
.DE
Begin delayed text.
Everything in the next keep
is saved for output
later with
.b .pd ,
in a manner
similar to footnotes.
.TL
.b .)d
.i n
.DE
End delayed text.
The delayed text number register
.NR ($d
and the associated string
.ST #
are incremented if
.ST #
has been referenced.
.TL
.b .pd
.DE
Print delayed text.
Everything diverted via
.b .(d
is printed and truncated.
This might be used
at the end of each chapter.
.TL
.b .(f
.DE
Begin footnote.
The text of the footnote
is floated to the bottom
of the page
and set in font
.NR (ff
[1]
and size
.NR (fp
[8p].
Each entry
is preceeded by
.NR (fs
[0.2v]
space,
is indented
.NR (fi
[3n]
on the first line,
and is indented
.NR (fu
[0]
from the right margin.
Footnotes line up underneath
two columned output.
If the text of the footnote
will not all fit on one page
it will be carried over
to the next page.
.TL
.b .)f
.i n
.DE
End footnote.
The number register
.NR ($f
and the associated string
.ST *
are incremented
if they have been referenced.
.TL
.b .$s
.DE
The macro to output the footnote seperator.
This macro may be redefined
to give other size lines or other types
of separators.
Currently
it draws a 1.5i line.
.TL
.b .(x
.i x
.DE
Begin index entry.
Index entries are saved in the index
.i x
[\c
.b x ]
until called up with
.b .xp.
Each entry is preceeded
by a
.NR (xs
[0.2v]
space.
Each entry is
.q undented
by
.NR (xu
[0.5i];
this register tells how far the page number
extends into the right margin.
.TL
.b .)x
.i P
.i A
.DE
End index entry.
The index entry
is finished with a row of dots
with
.i A
[null]
right justified on the last line
(such as for an author's name),
followed by P
[\c
.NR % ].
If
.i A
is specified,
.i P
must be specified;
.NR %
can be used to print the current page number.
If
.i P
is an underscore,
no page number
and no row of dots
are printed.
.TL
.b .xp
.i x
.DE
Print index
.i x
[\c
.b x ].
The index is formated in the font, size, and so forth
in effect at the time it is printed,
rather than at the time it is collected.
.sh 1 "Columned Output"
.TL
.b .2c
.i +S
.i N
.DE
Enter two-column mode.
The column separation is set to
.i +S
[4n, 0.5i in ACM mode]
(saved in
.NR ($s ).
The column width,
calculated to fill the single column line length
with both columns,
is stored in
.NR ($l .
The current column
is in
.NR ($c .
You can test register
.NR ($m
[1]
to see if you are in single column
or double column mode.
Actually,
the request enters
.i N
[2]
columned output.
.TL
.b .1c
.DE
Revert to single-column mode.
.TL
.b .bc
.DE
Begin column.
This is like
.b .bp
except that it begins a new column
on a new page
only if necessary,
rather than forcing a whole new page
if there is another column left
on the current page.
.sh 1 "Fonts and Sizes"
.TL
.b .sz
.i +P
.DE
The pointsize is set to
.i P
[10p],
and the line spacing is set proportionally.
The ratio of line spacing to pointsize
is stored in
.NR ($r .
The ratio used internally
by displays and annotations
is stored in
.NR ($R
(although this is not used by
.b .sz ).
.TL
.b .r
.i W
.i X
.DE
Set
.i W
in roman font,
appending
.i X
in the previous font.
To append different font requests,
use
.i X
=
.b \ec .
If no parameters,
change to roman font.
.TL
.b .i
.i W
.i X
.DE
Set
.i W
in italics,
appending
.i X
in the previous font.
If no parameters,
change to italic font.
Underlines in \*N.
.TL
.b .b
.i W
.i X
.DE
Set
.i W
in bold font
and append
.i X
in the previous font.
If no parameters,
switch to bold font.
In \*N,
underlines.
.TL
.b .rb
.i W
.i X
.DE
Set
.i W
in bold font
and append
.i X
in the previous font.
If no parameters,
switch to bold font.
.b .rb
differs from
.b .b
in that
.b .rb
does not underline in \*N.
.TL
.b .u
.i W
.i X
.DE
Underline
.i W
and append
.i X .
This is a true underlining,
as opposed to the
.b .ul
request,
which changes to
.q "underline font"
(usually italics in \*T).
It won't work right
if
.i W
is spread or broken (including hyphenated).
In other words,
it is safe in nofill mode only.
.TL
.b .q
.i W
.i X
.DE
Quote
.i W
and append
.i X .
In \*N
this just surrounds
.i W
with double quote marks
(`\|\c
.b """" \|'),
but in \*T
uses directed quotes.
.TL
.b .bi
.i W
.i X
.DE
Set
.i W
in bold italics
and append
.i X .
Actually,
sets
.i W
in italic
and overstrikes once.
Underlines in \*N.
It won't work right
if
.i W
is spread or broken (including hyphenated).
In other words,
it is safe in nofill mode only.
.TL
.b .bx
.i W
.i X
.DE
Sets
.i W
in a box,
with
.i X
appended.
Underlines in \*N.
It won't work right
if
.i W
is spread or broken (including hyphenated).
In other words,
it is safe in nofill mode only.
.sh 1 "Roff Support"
.TL
.b .ix
.i +N
.DE
Indent,
no break.
Equivalent to
.b \(aain
.i N .
.TL
.b .bl
.i N
.DE
Leave
.i N
contiguous white space,
on the next page if not enough room
on this page.
Equivalent to a
.b .sp
.i N
inside a block.
.TL
.b .pa
.i +N
.DE
Equivalent to
.b .bp .
.TL
.b .ro
.DE
Set page number
in roman numerals.
Equivalent to
.b ".af % i" .
.TL
.b .ar
.DE
Set page number in arabic.
Equivalent to
.b ".af % 1" .
.TL
.b .n1
.DE
Number lines in margin from one
on each page.
.TL
.b .n2
.i N
.DE
Number lines from
.i N ,
stop if
.i N
= 0.
.TL
.b .sk
.DE
Leave the next output page blank,
except for headers and footers.
This is used to leave space
for a full-page diagram
which is produced externally
and pasted in later.
To get a partial-page paste-in display,
say
.b .sv \ \c
.i N ,
where
.i N
is the amount of space
to leave;
this space will be output immediately
if there is room,
and will otherwise be output
at the top of the next page.
However, be warned:
if
.i N
is greater than the amount of available space
on an empty page,
no space will ever be output.
.sh 1 "Preprocessor Support"
.TL
.b .EQ
.i m
.i T
.DE
Begin equation.
The equation is centered
if
.i m
is
.b C
or omitted,
indented
.NR (bi
[4n]
if
.i m
is
.b I ,
and left justified if
.i m
is
.b L .
.i T
is a title printed on the right margin
next to the equation.
See
.i "Typesetting Mathematics \- User's Guide"
by Brian W. Kernighan
and Lorinda L. Cherry.
.TL
.b .EN
.i c
.DE
End equation.
If
.i c
is
.b C
the equation must be continued
by immediately following
with another
.b .EQ ,
the text of which
can be centered
along with this one.
Otherwise,
the equation is printed,
always on one page,
with
.NR (es
[0.5v in \*T, 1v in \*N]
space
above and below it.
.TL
.b .TS
.i h
.DE
Table start.
Tables are single spaced
and kept on one page
if possible.
If you have a large table
which will not fit on one page,
use
.i h
=
.b H
and follow the header part
(to be printed on every page of the table)
with a
.b .TH .
See
.i "Tbl \- A Program to Format Tables"
by M. E. Lesk.
.TL
.b .TH
.DE
With
.b ".TS H" ,
ends the header portion of the table.
.TL
.b .TE
.DE
Table end.
Note that this table
does not float,
in fact,
it is not even guaranteed to stay on one page
if you use requests such as
.b .sp
intermixed with the text
of the table.
If you want it to float
(or if you use requests
inside the table),
surround the entire table
(including the
.b .TS
and
.b .TE
requests)
with the requests
.b .(z
and
.b .)z .
.sh 1 "Miscellaneous"
.TL
.b .re
.DE
Reset tabs.
Set to every 0.5i
in \*T
and every 0.8i in \*N.
.TL
.b .ba
.i +N
.DE
Set the base indent
to
.i +N
[0]
(saved in
.NR ($i ).
All paragraphs,
sections,
and displays
come out indented by this amount.
Titles and footnotes
are unaffected.
The
.b .sh
request performs a
.b .ba
request
if
.NR (si
[0] is not zero,
and sets the base indent to
.NR (si \c
.b * \c
.NR ($0 .
.TL
.b .xl
.i +N
.DE
Set the line length to
.i N
[6.0i].
This differs
from
.b .ll
because it only affects the current environment.
.TL
.b .ll
.i +N
.DE
Set line length in all environments
to
.i N
[6.0i].
This should not be used
after output has begun,
and particularly not in two-columned output.
The current line length is stored in
.NR ($l .
.TL
.b .hl
.DE
Draws a horizontal line
the length of the page.
This is useful
inside floating keeps
to differentiate
between the text
and the figure.
.TL
.b .lo
.DE
This macro loads another set of macros
(in
.b /usr/lib/me/local.me )
which is intended to be a set of locally defined macros.
These macros
should all be of the form
.b .* \c
.i X ,
where
.i X
is any letter
(upper or lower case)
or digit.
.sh 1 "Standard Papers"
.TL
.b .tp
.DE
Begin title page.
Spacing at the top of the page
can occur,
and headers and footers are supressed.
Also,
the page number
is not incremented
for this page.
.TL
.b .th
.DE
Set thesis mode.
This defines the modes acceptable
for a doctoral dissertation
at Berkeley.
It double spaces,
defines the header
to be a single page number,
and changes the margins
to be 1.5 inch on the left
and one inch on the top.
.b .++
and
.b .+c
should be used with it.
This macro must be stated
before
initialization,
that is,
before the first call of a paragraphing
macro
or
.b .sh .
.TL
.b .++
.i m
.i H
.DE
This request defines the section of the paper
which we are entering.
The section type is defined by
.i m .
.b C
means that we are entering the chapter portion
of the paper,
.b A
means that we are entering the appendix portion
of the paper,
.b P
means that the material following
should be the preliminary portion
(abstract, table of contents, etc.)
portion of the paper,
.b AB
means that we are entering the abstract
(numbered independently from 1
in Arabic numerals),
and
.b B
means that we are entering the bibliographic
portion at the end of the paper.
Also, the variants
.b RC
and
.b RA
are allowed,
which specify renumbering of pages
from one at the beginning of each
chapter or appendix,
respectively.
The
.i H
parameter defines the new header.
If there are any spaces in it,
the entire header must be quoted.
If you want the header to have the chapter number
in it,
Use the string
.b "\e\e\e\en(ch" .
For example, to number appendixes
.b A.1
etc.,
type
.b ".++ RA \(aa\(aa\(aa\e\e\e\en(ch.%\(aa" .
Each section
(chapter, appendix, etc.)
should be preceeded by the
.b .+c
request.
It should be mentioned
that it is easier when using
\*T to put the front material
at the end of the paper,
so that the table of contents
can be collected and output;
this material can then be physically
moved to the beginning of the paper.
.TL
.b .+c
.i T
.DE
Begin chapter with title
.i T .
The chapter number
is maintained in
.NR (ch .
This register is incremented
every time
.b .+c
is called with a parameter.
The title and chapter number
are printed by
.b .$c .
The header is moved to the footer
on the first page
of each chapter.
If
.i T
is omitted,
.b .$c
is not called;
this is useful for doing your own
.q "title page"
at the beginning of papers
without a title page proper.
.b .$c
calls
.b .$C
as a hook so that chapter titles can be inserted
into a table of contents automatically.
The footnote numbering is reset to one.
.TL
.b .$c
.i T
.DE
Print chapter number
(from
.NR (ch )
and
.i T .
This macro can be redefined to your liking.
It is defined by default
to be acceptable
for a PhD thesis
at Berkeley.
This macro calls
.b $C ,
which can be defined to make index entries,
or whatever.
.TL
.b .$C
.i K
.i N
.i T
.DE
This macro is called by
.b .$c .
It is normally undefined,
but can be used to automatically insert
index entries,
or whatever.
.i K
is a keyword,
either
.q Chapter
or
.q Appendix
(depending on the
.b .++
mode);
.i N
is the chapter or appendix number,
and
.i T
is the chapter or appendix title.
.TL
.b .ac
.i A
.i N
.DE
This macro
(short for
.b .acm )
sets up the \*N environment
for photo-ready papers
as used by the ACM.
This format is 25% larger,
and has no headers or footers.
The author's name
.i A
is printed at the bottom of the page
(but off the part which will be printed
in the conference proceedings),
together with the current page number
and the total number of pages
.i N .
Additionally,
this macro loads the file
.b /usr/lib/me/acm.me ,
which may later be augmented with other macros
useful for printing papers
for ACM conferences.
It should be noted
that this macro will not work correctly in \*T,
since it sets the page length
wider than the physical width
of the phototypesetter roll.
.sh 1 "Predefined Strings"
.TL
.ST *
.DE
Footnote number, actually
.ST [ \c
.NR ($f \c
.ST ] .
This macro is incremented
after each call to
.b .)f .
.TL
.ST #
.DE
Delayed text number.
Actually
[\c
.NR ($d ].
.TL
.ST [
.DE
Superscript.
This string gives upward movement
and a change to a smaller point size
if possible,
otherwise it gives the left bracket character
(`\^\c
.b [ \^').
Extra space is left above the line
to allow room for the superscript.
.TL
.ST ]
.DE
Unsuperscript.
Inverse to
.ST [ .
For example,
to produce a superscript
you might type
.b x \c
.ST [ \c
.b 2 \c
.ST ] ,
which will produce
.b x\*[2\*] .
.TL
.ST <
.DE
Subscript.
Defaults to
`\^<\^'
if half-carriage motion not possible.
Extra space is left below the line
to allow for the subscript.
.TL
.ST >
.DE
Inverse to
.ST < .
.TL
.ST (dw
.DE
The day of the week,
as a word.
.TL
.ST (mo
.DE
The month,
as a word.
.TL
.ST (td
.DE
Today's date,
directly printable.
The date is of the form \*(td.
Other forms of the date can be used
by using
.NR (dy
(the day of the month;
for example, \n(dy),
.ST (mo
(as noted above)
or
.NR (mo
(the same,
but as an ordinal number;
for example, \*(mo is \n(mo),
and
.NR (yr
(the last two digits of the current year).
.TL
.ST (lq
.DE
Left quote marks.
Double quote in \*N.
.TL
.ST (rq
.DE
Right quote.
.TL
.ST \-
.DE
\(34 em dash in \*T;
two hyphens in \*N.
.sh 1 "Special Characters and Marks"
.pp
There are a number of special characters
and diacritical marks
(such as accents)
available through \-me.
To reference these characters,
you must call the macro
.b .sc
to define the characters before using them.
.TL
.b .sc
.DE
Define special characters and diacritical marks, as described
in the remainder of this section.
This macro must be stated
before initialization.
.pp
The special characters available
are listed below.
.ta 15 +5 +6
.nf
Name	Usage	Example
Acute accent	\e*\(aa	a\e*\(aa	a\*'
Grave accent	\e*\(ga	e\e*\(ga	e\*`
Umlat	\e*:	u\e*:	u\*:
Tilde	\e*~	n\e*~	n\*~
Caret	\e*^	e\e*^	e\*^
Cedilla	\e*,	c\e*,	c\*,
Czech	\e*v	e\e*v	e\*v
Circle	\e*o	A\e*o	A\*o
There exists	\e*(qe		\*(qe
For all	\e*(qa		\*(qa
.fi
.sp 1i
.b Acknowledgments
.pp
I would like to thank
Bob Epstein,
Bill Joy,
and Larry Rowe
for having the courage
to use the \-me macros
to produce non-trivial papers
during the development stages;
Ricki Blau,
Pamela Humphrey,
and Jim Joyce
for their help with the documentation phase;
and the plethora of people who have contributed ideas
and have given support for the project.
