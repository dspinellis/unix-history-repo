.\" psfig documentation and users guide
.nr Ns 0i
.de Ip
.IP \&\s8\z\(ui\H'10'\f(CB\\$1\s0\z\(ui\H'0'\fP\fI\\$2\fP
..
.F+
delim ##
define "ht"		| height |
define "wd"		| width |
define "pretzel"	| pretzel.ps wd 1.3n |
define "rosette"	| rosette.ps |

define "nedface"	| figure ned.ps height 1i reserve level 101 |
define "trevorface"	| figure trevor.ps height 1i reserve level 101 |
.F-
.LP		\" Initialize -ms
.rs
.sp .5i
.in +.5i \" **
.nf
.ce 99
.ps 12
.vs 15p
.B
Psfig \*- A \*(Dt Preprocessor for \*(Ps Figures
.ps
.vs
.ce 0
.sp
.ta 2.0iC 3.6iC
.I
	Ned Batchelder\(dg	Trevor Darrell
.R
.sp .1i
	#nedface#	#trevorface#
.sp .8
.ce 99
Computer and Information Science Department
University of Pennsylvania
200 South 33\s-2\urd\d\s0 Street
Philadelphia, PA  19104
.sp .3i
.I ABSTRACT
.sp
.ce 0
.fi
.in +.5i
.ll -.5i
.ti +\n(PIu
.I Psfig
is a new preprocessor for \*(Tr.
It implements a general figure inclusion, where a figure is any
\*(Ps file.
For example:
.F+
rosette height 1.25i
.F-
Figures are automatically scaled and positioned, with all sizes under user
control.
.I Psfig
can be used not only for actual figures, but also to provide special effects
in standard text, like
\v'2.7p'# black.ps height \n(.vu width "\w'white\ on\ black'u+1n" reserve 0 0 #\v'-2.7p'\c
# literal /1 setgray/ #\h'.5n'white\ on\ black# literal /0 setgray/ #,
or custom special characters, like `#pretzel#'.
For speed and for compatibility with non-\*(Ps systems, a draft
mode is available that simply shows the name of the \*(Ps file and the
extent of the figure.
.ll
.in
.in -.5i
.sp 2
.2C
.FS \(dg
Author's current address:
Digital Equipment Corporation,
129 Parker Street, PKO3-1/K90
Maynard, MA  01754
.FE
.NH 1
Introduction
.PP
The \*(Ps language is a powerful page description tool that is rapidly becoming
a 
.I
de facto
.R
standard, and is available in printers with a wide range of price and
performance from several manufacturers. Since \*(Tr has a long
history of preprocessors that provide new functions (\c
.I
eqn, tbl, pic,
.R
and
.I grap 
provide equations, tables, line drawings, and graphs respectively),
we wanted to write a preprocessor that would provide the capability
to include arbitrary \*(Ps figures into a document. 
.I Psfig
is the result.
.NH 2
Simple Use
.PP
The simplest 
.I psfig
command is simply the word
.Ex figure
followed by the name of a file.
If we have a file called `rosette.ps' which contains the
\*(Ps code to draw the rosette in the abstract,
we would use the 
.I psfig
command
.Ex
figure rosette.ps
.En
to include it as a figure.
(We'll explain how to combine 
.I psfig
commands with the rest of a \*(Tr document in a little while.)
.I Psfig
will automatically position the figure to the proper place on the page,
regardless of its `natural' position.
It will also instruct \*(Tr to reserve the space occupied by the figure so that
it doesn't overlap with anything else on the page.
.PP
Because no mention of size was made, 
.I psfig
draws the figure at its natural size.
The rosette's natural size is about 4 inches across, which is a little
large; the rosette in the abstract was produced with:
.Ex
figure rosette.ps height 1.25i
.En
The 
.Ex height
clause specifies how high the figure should be.
We've asked for it to be 1\(14 inches high.
The word
.Ex 1.25i
is interpreted by \*(Tr, so any expression that \*(Tr can evaluate
is acceptable.
For explicit measurements, the units
.Ex i ,
.Ex c ,
and
.Ex p
for inches, centimeters and printer's points (1/72 inch) are
available (among others) for absolute distances, and 
.Ex m ,
.Ex n ,
and
.Ex v
for the current point size, half the current point size, and the current 
line spacing are available for distances that vary according 
to their environment.
.PP
Since the width of the figure wasn't specified, it was scaled equally so
that the shape of the figure is maintained.
Of course, if desired, both dimensions can be explicitly specified.
The command
.Ex
figure rosette.ps
           height .5i
           width \en(.lu
.En
produces:
.F+
figure rosette.ps height .5i width \n(.lu
.F-
(The width expression
.Ex \en(.lu
is a \*(Tr incantation that means the current line length.
It stretches the rosette across the column exactly.)
.PP
Multiple
.Ex figure
commands place figures side-by-side across the page, and space between such
figures is specified with a 
.Ex space
command.
.Ex
figure rosette.ps height .5i width .2i
space .5i
figure rosette.ps height .5i width .8i
.En
produces:
.F+
figure rosette.ps height .5i width .2i
space .5i
figure rosette.ps height .5i width .8i
.F-
.PP
Figure 1 is an overview of all of the components in the system, and by the way,
a fairly complex example of what can be done with
.I psfig .
.\"
.F+
define "page" /figure ht 1.1i wd .85i bounds 0 0 612 792 level 151/
define "erase" /figure white.ps ht 1.1i wd .85i reserve 0 0/
define "epg" /erase; page/
.F-
.KW
.PS
boxht = 1.1; boxwid = .85
linewid = .35
Source: box "\*(Tr" "Source"
arrow
box "\fIeqn, tbl, \fPetc." "(optional)" ht .5 wid .8
arrow
Pf: box "\fIpsfig\fP" ht .5 wid .8
arrow
box "\*(Dt" ht .5 wid .8
arrow
Psdit: box "\fIpsdit\fP" ht .5 wid .8

define spread | with .e at last box.e + (-.6, .1) |
.ds Pv "\v'.55i-.2m'
.ds P^ "\v'-.55i-.2m'
Output: [
P8: box "\*(Pv# epg paper.ps.12 #\*(P^"
P7: box "\*(Pv# epg paper.ps.11 #\*(P^" with .e at last box.e + (-.6, .1)
P6: box "\*(Pv# epg paper.ps.10 #\*(P^" with .e at last box.e + (-.6, .1)
P5: box "\*(Pv# epg paper.ps.9  #\*(P^" with .e at last box.e + (-.6, .1)
P4: box invis "..."  with .e at last box.e + (-.6, .1)
P3: box "\*(Pv# epg paper.ps.3 #\*(P^" with .e at last box.e + (-.6, .1)
P2: box "\*(Pv# epg paper.ps.2 #\*(P^" with .e at last box.e + (-.6, .1)
P1: box "\*(Pv# epg paper.ps.1 #\*(P^" with .e at last box.e + (-.6, .1)
Handle: (P6.ne, P3.ne)
] with .n at Pf.s - (0, .4)
"\*(Ps output" at last [].s below
spline -> from Psdit.e \
	then right .5 \
	then down .5 \
	then to Output.Handle
right
Figs: [
	Rosette: box "\*(Pv#page rosette.ps #\*(P^"
	move right .1
	Face: box "\*(Pv#page ned.ps #\*(P^"
	move same
	Pretzel: box "\*(Pv#page pretzel.ps #\*(P^"
] with .s at Pf.n + (0, .4)
"\*(Ps Figures" at Figs.n + (0, .05) above

spline from Figs.sw - (0, .05) \
	then down .15 \
	then to Pf.n + (0, .35) \
	then down .15
arrow to Pf.n
spline from Figs.se - (0, .05) \
	then down .15 \
	then to Pf.n + (0, .35) \
	then down .15
.PE
.sp 
.ce
.B "Figure 1. " " How the components fit together."
.sp
.WE	\" End the wide keep.
.NH 1
Design Overview
.PP
In designing
.I psfig ,
our first goal was to serve the average \*(Tr user who
desires to include figures in a document with the
power and generality of \*(Ps. The existing \*(Tr preprocessors do a
fine job in each of their specialized tasks, but none of them approach
the generality or descriptive power of \*(Ps.
We saw 
.I psfig
as being the base for a broad range of uses, so in addition to being easy to 
use, it had to be powerful.
We wanted the capability to include anything from a full
page high resolution image down to a special mathematical symbol, as well as
also to provide an ability to pass through literal \*(Ps for
special effects. 
.NH 2
Interface Design
.PP
Good user interfaces are as simple as possible in the base case,
yet retain full generality for more sophisticated use.
Our goal for 
.I psfig
was to design a user interface that is extremely simple for the simplest case
of including a \*(Ps file as a figure, but that has the power necessary to 
describe more complex operations that sophisticated users might want.
.PP
We also set out to make sure that it was a familiar interface. We explicitly
tried to make
.I psfig
as much like
.I eqn
as possible. We saw
.I eqn
as a good example of a powerful yet easy to use preprocessor which solved most
of the problems of interface design that we were going to face.
.PP
Lastly, we wanted to make sure that 
.I psfig
was as powerful as possible. In the spirit of the other \*(Tr translators, as
much information as possible is simply passed through the preprocessor. 
For example, we chose not to define our own units for the specification of
figure dimensions. Rather, we assume that the expression used will be 
interpretable by \*(Tr, and pass it through. This provides a powerful link
between 
.I psfig
and \*(Tr, and allows for uses that would have been impossible had 
.I psfig
interpreted all dimensions itself.
.NH 2
Figure Placement
.PP
To most users, the operation of including a figure into a document seems a natural one, and the behavior that should result from simple figure inclusion 
commands seems obvious. We wanted to make sure that 
.I psfig
would conform to these expectations that people had about figure placement.
.PP
There are eight values that govern how
.I psfig
manipulates a figure to position it properly:
four for where the figure lies in its natural \*(Ps coordinate system,
and four for where the figure is desired to fall on the page.
We use the term ``natural'' position
to refer to the size and location of the figure would have if it were printed
alone, and we adopt Adobe's convention
of describing a figure's natural size with a bounding box, specifying
the x and y coordinates for both the lower left and upper right 
corners.
.PP
To avoid confronting this collection of numbers for every figure,
we developed some defaults to use when positioning a figure. We assume the
desired position of the figure is at the current \*(Tr 
pen position, and that the figure should keep its natural height and width. 
If the figure conforms to 
the \*(Ps Document Structuring Convention as defined by Adobe Systems, 
the figure's natural size (and position) will be declared in a \*(Ps comment
which
.I psfig
can read.
More often than not, though, one does not want the figure at its natural size.
We designed 
.I psfig 's
figure manipulations to take into account not just a translation to move the
figure to the proper place on the page, but also a scaling to resize
the figure.
.I Psfig
allows you to specify a height and/or width for the figure.
If only one is given, the user's expectation is that the other will be 
calculated to maintain the figure's original aspect ratio, so this is what
.I psfig
does.
.PP
Because users will find varied purposes for
.I psfig ,
it understands about two different types of figures.
Broken out figures are similar to 
.I eqn 's
display equations: they reserve space across an entire column so that they 
occupy space like paragraphs. They are used for actual figures in documents.
The pie chart in Figure 2 is an example of a broken out figure.
.KF
.F+
piechart.ps width \n(.lu
.F-
.br
.ce 99
.sp .1i
.B "Figure 2. " " A Broken out figure"
.br
.KE
In-line figures are similar to 
.I eqn 's 
in-line equations: they occupy space within the current line of text, and are 
used to provide special characters for unusual applications. Examples include 
the pretzel (#pretzel#) from the abstract.
.PP
Because these two types of figure have different uses, 
they have different defaults for 
positioning. Broken out figures have their upper left corner placed on the 
\*(Tr baseline, so that they will extend down from the current position.
In-line figures have their lower left corners place on the baseline, so that 
they will behave similarly to other characters. Note that the baseline will in 
general be modified in some way prior to invoking the figure, so that the 
positioning will be correct.
.NH 2
Interactions With Other Components
.PP
One of the elegant aspects of \*(Tr's preprocessor system is that
there seem to be no restrictions on their use with each other. Any or
all of them can operate on different or even the same parts of the
document without any ill effects.  We worked hard to maintain this
standard of compatibility in creating
.I psfig .
It has been successfully used with the standard preprocessors
.I tbl ,
.I eqn ,
and
.I pic ,
just as \*(Tr users would expect (uses with
.I eqn
and
.I pic
are included in this paper).
.PP
Finally, we sought to make 
.I psfig
interact well
with other variants of \*(Tr. 
To be able to pass commands through to the output file, 
.I psfig
requires full \*(Dt, to which many people do not have access. It also obviously
presupposes a \*(Ps output device.
In order that source files using 
.I psfig
can be processed with 
other systems (for example, \*(Nr or vanilla \*(Tr without special
postprocessors or \*(Ps), a draft mode feature has been included 
which uses no unusual features of either the formatter or the output device.
Of course, none of the \*(Ps figures are included in the output, but 
indications of them are given, and the formatting of the \*(Tr output around 
them is the same. 
.NH 2
\*(Ps Environments
.PP
.I Psfig 
is possible because the \*(Ps imaging model 
allows the creation of nested, protected environments by transforming the
coordinate system and redefining certain system operators.
For example, it is very easy to take a figure that filled an
entire 8\(12" by 11" page and cause it to instead be printed in a
box one inch square in the lower right hand corner of the
page. \*(Ps can do this because all graphical operations are performed
relative to the current transformation matrix (CTM), a homogeneous
transform that establishes the scale, position and orientation of the
coordinate system relative to the physical page. 
Using the eight parameters mentioned we can calculate the translation and
scaling of the coordinate system needed to move a figure from its natural 
position to its desired position.
.\"
.PP
Given the bounding box and the desired location and size for a figure, the 
translation and scaling needed to cause the figure to be printed at the
desired location is done in three steps:
.IP \(bu
scale horizontally by desired
width divided by old width
.IP \(bu
scale vertically by desired
height divided by old height
.IP \(bu
translate the upper left hand
corner of the figure's bounding 
box to the current point.
.PP
\*(Ps also gives us the tools necessary to insure that any side effects of a 
figure do not affect the rest of the document. The \*(Ps operators 
.Ex save
and
.Ex restore
effectively undo the side effects of any code executed between them.
.I Psfig
brackets all figures with these operators to protect the document.
The environment in which the \*(Ps code for a figure is executed places
no restrictions on the commands which may be used, so any well formed
(and non-hostile) \*(Ps file can be included as a figure. 
The \*(Ps operators 
.Ex showpage ,
.Ex initgraphics ,
.Ex initmatrix ,
and
.Ex defaultmatrix
are locally redefined for the figure so they behave in a rational
way. For example 
.Ex initgraphics
first performs a `regular' initgraphics, but then restores the current
transformation matrix (CTM) to the one we created for the figure.
The redefinition of
.Ex showpage
is simply to do nothing. It is expected that no multi-page \*(Ps files will be 
included as figures, so this redefinition really amounts to ignoring any 
.Ex showpage
that may appear at the end of the figure.
.NH 1
The Preprocessor
.PP
The
.I psfig
preprocessor is responsible for the interface seen by the user. Essentially
it translates a higher-level syntax into file inclusion and 
literal \*(Ps calls which are passed though \*(Dt and interpreted by the 
postprocessor.
Like the other \*(Tr preprocessors, 
.I psfig
interprets those portions of the file that are marked as its input. This input 
it translates into raw \*(Tr code. All other portions of the file are passed 
directly through to the output, to be interpreted further down the line.
.PP
The basic 
.I psfig
command to include a figure is the optional keyword
.Ex figure ,
followed by the name of a file containing a \*(Ps program, followed by
any number of optional clauses. Some common clauses are:
.DS I \n(PIu		\" Indent by the standard amount
.Ex "height " \c
.I " h"
.Ex "width " \c
.I " w"
.Ex "bounds " \c
.I " llx lly urx ury"
.DE
which specify the size of the desired figure, and the bounding box of the
original figure. 
As promised, if there is no
.Ex bounds 
clause for a figure, 
.I psfig
scans the \*(Ps file for the bounding box comment, and will also
compute the height and width using the defaults discussed earlier.
.PP
.I Psfig
also provides an lower level interface with the
.Ex file
and 
.Ex literal
commands, which provide direct file and literal \*(Ps inclusion, respectively.
Finally, a 
.Ex global
option is available on 
.Ex file
and
.Ex literal
to download code that will remain present across the 
.Ex save
and 
.Ex restore
context normally surrounding each \*(Dt page.
.PP
In the simplest usage then, one need only specify the name of a 
file containing \*(Ps to include a figure, and 
.I psfig 
will perform a default set of ``reasonable'' actions.
.PP
The full input syntax is included in Appendix A.
.NH 1
\*(Dt and Postprocessor Hooks
.PP
The preprocessor bears the brunt of making things look and act in a
well behaved manner, but the real work is done in the postprocessor
and its \*(Ps prolog files.
.PP
.I Psfig
uses the \*(Dt 
.Ex \eX
command to pass commands through to the postprocessor. An input sequence of 
.Ex
\eX'test'
.En
will come through \*(Dt as
.Ex
x X test
.En
We used 
.I psdit ,
the \*(Dt to \*(Ps translator supplied with TranScript from Adobe Systems as
our postprocessor. We added two primitives that we call through
.Ex \eX :
.DS I \n(PIu
.Ex \eX'f \c
.I " filename" \c
.Ex '
.Ex \eX'p \c
.I " literal \*(Ps" \c
.Ex '
.DE
The former interpolates the contents of
.I filename
into the \*(Ps output
.I psdit ,
while the latter injects 
.I "literal \*(Ps" .
In passing literal arguments, we trick \*(Dt into
evaluating dimension expressions for us
by enclosing our expression in 
.Ex \ew'\eh' \c
.I expr \c
.Ex '' .\(dg
.FS \(dg
This \*(Tr incantation asks for the
overall width of a string that is nothing but a relative horizontal motion by
.I expr .
.FE
This will evaluate to the value of 
.I expr
in device units.
All scaling computation is done in this way, since the
preprocessor can not know the value of \*(Tr variables, which
may well be used in expressions. This also means the preprocessor
need not know anything about \*(Tr dimensions, and users can specify dimensions
in the same ways they always have.
Inside
.I psfig ,
the computation is manipulated in an algebraic manner, and is finally
evaluated when it passes through 
.Ex \eX .
.NH 1
Putting it all Together
.PP
Now we can examine exactly how we create the nested and protected 
environment.
We perform a simple figure inclusion in three steps:
.IP \(bu
Using 
.I psdit 's
new literal pass through command, construct a call to a `startFig'
\*(Ps function that we 
have included in the prolog prepended to all \*(Ps files from 
.I psdit .
The startFig
function takes the desired height and width, and natural bounding
box as arguments, issues a 
.Ex save ,
performs the necessary transformations of
the graphics state, then redefines system operators as needed.
.IP \(bu
Using 
.I psdit 's
file inclusion command, copy the figure file into the output stream.
.IP \(bu
Using literal, call our `endFig' \*(Ps function that undoes the effects
of `startFig'. EndFig needs no arguments.
.PP
So, to show a small example, if we had a \*(Ps figure in a file
.Ex smiley.ps
that contained the code:
.\"
.Ex
%!
%%BoundingBox: 0 0 36 36
newpath 18 18 10 0 360 arc stroke % head
newpath 18 18 6 180 360 arc stroke% mouth
newpath 22 22 .5 0 360 arc stroke % eyes
newpath 14 22 .5 0 360 arc stroke
showpage
.En
and 
.I psfig
was processing the \*(Tr source fragment:
.Ex
for a happy document!
\&.F+
figure smiley.ps 
\&.F-
\&.NH 1
What is a Figure?
.En
.I psfig
would translate `
.Ex "figure smiley.ps" '
into a series of
.Ex \eX
calls which would cause the following \*(Ps output from 
.I psdit :
.DS I \n(PIu		\" Indent by the standard amount
.Ex "760 4512(happy)N 976(document!)X"
.Ex "1422 4560 MXY"
.Ex "288 288 0.00 0.00 36.00 36.00 startFig"
.I "...contents of smiley.ps..."
.Ex "endFig"
.Ex "3 f 760 5040(6.)N"
.Ex "860(What)X 1071(Is)X 1153(A)X"
.Ex "1231(Figure?)X
.DE
The height and width are the first arguments to 
.Ex startFig ,
and are in \*(Dt device units, followed by the natural bounding box
of the figure in points.
.Ex startFig
will convert the height and width into points, then
perform the computation outlined above.
And all this makes for a happy document!
.F+
smiley.ps
.F-
.NH 1
What Is A Figure?
.PP
Since figures are simply \*(Ps files, 
.I psfig
allows dozens of utilities to be figure tools. Most graphical tools
either directly produce \*(Ps, or produce an output language (such as
Tektronix 4014, and Unix plot) that
can be translated into \*(Ps using available filters. 
.NH 2
Figure Requirements
.PP
Of course, in addition to the \*(Ps information about the appearance of the 
figure, 
.I psfig
will need some information about the figure that it can use for computing the
size of the figure.
The only requirement on a figure is that it produce valid \*(Ps code, 
and that it contain a
.Ex %%BoundingBox
comment as described in Adobe's Document Structuring Conventions.
.I Psfig
insures that a figure is actually \*(Ps by checking that the first two 
characters in the file are `%!'.
.PP
Note that the mere presence of the proper identifying characters and a
bounding box comment will not insure a figure will behave properly.
There are many ways a \*(Ps program could fail in a
.I psfig
environment, from having an erroneous bounding box, to causing a \*(Ps error
when executed, to circumventing
.I psfig 's
redefinition of system operators, to using operators that haven't
been protected by 
.I psfig ,
such as
.Ex exitserver .
As with most trap door mechanisms that allow arbitrary information to pass 
through a processor, 
.I psfig
has little choice but to trust the figures it deals with. It makes some minimal
checks that catch the most blatant problems, but it cannot do more than that.
.NH 2
Encapsulated \*(Ps
.PP
Recently Adobe has addressed the issue of \*(Ps programs that 
are designed explicitly for use as included figures, and have established an 
.I "Encapsulated \*(Ps File Format" .
Part of 
this standard deals with file formats for dual bitmap/\*(Ps representations,
primarily for Macintosh and MS-DOS applications, and
is not important for this discussion. Other parts, however, present
guidelines for safe \*(Ps code that can be imported into documents,
and they outline some of the techniques that are used by 
.I psfig
to set up a nested environment. The standard does not assume any operator
redefinition other than 
.Ex showpage ,
and it provides a list of operators that seriously disturb the state
of the interpreter, and are forbidden in conforming EPSF programs:
.F+
delim
.F-
.sp .1i
.TS
center box;
cfB s 
cfB s
_ _
l l.
Operators to avoid in 
imported files (EPSF 1.3)

exitserver	initgraphics
initmatrix	initclip
erasepage	copypage
grestoreall	framedevice
setpageparams	banddevice
nulldevice	renderbands
note
.TE
.sp .1i
.F+
delim ##
.F-
In particular, only operators documented in the body (that is, not an 
appendix) of
.I "\*(Ps Language Reference Manual" 
(commonly known as the Red Book) should be used since the availability of any
others cannot be guaranteed in all \*(Ps implementations.
In general, any \*(Ps file used with
.I psfig
should conform as closely as possible to the EPSF specification.
Note that even though
.Ex initgraphics
and
.Ex initmatrix
are redefined for
.I psfig
figures, their use is discouraged.
.\"
.\" We don't need this editorial: It's off our point.
.\"
.\".NH 2
.\"Macintosh Woes
.\".PP
.\"For better or worse, the Apple Macintosh is one of the most popular
.\"vehicles for creating \*(Ps drawings and diagrams.
.\"Unfortunately, Apple has chosen to make getting the \*(Ps from
.\"applications such as MacDraw no easy task.
.\"MacDraw doesn't produce pure \*(Ps, relying on a collection
.\"of procedures defined in a prolog which, in an AppleTalk environment,
.\"is always loaded into the printer.
.\"Since we use our printers for more than Macintosh applications, we must
.\"download the prolog file with each job that requires it.
.\"Methods for dealing with Mac figures are given in \(sc 7.7.
.\"Note that Apple seems to change the contents of this prolog with
.\"each new LaserWriter driver, so in general a file captured from one
.\"Mac will not print using a prolog captured from another Mac.
.\"Worse, certain recent version of the LaserWriter driver produce
.\"\*(Ps that causes problems with operator redefinition. Thumbs
.\"down to Apple for for yet another closed system.
.\"
.NH 1
Tutorial
.\ This section to be taken largely verbatim from existing paper
.NH 2
\*(Tr Interface
.PP
Like the other \*(Tr preprocessors, 
.I psfig
passes most of its input through to its output untouched.
Only text that is marked as a 
.I psfig
command is interpreted.
.PP
There are a number of ways to mark
.I psfig
commands in your \*(Tr document.
The first is to enclose them between
.Ex .F+
and
.Ex .F- :
.DS I \n(PIu
.Ex ".F+"
.I "psfig commands"
.Ex ".F-"
.DE
This is precisely equivalent to 
.I eqn 's
.Ex .EQ
and
.Ex .EN :
The 
.Ex .F+
and
.Ex .F-
lines are copied through to the output so that macro packages can do
some action before or after figures.
Any arguments to 
.Ex .F+
or
.Ex .F-
are copied through to the output but are otherwise ignored.
In our definitions of these macros, 
.Ex .F+
and
.Ex .F-
provide a displayed figure centered in the line, and giving
.Ex .F+
an argument of
.Ex L
will leave the figure left flush.
.PP
Like
.I eqn ,
.I psfig
has the ability to read commands from within a \*(Tr line.
The
.Ex delim
command specifies two characters that will delimit
.I psfig
commands:
.Ex
\&.F+
delim @@
\&.F-
.En
Any text that falls between the two characters specified will be
interpreted as commands by 
.I psfig .
In-line commands are most useful for generating special characters like the
pretzel in the abstract, because they don't cause a break in the text.
One restriction: an in-line command must not be broken across two lines.
.PP
.NH 2
Command Structure
.PP
.I Psfig
commands consist of words separated by white space (spaces, tabs, or
newlines).
Some words, like
.Ex figure
and
.Ex space
are reserved words, and mean something to 
.I psfig ,
while others, like
.Ex rosette.ps
and
.Ex \en(.lu
are assumed to mean something to someone else. (In this case, the file system
and \*(Tr).
.PP
A command that starts with a non-reserved word is assumed to be a 
.Ex figure
command, so the word
.Ex figure
can usually be omitted.
Semicolons are taken as command separators and can be used to avoid
ambiguities caused by the omission of a reserved word.
.PP
Because non-reserved words aren't interpreted, they must be quoted if they
contain any characters that
.I psfig
interprets specially.
Either single or double quotes may be used.
One exception: it is impossible for an in-line command to contain the closing
delimiter character, even if it is quoted.
.NH 2
In-line Figures
.PP
Figures that result from in-line 
commands are slightly different from figures created the other two ways.
First, whereas broken out figures have their top edge on
the current baseline and extend down,
in-line figures sit with their lower edge on the current
baseline, and extend up.
This facilitates the use of in-line figures to create custom characters like
the pretzel (#pretzel#).
For example, the last sentence ended with:
.Ex
pretzel (@ pretzel.ps width 1.3n @).
.En
The width here is specified in the \*(Tr unit 
.Ex n ,
which is the width of a lower case `n' in the current point size.
Specifying a width this way makes the character the right size regardless of
the current text size:
.DS I \n(PIu
.ps 18
A larger pretzel: `# pretzel #'.
.ps 10
.DE
.PP
Characters designed this way can be used anywhere a standard character can
be used:
.EQ
sum from {i = 0 } to "# pretzel #" ^x sup "# pretzel #"
^=^ 2 sin("#pretzel#")
.EN
One hint for use with 
.I eqn :
always enclose
.I psfig
commands with quotes when inside
.I eqn
commands.
For example, part of the above equation was created with
.Ex
x sup "@ pretzel width 1.3n @"
.En
.sp \n(Nsu
.PP
Another difference between in-line and broken out figures is that by default,
in-line figures don't reserve any vertical space, under the assumption that they
will fit within the current line anyway.
If your in-line figure is higher than anything else on the line, and you want
the space to be reserved, then add the word
.Ex reserve
to your command.
If your figure isn't higher and you use
.Ex reserve ,
the spacing will be wrong, so only use it if you need it.
.NH 2
Macros
.PP
.I Psfig
provides a macro facility that is similar to
.I eqn 's.
A command of the form:
.Ex
define foo /bar/
.En
will define a macro named 
.Ex foo .
Any occurrence of the word
.Ex foo
will now be replaced by the word
.Ex bar .
The text of the macro is delimited by any character not included in the text
itself, and may be any sequence of characters, including any of the
characters that
.I psfig
interprets specially.
.PP
Macros can be useful for commonly used figures like in-line characters.
For example, this manual begins with the following lines:
.Ex
\&.F+
delim @@
define wd /width/
define pretzel /pretzel.ps wd 1.3n/
\&.F-
.En
and all the pretzels in the text were created with:
.Ex
@pretzel@
.En
Remember that using a width specified in 
.Ex n 's
gives us size independence, so that this macro will work in any environment
to give us the right size pretzel.
.PP
Macro expansion is attempted for every word that 
.I psfig
sees, unless it is quoted.
In particular, the name of a macro in a
.Ex define
command is expanded if possible, so be careful about redefinitions.
The best policy is to always enclose the name in quotes:
.Ex
define "wd" /width/
.En
Macros in the text of a macro are expanded when the macro is expanded, not
when it is defined.
.NH 2
Special Effects
.PP
.I Psfig
can also be used to provide interesting graphical effects.
.PP
.di Gb		\" Divert the Gray Box.
.ll -2n
For example, this paragraph has been printed on a gray background.
We diverted the text of the paragraph, scaled a gray box to fit
around it, and then printed the text on top of the gray.
.br
.di
.ll
.F+ L
figure gray.ps
   height \n(dnu+1n
   width \n(.lu
   reserve 0 0
.F-
.nf	\" read diversions in no-fill.
.sp -2p
.in 1n
.Gb	\" Get the text.
.in
.fi	\" back to fill mode.
.PP
The command used to create the gray box on which the text sits was:
.Ex
\&.F+ L
figure gray.ps
   height \\n(dnu+1n
   width \\n(.lu
   reserve 0 0
\&.F-
.En
The file `gray.ps' draws a unit square filled with a light gray.
We specified the height to be a little bit more than the height of the
last diversion (the paragraph), and the width to be the same as the
width of a line.
.PP
The
.Ex reserve
clause tells \*(Tr how much space to reserve, here, none.
Normally, 
.I psfig
has \*(Tr reserve the space taken by the figure so that it won't overlap
with anything else.
Here we want it to overlap, so we override the default, and have
\*(Tr reserve no space.
The
.Ex L
on the line with
.Ex .F+
overrides the default centering, so that the box is flush left.
.NH 2
Raw \*(Ps
.PP
In addition to specifying files to include into the \*(Dt output, you can
also specify literal \*(Ps text to be output.
The basic command is 
.Ex
literal /text to be output/
.En
The text (which is delimited by any character, just like the
text of
.Ex define 
commands) will be inserted into the \*(Ps output without any
protection around it.
No macros are expanded in the literal text, but interpretation of \*(Tr
constructs is performed.
.PP
Because there is no protection, you must be careful when writing
.Ex literal s.
Any modifications you make to the state of the \*(Ps interpreter will linger
into the rest of your document.
Also keep in mind that the \*(Ps text is interpreted in the environment of your
document, not a special figure environment, so any output generated will
probably be wrong.
.PP
Because of this,
literal text is really designed to be used as a way to output small amounts
of \*(Ps code to modify the way something else will work, rather than
generating output itself.
For example, the white on black effect in the abstract was produced in part by
bracketing the words `white on black' with some
.Ex literal s
that change the color to white and then back to black:
.Ex
@ literal /1 setgray/ @
white on black
@ literal /0 setgray/ @
.En
.sp \n(Nsu
.PP
Another example of the use of 
.Ex literal
is to modify the way lines are drawn by
.I pic .
Normally, 
.I pic 
allows simple dashed or dotted lines, but not complex dash patterns or
dashed splines.
By using 
.Ex literal s
to change \*(Ps's dash parameter, you can achieve these effects:
.Ex
\&.PS
define ps | box invis ht 0 wid 0 |
ps "@literal /[25 15] 0 setdash/@"
circle
ps "@literal /[20 15 40 15] 0 setdash/@"
spline right .5 \\
	then down .5 left .5 \\
	then right .5
ps "@literal /[] 0 setdash/@"
circle
\&.PE
.En
produces
.PS
define ps | box invis ht 0 wid 0 |
ps "#literal /[25 15] 0 setdash/#"
circle
ps "#literal /[20 15 40 15] 0 setdash/#"
spline right .5 \
	then down .5 left .5 \
	then right .5
ps "#literal /[] 0 setdash/#"
circle
.PE
Notice that we used a 
.I pic
macro called 
.Ex ps
to hide the 
.I psfig
commands.
.PP
Another form of raw \*(Ps output is the
.Ex file
command, which takes the named file and outputs it at the current point with
no protection.
No scaling or positioning is done, so in general, the file should not
produce any output, since it will not be able to predict its position on the
page.
.NH 2 
Preludes And Postludes
.PP
The main use for the raw output forms discussed in the last section is to
provide auxiliary information for a figure.
For example, let's suppose that you have a file named `fig.mac'
which contains some \*(Ps output from MacDraw.
Since Macintosh applications assume that the \*(Ps they generate will be
preceded by a header file (`mac.pro')
full of function definitions that the application
can make use of, fig.mac will not work properly without
the header.
One solution would simply be to modify fig.mac by copying the header file
into the beginning of it.
.PP
Rather than force you to do that,
.I psfig
provides you with a way to specify the relationship between fig.mac and
mac.pro.
Our current example would be specified like this:
.Ex
figure fig.mac {
	file mac.pro
	figure
}
.En
The braces enclose a list of things to be output in the order they should
appear.
We name the file `mac.pro' first, so it is output first.
Then the word
.Ex figure
by itself means the figure named at the beginning of the command.
Both of these are enclosed in one environment.
An example of MacDraw output is at the top of the next page.
.KW
.F+
figure lab.ps {
	file figs/mac.pro
} width 4.5i
.F-
.sp 
.ce 99
.B "Figure 2. " "Some sample MacDraw output."
.ce 0
.sp 
.WE
.PP
The list of things to output can be placed anywhere in the 
.Ex figure 
command, even before the file name of the figure, and may contain any number
of entries, although the figure must be referred back to (by the word
.Ex figure )
at most once.
The entries (aside from the
.Ex figure )
can be either
.Ex file s
or 
.Ex literal s, 
and may appear either before or after the
.Ex figure .
If the word
.Ex figure
doesn't appear it is assumed to be the last item in the list.
.PP
This mechanism provides a general way to modify the behavior of figures.
For example, a figure could be designed so that it reads arguments off
the \*(Ps stack, with a
.Ex literal
providing them at run time:
.Ex
figure takesargs.ps {
	literal /arg1 arg2/
	figure
}
.En
Or perhaps you have a shape that you want outlined sometimes and filled
sometimes.
You can put the commands to create the path into a file called `logo.ps' and
then make use of a
.Ex literal
after the figure to draw it:
.Ex
figure logo.ps {
	figure
	literal /stroke/
}
.En
.sp \n(Nsu
.PP
This feature can be coupled with the macro definition feature in a clever way.
If you are going to be dealing with many MacDraw figures, you could define a
macro:
.Ex
define "macfig" /
figure {
        file mac.pro
        figure
}
/
.En
and then simply say
.Ex
macfig fig.mac
.En
to include the figure.
.NH 2
Global Data
.PP
The above technique for including MacDraw documents points up a problem:
the header file will be downloaded for each figure that needs it.
Since the header file can be quite large (mac.pro is more than 25K bytes),
this could get quite wasteful.
.PP 
One solution would be to download the header once, and then to just download
each figure separately.
This will work except that each \*(Dt page is an isolated environment, and
each page begins with the environment that the entire document began in.
The header file will be available to every figure on the page in which it was
downloaded, but will be lost when another page is started.
.PP
.I Psfig
provides a solution to this by allowing the user to modify the environment
in which pages are started.
The word 
.Ex global
can be used to modify the
.Ex file
or
.Ex literal
commands, and they will be executed in such a way that their effects are
seen throughout the rest of the document.
.PP
We can use
.Ex global
to create a macro that does the work of loading the header for us:
.Ex
define "macfig" |
	file mac.pro global;
	define "macfig" / figure /;
	figure
|
.En
The first use of the macro downloads the header file, redefines
.Ex macfig ,
and begins a figure command.
Other uses are then simply 
.Ex figure
commands.
.PP
Careful use of 
.Ex global s
can produce interesting results, but care must be taken.
For example, because successive pages depend on
.Ex global s
on previous pages, the pages of the document cannot be reversed and still
print properly.
.NH 2
Clipping
.PP
Normally, no clipping is done on figures; they are trusted to print only
within their declared bounding box.
If clipping is desired, the word
.Ex clip
can be added to a
.Ex figure
command, and the figure will be clipped to its bounding box.
.NH 2
Draft Levels
.PP
Because some \*(Ps figures can be expensive to print (half-toned
pictures, for example), and because documents designed to be printed on
\*(Ps printers may have to be printed on less capable printers, 
.I psfig
allows the user to control the extent of the inclusion of figures.
Every figure has associated with it a `level', which should
correspond roughly to the cost of printing it.
When 
.I psfig
processes a file, it runs at a certain level, and figures whose cost is less
than the current level get printed.
Broken out figures whose cost is more than the current level are omitted,
and a box is drawn around where they would be:
.F+
rosette.ps height 1i level 9999
.F-
The box has the name of the file in it for identification.
In-line figures are simply omitted, but the space they occupy is still
reserved by \*(Tr.
Here is a draft pretzel: `#pretzel level 9999#'.
Since a box takes less time to draw than a complicated figure, the careful
use of draft levels can speed up the printing of your document.
Also, the box is drawn with standard \*(Tr commands, so by setting
.I psfig 
to run at the lowest level (so that it decides that all the figures are
too expensive), you'll get output that can be formatted by a generic \*(Tr
(not even \*(Dt is required!).
Of course, you won't have the figures, but the layout will be the same,
because the space has been reserved.
.PP
The default level that 
.I psfig 
runs at is 100.
In-line figures get a cost of 5 by default, and broken out figures get a cost
of 10.
To set the cost of a figure, simply tack on a
.Ex level
clause.
The box above was made by:
.Ex
rosette.ps height 1i level 9999
.En
.sp \n(Nsu
.NH 1
Using Psfig
.PP
Since 
.I psfig
is a \*(Tr preprocessor in the classic style, it operates as a pure filter.
It can be used anywhere in the pipeline of preprocessors, but it is safest
if you run it last (just before \*(Tr).
The macro definitions of 
.Ex .F+
and 
.Ex .F-
must be included with the 
.Ex -mpsfig
option on the \*(Dt command line.
For example, this paper was produced with the equivalent of:
.Ex
pic | tbl | eqn | psfig | 
.br
\ \ \ \ \ ditroff -mpsfig | psdit
.En
.sp \n(Nsu
.PP
There are a few options that can be specified on the command line.
.PP
.Ex -d <level>
specifies the draft level to run at.
If <level> is omitted, then zero is assumed, causing all figures to be
omitted.
.PP
.Ex -f
specifies that \*(Dt codes should be output that work around a bug in \*(Dt
that was discovered during the development of
.I psfig .
Broken out figures won't center properly with unfixed \*(Dt's without this
flag.
Also, special characters in
.I eqn
won't work on these unfixed \*(Dt's, even with 
.Ex -f .
.PP
.Ex -D <dir>
specifies a directory in which to search for files.
Any number of these can be specified, and they will be searched in turn.
The current directory is always searched first.
.NH 1
Psfig/\*(Tx
.PP
We have a package of similar functionality available for the \*(Tx
document preparation system. 
.I Psfig/\*(Tx
uses no preprocessor, and is implemented entirely in \*(Tx macros,
using the
.I dvips
postprocessor from ArborText. Files 
.I are
scanned for the
.Ex %%BoundingBox
comment,
but they are not checked to see that they conform to the structuring
convention (for example, the bounding box could be in the middle of the file,
rather than in the header or trailer as required by the convention).
The 
.I psfig/\*(Tx
command
.DS I \n(PIu
.Ex "\epsfig{file=" \c
.I "name, clause, clause,... " \c
.Ex "}"
.DE
is the equivalent of the
.I psfig
command
.DS I \n(PIu
.Ex "figure " \c
.I " name clause clause ..."
.DE
The \*(Ps implementation of 
.I psfig
and
.I psfig/\*(Tx
are very similar, differing only in the scaling factors used to convert
\*(Dt or \*(Tx units into points, and in the code to implement
.Ex global
(each post-processor has different variables that must be restored when
we return to the current save context.)
.\ no in-line figures
.NH 1
Getting Psfig
.PP
Inquiries about 
.I psfig
may be directed to trevor@linc.cis.upenn.edu, or
the U.S. mail address listed above.
.I Psfig
will be available as part of future releases of the T\s-2RAN\s0S\s-2CRIPT\s0
package from Adobe, as well as through uucp/ftp distributions.
.PP
.NH 1
Acknowledgments
.PP
We would like to thank: the University of Pennsylvania and in particular
Ira Winston for supporting and encouraging this work;
Brian Kernighan for helping gracefully with the internals of \*(Dt and 
for having written
.I eqn
to guide us through the darkness; 
and Adobe Systems for having designed and implemented \*(Ps, which made it
all possible.
.PP 
.SH
A. Language Syntax
.PP
.I Psfig
recognizes these commands found between
.Ex ".F+"
and 
.Ex ".F-"
or in-line delimiters:
.de Ds
.sp
.nf
.in .5i
..
.de De
.fi
.sp
.in -.5i
..
.Ds
[\c
.Ex "figure" \c
]\c
.I " path " \c
[\c
.I "clauses" \c
] [\c
.I "modifiers" \c
]
.\"
.Ex "file " \c
.I path \c
[\c
.I "modifiers" \c
]
.\"
.Ex "literal " \c
.I "text " [\c
.I "modifiers" ]
.\"
.Ex "space " \c
.I dimen
.\"
.Ex "define " \c
.I "word text"
.\"
.Ex "delim " \c
.I "char" \c
[\c
.I char ]
.De
.I Modifier 
is one or more of:
.Ds
.Ex "level " \c
.I num 
.Ex "global"
.De
.I Clauses
is one or more of:
.Ds 
.Ex "height " \c 
.I "dimen"
.Ex "width " \c
.I "dimen"
.Ex "bounds " \c
.I "int int int int"
.Ex "reserve " \c
.I "dimen dimen" 
.Ex "clip " 
{ \c
.I environment \c
}
.De
.LP
.I Environment 
is a series of 
.Ex file
and/or 
.Ex literal
commands, and the keyword
.Ex figure .
.LP
.I Path 
is a valid Unix file path.
.LP
.I Dimen 
is a \*(Tr expression that will evaluate to a length.
.LP
.I Text
is any string of characters which is delimited by a single character, and
does not contain that character.
.PP
The path, clauses, and modifiers of a figure command may be present in
any order.  Whitespace is ignored (except that in-line commands may not
cross lines), and semicolons optionally separate commands.
.sp
.F+
delim
.F-
