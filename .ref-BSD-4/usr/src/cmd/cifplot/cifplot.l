.TH CIFPLOT PUBLIC 2/1/80
.UC
.SH NAME
cifplot \- CIF interpreter and plotter
.SH SYNOPSIS
.B cifplot
[
.I options
]
file1.cif
[
file2.cif ...
]
.SH DESCRIPTION
.I Cifplot
takes a description in Cal-Tech Intermediate Form (CIF) and
produces a plot.
CIF is a low-level graphics language suitable for describing
integrated circuit layouts.
Although CIF can be used for other graphics applications,
for ease of discussion it will be assumed that CIF is used to
descibe integrated circuit designs.
.I Cifplot
interprets any legal CIF 2.0  description including symbol renaming
and Delete Definition commands.
In addition,
a number of local extensions have been added to CIF,
including text on plots and include files.
These are discussed later.
Care has been taken to avoid any arbitrary restrictions on
the CIF programs that can be plotted.
.PP
To get a plot call
.I cifplot
with the name of the CIF file to be plotted.
If the CIF description is divided among several files call
.I cifplot
with the names of all files to be used.
.I Cifplot
reads the CIF description from the files in the order
that they appear on the command line.
Therefore the CIF End command should be only in the last file since
.I cifplot
ignores everything after the end command.
After reading the CIF description but before plotting,
.I cifplot
will print a estimate of the size of the plot
and then ask if it should continue to produce a plot.
Type
.B y
to proceed and
.B n
to abort.
A typical run might look as follows:
.PP
.in +0.5i
%
.B
cifplot lib.cif sorter.cif
.br
Window -5700 174000 -76500 168900
.br
Scale: 1 micron is 0.004075 inches
.br
The plot will be 0.610833 feet
.br
Do you want a plot?
.B y
.PP
After typing
.I y
.I cifplot
will produce a plot on the Benson-Varian plotter.
Options to send the plot to a different device are discussed in the
next section.
.PP
.B Options
.PP
The following is a list of options that may be included on
the command line:
.br
.sp
.in 1.2i
.ti -0.5i
.B -w
.I xmin xmax ymin ymax
.br
.B window:
The -w options specifies the window;
by default the window is set to be large enough
to contain the entire plot.
The windowing commands lets you plot just a small
section of your chip, enabling you to see it in better
detail.
.I Xmin, xmax, ymin,
and
.I ymax
should be specified in CIF co-ordinates.
.sp	
.ti -0.5i
.B -s 
.I float
.br
.B scale:
The -s option sets the scale of the plot.
By default the scale is set so that the window will fill the
whole page.
.I Float
is a floating point number specifying the number of inches
which represents
1 micron.
A recommended size is 0.02.
.sp	
.ti -0.5i
.B -l 
.I layer_list
.br
.B layer:
Normally all layers are plotted.
The -l option specifies which layers NOT to plot.
The
.I layer_list
consists of the layer names separated by commas, no spaces.
There are two reserved names:
.B text
and
.BR bbox .
Including the layer name
.B text
in the list suppresses the plotting of text;
.B bbox
suppresses the bounding box around symbols.
.sp
.ti -0.5i
.B -d
.I n
.br
.B depth:
This option lets you limit the amount of detail plotted
in a hierarchically designed chip.
It will only instanciate the plot down
.I n
levels of calls.
Sometimes too much detail can hide important features in a circuit.
.sp
.ti -0.5i
.B -g
.I n
.br
.B grid:
Draw a grid over the plot with spacing every
.I n
CIF units.
.sp
.ti -0.5i
.B -f 	fuzzy:
Don't print the border outlines around the merged features on each layer.
.sp
.ti -0.5i
.B -h	half:
Plot at half normal resolution.
.I
(Not yet implemented.)
.sp
.ti -0.5i
.B -e	extensions:
Accept only standard CIF. User
extensions produce warnings.
.sp
.ti -0.5i
.B -I	non-Interactive:
Do not ask for confirmation.
Always plot.
.sp
.ti -0.5i
.B -L	List:
Produce a listing of the CIF file as it is parsed.
Not recommended unless debugging hand-coded CIF
since CIF code can be rather long.
.sp
.ti -0.5i
.B -a
.I n
.br
.B approximate:
Approximate a roundflash with an
.IR n -sided
polygon.
By default
.I n
equals 8.
(I.e. roundflashes are approximated by octagons.)
If
.I n
equals 0 then output circles for roundflashes.
(It is best not to use full circles since they significantly
slow down plotting.)
.I
(Full circles not yet implemented.)
.sp
.ti -0.5i
.B -b
.I
"text"
.br
.B banner:
Print the text at the top of the plot.
.sp
.ti -0.5i
.B -C	Comments:
Treat comments as though they were spaces.
Sometimes CIF files created at other universities
will have several errors due to syntaxically incorrect comments.
(I.e. the comments may appear in the middle of a CIF command or
the comment does not end with a semi-colon.)
Of course, CIF files should not have any errors
and these comment related errors must be fixed
before transmitting the file for fabrication.
But many times fixing these errors seems to be more trouble
than it is worth, especically if you just want to get a plot.
This option is useful in getting rid of many of these
comment related syntax errors.
.sp
.ti -0.5i
.B -r	rotate:
Rotate the plot 90 degrees.
.sp
.ti -0.5i
.B -V	Varian:
Send output to the varian.
(This is the default option.)
.sp
.br
.ti -0.5i
.B -W	Wide:
Send output directly to the versatec.
.sp
.ti -0.5i
.B -S	Spool:
Store the output in a temporary file then
dump the output quickly onto the Versatec.
Makes nice crisp plots;
also takes up a lot of disk space.
.sp
.br
.ti -0.5i
.B -T
.I n
.br
.B Terminal:
Send output byte stream to standard output.
Useful for setting up pipes.
.I N
is the number of bytes per line that the plotting device expects.
.I
(Not yet implemented.)
.sp
.br
.ti -0.5i
.B -H	HP2648:
Send output to HP2648A terminal.
This requires that
.I cifplot
is running in the foreground on an HP2648
and that there is a scratch tape in the left tape drive of the terminal.
.I
(Not yet fully implemented.)
.sp
.ti -0.5i
.B -X
.I basename
.br
.B eXtractor:
From the CIF file create a circuit description
suitable for switch level simulation.
It creates two files:
.IB basename .sim
which contains the circuit description, and
.IB basename .node
which contains the node numbers and their location
used in the circuit description.
.sp
When this option is invoked no plot is made.
Therefore it is advisable not to use any of the other
options that deal only with plotting.
However, the
.IR window ,
.IR layer ,
and
.I approximate
options are still appropriate.
To get a plot of the circuit with the node numbers call
.I cifplot
again, without the
.B -X
option, and include
.IB basename .nodes
in the list of CIF files to be plotted.
(This file must appear in the list of files
before the file with the CIF End command.)
.I
(Not yet implemented.)
.sp
.ti -0.5i
.B -P
.I pattern_file
.br
.B Pattern:
The -P option lets you specify your own
layers and stipple patterns.
.I Pattern_file
may contain an arbitrary number of layer descriptors.
A layer descriptor is the layer name in double quotes,
followed by 8 integers.
Each integer specifies 32 bits
where ones are black and zeroes are white.
Thus the 8 integers specify a 32 by 8 bit stipple pattern.
The integers may be in decimal, octal, or hex.
Hex numbers start with '0x';
octal numbers start with '0'.
The CIF syntax requires that layer names be made up of
only uppercase letters and digits,
and not longer than four characters.
.br
.sp
.ti -0.5i
.B -F
.I font_file
.br
.B Font
The -F option indicates which font you want for your text.
The file must be in the directory '/usr/lib/font'.
The default font is Roman 6 point.
Obviously, this command is only useful if you have text on your plot.
.sp
.ti -0.5i
.B -O
.I filename
.br
.B Output:
After parsing the CIF files, store an equivalent but easy to parse
CIF description in the specified file.
This option removes the include and array commands (see next section)
and replaces them with equivalent standard CIF statements.
.in -1.2i
.PP
.B Local Extensions
.PP
.I Cifplot
recognizes a number of extensions to standard CIF.
All extension commands begin with a number.
The following is a list of them.
.de Q1
.br
.sp
.ti -0.5i
..
.in 1.2i
.Q1
.B 0I
.IB filename ;
.br
.B Include:
Read from the specified file
as though it appeared in place of this command.
Include files can be nested up to 6 deep.
.Q1
.B 0A
.I s m n dx
.IB dy ;
.br
.B Array:
Repeat symbol
.I s m
times with
.I dx
spacing in the x-direction
and
.I n
times with
.I dy
spacing in the y-direction.
.I s, m,
and
.I n
are unsigned integers.
.I dx
and
.I dy
are signed integers in CIF units.
.Q1
.B 1
.IB message ;
.br
.B Print:
Print out the message on standard output when it is read.
.Q1
.B 2
.I
"text"
.IB transform ;
.ti -0.5i
.br
.B 2C
.I
"text"
.IB transform ;
.br
.B Text on Plot:
.I Text
is placed on the plot
at the position specified by the transformation.
The allowed transformations are the same as the
those allowed for the Call command.
The transformation affects only the point at which the beginning
of the text is to appear.
The text is always plotted horizontally,
thus the mirror and rotate transformations
are not really of much use.
Normally text is placed above and to the right of the reference point.
The
.B 2C
command centers the text about the reference point.
.Q1
.B 9
.IB name ;
.br
.B Name symbol:
.I name
is associated with the
current symbol.
.Q1
.B 94
.I name x
.IB y ;
.ti -0.5i
.br
.B 94
.I name x y
.IB layer ;
.br
.B Name point:
.I name
is associated with the point
.RI ( x,
.IR y ).
Any mask geometry crossing this point is also associated with
.IR name .
If
.I layer
is present then just geometry crossing the point
on that layer is associated with
.IR name .
For plotting this command is simialar to text on plot.
When doing circuit extraction this command is used to
give an explicit name to a node.
.I Name
must not have any spaces in it,
and it should not be a number.
.in -1.0i
.SH FILES
/usr/public/cifplot
.SH ALSO SEE
.I
A Guide to LSI Implementation
by Hon and Se\*'quin,
Second Edition
(Xerox PARC, 1980)
for a description of CIF.
.br
.I
Introduction to VLSI Systems
by Mead and Conway
(Addison-Wesley,
1980)
.sp
mkcif (public)
.br
vdump (public)
.SH AUTHOR
Dan Fitzpatrick
