.ds II \s-2INGRES\s0
.ds GQ \s-2GEO-QUEL\s0
.ds HE 'HOW TO SET UP GEO-QUEL''PAGE %'
.so nmacs
.ce
Instructions for setting up GEO-QUEL version 6.2.

.ce
(March 22, 1979)

This set of instructions will help you to get
\*(GQ running on your \*(II system.
If you have been running version 4.0 of \*(GQ
you should be aware of some differences
between the old and current versions.
They are identified in section 6 of this document.
Your \*(II system should have been completely
installed before starting to install \*(GQ.


.sh 1
Moving \*(GQ to /usr/bin.
.PS
The binary for \*(GQ is in .../bin/geoquel.
It should be moved or linked into /usr/bin.
This will provide you with a working system.

Move \*(GQ to /usr/bin:

.ti +8
mv .../bin/geoquel  /usr/bin/geoquel

Verify that the file mode of \*(GQ is 711.
Notice that it SHOULD NOT set user id.
The correct mode is:

.ti +8
-rwx--x--x 1 ingres	. . .	geoquel
.sh 1
\*(GQ documentation.
.PS
The reference manual for \*(GQ may be found
in .../doc/geoquel.
To print a copy of the \*(GQ reference manual
on the line printer,
execute the command:

.ti +8
setup georef | lpr

These instructions can be printed using the command:

.ti +8
nroff .../doc/other/setup_geo.nr

Documentation for the graphic terminal support
software, GRAFPAC, is currently limited to the information
in the code.
The files in .../geoquel/grafpac are organized such that a
file names start with a descriptive two letter code.
The codes are:

.in +8
.nf
gt	GT40-GT42 specific routines
tk	Tektronix 4014 specific routines
tv	Device independent routines
.fi
.in -8

GRAFPAC has been organized so that the
proceedure level interface is machine independent.

.sh 1
Display Terminal Support
.PS
\*(GQ supports two types of display
terminals: Digital Equipment Corporation's
GT40-GT42, and Tektronix's 4014.
The Tektronix 4014 version can be used to
drive any 4010 type terminal but a
4014 with extended graphics module is expected.

When running \*(GQ in background or from a
terminal that cannot display graphic output,
it must guess which type of display list should
be drawn, GT40-GT42 or 4014 type.
The default has been set to
4014.
If you would prefer another
default the file .../geoquel/geoquel.c
must be changed.
Specifically, the three compile options
at the head of the file must be adjusted.
See the comment in the code for further
information.

To use a GT40 or GT42, you must load a monitor program
in the PDP-11.
This monitor can be found in .../geoquel/gt42.
A loaded copy of the monitor is supplied
in .../geoquel/gt42/gt42mon and can be used directly.
Simply 'cat' this file to the terminal.

.ti +10
cat .../geoquel/gt42/gt42mon

The format of this file is suitable for use with
the standard ROM loader normally at address 166000.

If you decide to provide support for
another graphic device, you must re-write
the routines in .../geoquel/grafpac.
Naturally, we would be interested in hearing
about any other devices you develop support for.

.sh 1
Possible source code changes.
.dn
If your machine has a hardware floating point unit.
.PS
You may wish to recompile \*(GQ without the
software floating point interpreter.
This will not affect the operation in any way
(since the software interpreter
will not be used if your
machine has floating point hardware).
The resulting copies of the object code will be
somewhat smaller.
There are various shell files which contain
C-compile statements.
On each of these
the "-f" flag must be removed.
The shell files are all located in the directory .../geoquel/setup.
The one exception is .../setup/geoquel.sh.

.in +8
ed .../setup/geoquel.sh
.ti +5
(find "-f" on cc statement and remove)
.br
chdir .../geoquel/setup
.ti +5
(examine all files and remove -f's)
.in -8

.sh
Recompilation.
.PS
To recompile \*(GQ, execute the command:

	setup geoquel

This command will recompile \*(GQ from the
source code.
Various messages will be printed on the terminal telling
you what is being done and where it is.
The resulting copy of \*(GQ will be placed in .../bin/geoquel.
It should be moved to /usr/bin/geoquel.

.sh 1
Setting up the ttytype file.
.PS
If you have been using version 4.0 of \*(GQ,
you will not need to do this step.

The ttytype file describes
each terminal on your system.
\*(GQ will
not attempt to display graphical output on terminals
that are not capable of displaying it.
There is a sample of the file in

.ti +8
".../geoquel/ttytype.sample".

This is a copy of the file in use on the Berkeley system.

The ttytype file consists of a series of
lines;
the first character is the terminal id,
and the rest of the line tells
the type of the terminal.
The first of these characters is a terminal class,
and the rest signify the brand, or some other more
descriptive indication.
A completely blank line terminates the useful part
of the file, after which comments may 
appear unrestricted.
In the sample file the currently recognized
(defined) terminal types are listed.

Edit the sample file or create a new one to
match your system.
Place the file in .../files/ttytype:

.ti +8
mv .../geoquel/ttytype.sample  .../files/ttytype

.sh 1
Previous versions of \*(GQ.
.PS
The large change in version number (4.0 to 6.2)
was done so that \*(GQ matches the \*(II version numbering.

There has been very few external changes to the
\*(GQ language.
The HISTOGRAM command was greatly improved and the documentation
cleaned up.
In later modifications of \*(GQ there will be
some additional monitor commands and the underlying
graphics package will be updated.
Also, a rudimentary facility for accessing the \*(II
HELP and PRINT commands was added.
