.ds N \s-1NROFF\s0
.ds T \s-1TROFF\s0
.ds U \s-1UNIX\s0
.ds R \s-1ROFF\s0
.he '\-ME INSTALLATION INSTRUCTIONS''%'
.+c
.sp 5
.ce 10
.sz 14
.b "\-ME INSTALLATION INSTRUCTIONS"
.sz
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
The \-me macros provide a much more flexible environment
than most other macro packages available
for \*N\(dg and \*T\(dg,
.(f
\(dg\*N, \*T, and \*R are Trademarks of Bell Laboratories.
.)f
including the \-ms macros,
and include many more self check facilities.
Many hooks are included in the macros
to allow user personalization
of the macros,
it is felt that the rather common practice
of redefining internal \-ms macros
can be almost completely eliminated
by tweaking parameters
and grabbing onto hooks.
Unlike \-ms and other packages,
\-me follows a rigid and consistant
internal naming convention
to avoid name conflicts
with user-defined macros,
strings,
and number registers.
The \-me macros are largely compatible with \*R\(dg;
the notable exception is that single and double spacing
must be accomplished with the
.b .ls
command.
.pp
In order to run \-me,
you must already have the version seven
phototypesetter package,
including \*N and \*T.
It is easy to tell if you have the correct version
of this software, as follows:
.(l
.in +5
.ti -5
Version 6:
Source Language: assembly language
Documentation: separate for \*N and \*T
.ti -5
Version 7:
Source Language: C
Documentation: single, integrated manual
.)l
.pp
..To read in the tape,
..create a temporary directory somewhere
..and chdir into it.
..The tape is flat;
..no subdirectories need be created.
..Read the tape into this directory.
...(b
..mkdir <temp>
..chdir <temp>
..tp xm0
...)b
...pp
The
.b \-me
macro package for \*N and \*T
expects to reside
in the directory
.b /usr/lib ,
on version 6 systems, or in
.b /usr/lib/tmac.e
on version 7 systems.
In addition,
a subdirectory
.b /usr/lib/me
is expected
which contains special features
which are loaded as needed.
All special features
are in files
with names which end
.q .me .
The directory
.b /usr/lib/me
should be created
mode
.b 755 ,
and the file
.b /usr/lib/tmac.e
and all files in
.b /usr/lib/me
should be mode 644;
all of these should be owned by some
appropriate user,
probably
.b bin .
All documentation resides in
the directory 
.b doc/me
on this tape.
..files with names
..which end in
...q .nr ;
..this information should be copied to some safe place.
.pp
In order to speed the loading of the macros,
the comments should be stripped out of them.
However, you should keep a copy of the macros unstripped
in case you ever have to read them.
A shell file
.b strip-me.ed
will strip out the comments.
...(b
..mkdir /usr/lib/me
..chmod 755 /usr/lib/me
..chown bin /usr/lib/me
..cpall *.nr tmac.e *.me <safeplace>
..sh strip-me tmac.e *.me
..cp tmac.e /usr/lib/tmac.e
..cpall *.me /usr/lib/me
..chmod 644 /usr/lib/tmac.e /usr/lib/me/*
..chown bin /usr/lib/tmac.e /usr/lib/me/*
..rm tmac.e *.me
...)b
The shell script
.b make.script
in this directory will install and strip the macros.
.pp
If you wish to move the special options
to a directory other than
.b /usr/lib/me ,
change the string
.b |\||
to be the name of the directory
in which they reside.
This string is defined
in the file
.b tmac.e ;
the line may be found
by searching in the editor with
.(b
/ds |\||/
.)b
.pp
Also included with this tape
for interested souls
are some updated copies
of some of the terminal driver tables
for \*N.
These files describe what to print
for each character on output.
The files
.b 300.pica10
and
.b 300.courier10
are copies of the file
.b code.300
in the directory
.b troff/terminals
which came with the version 7 \*N/\*T tape.
These files are tuned for the
pica10 and courier10
dasy-style typewheels respectively.
Along with providing nicer copy
for many of the greek and special characters,
several more characters are defined.
The files
.b tabtn300.c
and
.b tab37.c
are revisions of the files
of the same name
in the
.b troff/terminals
directory;
they have more of the special characters defined.
.pp
To use these code files,
copy them into the directory
.b troff/terminals .
Select the 300.* file
which you want to be your standard
for DTC-type terminals;
copy that file into the file
.b code.300
(included by the files
.b tab300.c ,
.b tab300s.c ,
.b tab300s12.c ,
and possibly others).
You should then use the
.b termrc
shell file to recompile the code tables
for the terminals you use.
For example:
.(b
termrc tn300
.)b
will create a new character table
for terminet 300 style terminals
(that is,
any terminal without half line capabilities
or other goodies).
It is pretty easy
to define two types of terminals
for the different typewheels if you want;
create files
.b tabpica.c
and
.b tabcourier.c
as copies of the file
.b tab300s.c
(or
.b tab300.c
as appropriate for your installation)
and change the line which reads
.(b
# include "code.300"
.)b
to include
.b 300.pica10
and
.b 300.courier10
respectively.
Then compile the code tables
for types
.b pica
and
.b courier .
.(b
cpall 300.* tab*.c <??>/troff/terminals
chdir <??>/troff/terminals
cp 300.pica10 code.300
termrc tn300
termrc 37
termrc 300s
termrc 300
.)b
.pp
Lastly, there are some test programs
to check out certain characters.
The file
.b testdiacrit
tests the special characters and diacritical marks
defined in
.b /usr/lib/me/chars.me ;
.b testgreeks
tests the special and greek characters
which are in the terminal driver tables.
You can save these somewhere
or chuck them out.
.b Testdiacrit
acts on a local copy of
.b chars.me ;
make a copy before testing them.
Both of these tests
run outside of \-me.
.(b
chdir <temp>
nroff -T300s testgreeks
cp /usr/lib/me/chars.me .
nroff -T300s testdiacrit
troff testdiacrit
.)b
