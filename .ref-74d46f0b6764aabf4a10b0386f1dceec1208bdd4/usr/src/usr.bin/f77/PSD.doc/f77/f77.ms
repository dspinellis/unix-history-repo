.\" Copyright (c) 1985 The Regents of the University of California.
.\" All rights reserved.
.\"
.\" %sccs.include.redist.man%
.\"
.\"	@(#)f77.ms	5.5 (Berkeley) %G%
.\"
.rm CM
.hw name-list
.de XX
.ne 3
.sp .3
.ti -0.8i
.ta 0.8i
\\$1	\c
..
.\"
.\"	Nh macro - same as NH but also saves heading for table of contents
.\"	Nh usage: Nh level string, e.g.:  .Nh 2 "Short Integers"
.de Nh
.NH \\$1
\\$2
.XS
.if '2'\\$1' .ti .25i
.if '3'\\$1' .ti .5i
\\*(SN \\$2
.XE
.IP
..
.EQ
delim $$
.EN
.ND ""
.\".RP
.TL
A Portable Fortran 77 Compiler
.AU
S. I. Feldman
.AU
P. J. Weinberger
.AI
Bell Laboratories
Murray Hill, New Jersey 07974
.AU
J. Berkman
.AI
University of California
Berkeley, CA 94720
.AB
The Fortran language has been revised.
The new language, known as\p
Fortran 77,
became an official American National Standard on April 3, 1978.
We report here on a compiler and run-time system for the new extended language.
It is believed to be the first complete Fortran 77 system to be implemented.
This compiler is designed to be portable,
to be correct and complete,
and to generate code compatible with calling sequences produced by C compilers.
In particular, this Fortran is quite usable on
.UX
systems.
In this paper, we describe the language compiled,
interfaces between procedures,
and file formats assumed by the \s-1I/O\s0 system.
Appendix A describes the Fortran 77 language extensions.
.PP
This is a standard Bell Laboratories
document reproduced with minor modifications
to the text.
The Bell Laboratory's appendix
on ``Differences Between Fortran 66 and Fortran 77''
has been changed to Appendix A,
and a local appendix has been added.
Appendix B contains
a list of Fortran 77 references
(some from the original Bell document
and some added at Berkeley).
.sp 2
.LP
Revised September, 1985
.AE
.LP
.CS 9 10 19 0 0 8
.\" the Table of Contents uses pages 2 and 3,
.\" hence the document begins at page 4.
.pn 4
.EH 'PS1:2-%''A Portable Fortran 77 Compiler'
.OH 'A Portable Fortran 77 Compiler''PS1:2-%'
.bp
.NH 0
INTRODUCTION
.XS
\*(SN Introduction
.XE
.LP
The Fortran language has been revised.
The new language, known as Fortran 77,
became an official American National Standard [1] on April 3, 1978.
Fortran 77 supplants 1966 Standard Fortran [2].
We report here on a compiler and run-time system for the new extended language.
The compiler and computation library were written by S.I.F., the \s-1I/O\s0 system by P.J.W.
We believe ours to be the first complete Fortran 77 system to be implemented.
This compiler is designed to be portable to a number of different machines,
to be correct and complete,
and to generate code compatible with calling sequences produced
by compilers for the C language [3].
In particular,
it is in use on
\s-1UNIX\s0
systems.
Two families of C compilers are in use at Bell Laboratories,
those based on D. M. Ritchie's \s-1PDP-11\s0 compiler [4]
and those based on S. C. Johnson's portable C compiler [5].
This Fortran compiler can drive the second passes of either family.
In this paper, we describe the language compiled,
interfaces between procedures,
and file formats assumed by the \s-1I/O\s0 system.
We will describe implementation details in companion papers.
.Nh 2 Usage
At present, versions of the compiler run on and compile
for the \s-1PDP-11\s0,
the \s-1VAX-11/780\s0,
and the Interdata 8/32
\s-1UNIX\s0
systems.
The command to run the compiler is
.DS
f\|77  \fIflags  file . . .\fR
.DE
.B f\|77
is a general-purpose command for compiling and loading Fortran and Fortran-related files.
\s-1EFL\s0 [6] and Ratfor [7] source files will be preprocessed before being presented to the Fortran compiler.
C and assembler source files will be compiled by the appropriate programs.
Object files will be loaded.
(The
.B f\|77
and
.B cc
commands cause slightly different loading sequences to be generated,
since Fortran programs need a few extra libraries and a different startup routine
than do C programs.)
The following file name suffixes are understood:
.DS 
 \f3.f\f1	Fortran source file
 \f3.F\f1	Fortran source file
 \f3.e\f1	\s-1EFL\s0 source file
 \f3.r\f1	Ratfor source file
 \f3.c\f1	C source file
 \f3.s\f1	Assembler source file
 \f3.o\f1	Object file
.DE
.IP
Arguments whose names end with \f3.f\f1 are taken to be
Fortran 77 source programs;
they are compiled, and
each object program is left on the file in the current directory
whose name is that of the source with \f3.o\f1 substituted
for \f3.f\f1.
.IP
Arguments whose names end with \f3.F\f1
are also taken to be Fortran 77 source programs; these are first
processed by the C preprocessor before being compiled by \fBf77\fP.
.IP
Arguments whose names end with \f3.r\f1 or \f3.e\f1
are taken to be Ratfor or \s-1EFL\s0
source programs, respectively; these are first transformed by the
appropriate preprocessor, then compiled by \fBf77\fP.
.IP
In the same way,
arguments whose names end 
with \f3.c\f1 or \f3.s\f1
are taken to be C or assembly source programs
and are compiled or assembled, producing a \f3.o\f1 file.
.IP
The following flags are understood:
.in +0.8i
.XX \(mi\f3c\f1
Compile but do not load.
Output for
.B x.f ,
.B x.F ,
.B x.e ,
.B x.r ,
.B x.c ,
or
.B x.s
is put on file
.B x.o .
.XX \(mi\f3d\f1
Used in debugging the compiler.
.XX \(mi\f3g\f1
Have the compiler produce additional
symbol table information for \fIdbx(1)\fR.
This flag is incompatible with \(mi\f3O\f1.
See section 1.4 for more details.
.XX \(mi\f3i2\f1
On machines which support short integers,
make the default integer constants and variables short
(see section 2.14).
(\fB\(mii4\fR is the standard value of this option).  
All logical quantities will be short.
.XX \(mi\f3m\f1
Apply the M4 macro preprocessor
to each \s-1EFL\s0 or Ratfor source file
before using the appropriate compiler.
.XX "\(mi\f3o\f1 \fIfile\fR"
Put executable module on file
.I file .
(Default is \fBa.out\fR).
.ne 6
.XX \(mi\f3onetrip\f1\ or\ \(mi\f31\f1
.br
Compile code that performs every
.B do
loop at least once
(see section 2.12).
.XX \(mi\f3p\f1
Generate code to produce usage profiles.
.XX \(mi\f3pg\f1
Generate code in the manner of \fB\(mip\fR, but invoke a run-time
recording mechanism that keeps more extensive statistics.
See
.I gprof (1).
.XX \(mi\f3q\f1
Suppress printing of file names and program unit names during compilation.
.XX \(mi\f3r8\f1
Treat all floating point variables,
constants, functions and intrinsics
as double precision and all complex
quantities as double complex.  See section 2.17.
.XX \(mi\f3u\f1
Make the default type of a variable
.B undefined
(see section 2.3).
.XX \(mi\f3v\f1
Print the version number of the compiler and the name of each pass.
.XX \(mi\f3w\f1
Suppress all warning messages.
.XX \(mi\f3w66\f1
Suppress warnings about Fortran 66 features used.
.XX \(mi\f3C\f1
Compile code that checks that subscripts are within array bounds.
For multi-dimensional arrays, only the equivalent linear subscript is
checked.
.XX \(mi\fBD\fP\fIname=def\fR
.XX \(mi\fBD\fP\fIname\fR
Define the
.I name
to the C preprocessor, as if by `#define'. If no definition is given, the name
is defined as "1". (\fB.F\fR files only).
.XX \(mi\f3E\f1\fIstr\fR
Use the string \fIstr\fR as an
\s-1EFL\s0 option in processing \f3.e\f1 files.
.XX \(mi\f3F\f1
Ratfor, \s-1EFL\s0, and \f3.F\f1 source files
are pre-processed into \f3.f\f1 files,
and those \f3.f\f1 files are left on the disk without being compiled.
.XX \(mi\fBI\fP\fIdir\fR
`#include' files whose names do not begin with `/' are always sought
first in the directory of the \fIfile\fR
argument, then in directories named in \fB\(miI\fR
options, then in directories on a standard list. (\fB.F\fR files only).
.XX \(mi\f3N\f1[\fBqxscn\fR]\fInnn\f1
.br
Make static tables in the compiler bigger. The compiler will complain
if it overflows its tables and suggest you apply one or more of these
flags. These flags have the following meanings:
.RS
.IP \fBq\fP
Maximum number of equivalenced variables. Default is 150.
.IP \fBx\fP
Maximum number of external names (common block names, subroutine and
function names). Default is 200.
.IP \fBs\fP
Maximum number of statement numbers. Default is 401.
.IP \fBc\fP
Maximum depth of nesting for control statements (e.g. DO loops). Default is
20.
.IP \fBn\fP
Maximum number of identifiers. Default is 1009.
.RE
.XX \(mi\f3O\f1
Invoke the object code optimizer.
Incompatible with \(mi\f3g\f1.
.XX \(mi\f3R\f1\fIstr\fR
Use the string \fIstr\fR as a Ratfor option
in processing \f3.r\f1 files.
.XX \(mi\f3U\f1
Do not convert upper case letters to lower case.
The default is to convert Fortran programs to lower case
except within character string constants.
.XX \(mi\f3S\f1
Generate assembler output for each source file, but do not assemble it.
Assembler output for a source file
.B x.f ,
.B x.F ,
.B x.e ,
.B x.r ,
or
.B x.c
is put on file
.B x.s .
.in -0.8i
.IP
Other flags,
all library names (arguments beginning \fB\(mil\fR),
and any names not ending with one of the understood suffixes are passed
to the loader.
.Nh 2 Documentation\ Conventions
In running text, we write Fortran keywords and other literal strings in boldface lower case.
Examples will be presented in lightface lower case.
Names representing a class of values will be printed in italics.
.Nh 2 Implementation\ Strategy
The compiler and library are written entirely in C.
The compiler  generates C compiler intermediate code.
Since there are C compilers running on a variety of machines,
relatively small changes will make this Fortran compiler generate code for any of them.
Furthermore, this approach guarantees that the resulting programs are compatible with C usage.
The runtime computational library is complete.
The runtime \s-1I/O\s0 library makes use of D. M. Ritchie's Standard C \s-1I/O\s0 package [8]
for transferring data.
With the few exceptions described below, only documented calls are used,
so it should be relatively easy to modify to run on other operating
systems.
.Nh 2 Debugging\ Aids
A memory image is sometimes
written to a file \fBcore\fP in the current directory
upon abnormal termination for errors caught by the \fBf77\fP libraries,
user calls to \fBabort\fP, and certain signals (see \fBsigvec\fP\|(2)
in the \fI\s-1UNIX\s0 Programmer's Manual\fP).
\fBCore\fP is normally created only if
the \fB\(mig\fP flag was specified to \fBf77\fP during loading.\(dg
.FS
\(dgSpecify \fB\(mig\fP when loading with \fBcc\fP or \fBf77\fP;
specify \fB\(milg\fP as a library
when using \fBld\fP directly.
.FE
The source-level debugger
.I dbx (1)
may be used with the executable and the
.B core
file to examine the image and
determine what went wrong.
.IP
In the event that it is necessary to override this default behavior,
the user may set the environment variable \fBf77_dump_flag\fP.
If \fBf77_dump_flag\fP is set to a value beginning
with \fBn\fP, a \fBcore\fP file is not produced regardless of whether
\fB\(mig\fP was specified at compile time,
and if the value begins with \fBy\fP,
dumps are produced even if \fB\(mig\fP was not specified.
.NH 1
LANGUAGE EXTENSIONS
.XS
\*(SN Language Extensions
.XE
.LP
Fortran 77 includes almost all of Fortran 66 as a subset.
We describe the differences briefly in Appendix A.
The most important additions are a character string data type,
file-oriented input/output statements, and random access \s-1I/O\s0.
Also, the language has been cleaned up considerably.
.LP
In addition to implementing the language specified in the new Standard,
our compiler implements a few extensions described in this section.
Most are useful additions to the language.
The remainder are extensions
to make it easier to communicate with C procedures
or to permit compilation of
old (1966 Standard) programs.
.Nh 2 Double\ Complex\ Data\ Type
The new type
.B "double complex"
is defined.
Each datum is represented by a pair of double precision real values.
The statements
.DS
z1 = ( 0.1d0, 0.2d0 )
z2 = dcmplx( dx, dy )
.DE
assign double complex values to \fBz1\fP and \fBz2\fP.
The double precision values which constitute the double complex
value may be isolated by using \fBdreal\fP or \fBdble\fP for the
real part and \fBimag\fP or \fBdimag\fP for the
imaginary part.
To compute the double complex conjugate of a double complex value,
use \fBconjg\fP or \fBdconjg\fP.
The other \fBdouble complex\fP intrinsic functions may be
accessed using their generic names or specific names.
The generic names are: \fBabs\fP, \fBsqrt\fP, \fBexp\fP,
\fBlog\fP, \fBsin\fP, and \fBcos\fP.
The specific names are the same as the generic names preceded by
either \fBcd\fP or \fBz\fP, e.g. you may code \fBsqrt\fP,
\fBzsqrt\fP or \fBcdsqrt\fP to compute the square root of a double
complex value.
.Nh 2 Internal\ Files
The Fortran 77 standard introduces ``internal files'' (memory arrays), but
restricts their use to formatted sequential \s-1I/O\s0 statements.
Our \s-1I/O\s0 system also permits internal files to be used
in formatted direct reads and writes and list directed sequential read and
writes.
.Nh 2 Implicit\ Undefined\ Statement
Fortran 66 has a fixed rule that the type of a variable that does not appear in a type statement
is
.B integer
if its first letter is
\fBi, j, k, l, m\fR or \fBn\fR,
and
.B real
otherwise.
Fortran 77 has an
.B implicit
statement for overriding this rule.
As an aid to good programming practice, we permit an additional type,
.B undefined.
The statement
.DS
implicit undefined(a-z)
.DE
turns off the automatic data typing mechanism,
and the compiler will issue a diagnostic for each variable that is used but does
not appear in a type statement.
Specifying the
.B \(miu
compiler flag is equivalent to beginning each procedure with this statement.
.Nh 2 Recursion
Procedures may call themselves,
directly or through a chain of other procedures.
Since Fortran variables are by default
.B static ,
it is often necessary to use the
.B automatic
storage extension to prevent unexpected results
from recursive functions.
.Nh 2 Automatic\ Storage
Two new keywords are recognized,
.B static
and
.B automatic.
These keywords may appear as ``types'' in type statements and in
.B implicit
statements.
Local variables are static by default;
there is only one instance of the variable.
For variables declared
.B automatic,
there is a separate instance of the variable for each
invocation of the procedure.
Automatic variables may not appear in
.B equivalence,
.B data,
or
.B save
statements.
Neither type of variable is guaranteed to retain its value between
calls to a subprogram (see the \fBsave\fP statement in Appendix A).
.Nh 2 Source\ Input\ Format
The Standard expects input to the compiler to be in 72-column format:
except in comment lines,
the first five characters are the statement number, the next is the continuation character,
and the next 66 are the body of the line.
(If there are fewer than 72 characters on a line, the compiler pads it with blanks;
characters after the seventy-second are ignored.)
.IP
In order to make it easier to type Fortran programs,
our compiler also accepts input in variable length lines.
An ampersand ``&'' in the first position of a line indicates a continuation
line; the remaining characters form the body of the line.
A tab character in one of the first six positions of a line signals the
end of the statement number and continuation part of the line;
the remaining characters form the body of the line.
A tab elsewhere on the line is treated as another kind of blank by the
compiler.
.IP
In the Standard, there are only 26 letters \(em Fortran is a one-case language.
Consistent with ordinary
\s-1UNIX\s0
system usage, our compiler expects lower case input.
By default, the compiler converts all upper case characters to lower case except those inside character constants.
However, if the
.B \(miU
compiler flag is specified, upper case letters are not transformed.
In this mode, it is possible to specify external names with upper case letters in them,
and to have distinct variables differing only in case.
If \(mi\f3U\f1 is specified, 
keywords will only be recognized in lower case.
.Nh 2 Include\ Statement
The statement
.DS
include \(fmstuff\|\(fm
.DE
is replaced by the contents of the file
.B stuff ;
.B include
statements may be nested to a reasonable depth, currently ten.
.Nh 2 Binary\ Initialization\ Constants
A variable may be initialized in a
.B data
statement
by a binary constant, denoted by a letter followed by a quoted string.
If the letter is \fBb\fR, the string is binary, and only zeroes and ones are permitted.
If the letter is \fBo\fR, the string is octal, with digits \fB0\(mi7\fR.
If the letter is \fBz\fR or \fBx\fR, the string is hexadecimal, with digits \fB0\(mi9\fR, \fBa\(mif\fR.
Thus, the statements
.DS
integer a(3)
data a / b\(fm1010\|\(fm, o\(fm12\|\(fm, z\(fma\|\(fm /
.DE
initialize all three elements of
.B a
to ten.
.Nh 2 Character\ Strings
For compatibility with C usage, the following backslash escapes are recognized:
.DS
\f3\en\f1	newline
\f3\et\f1	tab
\f3\eb\f1	backspace
\f3\ef\f1	form feed
\f3\e0\f1	null
\f3\e\(fm\f1	apostrophe (does not terminate a string)
\f3\e"\f1	quotation mark (does not terminate a string)
\f3\e\e\f1	\e
\f3\e\fP\fIx\fR	\fIx\fR,  where \fIx\fR is any other character
.DE
Fortran 77 only has one quoting character, the apostrophe.
Our compiler and \s-1I/O\s0 system recognize
both the apostrophe `` \(fm '' and the double-quote `` " ''.
If a string begins with one variety of quote mark, the other may be embedded within it
without using the repeated quote or backslash escapes.
.IP
Each character string constant appearing outside a
.B data
statement is followed by a
null character to ease communication with C routines.
.Nh 2 Hollerith
Fortran 77 does not have the old Hollerith ``\fIn\fP\|\fBh\fR''
notation,
though the new Standard recommends implementing the old Hollerith feature
in order to improve compatibility with old programs.
In our compiler, Hollerith data may be used in place of character string constants,
and may also be used to initialize non-character variables in
.B data
statements.
.Nh 2 Equivalence\ Statements
As a very special and peculiar case,
Fortran 66 permits an element of a multiply-dimensioned array to be represented by
a singly-subscripted reference in
.B equivalence
statements.
Fortran 77 does not permit this usage, since
subscript lower bounds may now be different from 1.
Our compiler permits single subscripts in
.B equivalence
statements,
under the interpretation that all missing subscripts are equal to 1.
A warning message is printed for each such incomplete subscript.
.Nh 2 One-Trip\ \s-1DO\s0\ Loops
The Fortran 77 Standard requires that the range of a
.B do
loop not be performed
if the initial value is already past the limit value,
as in
.DS
do 10 i = 2, 1
.DE
The 1966 Standard stated that the effect of such a statement was undefined,
but it was common practice that the range of a
.B do
loop would be performed
at least once.
In order to accommodate old programs, though they were in violation of the 1966 Standard,
the
.B \(mionetrip
or
.B \(mi1
compiler flags causes non-standard loops to be generated.
.Nh 2 Commas\ in\ Formatted\ Input
The \s-1I/O\s0 system attempts to be more lenient than the
Standard when it seems worthwhile.
When doing a formatted read of non-character variables,
commas may be used as value separators in the input record,
overriding the field lengths given in the format statement.
Thus,
the format
.DS
(i10, f20.10, i4)
.DE
will read the record
.DS
\(mi345,.05e\(mi3,12
.DE
correctly.
.Nh 2 Short\ Integers
On machines that support halfword integers,
the compiler accepts declarations of type
.B integer\(**2.
(Ordinary integers follow the Fortran rules about occupying the same
space as a real variable; they are assumed to be of C type
.B "long int" ;
halfword integers are of C type
.B "short int" .)
An expression involving only objects of type
.B integer\(**2
is of that type.
Generic functions return short or long integers depending on the actual types of their arguments.
If a procedure is compiled using the
.B \(mii2
flag, all small integer constants will be
of type
.B integer\(**2.
If the precision of an integer-valued intrinsic function is not determined by the generic function rules,
one will be chosen that returns the prevailing length
(\fBinteger\(**2\fR when the \fB\(mii2\fR command flag is in effect).
When the
.B \(mii2
option is in effect, all quantities of type
.B logical
will be short.
Note that these short integer and logical quantities do not obey the standard rules for storage association.
.Nh 2 Additional\ Intrinsic\ Functions
This compiler supports all of the
intrinsic functions specified in the Fortran 77 Standard.
In addition, there are built-in functions
for performing bitwise logical and boolean operations on
integer and logical values
(\fBor\fR, \fBand\fR, \fBxor\fR, \fBnot\fR, \fBlshift\fP, and \fBrshift\fP),
and intrinsic functions for \fBdouble complex\fP values (see section 2.1).
The \fBf77\fP library contains many other functions, such as accessing
the \s-1UNIX\s0 command arguments (\fBgetarg\fR and \fBiargc\fR)
and environment (\fBgetenv\fR).
See \fBintro\fP(3f) and \fBbit\fP(3f) in the \fI\s-1UNIX\s0
Programmer's Manual\fP
for more information.
.Nh 2 Namelist\ \s-1I/O\s0
Namelist \s-1I/O\s0 provides an easy way to input and output information without
formats.
Although not part of the standard, namelist \s-1I/O\s0 was part of many
Fortran 66 systems and is a common extension to Fortran 77 systems.
.IP
Variables and arrays to be used in namelist \s-1I/O\s0 are declared as part of
a namelist in a \fBnamelist\fP statement, e.g.:
.DS
	character str\(**12
	logical flags(20)
	complex c(2)
	real arr1(2,3), arr2(0:3,4)
	namelist /basic/  arr1, arr2, key, str, c /flglst/ key, flags
.DE
This defines two namelists: list \fBbasic\fP consists of variables
\fBkey\fP and \fBstr\fP and arrays \fBarr1\fP, \fBarr2\fP,
and \fBc\fP; list \fBflglst\fP consists of variable \fBkey\fP and
array \fBflags\fP.
A namelist can include variables and arrays of any type, and
a variable or array may be in several different namelists.
However dummy arguments and array elements may not be in a namelist.
A namelist name may be used in external sequential \fBread\fP, \fBwrite\fP
and \fBprint\fP statements wherever a format could be used.
.IP
In a namelist \fBread\fP, column one of each data record is ignored.
The data begins with an ampersand in column 2 followed by
the namelist name and a blank.
Then there is a sequence of value assignments separated by commas
and finally an ``&end''.
A simple example of input data corresponding to namelist \fBbasic\fP is:
.DS
\ &basic key=5, str=\(fmhi there\(fm &end
.DE
.EQ
delim off
.EN
For compatibility with other systems, dollar signs
may be used instead of the ampersands:
.DS
\ $basic key=5, str=\(fmhi there\(fm $end
.DE
.IP
.EQ
delim $$
.EN
A value assignment in the data record must be one of three forms.
The simplest is a variable name followed by an equal sign
followed by a data value which is assigned to that variable,
e.g. ``key=5''.
The second form consists of an array name followed by ``=''
followed by one or more values to be assigned to the array,
e.g.:
.DS
c=(1.1,\-2.9),(\-1.8e+10,14.0e\-3)
.DE
assigns values to c(1) and c(2) in the complex array c.
.IP
As in other \fBread\fP statements, values are assigned in the order of the
array in memory, i.e. column-major order for two dimensional arrays.
Multiple copies of a value may be represented by a repetition count
followed by an asterisk followed by the value; e.g. ``3*55.4'' is the
same as ``55.4, 55.4, 55.4''.
It is an error to specify more values than the array can hold;
if less are specified, only that number of elements of the array
are changed.
The third form of a value assignment is a subscripted variable
name followed by ``='' followed by a value or values,
e.g.: ``arr2(0,4)=15.2''.
Only integer constant subscripts may be used.
The correct number of subscripts must be used and the subscripts
must be legal.
This form is the same as the form with an array name except the
array is filled starting at the named element.
.IP
In all three forms, the variable or array name must be declared
in the namelist.  The form of the data values is the same as in
list directed input except that in namelist \s-1I/O\s0,
character strings in the data must be enclosed in apostrophes or
double quotes, and
repetition counts must be followed by data values.
.IP
One use of namelist input is to read in a list of options or flags.
For example:
.DS
	logical flags(14)
	namelist /pars/ flags, iters, xlow, xhigh, xinc
	data flags/14*.false./

10	read(5,pars,end=900)
	print pars
	call calc( xlow, xhigh, xinc, flags, iters )
	go to 10
900	continue
	end
.DE
could be run with the following data (each record begins with a space):
.DS
\ &pars iters=10, xlow=0.0, xhigh=1.0, xinc=0.1 &end
\ &pars xinc=0.2,
\ \ \ flags(2)=2*.true., flags(8)=.true. &end
\ &pars xlow=2.0, xhigh=8.0 &end
.DE
The program reads parameters for the run from the first data set
and computes using them.
Then it loops and each successive set of namelist input data
specifies only those data items which need to be changed.
Note the second data set sets the $2 sup nd$, $3 sup rd$,
and $8 sup th$ elements in the array \fBflags\fP to \fB.true.\fP.
.IP
When a namelist name is used in a \fBwrite\fP or \fBprint\fP statement,
all the values in the namelist are output together with their names.
For example the \fBprint\fP in the program above prints the following:
.DS
\ &pars  flags=  f,  f,  f,  f,  f,  f,  f,  f,  f,  f,  f,  f,  f,  f, iters=
\   10, xlow=  0., xhigh=   1.00000, xinc=  0.100000
\ &end
\ &pars  flags=  f,  t,  t,  f,  f,  f,  f,  t,  f,  f,  f,  f,  f,  f, iters=
\   10, xlow=  0., xhigh=   1.00000, xinc=  0.200000
\ &end
\ &pars  flags=  f,  t,  t,  f,  f,  f,  f,  t,  f,  f,  f,  f,  f,  f, iters=
\   10, xlow=   2.00000, xhigh=   8.00000, xinc=  0.200000
\ &end
.DE
.IP
Each line begins with a space so that namelist output can be used as input to 
a namelist \fBread\fP.
The default is to use ampersands in namelist \fBprint\fP and
\fBwrite\fP.
However, dollar signs will be used if the last preceding namelist \fBread\fP
data set used dollar signs.
The character to be used is stored as the first character of the common
block \fBnamelistkey\fP.
.Nh 2 Automatic\ Precision\ Increase
The \(mi\fBr8\fP flag allows a user to run a program with increased
precision without changing any of the program source,
i.e. it allows a user to take a program coded in
single precision and compile and execute it as if it had
been coded in double precision.
The option extends the precision of all single precision real
and complex constants, variables, external functions, and intrinsic functions.
For example, the source:
.DS
	implicit complex(c)
	real last
	intrinsic sin, csin
	data last/0.3/

	x = 0.1
	y = sqrt(x)+sqrt(last)
	c1 = (0.1,0.2)
	c2 = sqrt(c1)
	x = real(i)
	y = aimag(c1)
	call fun(sin,csin)
.DE
is compiled under this flag as if it had been written as:
.DS
	implicit double precision (a-b,d-h,o-z), double complex(c)
	double precision last
	intrinsic dsin, cdsin
	data last/0.3d0/

	x = 0.1d0
	y = sqrt(x)+sqrt(last)
	c1 = (0.1d0,0.2d0)
	c2 = sqrt(c1)
	x = dreal(i)
	y = dimag(c1)
	call fun(dsin,cdsin)
.DE
When the \(mi\fBr8\f flag is invoked,
the calls using the generic name \fBsqrt\fP will refer to a different
specific function since the types of the arguments have changed.
This option extends the precision of all single precision \fBreal\fP
and \fBcomplex\fP variables and functions,
including those declared \fBreal\(**4\fP and \fBcomplex\(**8\fP.
.IP
In order to successfully use this flag to increase precision,
the entire program including
all the subroutines and functions it calls must be recompiled.
Programs which use dynamic memory allocation or
use equivalence or common statements to associate variables of different types
may have to be changed by hand.
Similar caveats apply to the sizes of records
in unformatted \s-1I/O\s0.
.Nh 2 Characters\ and\ Integers
A character constant of integer length or less
may be assigned to an integer variable.
Individual bytes are packed into
the integer in the native byte order.
The character constant is padded with blanks
to the width of the integer during the assignment.
Use of this feature is deprecated;
it is intended only as a porting aid
for extended Fortran 66 programs.
Note that the intrinsic
.B ichar
function behaves as the standard requires,
converting only single bytes to integers.
.NH 1
VIOLATIONS OF THE STANDARD
.XS
\*(SN Violations of the Standard
.XE
.LP
We know only a few ways in which our Fortran system violates the new standard:
.Nh 2 Double\ Precision\ Alignment
The Fortran Standards (both 1966 and 1977)
permit
.B common
or
.B equivalence
statements to force a double precision quantity onto an odd word boundary,
as in the following example:
.DS I
real a(4)
double precision b,c
.sp .5
equivalence (a(1),b), (a(4),c)
.DE
Some machines (e.g., Honeywell 6000, \s-1IBM 360\s0) require that double precision quantities be on double word boundaries;
other machines (e.g., \s-1IBM 370\s0), run inefficiently if this alignment rule is not observed.
It is possible to tell which equivalenced and common variables suffer from a forced odd
alignment, but every double precision argument would have to be assumed on a bad boundary.
To load such a quantity on some machines,
it would be necessary to use separate operations to move the upper and lower halves
into the halves of an aligned temporary, then to load that double precision temporary; the reverse would be
needed to store a result.
We have chosen to require that all double precision real and complex quantities
fall on even word boundaries on machines with corresponding hardware requirements,
and to issue a diagnostic if the source code demands a violation of the rule.
.Nh 2 Dummy\ Procedure\ Arguments
If any argument of a procedure is of type character,
all dummy procedure arguments of that procedure must be declared
in an
.B external
statement.
This requirement arises as a subtle corollary of the way we represent character string arguments
and of the one-pass nature of the compiler.
A warning is printed if a dummy procedure is not declared
.B external.
Code is correct if there are no
.B character
arguments.
.Nh 2 T\ and\ TL\ Formats
The implementation of the
.B t
(absolute tab)
and
.B tl
(leftward tab)
format codes
is defective.
These codes allow rereading or rewriting part of the
record which has already been processed
(section 6.3.2 in Appendix A).
The implementation uses seeks,
so if the unit is not one which allows seeks,
such as a terminal,
the program is in error.
A benefit of the implementation chosen is
that there is no upper limit on the length of
a record,
nor is it necessary to predeclare any record
lengths except where specifically required
by Fortran or the operating system.
.Nh 2 Carriage\ Control
The Standard leaves as implementation dependent which logical unit(s)
are treated as ``printer'' files.
In this implementation there is no printer file and
thus by default, no carriage control is recognized on formatted output.
This can be changed using \fBform=\|\(fmprint\|\(fm\fP in the
\fBopen\fP statement
for a unit, or by using the \fBfpr\fP(1) filter for output; see [9].
.Nh 2 Assigned\ Goto
The optional
.I list
associated with an assigned
.B goto
statement is not checked against the actual assigned value during execution.
.NH 1
INTER-PROCEDURE INTERFACE
.XS
\*(SN Inter-Procedure Interface
.XE
.LP
To be able to write C procedures that call or are called by Fortran procedures,
it is necessary to know the conventions for procedure names,
data representation,
return values,
and argument lists that the compiled code obeys.
.Nh 2 Procedure\ Names
On
\s-1UNIX\s0
systems,
the name of a common block or a Fortran procedure
has an underscore appended to it by the compiler
to distinguish it from a C procedure or external variable
with the same user-assigned name.
Fortran built-in procedure names have embedded underscores to avoid clashes
with user-assigned subroutine names.
.Nh 2 Data\ Representations
The following is a table of
corresponding Fortran and C declarations:
.KS
.TS
center;
l l
l l.
.B
Fortran	C
.R
.sp .5
integer\(**2 x	short int x;
integer x	long int x;
logical x	long int x;
real x	float x;
double precision x	double x;
complex x	struct { float r, i; } x;
double complex x	struct { double dr, di; } x;
character\(**6 x	char x[6];
.TE
.KE
(By the rules of Fortran,
.B integer,
.B logical,
and
.B real
data occupy the same amount of memory.)
.Nh 2 Arrays
The first element of a C array always has subscript zero,
while Fortran arrays begin at 1 by default.
Fortran arrays are stored in column-major order in contiguous storage,
C arrays are stored in row-major order.
Many mathematical libraries have subroutines which transpose a two dimensional
matrix, e.g. \fBf01crf\fP in the \fB\s-1NAG\s0\fP library and
\fBvtran\fP in the \fB\s-1IMSL\s0\fP library.
These may be used to transpose a two-dimensional array stored in C in row-major
order to Fortran column-major order or vice-versa.
.Nh 2 Return\ Values
A function of type
.B integer,
.B logical,
.B real,
or
.B "double precision"
declared as a C function returns the corresponding type.
A
.B complex
or
.B "double complex"
function is equivalent to a C routine
with an additional
initial argument that points to the place where the return value is to be stored.
Thus,
.DS
complex function f( . . . )
.DE
is equivalent to
.DS
f_(temp, . . .)
struct { float r, i; } \(**temp;
 . . .
.DE
A character-valued function is equivalent to a C routine with
two extra initial arguments:  a data address and a length.
Thus,
.DS
character\(**15 function g( . . . )
.DE
is equivalent to
.DS
g_(result, length, . . .)
char result[ ];
long int length;
 . . .
.DE
and could be invoked in C by
.DS
char chars[15];
 . . .
g_(chars, 15L, . . . );
.DE
Subroutines are invoked as if they were \fBinteger\fR-valued functions
whose value specifies which alternate return to use.
Alternate return arguments (statement labels) are not passed to the function,
but are used to do an indexed branch in the calling procedure.
(If the subroutine has no entry points with alternate return arguments,
the returned value is undefined.)
The statement
.DS
call nret(\(**1, \(**2, \(**3)
.DE
is treated exactly as if it were the computed
.B goto
.DS
goto (1, 2, 3),  nret( )
.DE
.Nh 2 Argument\ Lists
All Fortran arguments are passed by address.
In addition,
for every argument that is of type character or
that is a dummy procedure,
an argument giving the length of the value is passed.
(The string lengths are
.B "long int"
quantities passed by value.)
The order of arguments is then:
.DS
Extra arguments for complex and character functions
Address for each datum or function
A \fBlong int\fR for each character or procedure argument
.DE
Thus, the call in
.DS
external f
character\(**7 s
integer b(3)
 . . .
call sam(f, b(2), s)
.DE
is equivalent to that in
.DS
int f();
char s[7];
long int b[3];
 . . .
sam_(f, &b[1], s, 0L, 7L);
.DE
.Nh 2 System\ Interface
To run a Fortran program, the system invokes a small C program which
first initializes signal handling, then calls \fBf_init\fP to initialize
the Fortran \s-1I/O\s0 library, then calls your Fortran main program,
and then calls \fBf_exit\fP to close any Fortran files opened.
.IP
\fBf_init\fP initializes Fortran units 0, 5, and 6 to standard error,
standard input, and standard output respectively.
It also calls \fBsetlinebuf\fP to initiate line buffering 
of standard error.
If you are using Fortran subroutines which may do \s-1I/O\s0
and you have a C main program,
call \fBf_init\fP before calling the Fortran subroutines.
Otherwise, Fortran units 0, 5, and 6 will be connected to files
\fBfort.0\fP, \fBfort.5\fP, and \fBfort.6\fP,
and error messages from the \fBf77\fP libraries will be written
to \fBfort.0\fP instead of to standard error.
If your C program terminates by calling the C function \fBexit\fP,
all files are automatically closed.
If there are Fortran scratch files to be deleted, first call \fBf_exit\fP.
\fBF_init\fP and \fBf_exit\fP do not have any arguments.
.IP
The \fB\(mid\fP flag will show what libraries are used in loading Fortran
programs.
.NH 1
FILE FORMATS
.XS
\*(SN File Formats
.XE
.Nh 2 Structure\ of\ Fortran\ Files
Fortran requires four kinds of external files:
sequential formatted and unformatted,
and direct formatted and unformatted.
On
\s-1UNIX\s0
systems,
these are all implemented as ordinary files
which are assumed to have the proper
internal structure.
.IP
Fortran \s-1I/O\s0 is based on \f2records\f1.
When a direct file is opened in a Fortran program,
the record length of the records must be given,
and this is used by the Fortran \s-1I/O\s0 system to
make the file look as if it is made up of records
of the given length.
In the special case that the record length is given
as 1,
the files are not considered to be divided into records,
but are treated as byte-addressable byte strings;
that is,
as ordinary
\s-1UNIX\s0
file system files.
(A read or write request on such a file keeps consuming bytes until
satisfied, rather than being restricted to a single record.)
.IP
The peculiar requirements on sequential unformatted files
make it unlikely that they will ever be read or written by any means except Fortran \s-1I/O\s0 statements.
Each record is preceded and followed by
an integer containing the record's length in bytes.
.IP
The Fortran \s-1I/O\s0 system breaks sequential formatted files
into records while reading by using each newline
as a record separator.
The result of reading off the end of a record is undefined according to the Standard.
The \s-1I/O\s0 system is permissive and
treats the record as being extended by blanks.
On output,
the \s-1I/O\s0 system will write a newline at the end of each
record.
It is also possible for programs to write newlines
for themselves.
This is an error,
but the only effect will be that the single record
the user thought he wrote will be treated as
more than one record when being read or
backspaced over.
.Nh 2 Portability\ Considerations
The Fortran \s-1I/O\s0 system uses only the facilities of the
standard C \s-1I/O\s0 library,
a widely available and fairly portable package,
with the following two nonstandard features:
the \s-1I/O\s0 system needs to know whether a file
can be used for direct \s-1I/O\s0,
and whether or not it is possible to backspace.
Both of these facilities are implemented
using the
.B fseek
routine,
so there is a routine
.B canseek
which determines if
.B fseek
will have the desired effect.
Also, the
.B inquire
statement provides the user
with the ability to find out if two files are the
same,
and to get the name of an already opened file
in a form which would enable the program to reopen
it.
Therefore there are two routines which
depend on facilities of the operating system
to provide these two services.
In any case,
the \s-1I/O\s0 system
runs on the \s-1PDP-11\s0, \s-1VAX-11/780\s0, and Interdata 8/32
\s-1UNIX\s0
systems.
.Nh 2 Logical\ Units\ and\ Files
Fortran logical unit numbers may be any integer between 0 and 99.
The number of simultaneously open files is currently limited to 48.
.IP
Units 5, 6, and 0 are connected before the program begins to
standard input, standard output, and standard error respectively.
.IP
If an unit is opened explicitly by an \fBopen\fP statement with
a \fBfile=\fP keyword, then the file name is the name from
the \fBopen\fP statement.
Otherwise, the default file name corresponding to unit \fIn\fP is
\fBfort.\fP\fIn\fP.
If there is an environment variable whose name is the same as
the tail of the file name after periods are deleted,
then the contents of that environment variable
are used as the name of the file.
See [9] for details.
.IP
The default connection for all units is for sequential formatted \s-1I/O\s0.
The Standard does not specify where a file which has been explicitly
\fBopen\fRed
for sequential \s-1I/O\s0 is initially positioned.
The \s-1I/O\s0 system will position the file at the beginning.
Therefore a
.B write
will destroy any data already in the file, but a
.B read
will work reasonably.
To position a file to its end,
use a \fBread\fP loop, or the system dependent function \fBfseek\fP.
The preconnected units
0, 5, and 6 are positioned as they come
from the program's parent process.
.bp
.SH
APPENDIX A:  Differences Between Fortran 66 and Fortran 77
.XS
Appendix A.  Differences Between Fortran 66 and Fortran 77
.XE
.LP
The following is a very brief description of the differences
between the 1966 [2] and the 1977 [1] Standard languages.
We assume that the reader is familiar with Fortran 66.
We do not pretend to be complete, precise,
or unbiased,
but plan to describe what we feel are the most important aspects of the new language.
The best current information on the 1977 Standard is in publications of the
\s-1X3J3\s0 Subcommittee of the
American National Standards Institute, and
the \s-1ANSI\s0 X3.9-1978 document, the official description of the language.
The Standard is written in English rather than a meta-language,
but it is forbidding and legalistic.
A number of tutorials and textbooks
are available
(see Appendix B).
.NH 0
Features Deleted from Fortran 66
.XS
\*(SN Features Deleted from Fortran 66
.XE
.Nh 2 Hollerith
All notions of ``Hollerith''
(\fIn\fP\|\fBh\fR)
as data
have been officially removed, although our compiler, like almost all in the foreseeable future,
will continue to support this archaism.
.Nh 2 Extended\ Range\ of\ DO
.IP
In Fortran 66, under a set of very restrictive and rarely-understood conditions, it is permissible
to jump out of the range of a
.B do
loop, then jump back into it.
Extended range has been removed in the Fortran 77 language.
The restrictions are so special, and the implementation of extended range is so unreliable in many compilers,
that this change really counts as no loss.
.NH 1
Program Form
.XS
\*(SN Program Form
.XE
.Nh 2 Blank\ Lines
Completely blank lines are now legal comment lines.
.Nh 2 Program\ and\ Block\ Data\ Statements
A main program may now begin with a statement that gives that program an external name:
.DS
program work
.DE
Block data procedures may also have names.
.DS
block data stuff
.DE
There is now a rule that only
.I one
unnamed
block data procedure may appear in a program.
(This rule is not enforced by our system.)
The Standard does not specify the effect of the program and block data names,
but they are clearly intended to aid conventional loaders.
.Nh 2 ENTRY\ Statement
Multiple entry points are now legal.
Subroutine and function subprograms may have additional entry points,
declared by an
.B entry
statement with an optional argument list.
.DS
entry extra(a, b, c)
.DE
Execution begins at the first statement following the
.B entry
line.
All variable declarations must precede all executable statements in the procedure.
If the procedure begins with a
.B subroutine
statement,
all entry points are subroutine names.
If it begins with a
.B function
statement, each entry is a function entry point,
with type determined by the type declared for the entry name.
If any entry is a character-valued function,
then all entries must be.
In a function, an entry name of the same type as that where control entered
must be assigned a value.
Arguments do not retain their values between calls.
(The ancient trick of calling one entry point with a large number of arguments
to cause the procedure to ``remember'' the locations of those arguments,
then invoking an entry with just a few arguments for later calculation,
is still illegal.
Furthermore, the trick doesn't work in our implementation,
since arguments are not kept in static storage.)
.Nh 2 \s-1DO\s0\ Loops
.B do
variables and range parameters may now be of integer, real, or double precision types.
(The use of floating point
.B do
variables is very dangerous
because of the possibility of unexpected roundoff,
and we strongly recommend against their use.)
The action of the
.B do
statement is now defined for all values of the
.B do
parameters.
The statement
.DS
do 10 i = l, u, d
.DE
performs
$ max (0^,^ left floor ( u - l + d ) / d^ right floor )$
iterations.
The
.B do
variable has a predictable value when exiting a loop:
the value at the time a
.B goto
or
.B return
terminates the loop;
otherwise
the value that failed the limit test.
.Nh 2 Alternate\ Returns
In a
.B subroutine
or subroutine
.B entry
statement,
some of the arguments may be noted by an asterisk, as in
.DS
subroutine s(a, \(**, b, \(**)
.DE
The meaning of the ``alternate returns'' is described
in section 5.2 of Appendix A.
.NH 1
Declarations
.XS
\*(SN Declarations
.XE
.Nh 2 CHARACTER\ Data\ Type
One of the biggest improvements to the language is the addition of a character-string data type.
Local and
common character variables must have a length denoted by a constant expression:
.DS
character\(**17 a, b(3,4)
character\(**(6+3) c
.DE
If the length is omitted entirely, it is assumed equal to 1.
A character string argument may have a constant length,
or the length may be declared to be the same as that of the corresponding actual argument at run time
by a statement like
.DS
character\(**(\(**) a
.DE
(There is an intrinsic function
.B len
that returns the actual length of a character string.)
Character arrays and common blocks containing character variables must be packed:
in an array of character variables, the first character of one element must follow the last character of
the preceding element, without holes.
.Nh 2 IMPLICIT\ Statement
The traditional implied declaration rules still hold:
a variable whose name begins with
\fBi, j, k, l, m,\fR or \fBn\fR is of type
\f3integer\f1;
other variables are of type
.B real,
unless otherwise declared.
This general rule may be overridden with an
.B implicit
statement:
.DS
implicit real(a-c,g), complex(w-z), character\(**(17) (s)
.DE
declares that variables whose name begins with an
\fBa ,b, c,\fR
or
\fBg\fR
are
.B real,
those beginning with
\fBw, x, y,\fR
or
\fBz\fR
are assumed
.B complex,
and so on.
It is still poor practice to depend on implicit typing, but this statement is an industry standard.
.Nh 2 PARAMETER\ Statement
It is now possible to give a constant a symbolic name, as in
.DS
character str\(**(\(**)
parameter (x=17, y=x/3, pi=3.14159d0, str=\(fmhello\(fm)
.DE
The type of each parameter name is governed
by the same implicit and explicit rules as for a variable.
Symbolic names for \fBcharacter\fP constants may be declared with
an implied length ``(\(**)''.
The right side of each equal sign must be a constant expression
(an expression made up of constants, operators, and already defined parameters).
.Nh 2 Array\ Declarations
Arrays may now have as many as seven dimensions.
(Only three were permitted in 1966.)
The lower bound of each dimension may be declared
to be other than 1 by
using a colon.
Furthermore, an adjustable array bound may be an integer expression involving constants,
arguments, and variables in
.B common.
.DS
real a(\(mi5:3, 7, m:n), b(n+1:2\(**n)
.DE
The upper bound on the last dimension of an array argument may be denoted by an asterisk
to indicate that the upper bound is not specified:
.DS
integer a(5, \(**),  b(\(**), c(0:1, \(mi2:\(**)
.DE
.Nh 2 SAVE\ Statement
A little known rule of Fortran 66 is that variables in a procedure do
not necessarily retain their values between invocations of that procedure.
This rule permits overlay and stack implementations for the affected variables.
In Fortran 77, three types of variables automatically keep there values:
variables in blank common,
variables defined in \fBdata\fP statements and never changed, and 
variables in named common blocks which have not become undefined.
At any instant in the execution of a program,
if a named common block is declared neither in the currently executing procedure
nor in any of the procedures in the chain of callers,
all of the variables in that common block become undefined.
Fortran 77 permits one to specify that certain variables and common blocks are
to retain their values between invocations.
The declaration
.DS
save a, /b/, c
.DE
leaves the values of the variables
.B a
and
.B c
and all of the contents of common block
.B b
unaffected by an exit from the procedure.
The simple declaration
.DS
save
.DE
has this effect on all variables and common blocks in the procedure.
A common block must be \fBsave\fRd in every procedure in which it is declared
if the desired effect is to occur.
.Nh 2 INTRINSIC\ Statement
All of the functions specified in the Standard are in a single category,
``intrinsic functions'', rather than being divided into ``intrinsic'' and ``basic external'' functions.
If an intrinsic function is to be passed to another procedure, it must be declared
.B intrinsic.
Declaring it
.B external
(as in Fortran 66) causes a function other than the built-in one to be passed.
.NH 1
Expressions
.XS
\*(SN Expressions
.XE
.Nh 2 Character\ Constants
Character string constants are marked by strings surrounded by apostrophes.
If an apostrophe is to be included in a constant, it is repeated:
.DS
 \(fmabc\(fm
 \(fmain\(fm\(fmt\(fm
.DE
Although null (zero-length) character strings are not allowed in the
standard Fortran, they may be used with \fBf77\fP.
Our compiler has two different quotation marks, `` \(fm '' and `` " ''.
(See section 2.9 in the main text.)
.Nh 2 Concatenation
One new operator has been added,
character string concatenation, marked by a double slash
``//''.
The result of a concatenation
is the string containing the characters
of the left operand followed by the characters of
the right operand.
The character expressions
.DS
 \(fmab\(fm // \(fmcd\(fm
 \(fmabcd\(fm
.DE
are equal.
.IP
Dummy arguments of type character may be declared with implied
lengths:
.DS
subroutine s ( a, b )
character a\(**(\(**), b\(**(\(**)
.DE
Such dummy arguments may be used in concatenations in assign statements:
.DS
s = a // b
.DE
but not in other contexts.  For example:
.DS
if( a // b .eq. \(fmabc\(fm ) key = 1
call sub( a // b )
.DE
are legal statements if ``a'' and ``b'' are dummy arguments
declared with explicit lengths, or if they are not arguments.
These are illegal if they are declared with implied lengths.
.Nh 2 Character\ String\ Assignment
The left and right sides of a character assignment may not share storage.
(The assumed implementation of character assignment is to copy characters from the right to the left side.)
If the left side is longer than the right, it is padded with blanks.
If the left side is shorter than the right, trailing characters are discarded.
Since the two sides of a character assignment must be disjoint, the
following are illegal:
.DS
str = \(fm\ \(fm // str
str = str(2:)
.DE
These are not flagged as errors during compilation or execution,
however the result is undefined.
.Nh 2 Substrings
It is possible to extract a substring of a character variable or character
array element, using the colon notation:
.DS
a(i,\|j) (m:n)
.DE
is the string of $(n-m+1)$ characters beginning at the
$m sup th$ character of the character array element $a sub ij$.
Results are undefined unless $m<=n$.
Substrings may be used on the left sides of assignments and as procedure actual arguments.
.Nh 2 Exponentiation
It is now permissible to raise real quantities to complex powers,
or complex quantities to real or complex powers.
(The principal part of the logarithm is used.)
Also, multiple exponentiation is now defined:
.DS
a\(**\(**b\(**\(**c is equivalent to a \(**\(** (b\(**\(**c)
.DE
.Nh 2 Relaxation\ of\ Restrictions
Mixed mode expressions are now permitted.
(For instance,
it is permissible to combine integer and complex quantities in an expression.)
.IP
Constant expressions are permitted where a constant is allowed,
except in
.B data
statements
and
.B format
statements.
(A constant expression is made up of explicit constants and
\fBparameter\fRs
and the Fortran operators,
except for exponentiation to a floating-point power.)
An adjustable dimension may now be an integer expression involving constants,
arguments, and variables in
.B common.
.IP
Subscripts may now be general integer expressions;
the old
$c v +- c'$
rules have been removed.
.B do
loop bounds may be general integer, real, or double precision expressions.
Computed
.B goto
expressions and \s-1I/O\s0 unit numbers may be general integer expressions.
.NH 1
Executable Statements
.XS
\*(SN Executable Statements
.XE
.Nh 2 IF-THEN-ELSE
At last, the
if-then-else
branching structure has been added to Fortran.
It is called a ``Block If\|''.
A Block If begins with a statement of the form
.DS
if ( . . . ) then
.DE
and ends with an
.DS
end if
.DE
statement.
Two other new statements may appear in a Block If.
There may be several
.DS
else if (. . .) then
.DE
statements,
followed by at most one
.DS
else
.DE
statement.
If the logical expression in the Block If statement is true,
the statements following it up to the next
.B "else if",
.B else,
or
.B "end if"
are executed.
Otherwise, the next
.B "else if"
statement in the group is executed.
If none of the
.B "else if"
conditions are true, control passes to the statements following the
.B else
statement, if any.
(The
.B else
block must follow all
.B "else if"
blocks in a Block If.
Of course, there may be Block Ifs embedded inside of other Block If structures.)
A case construct may be rendered:
.DS
if (s .eq. \(fmab\(fm) then
 . . .
else if (s .eq. \(fmcd\(fm) then
 . . .
else
 . . .
end if
.DE
.Nh 2 Alternate\ Returns
Some of the arguments of a subroutine call may be statement labels preceded by an asterisk, as in:
.DS
call joe(j, \(**10, m, \(**2)
.DE
A
.B return
statement may have an integer expression, such as:
.DS
return k
.DE
If the entry point has
$n$
alternate return (asterisk) arguments
and if $1<=k<=n$, the return is followed by a branch to the corresponding statement label;
otherwise the usual return to the statement following the
.B call
is executed.
.NH 1
Input/Output
.XS
\*(SN Input/Output
.XE
.Nh 2 Format\ Variables
A format may be the value of a character expression (constant or otherwise),
or be stored in a character array, as in:
.DS
write(6, \(fm(i5)\(fm) x
.DE
.Nh 2 END=,\ ERR=,\ and\ IOSTAT=\ Clauses
A
.B read
or
.B write
statement may contain
.B end=,
.B err=,
and
.B iostat=
clauses, as in:
.DS
write(6, 101, err=20, iostat=a(4))
read(5, 101, err=20, end=30, iostat=x)
.DE
Here 5 and 6 are the
.I units
on which the \s-1I/O\s0 is done,
101 is the statement number of the associated format,
20 and 30 are statement numbers,
and
.B a
and
.B x
are integer variables.
If an error occurs during \s-1I/O\s0,
control returns to the program at statement 20.
If the end of the file is reached,
control returns to the program at statement 30.
In any case, the variable referred to in
the
.B iostat=
clause is given a value when
the \s-1I/O\s0 statement finishes.
(Yes, the value is assigned to the name on the right side of the equal sign.)
This value is zero if all went well,
negative for end of file,
and some positive value for errors.
.Nh 2 Formatted\ \s-1I/O\s0
.NH 3
Character Constants
.IP
Character constants in formats are copied literally to the output.
.IP
A format may be specified as a character constant within the
.B read
or
.B write
statement.
.DS
write(6,\|\(fm(i2,\|\(fm\|\(fm isn\|\(fm\|\(fm\|\(fm\|\(fm\|t \|\(fm\|\(fm,i1)\|\(fm) 7, 4
.DE
produces
.DS
 7 isn\|\(fm\|t 4
.DE
In the example above, the format is the character constant
.DS
(i2,\|\(fm isn\|\(fm\|\(fmt \|\(fm,i1)
.DE
and the embedded character constant
.DS
 isn\|\(fmt
.DE
is copied into the output.
.IP
The example could have been written more legibly by taking advantage
of the two types of quote marks.
.DS
write(6,\|\(fm(i2," isn\|\(fm\|\|\(fm\|t ",i1)\|\(fm) 7, 4
.DE
However, the double quote is not standard Fortran 77.
.IP
The standard does not allow reading into character constants or
Hollerith fields.
In order to facilitate running older programs, the Fortran \s-1I/O\s0 library
allows reading into Hollerith fields; however this is a practice to be
avoided.
.NH 3
Positional Editing Codes
.IP
.B t,
.B tl,
.B tr,
and
.B x
codes
control where the
next character is in the record.
\fBtr\fIn\fR
or
\fIn\fBx\fR
specifies that the next character is
$n$ to the right of the current position.
\fBtl\fIn\fR
specifies that the next character is
$n$ to the left of the current position,
allowing parts of the record to be reconsidered.
\fBt\fIn\fR
says that the next character is to be character
number $n$ in the record.
(See section 3.3 in the main text.)
.NH 3
Colon
.IP
A colon in the format terminates the \s-1I/O\s0 operation
if there are no more data items in the \s-1I/O\s0 list,
otherwise it has no effect.
In the fragment
.DS
x=\(fm("hello", :, " there", i4)\(fm
write(6, x) 12
write(6, x)
.DE
the first
.B write
statement prints
.DS
hello there 12
.DE
while the second only prints
.DS
hello
.DE
.NH 3
Optional Plus Signs
.IP
According to the Standard,
each implementation has the option of putting
plus signs in front of non-negative
numeric output.
The
.B sp
format code may be used to make the optional plus
signs actually appear for all subsequent items
while the format is active.
The
.B ss
format code guarantees that the \s-1I/O\s0 system will not
insert the optional plus signs,
and the
.B s
format code restores the default behavior of
the \s-1I/O\s0 system.
(Since we never put out optional plus signs,
.B ss
and
.B s
codes have the same effect in our implementation.)
.NH 3
Blanks on Input
.IP
Blanks in numeric input fields,
other than leading blanks,
will be ignored following a
.B bn
code in a format
statement,
and will be treated as zeros following a
.B bz
code in a format statement.
The default for a unit may be changed by using
the
.B open
statement.
(Blanks are ignored by default.)
.NH 3
Unrepresentable Values
.IP
The Standard requires that if a numeric item
cannot be represented in the form required by a format code,
the output field must be filled with asterisks.
(We think this should have been an option.)
.NH 3
Iw.m
.IP
There is a new integer output code,
\fBi\fIw.m.\fR
It is the same as
\fBi\fIw\fR,
except that there will be at least $m$
digits in the output field,
including,
if necessary,
leading zeros.
The case \fBi\fR$w.0$ is special,
in that if the value being printed is 0,
the output field is
entirely blank.
\fBi\fIw\fB.1\fR
is the same as
\fBi\fIw\fR.
.NH 3
Floating Point
.IP
On input, exponents may start with the letter
\fBE, D, e, \fRor \fBd.\fR
All have the same meaning.
On output we always use \fBe\fR or \fBd\fR.
The
.B e
and
.B d
format codes also have identical meanings.
A leading zero before the decimal point in
.B e
output
without a scale factor is optional with the
implementation.
There is a
\fBg\fIw.d\fR
format code which is the same as
\fBe\fIw.d\fR
and
\fBf\fIw.d\fR
on input,
but which chooses
.B f
or
.B e
formats for output depending
on the size of the number and of $d$.
.NH 3
``A'' Format Code
.IP
The
.B a
code is used for character data.
\fBa\fIw\fR
uses a field width of $w$,
while a plain
.B a
uses the length of the internal character item.
.Nh 2 Standard\ Units
There are default formatted input and output units.
The statement
.DS
read 10, a, b
.DE
reads from the standard unit using format statement 10.
The default unit may be explicitly specified by an asterisk, as in
.DS
read(\(**, 10) a, b
.DE
Similarly, the standard output unit is specified by a
.B print
statement or an asterisk unit:
.DS
print 10
write(\(**, 10)
.DE
.Nh 2 List-Directed\ \s-1I/O\s0
List-directed \s-1I/O\s0 is a
kind of free form input for sequential \s-1I/O\s0.
It is invoked by using an asterisk as the
format identifier, as in
.DS
read(6, \(**) a,b,c
.DE
.IP
On input,
values are separated by strings of blanks
and possibly a comma.
On \s-1UNIX\s0, tabs may be used
interchangeably with blanks as separators.
Values,
except for character strings,
cannot contain blanks.
End of record counts as a blank,
except in character strings,
where it is ignored.
Complex constants are given as two real constants
separated by a comma and enclosed in parentheses.
A null input field,
such as between two consecutive commas,
means the corresponding variable in the
\s-1I/O\s0 list is not changed.
Values may be preceded by repetition counts,
as in
.DS
4\(**(3.,2.)  2\(**, 4\(**\|\(fm\|hello\|\(fm
.DE
which stands for 4 complex constants, 2 null values,
and 4 string constants.
.IP
The Fortran standard requires data being read into \fBcharacter\fP variables
by a list-directed read to be enclosed in quotes.
In our system, the quotes are optional for strings which do not start with
a digit or quote and do not contain separators.
.IP
For output, suitable formats are chosen for
each item.
The values of character strings are printed;
they are not enclosed in quotes.
According to the standard,
they could not be read back
using list-directed input.
However much of this data could be read back in with list-directed
\s-1I/O\s0 on our system.
.Nh 2 Direct\ \s-1I/O\s0
A file connected for direct access consists of
a set of equal-sized records each of which is
uniquely identified by a positive integer.
The records may be written or read in any order,
using direct access \s-1I/O\s0 statements.
.IP
Direct access
.B read
and
.B write
statements
have an extra argument,
.B rec=,
which gives the record number to be read or written.
.DS
read(2, rec=13, err=20) (a(i), i=1, 203)
.DE
reads the thirteenth record into the array
.B a.
.IP
The size of the records must be given by an
.B open
statement
(see below).
Direct access files may be connected for either formatted
or unformatted \s-1I/O\s0.
.Nh 2 Internal\ Files
Internal files are character string objects,
such as variables or substrings,
or arrays of type character.
In the former cases there is only a single record
in the file;
in the latter case each array element is a record.
The Standard includes only sequential
formatted \s-1I/O\s0 on internal files.
(\s-1I/O\s0 is not a very precise term to use here,
but internal files are dealt with using
.B read
and
.B write.)
Internal files are used by giving the name of the
character object in place of the unit number, as in
.DS
character\(**80 x
read(5,\(fm(a)\(fm) x
read(x,\(fm(i3,i4)\(fm) n1,n2
.DE
which reads a character string into
.B x
and then reads
two integers from the front of it.
A sequential
.B read
or
.B write
always starts at the beginning
of an internal file.
.IP
We also support two extensions of the standard.  The first is
direct \s-1I/O\s0 on internal files.
This is like direct \s-1I/O\s0 on external files,
except that the number of records in the file cannot be
changed.
In this case a record is a single element of an array of character strings.
The second extension is list-directed \s-1I/O\s0 on internal files.
.Nh 2 OPEN,\ CLOSE,\ and\ INQUIRE\ Statements
These statements are used to connect and disconnect
units and files,
and to gather information about units and files.
.NH 3
OPEN
.IP
The
.B open
statement is used to connect a file with a
unit,
or to alter some properties of the connection.
The following is a minimal example.
.DS
open(1, file=\(fmfort.junk\(fm)
.DE
.B open
takes a variety of arguments with meanings described below.
.EQ
delim off
.EN
.	\" macros here
.de HP
.RT
.if !\\(IP .nr IP +1
.sp \\n(PDu
.ne 3v
.in +\\n(PIu
.ti -\\n(PIu
\fB\\$1\fR\ \c
..
.de P1
.KS
.nf
.in +.3i
.ta .3i .6i .9i 1.2i 1.5i 1.8i
.sp
..
.de P2
.fi
.in -.3i
.sp
.KE
..
.de TH
.RT
.sp \\n(PDu
.ne 3v
\fB\\$1\\$2\\$3\\$4\\$5\\$6\fR\ \c
..
.	\" end of macros
.RS
.HP unit=
an integer between 0 and 99 inclusive which is the unit to
which the file is to be connected (see section 5.3 in the text).
If this parameter is the first one in the
.B open
statement,
the
.B unit=
can be omitted.
.HP iostat=
is the same as in
.B read
or
.B write.
.HP err=
is the same as in
.B read
or
.B write.
.HP file=
a character expression,
which when stripped of trailing blanks,
is the name of the file to be connected to the unit.
The file name should not be given if the
.B status=\(fmscratch\(fm.
.HP status=
one of
.B \(fmold\(fm,
.B \(fmnew\(fm,
.B \(fmscratch\(fm,
or
.B \(fmunknown\(fm.
If this parameter is not given,\p
.B \(fmunknown\(fm
is assumed.
The meaning of
.B \(fmunknown\(fm
is processor dependent;
our system will create the file if it doesn't exist.
If
.B \(fmscratch\(fm
is given,
a temporary file will be created.
Temporary files are destroyed at the end of execution.
If
.B \(fmnew\(fm
is given, the file must not exist.
It will be created for both reading and writing.
If
.B \(fmold\(fm
is given, it is an error for the file not to exist.
.HP access=
.B \(fmsequential\(fm
or
.B \(fmdirect\(fm,
depending on whether the file is
to be opened for sequential or direct \s-1I/O\s0.
.HP form=
.B \(fmformatted\(fm
or
.B \(fmunformatted\(fm.
On
\s-1UNIX\s0
systems,
.B form=\(fmprint\(fm
implies
.B \(fmformatted\(fm
with vertical format control.
(See section 3.4 of the text).
.HP recl=
a positive integer specifying the record length of
the direct access file being opened.
We measure all record lengths in bytes.
On
\s-1UNIX\s0
systems a record length of 1 has the special meaning explained
in section 5.1 of the text.
.HP blank=
.B \(fmnull\(fm
or
.B \(fmzero\(fm.
This parameter has meaning only for formatted \s-1I/O\s0.
The default value is
.B \(fmnull\(fm.
.B \(fmzero\(fm
means that blanks,
other than leading blanks,
in numeric input fields are to be treated as zeros.
.RE
.IP
Opening a new file on a unit which is already connected
has the effect of first closing the old file.
.NH 3
CLOSE
.IP
.B close
severs the connection between a unit and a file.
The unit number must be given.
The optional parameters are
.B iostat=
and
.B err=
with
their usual meanings,
and
.B status=
either
.B \(fmkeep\(fm
or
.B \(fmdelete\(fm.
For scratch files the default is
.B \(fmdelete\(fm;
otherwise
.B \(fmkeep\(fm
is the default.
.B \(fmdelete\(fm
means the file will be removed.
A simple example is
.DS
close(3, err=17)
.DE
.NH 3
INQUIRE
.IP
The
.B inquire
statement gives information about
a unit
(``inquire by unit'')
or a file (``inquire by file'').
Simple examples are:
.DS
inquire(unit=3, name=xx)
inquire(file=\(fm\|junk\|\(fm, number=n, exist=l)
.DE
.RS
.HP file=
a character variable specifies the file the
.B inquire
is about.
Trailing blanks in the file name are ignored.
.HP unit=
an integer variable specifies the unit the
.B inquire
is about.
Exactly one of
.B file=
or
.B unit=
must be used.
.HP "iostat=, err="
are as before.
.HP exist=
a logical variable.
The logical variable is set to
.B ".true."
if the file or unit
exists and is set to
.B ".false."
otherwise.
.HP opened=
a logical variable.
The logical variable is set to
.B ".true."
if the file
is connected to a unit or if the unit is connected
to a file,
and it is set to
.B ".false."
otherwise.
.HP number=
an integer variable to which is assigned the
number of the unit connected to the file,
if any.
.HP named=
a logical variable to which is assigned
.B ".true."
if
the file has a name,
or
.B ".false."
otherwise.
.HP name=
a character variable to which is assigned the name
of the file (inquire by file) or the name of the
file connected to the unit (inquire by unit).
.HP access=
a character variable to which will be assigned
the value
.B \(fmsequential\(fm
if the connection is for
sequential \s-1I/O\s0,
.B \(fmdirect\(fm
if the connection is for direct \s-1I/O\s0,
.B \(fmunknown\(fm
if not connected.
.HP sequential=
a character variable to which is assigned the
value
.B \(fmyes\(fm
if the file could be connected for
sequential \s-1I/O\s0,
.B \(fmno\(fm
if the file could not be connected for sequential \s-1I/O\s0,
and
.B \(fmunknown\(fm
if we can't tell.
.HP direct=
a character variable to which is assigned the value
.B \(fmyes\(fm
if the file could be connected for direct \s-1I/O\s0,
.B \(fmno\(fm
if the file could not be connected for direct
\s-1I/O\s0, and
.B \(fmunknown\(fm
if we can't tell.
.HP form=
a character variable to which is assigned the value
.B \(fmunformatted\(fm
if the file is connected for unformatted \s-1I/O\s0,
.B \(fmformatted\(fm
if the file is connected for formatted \s-1I/O\s0,
.B \(fmprint\(fm
for formatted \s-1I/O\s0 with vertical format control, or
.B \(fmunknown\(fm
if not connected.
.HP formatted=
a character variable to which is assigned the value
.B \(fmyes\(fm
if the file could be connected for formatted \s-1I/O\s0,
.B \(fmno\(fm
if the file could not be connected for formatted \s-1I/O\s0,
and
.B \(fmunknown\(fm
if we can't tell.
.HP unformatted=
a character variable to which is assigned the value
.B \(fmyes\(fm
if
the file could be connected for unformatted \s-1I/O\s0,
.B \(fmno\(fm
if the file could not be connected for unformatted \s-1I/O\s0,
and
.B \(fmunknown\(fm
if we can't tell.
.HP recl=
an integer variable to which is assigned the record length
of the records in the file if the file is connected
for direct access.
.HP nextrec=
an integer variable to which is assigned one more
than the number of the the last record read from a file connected
for direct access.
.HP blank=
a character variable to which is assigned the value
.B \(fmnull\(fm
if null blank control is in effect for the file
connected for formatted \s-1I/O\s0,
.B \(fmzero\(fm
if blanks are being converted to zeros and
the file is connected for formatted \s-1I/O\s0.
.RE
.IP
For information on file permissions, ownership, etc.,
use the Fortran library routines \fBstat\fP and \fBaccess\fP.
.IP
For further discussion of the \s-1UNIX\s0 Fortran \s-1I/O\s0 system
see ``Introduction to the f77 I/O Library'' [9].
.bp
.SH
APPENDIX B:  References and Bibliography
.XS
Appendix B.  References and Bibliography
.XE
.LP
.sp
.B
References
.R
.IP 1. 3
\f2American National Standard Programming Language \s-1FORTRAN\s0,
\s-1ANSI\s0 X3.9-1978\f1.
New York:  American National Standards Institute, 1978.
.IP 2.
\f2\s-1USA\s0 Standard \s-1FORTRAN\s0, \s-1USAS X\s03.9-1966\f1.
New York:  United States of America Standards Institute, 1966.
Clarified in \f2Comm. \s-1ACM\s0\f1 12:289 (1969)
and \f2Comm. \s-1ACM\s0\f1 14:628 (1971).
.IP 3.
Kernighan, B. W., and D. M. Ritchie.  \f2The C Programming Language.\f1
Englewood Cliffs:  Prentice-Hall, 1978.
.IP 4.
Ritchie, D. M.  Private communication.
.IP 5.
Johnson, S. C.  ``A Portable Compiler:  Theory and Practice,''
\f2Proceedings of Fifth \s-1ACM\s0 Symposium on
Principles of Programming Languages\f1.  1978.
.IP 6.
Feldman, S. I.  ``An Informal Description of \s-1EFL\s0,''
internal memorandum.
.IP 7.
Kernighan, B. W.  ``\s-1RATFOR\s0\(emA Preprocessor for
Rational Fortran,'' \f2Bell Laboratories Computing Science
Technical Report #55\f1.  1977.
.IP 8.
Ritchie, D. M.  Private communication.
.IP 9.
Wasley, D. L. ``Introduction to the f77 I/O Library'',
\fI\s-1UNIX\s0 Programmer's Manual, Volume 2c\fR.
.sp
.LP
.B
Bibliography
.R
.LP
The following books or documents describe aspects of Fortran 77.
This list cannot pretend to be complete.
Certainly no particular endorsement is implied.
.IP 1. 3
Brainerd, Walter S., et al.  \f2Fortran 77 Programming.\f1
Harper Row, 1978.
.IP 2.
Day, A. C.  \f2Compatible Fortran.\f1  Cambridge University Press, 1979.
.IP 3.
Dock, V. Thomas.  \f2Structured Fortran IV Programming.\f1  West, 1979.
.IP 4.
Feldman, S. I.  ``The Programming Language \s-1EFL\s0,''
\f2Bell Laboratories Technical Report\f1.
June 1979.
.IP 5.
Hume, J. N., and R. C. Holt.  \f2Programming Fortran 77.\f1
Reston, 1979.
.IP 6.
Katzan, Harry, Jr.  \f2Fortran 77.\f1  Van Nostrand-Reinhold, 1978.
.IP 7.
Meissner, Loren P., and Organick, Elliott I.  \f2Fortran 77 Featuring
Structured Programming\f1, Addison-Wesley, 1979.
.IP 8.
Merchant, Michael J.  \f2\s-1ABC\s0's of Fortran Programming.\f1
Wadsworth, 1979.
.IP 9.
Page, Rex, and Richard Didday.  \f2Fortran 77 for Humans.\f1
West, 1980.
.IP 10.
Wagener, Jerrold L.  \f2Principles of Fortran 77 Programming.\f1
Wiley, 1980.
.\" want Table of Contents to begin on page 2 hence must expand body
.\" of .TC macro to enable us to get handle on page number.
.pn 2
.bp
.PX
