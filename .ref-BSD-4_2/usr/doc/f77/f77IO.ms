.de Fo
\s-1FORTRAN\s0\\$1
..
.de Sm
\s-1\\$1\s0
..
.ND "April 1983"
.\" .nr ll 7.0i
.\" .nr LL 7.0i
.\" .po 0.0i
.\" .rm PT
.\" .rm BT
.RP
.TL
Introduction to the f77 I/O Library
.AU
David L. Wasley
.AI
University of California, Berkeley
Berkeley, California 94720
.AB
.LP
The f77 I/O Library implements
.Sm ANSI
1978
.Fo
standard
input and output
with a few minor exceptions.
Where the standard is vague, we have tried to provide flexibility
within the constraints of the
.UX
operating system.
The I/O Library was written originally by Peter J. Weinberger at Bell Labs.
A number of logical extensions and enhancements have been provided
by this author.
.AE
.PP
The f77 I/O library, libI77.a,
includes routines to perform all of the standard types of
.Fo
input and output.
Several enhancements and extensions to
.Fo
I/O have been added.
The f77 library routines use the C stdio library routines to provide
efficient buffering for file I/O.
.sp 1
.NH 1
FORTRAN I/O
.PP
The requirements of the
.Sm ANSI
standard impose significant overhead
on programs that do large amounts of I/O. Formatted I/O can be
very ``expensive'' while direct access binary I/O is usually very efficient.
Because of the complexity of
.Fo
I/O,
some general concepts deserve clarification.
.NH 2
Types of I/O
.PP
There are three forms of I/O:
.B formatted,
.B unformatted,
and
.B list-directed.
The last is
related to formatted but does not obey all the rules for formatted I/O.
There are two modes of access to
.B external
and
.B internal
files:
.B direct
and
.B sequential.
The definition of a logical record depends upon the
combination of I/O form and mode specified by the
.Fo
I/O statement.
.NH 3
Direct access
.PP
A logical record in a
.B direct
access
.B external
file is a string of bytes
of a length specified when the file is opened.
Read and write statements must not specify logical records longer than
the original record size definition. Shorter logical records are allowed.
.B Unformatted
direct writes leave the unfilled part of the record undefined.
.B Formatted
direct writes cause the unfilled record to be padded with blanks.
.NH 3
Sequential access
.PP
Logical records in
.B sequentially
accessed
.B external
files may be of arbitrary
and variable length.
Logical record length for
.B unformatted
sequential files is determined by
the size of items in the iolist.
The requirements of this form of I/O cause the external physical
record size to be somewhat larger than the logical record size.
For
.B formatted
write statements, logical record length is determined by
the format statement interacting with the iolist at execution time.
The ``newline'' character is the logical record delimiter.
Formatted sequential access causes one or more logical records
ending with ``newline'' characters to be read or written.
.NH 3
List directed I/O
.PP
Logical record length for
.B list-directed
I/O is relatively meaningless.
On output, the record length is dependent on the magnitude of the
data items.
On input, the record length is determined by the data types and the file
contents.
.NH 3
Internal I/O
.PP
The logical record length for an
.B internal
read or write is the length of the
character variable or array element. Thus a simple character variable
is a single logical record. A character variable array is similar to
a fixed length direct access file, and obeys the same rules.
.B Unformatted
I/O is not allowed on "internal" files.
.NH 2
I/O execution
.PP
Note that each execution of a
.Fo
.B unformatted
I/O statement causes a single
logical record to be read or written. Each execution of a
.Fo
.B formatted
I/O statement causes one or more logical records to be read or written.
.PP
A slash, ``/'', will terminate assignment of
values to the input list during
.B list-directed
input and the remainder of the current input line is skipped.
The standard is rather vague on this point but seems to require that
a new external logical record be found at the start of any formatted
input. Therefore data following the slash is ignored and may be used
to comment the data file.
.PP
.B "Direct access list-directed"
I/O is not allowed.
.B "Unformatted internal"
I/O is not allowed.
Both the above will be caught by the compiler.
All other flavors of I/O are allowed, although some are not part of the
.Sm ANSI
standard.
.PP
Any error detected during I/O processing will cause the program to abort
unless alternative action has been provided specifically in the program.
Any I/O statement may include an
.B err=
clause (and
.B iostat=
clause)
to specify an
alternative branch to be taken on errors (and return the specific error code).
Read statements may include
.B end=
to branch on end-of-file.
File position and the value of I/O list items is undefined following an error.
.sp 1
.NH 1
Implementation details
.PP
Some details of the current implementation may be useful in understanding
constraints on
.Fo
I/O.
.NH 2
Number of logical units
.PP
The maximum number of logical units that a program may have open at one
time is the same as the
.UX
system limit, currently 20.
Unit numbers must be in the range 0 \- 19 because they are used to
index an internal control table.
.NH 2
Standard logical units
.PP
By default, logical units 0, 5, and 6
are opened to ``stderr'', ``stdin'', and ``stdout'' respectively.
However they can be re-defined with an 
.B open
statement.
To preserve error reporting, it is an error to close logical unit 0
although it may be reopened to another file.
.PP
If you want to open the default file name for any preconnected logical unit,
remember to 
.B close
the unit first.
Redefining the standard units may impair normal console I/O.
An alternative is to
use shell re-direction to externally re-define the above units.
To re-define default blank control or format of the standard input or output
files, use the 
.B open
statement specifying the unit number and no
file name (see \(sc\|2.4).
.PP
The standard units, 0, 5, and 6, are named internally ``stderr'', ``stdin'',
and ``stdout'' respectively.
These are not actual file names and can not be used for opening these units.
.B Inquire
will not return these names and will indicate
that the above units are not named unless they have been opened to real files.
The names are meant to make error reporting more meaningful.
.NH 2
Vertical format control
.PP
Simple vertical format control is implemented. The logical unit must be opened
for sequential access with
.B "form = 'print'"
(see \(sc\|3.2).
Control codes ``0'' and ``1'' are replaced in the output file
with ``\\n'' and ``\\f'' respectively.
The control character ``+'' is not implemented and, like
any other character in the first position of a record
written to a ``print'' file, is dropped.
No vertical format control is recognized for
.B "direct formatted"
output
or
.B "list directed"
output.
.NH 2
The open statement
.PP
An 
.B open
statement need not specify a file name. If it refers to a logical
unit that is already open, the 
.B blank=
and 
.B form=
specifiers may be
redefined without affecting the current file position.
Otherwise, if
.B "status = 'scratch'"
is specified, a temporary file with a
name of the form ``tmp.FXXXXXX'' will be opened,
and, by default, will be deleted when closed or during
termination of program execution.
Any other 
.B status=
specifier without an associated file name results in
opening a file named ``fort.N'' where N is the specified logical unit number.
.PP
It is an error to try to open an existing file with
.B "status = 'new'"
\&.
It is an error to try to open a nonexistent file with
.B "status = 'old'"
\&.
By default,
.B "status = 'unknown'"
will be assumed, and a file will be created if necessary.
.PP
By default, files are positioned
at their beginning upon opening, but see \fIioinit\fP(3f) for alternatives.
Existing files are never truncated on opening.
Sequentially accessed external files are truncated to the current file
position on
.B close
\&,
.B backspace
\&, or
.B rewind
only if the last
access to the file was a write.
An
.B endfile
always causes such files to be truncated to the current
file position.
.NH 2
Format interpretation
.PP
Formats are parsed at the beginning of each execution of a formatted
I/O statement.
Upper as well as lower case characters are recognized in format statements
and all the alphabetic arguments to the I/O library routines.
.PP
If the external representation of a datum
is too large for the field width specified, the specified
field is filled with asterisks (\(**).
On \fBE\fPw.d\fBE\fPe output,
the exponent field will be filled with asterisks if the
exponent representation is too large.
This will only happen if ``e'' is zero (see appendix B).
.PP
On output, a real value that is truly zero will display as ``0.'' to
distinguish it from a very small non-zero value.
This occurs in
.B F
and
.B G
format conversions.
This was not done for
.B E
and
.B D
since the embedded blanks in the external
datum causes problems for other input systems.
.PP
Non-destructive tabbing is implemented for both internal and external
formatted I/O.
Tabbing left or right on output
does not affect previously written portions of a record.
Tabbing right on output
causes unwritten portions of a record to be filled with blanks.
Tabbing right off the end of an input logical record is an error.
Tabbing left beyond the beginning of an input logical record leaves
the input pointer at the beginning of the record.
The format specifier
.B T
must be followed by a positive non-zero number.
If it is not, it will have a different meaning (see \(sc\|3.1).
.PP
Tabbing left requires seek ability on the logical unit.
Therefore it is not allowed in I/O to a terminal or pipe.
Likewise, nondestructive tabbing in either direction is possible
only on a unit that can seek. Otherwise tabbing right or spacing with
.B X
will write blanks on the output.
.NH 2
List directed output
.PP
In formatting list directed output, the I/O system tries to prevent
output lines longer than 80 characters.
Each external datum will be separated by two spaces.
List-directed output of
.B complex
values includes an appropriate comma.
List-directed output distinguishes between
.B real
and
.B "double precision"
values
and formats them differently.
Output of a character string that includes ``\\n''
is interpreted reasonably by the output system.
.NH 2
I/O errors
.PP
If I/O errors are not trapped by the user's program an appropriate
error message will be written to ``stderr'' before aborting.
An error number will be printed in [ ] along with a brief error message
showing the logical unit and I/O state.
Error numbers < 100 refer to
.UX
errors, and are described in the
introduction to chapter 2 of the
.UX
Programmer's Manual.
Error numbers \(>= 100 come from the I/O library, and are described
further in the appendix to this writeup.
For internal I/O, part of the string will be printed with ``|'' at the
current position in the string.
For external I/O, part of the current record will be displayed if
the error was caused during reading from a file that can backspace.
.sp 1
.NH 1
Non-``ANSI Standard'' extensions
.PP
Several extensions have been added to the I/O system to provide
for functions omitted or poorly defined in the standard.
Programmers should be aware that these are non-portable.
.NH 2
Format specifiers
.PP
.B B
is an acceptable edit control specifier. It causes return to the
default mode of blank interpretation.
This is consistent with
.B S
which returns to default sign control.
.PP
.B P
by itself is equivalent to
.B 0P
\&. It resets the scale factor to the
default value, 0.
.PP
The form of the \fBE\fPw.d\fBE\fPe format specifier has been extended to
.B D
also.
The form \fBE\fPw.d.e is allowed but is not standard.
The ``e'' field specifies the minimum number of digits or spaces in the
exponent field on output.
If the value of the exponent is too large, the exponent notation
.B e
or
.B d
will be dropped from the output to allow one
more character position.
If this is still not adequate, the ``e'' field will be filled with
asterisks (\(**). The default value for ``e'' is 2.
.PP
An additional form of tab control specification has been added.
The
.Sm ANSI
standard forms \fBTR\fPn, \fBTL\fPn, and \fBT\fPn are supported
where
.I n
is a positive non-zero number.
If
.B T
or n\fBT\fP is specified, tabbing will
be to the next (or n-th) 8-column tab stop.
Thus columns of alphanumerics can be lined up without counting.
.PP
A format control specifier has been added to suppress the newline
at the end of the last record of a formatted sequential write. The
specifier is a dollar sign ($). It is constrained by the same rules
as the colon (:). It is used typically for console prompts.
For example:

.DS
write (\(**, "('enter value for x: ',$)")
read (\(**,\(**) x
.DE
.PP
Radices other than 10 can be specified for formatted integer I/O
conversion. The specifier is patterned after
.B P,
the scale factor for
floating point conversion. It remains in effect until another radix is
specified or format interpretation is complete. The specifier is defined
as [n]\fBR\fP where 2 \(<= \fIn\fP \(<= 36. If
.I n
is omitted,
the default decimal radix is restored.
.PP
In conjunction with the above, a sign control specifier has been added
to cause integer values to be interpreted as unsigned during output
conversion. The specifier is
.B SU
and remains in effect until another
sign control specifier is encountered, or format interpretation is
complete. Radix and ``unsigned'' specifiers could be used to format
a hexadecimal dump, as follows:

.DS
2000  format ( SU, 16R, 8I10.8 )
.DE

Note: Unsigned integer values greater than (2\(**\(**30 - 1),
i.e. any signed negative value, can not be read by
.Fo
input routines.
All internal values will be output correctly.
.NH 2
Print files
.PP
The
.Sm ANSI
standard is ambiguous regarding the definition of a ``print'' file.
Since
.UX
has no default ``print'' file, an additional
.B form=
specifier
is now recognized in the
.B open
statement.
Specifying
.B "form = 'print'"
implies
.B formatted
and enables vertical format
control for that logical unit.
Vertical format control is interpreted only on sequential formatted writes
to a ``print'' file.
.PP
The
.B inquire
statement will return
.B print
in the
.B form=
string variable
for logical units opened as ``print'' files.
It will return -1 for the unit number of an unconnected file.
.PP
If a logical unit is already open, an
.B open
statement including the
.B form=
option or the
.B blank=
option will do nothing but re-define those options.
This instance of the
.B open
statement need not include the file name, and
must not include a file name if
.B unit=
refers to a standard input or output.
Therefore, to re-define the standard output as a ``print'' file, use:

.DS
open (unit=6, form='print')
.DE
.NH 2
Scratch files
.PP
A
.B close
statement with
.B "status = 'keep'"
may be specified for temporary files.
This is the default for all other files.
Remember to get the scratch file's real name,
using
.B inquire
\&, if you want to re-open it later.
.NH 2
List directed I/O
.PP
List directed read has been modified to allow input of a string not enclosed
in quotes. The string must not start with a digit, and can not contain a
separator (, or /) or blank (space or tab). A newline will terminate the
string unless escaped with \\. Any string not meeting the above restrictions
must be enclosed in quotes (" or ').
.PP
Internal list-directed I/O has been implemented. During internal list reads,
bytes are consumed until the iolist is satisfied, or the 'end-of-file'
is reached.
During internal list writes, records are filled until the iolist is satisfied.
The length of an internal array element should be at least 20 bytes to
avoid logical record overflow when writing double precision values.
Internal list read was implemented to make command line decoding easier.
Internal list write should be avoided.
.sp 1
.NH 1
Running older programs
.PP
Traditional
.Fo
environments usually assume carriage control on all logical units,
usually interpret blank spaces on input as ``0''s, and often provide
attachment of global file names to logical units at run time.
There are several routines in the I/O library to provide these functions.
.NH 2
Traditional unit control parameters
.PP
If a program reads and writes only units 5 and 6, then including
.B \-lI66
in the f77 command will cause carriage control to be interpreted on
output and cause blanks to be zeros on input without further
modification of the program.
If this is not adequate,
the routine \fIioinit\fP(3f) can be called to specify control parameters
separately, including whether files should be positioned at their
beginning or end upon opening.
.NH 2
Preattachment of logical units
.PP
The \fIioinit\fP routine also can be used to attach logical units
to specific files at run time.
It will look for names of a user specified form in the environment
and open the corresponding logical unit for
.B "sequential formatted"
I/O. Names must be of the form \fB\s-1PREFIX\s0\fP\fInn\fP where
.B \\s-1PREFIX\\s0
is specified in the call to
.I ioinit
and
.I nn
is the logical unit to be opened. Unit numbers < 10 must include
the leading ``0''.
.PP
.I Ioinit
should prove adequate for most programs as written.
However, it
is written in
.Fo \-77
specifically so that it may serve as an example for similar
user-supplied routines.
A copy may be retrieved by ``ar x /usr/lib/libI77.a ioinit.f''.
.sp 1
.NH 1
Magnetic tape I/O
.PP
Because the I/O library uses stdio buffering, reading or writing
magnetic tapes should be done with great caution, or avoided if possible.
A set of routines has been provided to read and write arbitrary sized buffers
to or from tape directly. The buffer must be a
.B character
object.
.B Internal
I/O can be used to fill or interpret the buffer.
These routines do not use normal
.Fo
I/O processing and do not obey
.Fo
I/O rules.
See \fItapeio\fP(3f).
.sp 1
.NH 1
Caveat Programmer
.PP
The I/O library is extremely complex yet we believe there are few bugs left.
We've tried to make the system as correct as possible according to
the
.Sm ANSI
X3.9\-1978 document and keep it compatible with the
.UX
file system.
Exceptions to the standard are noted in appendix B.
.bp
.ce 3
Appendix A

I/O Library Error Messages
.sp 1
.PP
The following error messages are generated by the I/O library.
The error numbers are returned in the
.B iostat=
variable if the
.B err=
return is taken. Error numbers < 100 are generated by the
.UX
kernel.
See the introduction to chapter 2 of the
.UX
Programmers Manual for their description.
.DS
/* 100 */	"error in format"
		See error message output for the location
		of the error in the format. Can be caused
		by more than 10 levels of nested (), or
		an extremely long format statement.

/* 101 */	"illegal unit number"
		It is illegal to close logical unit 0.
		Negative unit numbers are not allowed.
		The upper limit is system dependent.

/* 102 */	"formatted io not allowed"
		The logical unit was opened for
		unformatted I/O.

/* 103 */	"unformatted io not allowed"
		The logical unit was opened for
		formatted I/O.

/* 104 */	"direct io not allowed"
		The logical unit was opened for sequential
		access, or the logical record length was
		specified as 0.

/* 105 */	"sequential io not allowed"
		The logical unit was opened for direct
		access I/O.

/* 106 */	"can't backspace file"
		The file associated with the logical unit
		can't seek. May be a device or a pipe.

/* 107 */	"off beginning of record"
		The format specified a left tab beyond the
		beginning of an internal input record.

/* 108 */	"can't stat file"
		The system can't return status information
		about the file. Perhaps the directory is
		unreadable.

/* 109 */	"no * after repeat count"
		Repeat counts in list-directed I/O must be
		followed by an * with no blank spaces.

.DE
.DS
/* 110 */	"off end of record"
		A formatted write tried to go beyond the
		logical end-of-record. An unformatted read
		or write will also cause this.

/* 111 */	"truncation failed"
		The truncation of an external sequential file on
		'close', 'backspace', 'rewind' or 'endfile' failed.

/* 112 */	"incomprehensible list input"
		List input has to be just right.

/* 113 */	"out of free space"
		The library dynamically creates buffers for
		internal use. You ran out of memory for this.
		Your program is too big!

/* 114 */	"unit not connected"
		The logical unit was not open.

/* 115 */	"read unexpected character"
		Certain format conversions can't tolerate
		non-numeric data. Logical data must be
		T or F.

/* 116 */	"blank logical input field"

/* 117 */	"'new' file exists"
		You tried to open an existing file with
		"status='new'".

/* 118 */	"can't find 'old' file"
		You tried to open a non-existent file
		with "status='old'".

/* 119 */	"unknown system error"
		Shouldn't happen, but .....

/* 120 */	"requires seek ability"
		Direct access requires seek ability.
		Sequential unformatted I/O requires seek
		ability on the file due to the special
		data structure required. Tabbing left
		also requires seek ability.

/* 121 */	"illegal argument"
		Certain arguments to 'open', etc. will be
		checked for legitimacy. Often only non-
		default forms are looked for.
.DE
.DS
/* 122 */	"negative repeat count"
		The repeat count for list directed input
		must be a positive integer.

/* 123 */	"illegal operation for unit"
		An operation was requested for a device
		associated with the logical unit which
		was not possible. This error is returned
		by the tape I/O routines if attempting to
		read past end-of-tape, etc.
.DE
.bp
.ce 3
Appendix B

Exceptions to the ANSI Standard
.sp 1
.PP
A few exceptions to the
.Sm ANSI
standard remain.
.LP
1) Vertical format control
.PP
The ``+'' carriage control specifier is not implemented.
It would be difficult to implement it correctly and still
provide
.UX -like
file I/O.
.PP
Furthermore, the carriage control implementation is asymmetrical.
A file written with carriage control interpretation can not be
read again with the same characters in column 1.
.PP
An alternative to interpreting carriage control internally is to
run the output file through a ``\s-1FORTRAN\s0 output filter''
before printing. This filter could recognize a much broader range
of carriage control and include terminal dependent processing.
.sp 1
.LP
2) Default files
.PP
Files created by default use of
.B rewind
or
.B endfile
statements are opened for
.B "sequential formatted"
access. There is no way to redefine such a file to allow
.B direct
or
.B unformatted
access.
.sp 1
.LP
3) Lower case strings
.PP
It is not clear if the
.Sm ANSI
standard requires internally generated strings to be upper case or not.
As currently written, the
.B inquire
statement will return lower case strings for any alphanumeric data.
.sp 1
.LP
4) Exponent representation on Ew.dEe output
.PP
If the field width for the exponent is too small, the standard
allows dropping the exponent character but only if the exponent
is > 99. This system does not enforce that restriction.
Further, the standard implies that the entire field, `w', should be
filled with asterisks if the exponent can not be displayed.
This system fills only the exponent field in the above case since
that is more diagnostic.
