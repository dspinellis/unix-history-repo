.de s
.sp .5v
..
.de sr
.ft I
.ne 2
\\$1
.if t .sp .2
.br
.ft R
..
.de it
\fI\\$1\fR
..
.TL
A New Input-Output Package
.AU
D. M. Ritchie
.AI
.MH
.PP
A new package of IO routines is available.
It was designed with the following goals in mind.
.IP 1.
It should be similar in spirit to the earlier Portable
Library, and, to the extent possible, be compatible with it.
At the same time a few dubious design choices
in the Portable Library will be corrected.
.IP 2.
It must be as efficient as possible, both in time and in space,
so that there will be no hesitation in using it
no matter how critical the application.
.IP 3.
It must be simple to use, and also free of the magic
numbers and mysterious calls the use
of which mars the understandability and portability
of many programs using older packages.
.IP 4.
The interface provided should be applicable on all machines,
whether or not the programs which implement it are directly portable
to other systems,
or to machines other than the PDP11 running a version of Unix.
.PP
It is intended that this package replace the Portable Library.
Although it is not directly compatible, as discussed below,
it is sufficiently similar that
modifying programs to use it should be a simple exercise.
.PP
The most crucial difference between this package and the Portable
Library is that the current offering names streams in terms
of pointers rather than by
the integers known as `file descriptors.'
Thus, for example, the routine which opens a named file
returns a pointer to a certain structure rather than a number;
the routine which reads an open file
takes as an argument the pointer returned from the open call.
.SH
General Usage
.PP
Each program using the library must have the line
.DS
		#include <stdio.h>
.DE
which defines certain macros and variables.
The library containing the routines is `/usr/lib/libS.a,'
so the command to compile is
.DS
		cc  . . .  \-lS
.DE
All names in the include file intended only for internal use begin
with an underscore `\_' to reduce the possibility
of collision with a user name.
The names intended to be visible outside the package are
.IP stdin 10
The name of the standard input file
.IP stdout 10
The name of the standard output file
.IP stderr 10
The name of the standard error file
.IP EOF 10
is actually \-1, and is the value returned by
the read routines on end-of-file or error.
.IP NULL 10
is a notation for the null pointer, returned by
pointer-valued functions
to indicate an error
.IP FILE 10
expands to `struct \_iob' and is a useful
shorthand when declaring pointers
to streams.
.IP BUFSIZ
is a number (viz. 512)
of the size suitable for an IO buffer supplied by the user.
See
.it setbuf,
below.
.IP "getc, getchar, putc, putchar, feof, ferror, fileno" 10

.br
are defined as macros.
Their actions are described below;
they are mentioned here
to point out that it is not possible to
redeclare them
and that they are not actually functions;
thus, for example, they may not have breakpoints set on them.
.PP
The routines in this package, like the Portable
Library,
offer the convenience of automatic buffer allocation
and output flushing where appropriate.
Absent, however, is the facility
of changing the default input and output streams
by assigning to `cin' and `cout.'
The names `stdin,' stdout,' and `stderr'
are in effect constants and may not be assigned to.
.SH
Calls
.PP
The routines in the library are in nearly one-to-one
correspondence with those in the Portable Library.
In several cases the name has been changed.
This is an attempt to reduce confusion.
.s
.sr "FILE *fopen(filename, type) char *filename, *type"
.it Fopen
opens the file and, if needed, allocates a buffer for it.
.it Filename
is a character string specifying the name.
.it Type
is a character string (not a single character).
It may be `"r",' `"w",' or `"a"' to indicate
intent to read, write, or append.
The value returned is a file pointer.
If it is NULL the attempt to open failed.
.s
.sr "FILE *freopen(filename, type, ioptr) char *filename, *type; FILE *ioptr
The stream named by
.it ioptr
is closed, if necessary, and then reopened
as if by
.it fopen.
If the attempt to open fails, NULL is returned,
otherwise
.it ioptr,
which will now refer to the new file.
Often the reopened stream is
.it stdin
or
.it stdout.
.s
.sr "int getc(ioptr) FILE *ioptr
returns the next character from the stream named by
.it ioptr,
which is a pointer to a file such as returned by
.it fopen,
or the name
.it stdin.
The integer EOF is returned on end-of-file or when
an error occurs.
The null character \(aa\e0\(aa is a legal character.
.s
.sr "int fgetc(ioptr) FILE *ioptr
acts like
.it getc
but is a genuine function,
not a macro.
.s
.sr "putc(c, ioptr) FILE *ioptr
.it Putc
writes the character
.it c
on the output stream named by
.it ioptr,
which is a value returned from
.it fopen
or perhaps
.it stdout
or
.it stderr.
The character is returned as value,
but EOF is returned on error.
.s
.sr "fputc(c, ioptr) FILE *ioptr
.it Fputc
acts like
.it putc
but is a genuine
function, not a macro.
.s
.sr "fclose(ioptr) FILE *ioptr
The file corresponding to
.it ioptr
is closed after any buffers are emptied.
A buffer allocated by the IO system is freed.
.it Fclose
is automatic on normal termination of the program.
.s
.sr "fflush(ioptr) FILE *ioptr
Any buffered information on the (output) stream named by
.it ioptr
is written out.
Output files are normally buffered
if and only if they are not directed to the terminal,
but
.it stderr
is unbuffered unless
.it setbuf
is used.
.s
.sr exit(errcode)
.it Exit
terminates the process and returns its argument as status
to the parent.
This is a special version of the routine
which calls
.it fflush
for each output file.
To terminate without flushing,
use
.it \_exit.
.s
.sr "feof(ioptr) FILE *ioptr
returns non-zero when end-of-file
has occurred on the specified input stream.
.s
.sr "ferror(ioptr) FILE *ioptr
returns non-zero when an error has occurred while reading
or writing the named stream.
The error indication lasts until the file has been closed.
.s
.sr "getchar( )"
is identical to
.it "getc(stdin).
.s
.sr "putchar(c)"
is identical to
.it "putc(c, stdout).
.s
.sr "char *gets(s) char *s
reads characters up to a new-line from the standard input.
The new-line character is replaced by a null character.
It is the user's responsibility to make sure that the character array
.it s
is large enough.
.it Gets
returns its argument, or NULL if end-of-file or error occurred.
Note that this routine is not compatible with
.it fgets;
it is included for downward compatibility.
.s
.sr "char *fgets(s, n, ioptr) char *s; FILE *ioptr
reads up to
.it n
characters from the stream
.it ioptr
into the character pointer
.it s.
The read terminates with a new-line character.
The new-line character is placed in the buffer
followed by a null character.
The first argument,
or NULL if error or end-of-file occurred,
is returned.
.s
.sr "puts(s) char *s
writes the null-terminated string (character array)
.it s
on the standard output.
A new-line is appended.
No value is returned.
Note that this routine
is not compatible with
.it fputs;
it is included for downward compatibility.
.s
.sr "*fputs(s, ioptr) char *s; FILE *ioptr
writes the null-terminated string (character array)
.it s
on the stream
.it ioptr.
No new-line is appended.
No value is returned.
.s
.sr "ungetc(c, ioptr) FILE *ioptr
The argument character
.it c
is pushed back on the input stream named by
.it ioptr.
Only one character may be pushed back.
.s
.sr "printf(format, a1, . . .) char *format
.sr "fprintf(ioptr, format, a1, . . .) FILE *ioptr; char *format
.sr "sprintf(s, format, a1, . . .)char *s, *format
.it Printf
writes on the standard output.
.it Fprintf
writes on the named output stream.
.it Sprintf
puts characters in the character array (string)
named by
.it s.
The specifications are as described in section
.it "printf
(III)
of the Unix Programmer's Manual.
There is a new conversion:
.it %m.n\fB\|g\fI
converts a double argument in the style of
.it e
or
.it f
as most appropriate.
.s
.sr "scanf(format, a1, . . .) char *format
.sr "fscanf(ioptr, format, a1, . . .) FILE *ioptr; char *format
.sr "sscanf(s, format, a1, . . .) char *s, *format
.it Scanf
reads from the standard input.
.it Fscanf
reads from the named input stream.
.it Sscanf
reads from the character string
supplied as
.it s.
The specifications are identical
to those of the Portable Library.
.it Scanf
reads characters, interprets
them according to a format, and stores the results in its arguments.
It expects as arguments
a control string
.it format,
described below,
and a set of arguments,
.I
each of which must be a pointer,
.R
indicating where the converted input should be stored.
.PP
The
control string
usually contains
conversion specifications, which are used to direct interpretation
of input sequences.
The control string may contain:
.IP 1.
Blanks, tabs or newlines, which are ignored.
.IP 2.
Ordinary characters (not %) which are expected to match
the next non-space character of the input stream
(where space characters are defined as blank, tab or newline).
.IP 3.
Conversion specifications, consisting of the
character %, an optional assignment suppressing character \**,
an optional numerical maximum field width, and a conversion
character.
.PP
A conversion specification is used to direct the conversion of the
next input field; the result
is placed in the variable pointed to by the corresponding argument,
unless assignment suppression was
indicated by the \** character.
An input field is defined as a string of non-space characters;
it extends either to the next space character or until the field
width, if specified, is exhausted.
.PP
The conversion character indicates the interpretation of the
input field; the corresponding pointer argument must
usually be of a restricted type.
The following conversion characters are legal:
.IP %
indicates that a single % character is expected
in the input stream at this point;
no assignment is done.
.IP d
indicates that a decimal integer is expected
in the input stream;
the corresponding argument should be an integer pointer.
.IP o
indicates that an octal integer is expected in the
input stream; the corresponding argument should be a integer pointer.
.IP x
indicates that a hexadecimal integer is expected in the input stream;
the corresponding argument should be an integer pointer.
.ti -0.2i
.IP s
indicates that a character string is expected;
the corresponding argument should be a character pointer
pointing to an array of characters large enough to accept the
string and a terminating `\e0', which will be added.
The input field is terminated by a space character
or a newline.
.IP c
indicates that a character is expected; the
corresponding argument should be a character pointer;
the next input character is placed at the indicated spot.
The normal skip over space characters is suppressed
in this case;
to read the next non-space character, try
.I
%1s.
.R
If a field width is given, the corresponding argument
should refer to a character array, and the
indicated number of characters is read.
.IP e
(or
.I f\|\fR)
.R
indicates that a floating point number is expected in the input stream;
the next field is converted accordingly and stored through the
corresponding argument, which should be a pointer to a
.it float.
The input format for
floating point numbers is
an optionally signed
string of digits
possibly containing a decimal point, followed by an optional
exponent field beginning with an E or e followed by an optionally signed integer.
.IP [
indicates a string not to be delimited by space characters.
The left bracket is followed by a set of characters and a right
bracket; the characters between the brackets define a set
of characters making up the string.
If the first character
is not circumflex (\|^\|), the input field
is all characters until the first character not in the set between
the brackets; if the first character
after the left bracket is ^, the input field is all characters
until the first character which is in the remaining set of characters
between the brackets.
The corresponding argument must point to a character array.
.PP
The conversion characters
.I
d, o
.R
and
.I
x
.R
may be capitalized or preceded
by
.I
l
.R
to indicate that a pointer to
.I
long
.R
rather than
.I
int
.R
is expected.
Similarly, the conversion characters
.I
e
.R
or
.I
f
.R
may be capitalized or
preceded by
.I
l
.R
to indicate that a pointer to 
.I
double
.R
rather than 
.I
float
.R
is in the argument list.
The character
.I
h
.R
will function similarly in the future to indicate
.I
short
.R
data items.
.PP
For example, the call
.DS
int i; float x; char name[50];
scanf( "%d%f%s", &i, &x, name);
.DE
with the input line
.DS
25   54.32E\(mi1  thompson
.DE
will assign to
.it i
the value
25,
.it x
the value 5.432, and
.it name
will contain
.it ``thompson\e0''.
Or,
.DS
int i; float x; char name[50];
scanf("%2d%f%\**d%[1234567890]", &i, &x, name);
.DE
with input
.DS
56789 0123 56a72
.DE
will assign 56 to
.it i,
789.0 to
.it x,
skip ``0123'',
and place the string ``56\e0'' in
.it name.
The next call to
.it getchar
will return `a'.
.PP
.it Scanf
returns as its value the number of successfully matched and assigned input
items.
This can be used to decide how many input items were found.
On end of file, EOF is returned; note that this is different
from 0, which means that the next input character does not
match what was called for in the control string.
.s
.sr "fread(ptr, sizeof(*ptr), nitems, ioptr) FILE *ioptr
reads
.it nitems
of data beginning at
.it ptr
from file
.it ioptr.
It behaves identically to the Portable Library's
.it cread.
No advance notification
that binary IO is being done is required;
when, for portability reasons,
it becomes required, it will be done
by adding an additional character to the mode-string on the
fopen call.
.s
.sr "fwrite(ptr, sizeof(*ptr), nitems, ioptr) FILE *ioptr
Like
.it fread,
but in the other direction.
.s
.sr "rewind(ioptr) FILE *ioptr
rewinds the stream
named by
.it ioptr.
It is not very useful except on input,
since a rewound output file is still open only for output.
.s
.sr "system(string) char *string
The
.it string
is executed by the shell as if typed at the terminal.
.s
.sr "getw(ioptr) FILE *ioptr
returns the next word from the input stream named by
.it ioptr.
EOF is returned on end-of-file or error,
but since this a perfectly good
integer
.it feof
and
.it ferror
should be used.
.s
.sr "putw(w, ioptr) FILE *ioptr
writes the integer
.it w
on the named output stream.
.s
.sr "setbuf(ioptr, buf) FILE *ioptr; char *buf
.it Setbuf
may be used after a stream has been opened
but before IO has started.
If
.it buf
is NULL,
the stream will be unbuffered.
Otherwise the buffer supplied will be used.
It is a character array of sufficient size:
.DS
char	buf[BUFSIZ];
.DE
.s
.sr "fileno(ioptr) FILE *ioptr
returns the integer file descriptor associated with the file.
.s
.sr "fseek(ioptr, offset, ptrname) FILE *ioptr; long offset
The location of the next byte in the stream
named by
.it ioptr
is adjusted.
.it Offset
is a long integer.
If
.it ptrname
is 0, the offset is measured from the beginning of the file;
if
.it ptrname
is 1, the offset is measured from the current read or
write pointer;
if
.it ptrname
is 2, the offset is measured from the end of the file.
The routine accounts properly for any buffering.
(When this routine is used on non-Unix systems,
the offset must be a value returned from
.it ftell
and the ptrname must be 0).
.s
.sr "long ftell(ioptr) FILE *ioptr
The byte offset, measured from the beginning of the file,
associated with the named stream is returned.
Any buffering is properly accounted for.
(On non-Unix systems the value of this call is useful only
for handing to
.it fseek,
so as to position the file to the same place it was when
.it ftell
was called.)
.s
.sr "getpw(uid, buf) char *buf
The password file is searched for the given integer user ID.
If an appropriate line is found, it is copied into
the character array
.it buf,
and 0 is returned.
If no line is found corresponding to the user ID
then 1 is returned.
.s
.sr "char *calloc(num, size)
allocates space for
.it num
items each of size
.it size.
The space is guaranteed to be set to 0 and the pointer is
sufficiently well aligned to be usable for any purpose.
NULL is returned if no space is available.
.s
.sr "cfree(ptr) char *ptr
Space is returned to the pool used by
.it calloc.
Disorder can be expected if the pointer was not obtained
from
.it calloc.
.LP
The following are macros defined by stdio.h.
.s
.sr isalpha(c)
returns non-zero if the argument is alphabetic.
.s
.sr isupper(c)
returns non-zero if the argument is upper-case alphabetic.
.s
.sr islower(c)
returns non-zero if the argument is lower-case alphabetic.
.s
.sr isdigit(c)
returns non-zero if the argument is a digit.
.s
.sr isspace(c)
returns non-zero if the argument is a spacing character:
tab, new-line, carriage return, vertical tab,
form feed, space.
.s
.sr toupper(c)
returns the upper-case character corresponding to the lower-case
letter c.
.s
.sr tolower(c)
returns the lower-case character corresponding to the upper-case
letter c.
