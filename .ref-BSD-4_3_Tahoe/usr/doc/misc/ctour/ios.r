.de sr
.sp 1
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
.PP
A new package of IO routines is available under the Unix system.
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
a set of relatively small, inexpensive adaptor routines
exist which make it appear identical to the current Portable Library
except in some very obscure details.
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
.RT
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
The routines in this package, like the current Portable
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
.RT
The routines in the library are in nearly one-to-one
correspondence with those in the Portable Library.
In several cases the name has been changed.
This is an attempt to reduce confusion.
If the attempt is judged to fail the names may be made identical even
though
the arguments may be different.
The order of this list generally follows the order
used in the Portable Library document.
.sr "FILE *fopen(filename, type)"
.it Fopen
opens the file and, if needed, allocates a buffer for it.
.it Filename
is a character string specifying the name.
.it Type
is a character string (not a single character).
It may be `"r",' `"w",' or `"a"' to indicate
intent to read, write, or append.
The value returned is a file pointer.
If it is null the attempt to open failed.
.sr "int getc(ioptr)"
returns the next character from the stream named by
.it ioptr,
which is a pointer to a file such as returned by
.it fopen,
or the name
.it stdin.
The integer EOF is returned on end-of-file or when
an error occurs.
The null character is a legal character.
.sr "putc(c, ioptr)"
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
.sr fclose(ioptr)
The file corresponding to
.it ioptr
is closed after any buffers are emptied.
A buffer allocated by the IO system is freed.
.it Fclose
is automatic on normal termination of the program.
.sr fflush(ioptr)
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
.sr feof(ioptr)
returns non-zero when end-of-file
has occurred on the specified input stream.
.sr ferror(ioptr)
returns non-zero when an error has occurred while reading
or writing the named stream.
The error indication lasts until the file has been closed.
.sr "getchar( )"
is identical to `getc(stdin)'.
.sr "putchar(c)"
is identical to `putc(c, stdout)'.
.sr "char *gets(s)"
reads characters up to a new-line from the standard input.
The new-line character is replaced by a null character.
It is the user's responsibility to make sure that the character array
.it s
is large enough.
.it Gets
returns its argument, or null if end-of-file or error occurred.
.sr "char *fgets(s, n, ioptr)"
reads up to
.it n
characters from the stream
.it ioptr
into the character pointer
.it s.
The read terminates with a new-line character.
The new-line character is placed in the buffer
followed by a null pointer.
The first argument,
or a null pointer if error or end-of-file occurred,
is returned.
.sr puts(s)
writes the null-terminated string (character array)
.it s
on the standard output.
A new-line is appended.
No value is returned.
.sr "fputs(s, ioptr)"
writes the null-terminated string (character array)
on the stream
.it s.
No new-line is appended.
No value is returned.
.sr "ungetc(c, ioptr)"
The argument character
.it c
is pushed back on the input stream named by
.it ioptr.
Only one character may be pushed back.
.sr "printf(format, a1, . . .)"
.sr "fprintf(ioptr, format, a1, . . .)"
.sr "sprintf(s, format, a1, . . .)"
.it Printf
writes on the standard output.
.it Fprintf
writes on the named output stream.
.it Sprintf
puts characters in the character array (string)
named by
.it s.
The specifications are as usual.
.sr "scanf(format, a1, . . .)"
.sr "fscanf(ioptr, format, a1, . . .)"
.sr "sscanf(s, format, a1, . . .)"
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
.sr "fread(ptr, sizeof(*ptr), nitems, ioptr)"
writes
.it nitems
of data beginning at
.it ptr
on file
.it ioptr.
It behaves identically to the Portable Library's
.it cread.
No advance notification
that binary IO is being done is required;
when, for portability reasons,
it becomes required, it will be done
by adding an additional character to the mode-string on the
fopen call.
.sr "fwrite(ptr, sizeof(*ptr), nitems, ioptr)"
Like
.it fread,
but in the other direction.
.sr rewind(ioptr)
rewinds the stream
named by
.it ioptr.
It is not very useful except on input,
since a rewound output file is still open only for output.
.sr system(string)
.sr atof(s)
.sr tmpnam(s)
.sr abort(code)
.sr "intss( )"
.sr "cfree(ptr)"
.sr  "wdleng( )"
are available with specifications identical to those
described for the Portable Library.
.sr "char *calloc(n, sizeof(object))"
returns null when no space is available.
The space is guaranteed to be 0.
.sr ftoa
is not implemented but there are plausible alternatives.
.sr "nargs( )"
is not implemented.
.sr getw(ioptr)
returns the next word from the input stream named by
.it ioptr.
EOF is returned on end-of-file or error,
but since this a perfectly good
integer
.it feof
and
.it ferror
should be used.
.sr "putw(w, ioptr)"
writes the integer
.it w
on the named output stream.
.sr "setbuf(ioptr, buf)"
.it Setbuf
may be used after a stream has been opened
but before IO has started.
If
.it buf
is null,
the stream will be unbuffered.
Otherwise the buffer supplied will be used.
It is a character array of sufficient size:
.DS
char	buf[BUFSIZ];
.DE
.sr "fileno(ioptr)"
returns the integer file descriptor associated with the file.
.PP
Several additional routines are available.
.sr "fseek(ioptr, offset, ptrname)"
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
.sr "long ftell(iop)"
The byte offset, measured from the beginning of the file,
associated with the named stream is returned.
Any buffering is properly accounted for.
.sr "getpw(uid, buf)"
The password file is searched for the given integer user ID.
If an appropriate line is found, it is copied into
the character array
.it buf,
and 0 is returned.
If no line is found corresponding to the user ID
then 1 is returned.
.sr "strcat(s1, s2)"
.it S1
and
.it s2
are character pointers.
The end (null byte)
of the
.it s1
string is found and
.it s2
is copied to
.it s1
starting there.
The space pointed to by
.it s1
must be large enough.
.sr "strcmp(s1, s2)"
The character strings
.it s1
and
.it s2
are compared.
The result is positive, zero, or negative according as
.it s1
is greater than, equal to, or less than
.it s2
in ASCII collating sequence.
.sr "strcpy(s1, s2)
The null-terminated character string
.it s2
is copied to the location pointed to by
.it s1.
.sr "strlen(s)"
The number of bytes in s up to a null byte
is returned.
.it S
is a character pointer.
.sr "gcvt(num, ndig, buf)"
.it Num
is a floating or double quantity.
.it Ndig
significant digits are converted to ASCII and placed
into the character array
.it buf.
The conversion is in Fortran
.it e
or
.it f
style, whichever yields the shorter string.
Insignificant trailing zeros are eliminated.
