.\"	@(#)Clib.ms	6.1 (Berkeley) 5/14/86
.\"
.so /k3/unx/MACROS/cmac1
.CH 3 "C LIBRARIES"
.H 1 "C LIBRARIES"
.H 2 "GENERAL"
.P
This chapter and Chapter 4 describe the libraries that
are supported on the  UNIX operating system.
A library is a collection of related functions and/or declarations
that simplify programming effort
by linking only what is needed,
allowing use of locally produced functions, etc.
All of the functions described are also described in
Part 3 of the
.I "UNIX System Programmer Reference Manual" .
Most of the declarations described are in Part 5 of the
.I "UNIX System Programmer Reference Manual" .
The three main libraries on the UNIX system are:
.tr ~ 
.VL 20
.LI ~\fBC~library\fR
This is the basic  library for C language programs.
The C library is composed of functions and declarations used for file access,
string testing and manipulation, character testing and manipulation, memory allocation, and other functions.
This library is described later in this chapter.
.tr ~ 
.LE
.VL 20
.LI ~\fBObject~file\fR
This library provides functions for the access and manipulation of
object files.
This library is described in Chapter 4.
.tr ~ 
.LE
.VL 20
.LI ~\fBMath~library\fR
This library provides exponential, bessel functions, logarithmic,
hyperbolic, and trigonometric functions.
This library is described in Chapter 4.
.LE
.P
Some libraries consist of two portions - functions and declarations.
In some cases, the user must request that the functions (and/or declarations) of a specific library be included
in a program being compiled.
In other cases, the functions (and/or declarations) are included automatically.
.H 3 "Including Functions"
.br
.P
When a program is being compiled, the compiler will
automatically search the C language library 
to locate and include functions that are used in the program.
This is the case only for the C  library and no other library.
In order for the compiler to locate and include functions from other libraries,
the user must specify these libraries on the command line for the compiler.
For example, when using functions of the math library, the user must request that the
math library be searched by including the argument \fB-lm\fR on the command line, such as:
.DS I
cc file.c -lm
.DE
The argument \fB-lm\fR must come after all files that
reference functions in the math library in order for
the link editor to know which functions to include in
the a.out file.
.P 0
This method should be used for all functions that are not part of the C language library.
.H 3 "Including Declarations"
.br
.P
Some functions require a set of declarations in order to operate properly.
A set of declarations is stored in a file under the \fI/usr/include\fR
directory.
These files are referred to as \fIheader files\fR.
In order to include a certain header file, the user must specify this request within the C language program.
The request is in the form:
.DS I
#include <file.h>
.DE
.P 0
where \fIfile.h\fR is the name of the file.
Since the header files define the type of the functions and
various preprocessor constants, they must be included
before invoking the functions they declare.
.P
The remainder of this chapter describes the functions and
header files of the C Library.
The description of the library begins with the actions required by the user to include the functions
and/or header files in a program being compiled (if any).
Following the description of the actions required is information
in three-column format of the form:
.DS L
\fBfunction\fR\^	\fBreference\fR\^(N) 	 	Brief description.
.SP
.DE
.P 0
The functions are grouped by type while the
reference refers to section `N' in the
.I "UNIX System Programmer Reference Manual" .
Following this, are descriptions of the header files associated with these
functions (if any).
.H 2 "THE C LIBRARY"
.P
The C library  consists of several types of functions.
All the functions of the C library are loaded automatically by the compiler.
Various declarations must sometimes be included by the user as required.
The functions of the C library are divided into the following types:
.BL 6 1
.LI
Input/output control
.LI
String manipulation
.LI
Character manipulation
.LI
Time functions
.LI
Miscellaneous functions.
.LE
.H 3 "Input/Output Control"
.br
.P
These functions of the C library
are automatically included as needed
during the compiling of
a C language program.
No command line request is needed.
.P
The
header file required by the input/output functions should be included in the
program being compiled.
This is accomplished by including the line:
.DS I
#include <stdio.h>\fR
.DE
.P 0
near the beginning of each file that references
an input or output function.
.P
The input/output functions are grouped into the following
categories:
.BL 6 1
.LI
File access
.LI
File status
.LI
Input
.LI
Output
.LI
Miscellaneous.
.LE
.H 3 "File Access Functions"
.br
.P 0
.TS
expand;
lll
lll.
.if n .ft B
.if t .ft BI
.SP 2
FUNCTION	REFERENCE	BRIEF DESCRIPTION
.ft R
.SP
\fBfclose\fR\^	\fBfclose\fR\^(3S)	Close an open stream.
.SP
\fBfdopen\fR\^	\fBfopen\fR\^(3S)	Associate stream with
		an \fBopen\fR(2) ed file.
.SP
\fBfileno\fR\^	\fBferror\fR\^(3S)	File descriptor associated
		with an open stream.
.SP
\fBfopen\fR\^	\fBfopen\fR\^(3S)	Open a file with
		specified permissions. 
		\fBFopen\fR returns a pointer
		to a stream which is
		used in subsequent
		references to the file.
.SP
\fBfreopen\fR\^	\fBfopen\fR\^(3S)	Substitute named file
		in place of open
		stream.
.SP
\fBfseek\fR\^	\fBfseek\fR\^(3S)	Reposition the file
		pointer.
.SP
\fBpclose\fR\^	\fBpopen\fR\^(3S)	Close a stream opened
		by \fBpopen\fR.
.SP
\fBpopen\fR\^	\fBpopen\fR\^(3S)	Create pipe as a stream
		between calling process 
		and command.
.SP
\fBrewind\fR\^	\fBfseek\fR\^(3S)	Reposition file
		pointer at beginning
		of file.
.SP
\fBsetbuf\fR\^	\fBsetbuf\fR\^(3S)	Assign buffering to
		stream.
.SP
\fBvsetbuf	setbuf\fR(3S)	Similar to \fBsetbuf\fR, but
		allowing finer control.
.TE
.H 3 "File Status Functions"
.br
.P 0
.TS
expand;
lll
lll.
.if n .ft B
.if t .ft BI
.SP 2
FUNCTION	REFERENCE	BRIEF DESCRIPTION
.ft R
.SP 
\fBclearerr\fR\^	\fBferror\fR\^(3S)	Reset error condition on
		stream.
.SP
\fBfeof\fB	\fBferror\fR(3S)	Test for ``end of file''
		on stream.
.SP
\fBferror\fR\^	\fBferror\fR\^(3S)	Test for error condition
		on stream.
.SP
\fBftell\fR\^	\fBfseek\fR\^(3S)	Return current position
		in the file.
.TE
.H 3 "Input Functions"
.br
.P 0
.TS
expand;
lll
lll.
.if n .ft B
.if t .ft BI
.SP 2
FUNCTION	REFERENCE	BRIEF DESCRIPTION
.ft R
.SP
\fBfgetc\fR\^	\fBgetc\fR\^(3S)	True function for \fBgetc\fR\^
		(3S).
.SP
\fBfgets\fR\^	\fBgets\fR\^(3S)	Read string from stream.
.SP
\fBfread\fR\^	\fBfread\fR\^(3S)	General buffered read
		from stream.
.SP
\fBfscanf\fR\^	\fBscanf\fR\^(3S)	Formatted read from
		stream.
.SP
\fBgetc\fR\^	\fBgetc\fR\^(3S)	Read character from
		stream.
.SP
\fBgetchar\fR\^	\fBgetc\fR\^(3S)	Read character from
		standard input.
.SP
\fBgets\fR\^	\fBgets\fR\^(3S)	Read string from standard input.
.SP
\fBgetw\fR\^	\fBgetc\fR\^(3S)	Read word from stream.
.SP
\fBscanf\fR\^	\fBscanf\fR\^(3S)	Read using format from
		standard input.
.SP
\fBsscanf\fR\^	\fBscanf\fR\^(3S)	Formatted from
		string.
.SP
\fBungetc\fR\^	\fBungetc\fR\^(3S)	Put back one character on
		stream.
.TE
.H 3 "Output Functions"
.br
.P 0
.TS
expand;
lll
lll.
.if n .ft B
.if t .ft BI
.SP 2
FUNCTION	REFERENCE	BRIEF DESCRIPTION
.ft R
.SP
\fBfflush\fR\^	\fBfclose\fR\^(3S)	Write all currently buffered
		characters from stream.
.SP
\fBfprintf\fR\^	\fBprintf\fR\^(3S)	Formatted write to
		stream.
.SP
\fBfputc\fR\^	\fBputc\fR\^(3S)	True function for \fBputc\fR\^
		(3S).
.SP
\fBfputs\fR\^	\fBputs\fR\^(3S)	Write string to stream.
.SP
\fBfwrite\fR\^	\fBfread\fR\^(3S)	General buffered write to 
		stream.
.SP
\fBprintf\fR\^	\fBprintf\fR\^(3S)	Print using format to
		standard output.
.SP
\fBputc\fR\^	\fBputc\fR\^(3S)	Write character to
		standard output.
.SP
\fBputchar\fR\^	\fBputc\fR\^(3S)	Write character to
		standard output.
.SP
\fBputs\fR\^	\fBputs\fR\^(3S)	Write string to
		standard output.
.SP
\fBputw\fR\^	\fBputc\fR\^(3S)	Write word to stream.
.SP
\fBsprintf\fR\^	\fBprintf\fR\^(3S)	Formatted write to
		string.
.SP
\fBvfprintf	vprint\fR(3C)	Print using format to
		stream by \fBvarargs\fR(5)
		argument list.
.SP
\fBvprintf	vprint\fR(3C)	Print using format to
		standard output by
		\fBvarargs\fR(5) argument list.
.SP
\fBvsprintf	vprintf\fR(3C)	Print using format to
		stream string by
		\fBvarargs\fR(5) argument list.
.TE
.H 3 "Miscellaneous Functions"
.br
.P 0
.TS
expand;
lll
lll.
.if n .ft B
.if t .ft BI
.SP 2
FUNCTION	REFERENCE	BRIEF DESCRIPTION
.ft R
.SP
\fBctermid\fR\^	\fBctermid\fR\^(3S)	Return file name for 
		controlling terminal.
.SP
\fBcuserid\fR\^	\fBcuserid\fR\^(3S)	Return login name for
		owner of current process.
.SP
\fBsystem\fR\^	\fBsystem\fR\^(3S)	Execute shell command.
.SP
\fBtempnam\fR\^	\fBtempnam\fR\^(3S)	Create temporary file
		name using directory and
		prefix.
.SP
\fBtmpnam\fR\^	\fBtmpnam\fR\^(3S)	Create temporary file
		name.
.SP
\fBtmpfile\fR\^	\fBtmpfile\fR\^(3S)	Create temporary file.
.TE
.H 3 "String Manipulation Functions"
.br
.P
These functions are
used to locate characters within a string, copy,
concatenate, and compare strings.
These functions are automatically located and loaded during the compiling of
a C language program.
No command line request is needed
since these functions are part of the C library.
The string manipulation functions are declared in a header file that
may be included in the program being compiled.
This is accomplished by including the line:
.DS I
#include <string.h>
.DE
near the beginning of each file that uses one
of these functions.
.sp 
.P 0
.TS
expand;
lll
lll.
.if n .ft B
.if t .ft BI
.SP 2
FUNCTION	REFERENCE	BRIEF DESCRIPTION
.ft R
.SP
\fBstrcat\fR\^	\fBstring\fR\^(3C)	Concatenate two strings.
.SP
\fBstrchr\fR\^	\fBstring\fR\^(3C)	Search string for
		character.
.SP
\fBstrcmp\fR\^	\fBstring\fR\^(3C)	Compares two strings.
.SP
\fBstrcpy\fR\^	\fBstring\fR\^(3C)	Copy string.
.SP
\fBstrcspn\fR\^	\fBstring\fR\^(3C)	Length of initial string
		not containing set of 
		characters.
.SP
\fBstrlen\fR\^	\fBstring\fR\^(3C)	Length of string.
.SP
\fBstrncat\fR\^	\fBstring\fR\^(3C)	Concatenate two strings
		with a maximum length.
.SP
\fBstrncmp\fR\^	\fBstring\fR\^(3C)	Compares two strings
		with a maximum length.
.SP
\fBstrncpy\fR\^	\fBstring\fR\^(3C)	Copy string over string
		with a maximum length.
.SP
\fBstrpbrk\fR\^	\fBstring\fR\^(3C)	Search string for any
		set of characters.
.SP
\fBstrrchr\fR\^	\fBstring\fR\^(3C)	Search string backwards
		for character.
.SP
\fBstrspn\fR\^	\fBstring\fR\^(3C)	Length of initial string
		containing set of
		characters.
.SP
\fBstrtok\fR\^	\fBstring\fR\^(3C)	Search string for token
		separated by any of a
		set of characters.
.TE
.H 3 "Character Manipulation"
.br
.P
The following functions and declarations are used for
testing and translating ASCII characters.
These functions are located and loaded automatically during the compiling of
a C language program.
No command line request is needed
since these functions are part of the C library.
.P
The declarations associated with these functions should be included in the
program being compiled.
This is accomplished by including the line:
.DS I
#include <ctype.h>
.DE
.P 0
near the beginning of the file being compiled.
.H 3 "Character Testing Functions"
.br
.P
These functions can be used to identify characters as uppercase or
lowercase letters, digits, punctuation, etc.
.P 0
.TS
expand;
lll
lll.
.if n .ft B
.if t .ft BI
.SP 2
FUNCTION	REFERENCE	BRIEF DESCRIPTION
.ft R
.SP
\fBisalnum\fR\^	\fBctype\fR\^(3C)	Is character
		alphanumeric?
.SP
\fBisalpha\fR\^	\fBctype\fR\^(3C)	Is character alphabetic?
.SP
\fBisascii\fR\^	\fBctype\fR\^(3C)	Is integer ASCII
		character?
.SP
\fBiscntrl\fR\^	\fBctype\fR\^(3C)	Is character a control
		character?
.SP
\fBisdigit\fR\^	\fBctype\fR\^(3C)	Is character a digit?
.SP
\fBisgraph\fR\^	\fBctype\fR\^(3C)	Is character a printable
		character?
.SP
\fBislower\fR\^	\fBctype\fR\^(3C)	Is character a
		lowercase letter?
.SP
\fBisprint\fR\^	\fBctype\fR\^(3C)	Is character a printing
		character including
		space?
.SP
\fBispunct\fR\^	\fBctype\fR\^(3C)	Is character a
		punctuation character?
.SP
\fBisspace\fR\^	\fBctype\fR\^(3C)	Is character a white
		space character?
.SP
\fBisupper\fR\^	\fBctype\fR\^(3C)	Is character an uppercase
		letter?
.SP
\fBisxdigit\fR\^	\fBctype\fR\^(3C)	Is character a hex digit?
.TE
.H 3 "Character Translation Functions"
.br
.P
These functions provide
translation of uppercase to lowercase, lowercase to uppercase,
and integer to ASCII.
.P 0
.TS
expand;
lll
lll.
.if n .ft B
.if t .ft BI
.SP 2
FUNCTION	REFERENCE	BRIEF DESCRIPTION
.ft R
.SP
\fBtoascii\fR\^	\fBconv\fR\^(3C)	Convert integer to
		ASCII character.
.SP
\fBtolower\fR\^	\fBconv\fR\^(3C)	Convert character to 
		lowercase.
.SP
\fBtoupper\fR\^	\fBconv\fR\^(3C)	Convert character to 
		uppercase.
.TE
.H 3 "Time Functions"
.br
.P
These functions  are used for
accessing
and reformatting the systems idea of the current date and time.
These functions are located and loaded automatically during the compiling of
a C language program.
No command line request is needed
since these functions are part of the C library.
.P
The header file associated with these functions should be included in the
program being compiled.
This is accomplished by including the line:
.DS I
.ne 4
#include <time.h>
.DE
.P 0
near the beginning of any file using the time functions.
.P
These functions (except \fBtzset\fR) convert a time such as returned
by \fBtime\fR(2).
.ne 6
.P 0 
.TS
expand;
lll
lll.
.if n .ft B
.if t .ft BI
.SP 2
.ne 4
FUNCTION	REFERENCE	BRIEF DESCRIPTION
.ft R
.SP
.ne 4
\fBasctime\fR\^	\fBctime\fR\^(3C)	Return string
		representation
		of date and time.
.SP
.ne 4
\fBctime\fR\^	\fBctime\fR\^(3C)	Return string
		representation of
		date and time, given
		integer form.
.SP
.ne 4
\fBgmtime\fR\^	\fBctime\fR\^(3C)	Return Greenwich
		Mean Time.
.SP
.ne 4
\fBlocaltime\fR\^	\fBctime\fR\^(3C)	Return local time.
.SP
.ne 4
\fBtzset\fR\^	\fBctime\fR\^(3C)	Set time zone field
		from environment
		variable.
.TE
.P 0
.H 3 "Miscellaneous Functions"
.br
.P
These functions support a wide variety of operations.
Some of these are numerical conversion, password file and group file access,
memory allocation, random number generation, and table management.
These functions are automatically located and included in a program being compiled.
No command line request is needed since these functions are part of the C library.
.P
Some of these functions require declarations to be included.
These are described following the descriptions of the functions.
.H 3 "Numerical Conversion"
.br
.P
The following functions perform numerical conversion.
.P 0 
.TS
expand;
lll
lll.
.if n .ft B
.if t .ft BI
.SP 2
.ne 4
FUNCTION	REFERENCE	BRIEF DESCRIPTION
.ft R
.SP
.ne 4
\fBa64l\fR\^	\fBa64l\fR\^(3C)	Convert string to
		base 64 ASCII.
.SP
.ne 4
\fBatof\fR\^	\fBatof\fR\^(3C)	Convert string to
		floating.
.SP
.ne 4
\fBatoi\fR\^	\fBatof\fR\^(3C)	Convert string to
		integer.
.SP
.ne 4
\fBatol\fR\^	\fBatof\fR\^(3C)	Convert string to long.
.SP
.ne 4
\fBfrexp\fR\^	\fBfrexp\fR\^(3C)	Split floating into
		mantissa and exponent.
.SP
.ne 4
\fBl3tol\fR\^	\fBl3tol\fR\^(3C)	Convert 3-byte integer
		to long.
.SP
.ne 4
\fBltol3\fR\^	\fBl3tol\fR\^(3C)	Convert long to 3-byte
		integer.
.SP
.ne 4
\fBldexp\fR\^	\fBfrexp\fR\^(3C)	Combine mantissa and
		exponent.
.SP
.ne 4
\fBl64a\fR\^	\fBa64l\fR\^(3C)	Convert base 64 ASCII
		to string.
.SP
.ne 4
\fBmodf\fR\^	\fBfrexp\fR\^(3C)	Split mantissa into
		integer and fraction.
.TE
.H 3 "DES Algorithm Access"
.br
.P
The following functions allow access to the Data Encryption Standard (DES) algorithm 
used on the UNIX operating system.
The DES algorithm is implemented with variations to frustrate use of
hardware implementations of the DES for key search.
.P 0 
.TS
expand;
lll
lll.
.if n .ft B
.if t .ft BI
.SP 2
.ne 4
FUNCTION	REFERENCE	BRIEF DESCRIPTION
.ft R
.SP
.ne 4
\fBcrypt\fR\^	\fBcrypt\fR\^(3C)	Encode string.
.SP
.ne 4
\fBencrypt\fR\^	\fBcrypt\fR\^(3C)	Encode/decode string of
		0s and 1s.
.SP
.ne 4
\fBsetkey\fR\^	\fBcrypt\fR\^(3C)	Initialize for subsequent
		use of \fBencrypt\fR.
.TE
.H 3 "Group File Access"
.br
.P
The following functions are used to obtain entries from the group file.
Declarations for these functions must be included in the program being
compiled with the line:
.DS I
#include <grp.h>
.DE
.P 0
.P 0 
.TS
expand;
lll
lll.
.if n .ft B
.if t .ft BI
.SP 2
.ne 4
FUNCTION	REFERENCE	BRIEF DESCRIPTION
.ft R
.SP
.ne 4
\fBendgrent\fR\^	\fBgetgrent\fR\^(3C)	Close group file being
		processed.
.SP
.ne 4
\fBgetgrent\fR\^	\fBgetgrent\fR\^(3C)	Get next group file
		entry.
.SP
.ne 4
\fBgetgrgid\fR\^	\fBgetgrent\fR\^(3C)	Return next group with
		matching gid.
.SP
.ne 4
\fBgetgrnam\fR\^	\fBgetgrent\fR\^(3C)	Return next group with
		matching name.
.SP
.ne 4
\fBsetgrent\fR\^	\fBgetgrent\fR\^(3C)	Rewind group file being
		processed.
.SP
.ne 4
\fBfgetgrent	getgrent\fR(3C)	Get next group file entry
		from a specified file.
.TE
.H 3 "Password File Access"
.br
.P
These functions are used to search and access information stored in the
password file (/etc/passwd).
Some functions require declarations that can be included in the program
being compiled by adding the line:
.DS I
#include <pwd.h>
.DE
.P 0
.P 0 
.TS
expand;
lll
lll.
.if n .ft B
.if t .ft BI
.SP 2
.ne 4
FUNCTION	REFERENCE	BRIEF DESCRIPTION
.ft R
.SP
.ne 4
\fBendpwent\fR\^	\fBgetpwent\fR\^(3C)	Close password file
		being processed.
.SP
.ne 4
\fBgetpw\fR\^	\fBgetpw\fR\^(3C)	Search password file
		for uid.
.SP
.ne 4
\fBgetpwent\fR\^	\fBgetpwent\fR\^(3C)	Get next password file
		entry.
.SP
.ne 4
\fBgetpwnam\fR\^	\fBgetpwent\fR\^(3C)	Return next entry with
		matching name.
.SP
.ne 4
\fBgetpwuid\fR\^	\fBgetpwent\fR\^(3C)	Return next entry with
		matching uid.
.SP
.ne 4
\fBputpwent\fR\^	\fBputpwent\fR\^(3C)	Write entry on stream.
.SP
.ne 4
\fBsetpwent\fR\^	\fBgetpwent\fR\^(3C)	Rewind password file
		being accessed.
.SP
.ne 4
.ne 3
\fBfgetpwent	getpwent\fR(3C)	Get next password file
		entry from a specified
		file.
.TE
.H 3 "Parameter Access"
.br
.P
The following functions provide access to several different types of
paramenters.
None require any declarations.
.P 0 
.TS
expand;
lll
lll.
.if n .ft B
.if t .ft BI
.SP 2
.ne 4
FUNCTION	REFERENCE	BRIEF DESCRIPTION
.ft R
.SP
.ne 4
\fBgetopt\fR\^	\fBgetopt\fR\^(3C)	Get next option from
		option list.
.SP
.ne 4
\fBgetcwd\fR\^	\fBgetcwd\fR\^(3C)	Return string
		representation of
		current working directory.
.SP
.ne 4
\fBgetenv\fR\^	\fBgetenv\fR\^(3C)	Return string value
		associated with
		environment variable.
.SP
.ne 4
\fBgetpass\fR\^	\fBgetpass\fR\^(3C)	Read string from terminal
		without echoing.
.SP
.ne 4
\fBputenv	putenv\fR(3C)	Change or add value
		of an environment
		variable.
.TE
.H 3 "Hash Table Management"
.br
.P
The following functions are used to manage hash search tables.
The header file associated with these functions should be included
in the program being compiled.
This is accomplished by including the line:
.DS I
#include <search.h>
.DE
near the beginning of any file using the search functions.
.P 0 
.TS
expand;
lll
lll.
.if n .ft B
.if t .ft BI
.SP 2
.ne 4
FUNCTION	REFERENCE	BRIEF DESCRIPTION
.ft R
.SP
.ne 4
\fBhcreate\fR\^	\fBhsearch\fR\^(3C)	Create hash table.
.SP
.ne 4
\fBhdestroy\fR\^	\fBhsearch\fR\^(3C)	Destroy hash table.
.SP
.ne 4
\fBhsearch\fR\^	\fBhsearch\fR\^(3C)	Search hash table for
		entry.
.TE
.H 3 "Binary Tree Management"
.br
.P
The following functions are used to manage a binary tree.
The header file associated with these functions should be included
in the program being compiled.
This is accomplished by including the line:
.DS I
#include <search.h>
.DE
near the beginning of any file using the search functions.
.P 0 
.TS
expand;
lll
lll.
.if n .ft B
.if t .ft BI
.SP 2
.ne 4
FUNCTION	REFERENCE	BRIEF DESCRIPTION
.ft R
.SP
.ne 4
\fBtdelete\fR\^	\fBtsearch\fR\^(3C)	Deletes nodes from
		binary tree.
.SP
.ne 4
\fBtfind	tsearch\fR(3C)	Find element in
		binary tree.
.SP
.ne 4
\fBtsearch\fR\^	\fBtsearch\fR\^(3C)	Look for and add
		element to binary
		tree.
.SP
.ne 4
\fBtwalk\fR\^	\fBtsearch\fR\^(3C)	Walk binary tree.
.TE
.H 3 "Table Management"
.br
.P
The following functions are used to manage a table.
Since none of these functions allocate storage, sufficient
memory must be allocated before using these functions.
The header file associated with these functions should be included
in the program being compiled.
This is accomplished by including the line:
.DS I
#include <search.h>
.DE
near the beginning of any file using the search functions.
.P 0 
.TS
expand;
lll
lll.
.if n .ft B
.if t .ft BI
.SP 2
.ne 4
FUNCTION	REFERENCE	BRIEF DESCRIPTION
.ft R
.SP
.ne 4
\fBbsearch\fR\^	\fBbsearch\fR\^(3C)	Search table using
		binary search.
.SP
.ne 4
\fBlfind	lsearch\fR(3C)	Find element in
		library tree.
.SP
.ne 4
\fBlsearch\fR\^	\fBlsearch\fR\^(3C)	Look for and add
		element in binary
		tree.
.SP
.ne 4
\fBqsort\fR\^	\fBqsort\fR\^(3C)	Sort table using
		quick-sort algorithm.
.TE
.H 3 "Memory Allocation"
.br
.P
The following functions provide a means by which memory can be
dynamically allocated or freed.
.P 0 
.TS
expand;
lll
lll.
.if n .ft B
.if t .ft BI
.SP 2
.ne 4
FUNCTION	REFERENCE	BRIEF DESCRIPTION
.ft R
.SP
.ne 4
\fBcalloc\fR\^	\fBmalloc\fR\^(3C)	Allocate zeroed storage.
.SP
.ne 4
\fBfree\fR\^	\fBmalloc\fR\^(3C)	Free previously allocated
		storage.
.SP
.ne 4
\fBmalloc\fR\^	\fBmalloc\fR\^(3C)	Allocate storage.
.SP
.ne 4
\fBrealloc\fR\^	\fBmalloc\fR\^(3C)	Change size of allocated
		storage.
.TE
The following is another set of memory allocation functions
available.
.P 0 
.TS
expand;
lll
lll.
.if n .ft B
.if t .ft BI
.SP 2
.ne 4
FUNCTION	REFERENCE	BRIEF DESCRIPTION
.ft R
.SP
.ne 4
\fBcalloc	malloc\fR(3X)	Allocate zeroed storage.
.SP
.ne 4
\fBfree	malloc\fR(3X)	Free previously allocated
		storage.
.SP
.ne 4
\fBmalloc	malloc\fR(3X)	Allocate storage.
.SP
.ne 4
\fBmallopt	malloc\fR(3X)	Control allocation
		algorithm.
.SP
.ne 4
\fBmallinfo	malloc\fR(3X)	Space usage.
.SP
.ne 4
\fBrealoc	malloc\fR(3X)	Change size of
		allocated storage.
.TE
.H 3 "Pseudorandom Number Generation"
.br
.P
The following functions are used to generate pseudorandom numbers.
The functions that end with \fB48\fR are a family of interfaces to
a pseudorandom number generator based upon the linear congruent
algorithm and 48-bit integer arithmetic.
The \fBrand\fR\^ and \fBsrand\fR\^ functions provide an interface to
a multiplicative congruential random number generator with period of
232.
.P 0 
.TS
expand;
lll
lll.
.if n .ft B
.if t .ft BI
.SP 2
.ne 4
FUNCTION	REFERENCE	BRIEF DESCRIPTION
.ft R
.SP
.ne 4
\fBdrand48\fR\^	\fBdrand48\fR\^(3C)	Random double over
		the interval [0 to 1).
.SP
.ne 4
\fBlcong48\fR\^	\fBdrand48\fR\^(3C)	Set parameters for
		\fBdrand48\fR\^, \fBlrand48\fR\^,
		and \fBmrand48\fR.
.SP
.ne 4
\fBlrand48\fR\^	\fBdrand48\fR\^(3C)	Random long over the
		interval [0 to 2\v'-0.3'31\v'0.3').
.SP
.ne 4
\fBmrand48\fR\^	\fBdrand48\fR\^(3C)	Random long over the
		interval [-2\v'-0.3'31\v'0.3' to 2\v'-0.3'31\v'0.3').
.SP
.ne 4
\fBrand\fR\^	\fBrand\fR\^(3C)	Random integer over the
		interval [0 to 32767).
.SP
.ne 4
\fBseed48\fR\^	\fBdrand48\fR\^(3C)	Seed the generator for
		\fBdrand48\fR\^, \fBlrand48\fR\^, and
		\fBmrand48\fR.
.SP
.ne 4
\fBsrand\fR\^	\fBrand\fR\^(3C)	Seed the generator 
		for \fBrand\fR.
.SP
.ne 4
\fBsrand48\fR\^	\fBdrand48\fR\^(3C)	Seed the generator for
		\fBdrand48\fR\^, \fBlrand48\fR\^, and
		\fBmrand48\fR using a long.
.SP
.ne 4
.TE
.sp
.sp
.sp
.sp
.tr ~
.sp
.H 3 "Signal Handling Functions"
.br
.P
The functions \fBgsignal\fR\^ and \fBssignal\fR\^ implement a software
facility similar to \fBsignal\fR(2) in the
.I "UNIX System Programmer Reference Manual" .
This facility enables users to indicate the disposition of error
conditions and allows users to handle signals for their own purposes.
The declarations associated with these functions can be included in the
program being complied by the line
.DS I
#include <signal.h>
.DE
.P 0
These declarations define ASCII names for the 15 software signals.
.P 0
.TS
expand;
lll
lll.
.SP 2
.ne 4
.if n .ft B
.if t .ft BI
FUNCTION	REFERENCE	BRIEF DESCRIPTION
.ft R
.SP
.ne 4
\fBgsignal\fR\^	\fBssignal\fR\^(3C)	Send a software signal.
.SP
.ne 4
\fBssignal\fR\^	\fBssignal\fR\^(3C)	Arrange for handling
		of software signals.
.TE
.H 3 "Miscellaneous"
.br
.P
The following functions do not fall into any previously described
category.
.P 0
.TS
expand;
lll
lll.
.SP 2
.ne 4
.if n .ft B
.if t .ft BI
FUNCTION	REFERENCE	BRIEF DESCRIPTION
.ft R
.SP
.ne 4
\fBabort\fR\^	\fBabort\fR\^(3C)	Cause an IOT signal
		to be sent to the
		process.
.SP
.ne 4
\fBabs\fR\^	\fBabs\fR\^(3C)	Return the absolute
		integer value.
.SP
.ne 4
\fBecvt\fR\^	\fBecvt\fR\^(3C)	Convert double to
		string.
.SP
.ne 4
\fBfcvt\fR\^	\fBecvt\fR\^(3C)	Convert double to
		string using Fortran
		Format.
.SP
.ne 4
\fBgcvt\fR\^	\fBecvt\fR\^(3C)	Convert double to
		string using Fortran
		F or E format.
.SP
.ne 4
\fBisatty\fR\^	\fBttyname\fR\^(3C)	Test whether integer
		file descriptor is
		associated with a
		terminal.
.SP
.ne 4
\fBmktemp\fR\^	\fBmktemp\fR\^(3C)	Create file name
		using template.
.SP
.ne 4
\fBmonitor\fR\^	\fBmonitor\fR\^(3C)	Cause process to record
		a histogram of program
		counter location.
.SP
.ne 4
\fBswab\fR\^	\fBswab\fR\^(3C)	Swap and copy bytes.
.SP
.ne 4
\fBttyname\fR\^	\fBttyname\fR\^(3C)	Return pathname of
		terminal associated with
		integer file descriptor.
.TE
.TC 2 1 3 0
