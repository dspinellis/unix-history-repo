.\" Copyright (c) 1986 Regents of the University of California.
.\" All rights reserved.  The Berkeley software License Agreement
.\" specifies the terms and conditions for redistribution.
.\"
.\"	@(#)1.t	6.12 (Berkeley) 4/15/86
.\"
.sp
.ce
.LG
.B "Bug fixes and changes"
.SM
.sp
.LP
.LP
.LG
.ce
Section 1
.SM
.sp
.PP
.de BP
.IP \fB\\$1\\fR 14n
..
.BP adb
Locates the stack frame when debugging the kernel.
Slight changes were made to output formats.
.BP arcv
Has been retired to \fI/usr/old\fP.
.BP as
The default data alignment may now be specified on the
command line with a \fB\-a\fP flag.
A problem in handling filled data was fixed.
Some bugs in the handling of dbx stab information were fixed.
.BP at
The user may now choose to run \fIsh\fP or \fIcsh\fP.
Mail can now be sent to the user after the job has run;
mail is always sent if there were any errors during execution.
.I At
now runs with the user's full permissions.
All spool files are now owned by ``daemon''.
The last update time is in seconds instead of hours.
The problems with day and year increments have been fixed.
.BP awk
Problems when writing to pipes have been corrected.
.BP bc
.I Bc
will continue reading from standard input,
after failing to open a file specified from the command line.
.BP calendar
Now allows tabs as separators.
A subject line with the date of the reminder is added to each message.
.BP cat
Problems opening standard input multiple times have been fixed.
.I Cat
now runs much faster in the default (optionless) case.
.BP cb
No longer dumps core for unterminated comments
or large block comments.
For most purposes,
.I indent (1)
is far superior to
.I cb .
.BP cc
The C compiler has some new features as well as numerous bug fixes.
The principal new feature is a
.B \-f
flag that tells the compiler to compute expressions
of type
.B float
in single precision,
following the ANSI C standard proposals.
The C preprocessor has been extended to generate the dependency list for
source files.
The output is designed for inclusion in a makefile without
modification.
.IP
The bug fixes are many and varied.
Several fixes deal with type coercion and sign extension.
Signed
.B char
and
.B short
values are now properly sign-extended
in comparisons with unsigned values
of the same length.
Conversion of a signed
.B char
value to \fBunsigned short\fP now correctly sign-extends
to 16 bits (on the VAX).
Non-integer switch expressions now elicit warnings
and the appropriate conversions are emitted.
Unsigned longs were being treated as signed for the purpose of
conversion to floating types; the compiler now produces the appropriate
complicated instruction sequence to do this right.
An ancient misunderstanding that caused
.I "i *= d
to be treated as \fIi = i *\fP (\fBint\fP) \fId\fP
instead of \fIi =\fP (\fBdouble\fP) \fIi * d\fP
for \fBint\fP \fIi\fP
and \fBdouble\fP \fId\fP
has been corrected.
If a signed integer division or modulus is cast to unsigned,
the unsigned division or modulus routine
is no longer used to compute the operation.
.IP
Some problems with bogus input and bogus output
are now handled better;
more syntax errors are caught and
fewer code errors are emitted.
Many declarations and expressions involving type
.B void
that used to be disallowed now work;
some expressions that were not supposed to work
are now caught.
A pointer to a structure no longer stands a chance
of being incremented by the size of its first element
instead of the size of the structure
when the value of the element is used at the same
time the pointer is postincremented.
Side effects in the left hand side of
an unsigned assignment operator expression
are now performed only once.
Hex constants of the form 01234x56789 are now illegal.
External declarations of functions
may now possess arguments only if
they are also definitions of functions.
Declarations or initializations for objects of type structure
where the particular structure was not previously defined
used to result in confusing messages or even compiler errors;
it's now possible to deduce one's mistake.
.IP
Some effort has been put into
making the compiler more robust.
Initializers containing casts sometimes would draw
complaints about compiler loops or other problems;
these now work properly.
The register resource calculation now
takes into account implicit conversions from
.B float
to
.B double
type, so that the code generator
will not block by running out of registers.
The compiler is more diligent about
reducing structure type arguments to functions
and no longer gives up when it cannot
reduce the address to an offset from a register
in only two tries.
Programs that end in ``\|\e\|n\|#\|'' no longer cause compiler core dumps.
The compiler no longer dumps core
for floating point exceptions that
occur during reduction of constant expressions.
The compiler expression tree table
was enlarged so that it does not run out
of space as quickly when processing complex expressions
such as
.I putchar(c) .
The C preprocessor no longer uses a statically allocated
space for strings.
The preprocessor also now handles
.B "#\ line
directives properly
and correctly treats standard input from a terminal or a pipe.
Two fencepost errors in the C peephole optimizer
were adjusted and it now dumps core less often.
.IP
Some minor code efficiency changes were made.
An important change is that the compiler
now recognizes unsigned division and modulus operations
that can be done with masking and shifting;
this avoids the usual subroutine call overhead
associated with these operations.
The computation of register resources
has improved so that
the number of registers required for an expression
is not overestimated as often.
Register storage declarations for
.B float
variables now cause them to be put in registers
if the
.B \-f
flag is used.
The compiler itself is somewhat faster,
thanks primarily to a change that considerably
reduces symbol table searches
when entering and leaving blocks.
.IP
The compiler sources have been rearranged to make maintenance easier.
The names of some source files have been changed to protect the innocent;
header files now end in
.I .h ,
and names of files reflect their functions.
Configuration control has been simplified,
so that only a simple configuration include file
and the makefile flags variable should have to be considered
when putting the compiler together.
Redundant information has been eliminated from
include files and the makefile,
to reduce the chance of introducing changes
that will make data structures or defines inconsistent.
Values for opcodes are now taken
from an include file
.I pcc.h
that is common to all the compilers
that use the C compiler back end.
The peephole optimizer can now be compiled without
.B \-w .
.BP checknr
The
.B "\&.T&
.I tbl
directive was added to the list of known commands.
.BP chfn
Has been merged into
.I passwd (1).
.BP chgrp
An option has been added for recursively
changing the group of a directory tree.
.BP chmod
Can now recursively modify the permissions on a directory tree.
The mode string was extended to turn on the execute bit
conditionally if the file is executable or is a directory.
.BP chsh
Has been merged into
.I passwd (1).
.BP clear
Now has a proper exit status.
.BP colrm
Line length limitations have been removed.
.BP compact
Has been retired to \fI/usr/old\fP.
.BP compress
Replaces \fIcompact\fP as the preferred method to use in saving file
system space.
.BP cp
No longer suffers
problems when copying a directory to a nonexistent name or when
some directories are not writable in a recursive copy.
The \fB\-p\fP flag was added to preserve modes
and times when copying files.
.BP crypt
Waits for
.I makekey
to finish before reading from its pipe.
.BP csh
Has a new flag to stop argument processing so set user id shell scripts are more
secure.  File name completion may be optionally enabled.
.I Csh
keeps better
track of the current directory when traversing symbolic links.
Some major work was done on performance.
.BP ctags
.I Ctags
was modified to recognize LEX and YACC input files.
Files ending in
.I "\&.y
are presumed to be YACC input,
and a tag is generated for each non-terminal defined,
plus a tag
.B yyparse
for the first %% line in the file.
Files ending in \fI.l\fP are checked to see if they are
LEX or Lisp files.
A tag \fByylex\fP is generated for the first %% line in a LEX file.
In addition, for both kinds of files,
any C source after a second %% is scanned for tags.
.BP date
The
.I date
command can now be used to set the date on all machines in a network using the
.I timed (8)
program.
More information is logged regarding the setting of time.
.BP dbx
Major improvements have been made to
.I dbx
since the 4.2BSD release.
Large numbers of bug fixes have made
.I dbx
much more pleasant to use;
in particular many pointer errors that used to cause
.I dbx
to crash have been caught.
Some new features have been installed;
for instance it is now possible to search
for source lines with regular expressions.
The Fortran and Pascal language support is much improved,
and the DEC Western Research Labs Modula-2 compiler
is now supported.
.BP dd
Exit codes have been changed to correspond with normal conventions.
.BP deroff
.I Deroff
no longer throws out two letter words.
.BP diff
Context diffs merge nearby changes.
New flags were added for ignoring white space
differences and for insensitivity to case.
.BP diff3
The RCS version of \fIdiff3\fP has been merged
into the standard \fIdiff3\fP
under two new flags, \fB\-E\fP and \fB\-X\fP.
.BP echo
No longer accepts \fB\-n\fIanything\fR in place of \fB\-n\fP.
.BP error
Support for the DEC Western Research Labs Modula-2 compiler has been added.
.I Error
will now be able to run when there is no associated tty,
so it may now be driven from \fIat\fP(1), etc.
If the \fB\-n\fP and \fB\-t\fP options are selected,
.I error
will not touch files.
.BP ex
Support for changing window size has been added,
and terminals with many lines,
such as the WE5620,
are now handled.
Several small bug fixes were installed
and various facilities have been made faster.
.I Ex
only reads the file
.I "\&.exrc
if it is owned by the user, unless
the \fIsourceany\fP option is set.
It only looks for ``mode lines'' if the \fImodeline\fP option is set.
If Lisp mode is set, it allows ``\-'' to be used in ``words''.
.I Expreserve
now provides a better description of what happened
to a user's buffer when disaster struck.
.BP eyacc
\fIeyacc\fP is no longer a standard utility.
It has been moved to the Pascal source directory.
.BP f77
The Fortran compiler has been substantially improved.
Many serious bugs have been fixed since the last release;
the compiler now passes several widely used tests
such as the Navy Fortran Compiler Validation System and
the IMSL and NAG mathematical libraries.
The optimizer is now trustworthy and robust;
the many gruesome bugs that it used to inflict on programs,
such as resolving different variables in the same
.B common
block into the same temporary for purposes
of common subexpression elimination,
have been fixed.
.B Do
loops, which used to suffer from deadly problems
where loop variables, limit values and tests all managed
to misfire even without the help of the optimizer,
now produce proper results.
Many severe bugs with
.B character
variables and expressions have been fixed;
it is now possible to have variable length
.B character
variables on either side of an assignment,
and the lengths of concatenations are properly computed.
Several register allocation bugs have been fixed,
among them the awful bug that
.I "a = f\|(a)
where
.I a
is in a register would not alter the value of \fIa\fP.
Register allocation, though significantly improved,
is still pitifully naive compared
with the methods found in production Fortran compilers.
.B Save
statements cause variables to be retained,
even if a subroutine returns from inside a loop.
It is no longer possible to modify constants
that are passed as parameters to subroutines
and thus change all future uses of the constant
when it is used as a subroutine parameter.
Multi-level equivalences are no longer scrambled,
and the
.B cmplx
intrinsic conversion function
no longer garbles its result.
The compiler now generates integer move instructions
where it used to produce floating point move instructions,
even when not optimizing, so that non-standard use of equivalences
between
.B real
and
.B integer
types work as on most other systems.
.B Assign
statements now work with
.B format
statements.
The ``first character'' parameter of a substring
is now evaluated only once instead of twice.
Restrictions on
.B parameter
variables are now enforced,
and the compiler no longer aborts while
trying to make sense of impossible
.B parameter
variables.
The restrictions on array dimension declarators
are much closer to the standard and much more stringent.
Statement ordering used to be much more flexible, and wrong;
it is now strictly enforced,
leading to fewer compiler errors.
The compiler now chides the user for
declaring adjustable length character variables
that are not dummy arguments.
The compiler understands that subroutines and functions
are different and prevents them from being used interchangeably.
The parser is no longer fooled by excess
``positional I/O control'' parameters in I/O statements.
.IP
Several changes have been made to prevent the compiler
itself from aborting;
in particular, computed \fBgoto\fPs
do not elicit compiler core dumps,
nor do multiplications by zero,
nor do unusual statement numbers.
The compiler now recognizes and complains about
various kinds of hardware errors that
can result from evaluating constant expressions,
such as integer and floating overflow;
it no longer dies when it receives a SIGFPE.
Several memory management bugs that
caused the compiler to dump core for
seemingly random things have met their demise.
Some conversion operations used to cause
the code generator to emit impossible
assembly language instructions
that in turn caused the assembler some indigestion;
these are now fixed.
Some symbol table modifications were made to help out \fIdbx\fP(1),
so that values of
.B common
and
.B parameter
storage classes
and
.B logical
types are now accessible from \fIdbx\fP.
When the compiler does abort,
the error messages produced are now comprehensible to human beings
and messy core dumps are no longer left behind.
Some effort has been made to improve error reporting
for program errors and to handle exceptional conditions in which
the old compiler used to punt.
.IP
Some improvements in optimization were
added to the compiler.
Offsets to static data are now
shorter than before;
the compiler used to produce 32-bit offsets
for all local variables.
.B Real
variables may now be allocated to registers.
Format strings in \fBformat\fP statements are compiled
for considerable runtime savings;
for various reasons, format strings
in character constants and variables in I/O statements are not.
Common subexpression elimination
now reduces the re-evaluation of exponentiations
in polynomial expressions.
Some problems with alignment of data
that caused ghastly performance degradation have been repaired.
.IP
Some changes have been made in the way the compiler is put together.
The compiler front end now uses the common intermediate code format
established in the include file
.I pcc.h
to communicate with the back end.
The back end has been re-merged with the C compiler sources,
so that bug fixes to the C compiler
are automatically propagated to the Fortran back end.
Similarly, the Fortran and C peephole optimizers were re-merged.
.IP
Some new features were added to the compiler.
There is now a
.B \-r8
flag to coerce
.B real
and
.B complex
variables and constants to
double precision and double complex
types for extended precision.
There is a
.B \-q
flag to suppress listing of file and entry names during compilation.
Some foolproofing was added to the compiler driver;
it is no longer possible to wipe out a source file
by entering ``f77 \-o foo.f'',
and it now complains about incompatible combinations of options.     
.IP
Many I/O library bugs were fixed.
Auxiliary I/O has been fixed to be closer to the standard:
\fIclose\fP is a no-op on a non-existent or unconnected unit;
\fIrewind\fP and \fIbackspace\fP are no-ops on an unconnected unit;
\fIendfile\fP opens an unconnected unit.
\fIInquire\fP returns \fBtrue\fP when asked
if units 0-MAXUNIT exist, \fBfalse\fP for other integers; it used to return
\fBfalse\fP for legal but unconnected file numbers
and errors for illegal numbers.
\fIInquire\fP now fills in all requested fields,
even if the file or unit does not
exist or is unconnected.
\fIInquire\fP by unit now correctly returns the unit number.
Most of the formatted I/O input scanning has been rewritten to check
for invalid input.
For example, with an
.I f10.0
format term, the following all used to read as 12.345:
``1+2.345'', ``12.3abc45'', ``12.3.45'', ``12345e1-'';
they now generate errors.
Conversely, the legal datum ``12345-2'' for 12.345 used to be misread
as -1234.52.
The
.I b
format term is now fixed,
and
.I bz
now works for short records.
Reads of short
.B logical
variables no longer overwrite neighboring data
in memory.
Infinite loops in formatted output (an I/O list but no conversion terms
in the format) are now caught,
printing multiple records after the list is exhausted.
In list directed reads, a repeat count, \fIr\fP,
followed by an asterisk and
a space (and no comma) now follows the standard and skips
.I r
list items.
Repeat counts for complex constants now work.
Tabs are now fully equivalent to spaces in list directed input.
There are two new formatting terms,
.I x
for hex
and
.I o
for octal.
The library now attempts to get to the next record if doing an
\fBerr=\fP branch on error;
the standard does not require this,
but it is undesirable to leave the system hanging in mid record.
After input errors, the I/O library now tries to skip to
the next line if there is another read.
This functionality is not required by the standard
and is still not guaranteed to work.
.IP
The Fortran runtime and I/O libraries have several new features.
Many routines and variables have been made static, cutting
the number of symbols defined by the library almost in half.
Many source files have been reorganized to eliminate the loading of
extraneous routines; for example,
the formatted read routines are not loaded
if a program only performs formatted writes.
Standard error is now buffered.
All error processing is now centralized in a single routine, \fIf77_abort\fP.
The \fIf77_abort\fP
routine has been separated from the normal Fortran main routine so
that C code can call Fortran subroutines.  Fortran programs
that abort normally get a core file only if they are loaded with
\fB\-g\fP; the environment variable \fBf77_dump_flag\fP may be used to override
this by setting it to \fIy\fP or \fIn\fP.
The
.I rindex
routine now works as documented.
The C library
.I malloc
and
.I random
routines may now be accessed from Fortran.
.IP
The new VAX math library has been incorporated
and some bugs in calling math library routines have been fixed.
The routine \fId_dprod\fP was added for use with the \fB\-r8\fP flag.
The \fIsinh\fP and \fItanh\fP routines have been deleted as
they are loaded directly from the math library.
The \fIlog10\fP routine from the math library
is now used by \fIr_lg10\fP and \fId_lg10\fP.  
The \fIpow\fP routines now divide by zero when zero is raised
to a negative power so as to generate an exception.
Complex division by zero now generates an error message.
.IP
Appropriately named environment variables now override default file
names and names in open statements;
see ``Introduction to the f77 I/O Library'' for details.
Unit numbers may vary from 0 to 99; the maximum number 
that can be open simultaneously
depends on the system configuration limit
(the library does not check this value).
Namelist I/O similar to that in VMS Fortran has been added to the compiler,
and library routines to implement it have been added to the I/O library.
The documents ``A Portable Fortran 77 Compiler'' and ``Introduction
to the f77 I/O Library'' have been revised to describe these changes.
The new \fIhelp\fP system on the distribution tape in the user contributed
software section contains a large set of help files for f77.
.BP fed
Has been retired to \fI/usr/old\fP.
.BP find
Some new options have been added.
It is now possible to choose users or groups that have no names
by using the \fB\-nouser\fP and \fB\-nogroup\fP options.
The \fB\-ls\fP option provides a built in
.I ls
facility to allow the printing of various file attributes;
it is identical to ``ls \-lgids''.
It is now possible to restrict
.I find
to the file system of the initial path name with the \fB\-xdev\fP option.
A new type, \fB\-type\fP \fIs\fP,
for sockets has been added.
Symbolic links are now handled better.
Globbing is now faster.
.I Find
supports an abbreviated notation, ``find \fIpattern\fP,''
which searches for a pattern in a database
of the system's path names;
this is much faster than the standard method.
.BP finger
Despite numerous changes, \fIfinger\fP still has Berkeley parochialisms.
It has been modified to provide finger information over the network.
Control characters are mapped to their printable equivalents
(e.g. ^X) to avoid trojan horses in
\fI\&.plan\fP and \fI.profile\fP files.
.BP file
.I File
has been extended to recognize sockets,
compressed files (\fI.Z\fP),
and shell scripts.
When it determines that a file is a shell script, 
it tries to discover whether it is a Bourne shell script or a C shell script.
The special bits set user id, sticky, and append-only are also noted.
The value of a symbolic link is now printed.
.BP from
An error message is printed if the requested mailbox cannot be opened.
.BP ftp
Many bugs have been fixed.
New features are:
support for new RFC959 FTP features (such as ``store unique''),
new commands that manipulate local and remote file names
to better support connections to non-UNIX systems,
support for third party file transfers between two 
simultaneously connected remote hosts,
transfer abort support,
expanded and documented initialization procedures (the \fI.netrc\fP file),
and a simple command macro facility.
.BP gprof
Uses \fIsetitimer\fP to discover the clock frequency
instead of looking it up in \fI/dev/kmem\fP.
An alphabetical index printing routine has been added.
A few changes were made to the output format; a new column
indicates milliseconds per call.
.BP groups
Now prints out the group listed in the password file in addition to the 
groups listed in the groups file.
.BP help
Has been superseded by the \fIhelp\fP facility included in the
User Contributed Software.
.BP hostid
Has been extended to take an Internet address or hostname.
.BP indent
Has been completely rewritten;
its default mode now produces programs
somewhat more closely reflecting the local Berkeley style.
.BP install
The \fIchmod\fP in the \fIinstall\fP script
uses \fB\-f\fP so that it does not complain if it fails.
When \fImv\fP'ing and \fIstrip\fP'ing a binary (\fB\-s\fP and not \fB\-c\fP),
the \fIstrip\fP is done before the \fImv\fP to
avoid fragmentation on the destination file system.
.BP iostat
Disk statistics are collected by an alternate clock, if it exists.
Overflow detection has been added to avoid printing negative times.
A call to
\fIfflush\fP was added so that \fIiostat\fP works through pipes and sockets.
Code to handle additional disks was added in the same way as in \fIvmstat\fP.
The header is reprinted when \fIiostat\fP is restarted.
.BP kill
Signal 0 may now be used as documented.
.BP lastcomm
Several bug fixes were installed.
\fILastcomm\fP now understands the revised accounting units.
.BP ld
A list of directories to search for libraries may now be specified on
the command line.
.BP learn
The ``files'' lesson has been updated to reflect the default
system tty conventions for erase and kill characters.
.I Learn
now uses directory access routines so that trash files
can be removed properly between lessons.
.BP leave
Now ignores SIGTTOU and properly handles the +\fIhhmm\fP option.
.BP lex
The error messages have been made more informative.
.BP lint
Tests for negative or excessively large constant shifts were added.
For \fB\-a\fP, warnings for
expressions of type \fBlong\fP that are cast to type \fBvoid\fP
are no longer emitted.
A bug which caused \fIlint\fP to incorrectly report
clashes for the return types of functions has been fixed.
\fILint\fP now understands that \fBenum\fPs are not \fBint\fPs.
The lint description for the C library was updated to reflect
sections two and three of the Programmers Manual more accurately.
Several more libraries in
.I /usr/lib
now have lint libraries.
Changes were made to accommodate the restructuring of the C compiler
for common header files.
.BP lisp
The Berkeley version of Franz Lisp has not been changed much
since the 4.2BSD release.
It has been updated to reflect changes in the C library.
.BP ln
Now prints a more accurate error message
when asked to make a symbolic link into an unwritable directory.
.BP lock
.I Lock
now has a default fifteen minute timeout.
The root password may be used to override the lock.
If an EOF is typed, it is now cleared instead of spinning in a tight
loop until the timeout period.
.BP logger
A new program that logs its standard input using \fIsyslog\fP(3).
.BP login
The environment may be set up by another process that calls \fIlogin\fP.
It now uses the new
.IR getttyent (3)
routines to read \fI/etc/ttys\fP.
.BP lpr
Now supports ``restricted access'' to a printer\- printer use may
be restricted to only those users in a specific group-id.
.BP mail
\fIMail\fP now expects RFC822 headers instead of
the obsolete RFC733 headers.
A \fBretain\fP command has been added.
If the PAGER variable is set in the environment,
it is used to page messages instead of \fImore\fP\|(1).
The \fBwrite\fP command now deletes the entire header
instead of only the first line.
An \fBunread\fP/\fBUnread\fP command (to mark messages as not read) was added.
If \fBReplyall\fP is set,
the senses of \fBreply\fP and \fBReply\fP are reversed.
When editing a different file,
\fImail\fP always prints the headers of the first few messages.
\fIFlock\fP(2) is used for mailbox locking.
Commands ``\fB\-\fP'' and ``\fB+\fP'' skip over deleted messages;
\fBtype\fP\ \fIuser\fP now does a substring match
instead of a literal comparison.
A \fB\-I\fP flag was added
which causes \fImail\fP to assume that input is a terminal.
.BP make
A bug which caused \fImake\fP to run out of file descriptors
because too many files and directories were left open
has been fixed.
Long path names should not be a problem now.
A VPATH macro has been added to allow the user to specify
a path of directories to search for source files.
.BP man
Support for alternate manual directories for \fIman\fP,
\fIapropos\fP and \fIwhatis\fP was added.
A side effect of this is that the \fIwhatis\fP database
was moved to the \fIman\fP directory.
If the source for a manual page is not available,
\fIman\fP will display the formatted version.
This allows machines to avoid storing both formatted and unformatted
versions of the manual pages.
The environment variable MANPATH overrides the default directory \fI/usr/man\fP.
The \fB\-t\fP option is no longer supported.
The printing process has been streamlined by using ``more \-s \fIcatfile\fP''
instead of ``cat \-s \fIcatfile\fP | ul | more \-f''.
Searches of \fI/usr/man/mano\fP are more lenient about file name extensions.
The source for \fIman\fP was considerably cleaned up;
the magic search lists and commands were put at the top of the source file
and the private copy of \fIsystem\fP was deleted.
.BP mesg
So that terminals need not be writable to the world,
.I mesg
only changes the group ``write'' permission.
(Terminals are now placed in group \fItty\fP
so that users may restrict terminal write permission
to programs which are set-group-id \fItty\fP.)
.BP mkdir
Prints a ``usage'' error message
instead of an uninformative ``arg count'' message.
.BP more
Now allows backward scanning.
It will also handle window size changes.
It simulates ``crt'' style erase and kill processing if the terminal
mode includes those options.
.BP msgs
Will no longer update \fI.msgsrc\fP
if the saved message number is out of bounds.
.BP mv
No longer runs
.I cp (1)
to copy a file;
instead it does the copy itself.
.BP netstat
Routes and interfaces for Xerox NS networks are now shown.
The \fB\-I\fP option has been added to specify a particular interface
for the default display.
The \fB\-u\fP option has been added to show UNIX domain information.
Several new mbuf types and statistics are now displayed;
subnetting is now understood.
.BP nice
Is relative as documented, not absolute.
.BP nroff
No longer replaces single spaces with tabs when using the
\fB\-h\fP option.
.BP Pascal
The Pascal compiler and interpreter have been extensively
rewritten so that they will (nearly) pass through \fIlint\fP.
In theory they have not changed from a semantic point of view.
A few bugs have been fixed, and undoubtedly some new ones introduced.
The Pascal runtime support has improved error diagnostics.
Real number input scanning
now corresponds to standard Pascal conventions rather than
those of \fIscanf\fP\|(3S).
.BP passwd
The \fIpasswd\fP program incorporates the functions of
\fIchfn\fP and \fIchsh\fP under \fB\-f\fP and \fB\-s\fP flags. 
Whenever information is changed \fIpasswd\fP also
updates the associated \fIndbm\fP(3X) database used by
\fIgetpwnam\fP and \fIgetpwuid\fP.
Office room and phone numbers are less dependent on Berkeley's usage.
Checks are made for write errors before renaming the password file.
.BP plot
The output device resolution can now be specified using the \fB\-r\fP option.
Support has been added for the Imagen laser printer and the Tektronix 4013.
.BP pr
The buffer is now large enough for 66 x 132 output.
.BP print
Has been retired to \fI/usr/old\fP;
use ``lpr \-p'' instead.
.BP prmail
Has been retired to \fI/usr/old\fP;
use ``Mail -u \fIuser\fP'' instead.
.BP prof
Uses \fIsetitimer\fP to determine the clock frequency
instead of assuming 60 hertz.
.BP ps
Saves static information for faster startup.
It now prints symbolic
values for wait channels.
.BP pti
Has been retired to \fI/usr/old\fP.
.BP ptx
Cleans up after itself and exits with a zero status on successful completion.
.BP quota
Verifies that the system supports quotas before trying to
interpret the quota files.
.BP ranlib
The \fB\-t\fP option updates a library's internal time stamp without
rebuilding the table of contents.
``Old format'' and ``mangled string table'' are now warnings rather
than fatal errors.
Memory allocation is done dynamically.
.BP rcp
For the convenience of system managers, \fIrcp\fP has moved from \fI/usr/ucb\fP
to \fI/bin\fP, hence it can be used without mounting \fI/usr\fP.
Remote user names are now specified as \fIuser\fP@\fIhost\fP instead of
\fIhost.user\fP to support Internet domain hostnames
that contain periods (``.'').
A \fB\-p\fP option has been added that preserves file and directory
modes, access time, and modify time.
It now uses \fIgetservbyname\fP instead of compile time constants.
.BP rdist
A new program that keeps files on multiple machines consistent with
those on a master machine.
.BP refer
The key letter code was fixed so that control characters are not generated.
Several problems that caused the generation of duplicate citations,
particularly with the \fB\-e\fP and \fB\-s\fP options, have been fixed.
EOF on standard input is now properly handled.
\fIRefer\fP folds upper and lower case when sorting.
.BP rlogin
\fIRlogin\fP negotiates with \fIrlogind\fP to determine whether window
size changes should be passed through.
If the remote end is running a 4.3BSD \fIrlogind\fP,
it will agree to accept and pass through SIGWINCH signals
to user processes under its control.
The \fB\-8\fP flag allows an 8-bit path on input.
The \fB\-L\fP flag allows an 8-bit path on output.    
The escape character is now echoed as soon as a second non-command 
character is typed.
A new command character
\fI^Y\fP has been added to suspend only the input end of the
session without stopping output from the remote end
(unless \fBtostop\fP has been set).
The \fIioctl\fP TIOCSPGRP has been changed to \fIfcntl\fP F_SETOWN.
Several changes have been made to reduce the amount of data
sent after an interrupt has been typed, and to avoid flushing data when
changing modes.
.BP rm
The \fB\-f\fP option produces no error messages and exits with status 0.
The problem of running out of file descriptors when doing a recursive
remove have been fixed.
.BP rmdir
Improved error messages, in the same fashion as \fImkdir\fP.
.BP rsh
The \fB\-L\fP, \fB\-w\fP, and \fB\-8\fP flags are ignored so that they may be 
passed along with \fB\-e\fP to rlogin.
.BP ruptime
The \fB\-r\fP flag has been added to reverse sort order.
.BP rwho
Now allows hosts with long names
(greater than 16 characters).
.BP script
Now propagates window size changes.
.BP sed
No longer loops when the first regular expression is null.
.BP sendbug
Allows command line \fB\-D\fP arguments to override built in
defaults for name and host address of the bugs mailing list.
The ``Repeat-By'' field is now optional.
.I Sendbug
now checks the EDITOR environment variable instead of assuming \fIvi\fP.
.BP sh
``#'' is no longer considered a comment character when \fIsh\fP is interactive.
The IFS variable is not imported when \fIsh\fP runs
as root or if the effective user id differs from the real user id.
.BP size
Now exits with the number of errors encountered.
.BP sort
Checks for and exits on write errors.
.BP spell
A couple of trouble-causing words have been removed from \fIspell\fP's
stoplist; \fIe.g.\fP ``reus'' that caused ``reused'' to be flagged.
A few words that \fIspell\fP would not derive have been
removed from the stoplist.
Several hundred words that \fIspell\fP derives without
difficulty from existing words (\fIe.g.\fP ``getting'' from ``get''), or that
\fIspell\fP would accept anyway, \fIe.g.\fP ``1st, 2nd'' etc., have been removed
from \fI/usr/dict/words\fP.
.BP stty
Has been extended to handle window sizes and 8-bit input data paths.
"stty size" prints only the size of the associated terminal.
.BP su
Only members of group 0 may become root.
.BP symorder
Now reorders the string table as well as the name list.
.BP sysline
Now understands how to run in one-line windows and how to
adjust to window size changes.
Numerous small changes have been made in the output format.
.BP systat
A new program that provides a cursed form of \fIvmstat\fP,
as well as several other status displays.
.BP tail
Makes use of a much larger buffer.
.BP talk
The new version of \fItalk\fP has an incompatible
but well-defined protocol that works across a much broader
range of architectures.
The new talk rendezvouses at a new port so that the old version
can still be used during the conversion.
.I Talkd
looks for a writable terminal instead of giving up if a user's
first entry in \fI/etc/utmp\fP is not writable.
Root may always interrupt.
.I Talk
now runs set-group-id to group \fItty\fP so that it is no longer
necessary to make terminals world writable.
.BP tar
Preserves modified times of extracted directories.
The \fB\-B\fP option is turned on when reading from standard input.
Some sections were rewritten for efficiency.
.BP tbl
The hardwired line length has been removed.
.BP tcopy
A new program for doing tape to tape copy of multifile, arbitrarily
blocked magnetic tapes.
.BP tee
\fITee\fP's buffer size was increased.
.BP telnet
\fITelnet\fP first tries to interpret the destination as an address;
if that fails, it is then passed off to \fIgethostbyname\fP.
If multiple addresses are returned, each is tried in turn
until one succeeds, or the list is exhausted.
If a non-standard port is specified, the initial ``Suppress Go Ahead''
option is not sent.
Commands were added to escape the escape character,
send an interrupt command, and send ``Are You There''.
Carriage return is now mapped to carriage return, newline.
.BP tftp
Has many bug fixes.
It no longer loops upon reading EOF from standard input.
Re-transmission to send was added,
as well as an input buffer flush to both send and receive.
.BP tip
Lock files are no longer left lying about after
.I tip
exits,
and the \fIuucp\fP spool directory does not need to be world writable.
A new ``~$'' command sends output from a local program to a remote host.
Alternate phone numbers are separated only by ``,'';
thus several dialer characters that were previously illegal may now be used.
.I Tip
now arranges to copy a phone number argument to a safe place, then zero
out the original version.
This narrows the window in which the phone number
is visible to miscreants using \fIps\fP or \fIw\fP.
Also fixed was a bug that caused the phone number to be written in place of
the connection message.
Carrier loss is recognized and an appropriate disconnect action is taken.
Bugs in calculating time and fielding signals have been fixed.
Several new dialers were added.
.BP tn3270
A new program for emulating an IBM 3270 over a \fItelnet\fP connection.
.BP tp
Memory allocation was changed to avoid \fIrealloc\fP.
.BP tr
Checks for and exits on write errors.
.BP trman
Has been retired to \fI/usr/old\fP.
.BP tset
Can now set the interrupt character.
The defaults have been changed when the interrupt,
kill, or erase characters are NULL.
\fIReset\fP is now part of \fItset\fP.
The window size is set if it has not already been set.
\fITset\fP continues to prompt as long as the terminal type
is unknown.
.BP users
Now much quieter if there are no users logged on.
.BP uucp
Several fixes and changes from the Usenet have been incorporated.
The maximum length of a sitename has been increased from 7 to 14 characters.
.I Uucp
has been changed to understand the new format of \fI/etc/ttys\fP.
Support for more dialers has been added.
.BP vacation
A new program that answers mail while you are on vacation.
.BP vgrind
Has been extended to handle the DEC Western Research Labs Modula-2 compiler 
and \fIyacc\fP.
.BP vlp
Now properly handles indented lines.
.BP vmstat
The \fB\-i\fP flag was added to summarize interrupt activity.
The \fB\-s\fP listing was expanded to include cache hit rates for the
name cache and the text cache.
The standard display has been generalized to allow command line selection
of the disks to be displayed.
A new header is printed after the program is restarted.
If an alternative clock is being used to gather statistics,
it is properly taken into account.
.BP vpr
Has been retired to \fI/usr/old\fP.
.BP w
Users logged in for more than one day have login day and hour listed;
users idle for more than one day have their idle time listed in days.
.BP wall
Will now notify all users on large systems.
.BP whereis
Now also checks \fImanl\fP, \fImann\fP, and \fImano\fP.
.BP which
Now sets prompt before sourcing the user's \fI.cshrc\fP file to ensure
that initialization for interactive shells is done.
.BP whoami
Uses the effective user id instead of the real user id.
.BP window
A new program that provides multiple windows on ASCII terminals.
.BP write
Looks for a writable terminal instead of giving up if a user's
first entry in \fI/etc/utmp\fP is not writable.
Root may always interrupt.
Non-printable escape sequences can no longer be sent
to an unsuspecting user's terminal.
.I Write
now runs set-group-id to group \fItty\fP so that it is no longer
necessary to make terminals world writable.
.BP xsend
Notice of secret mail is now sent with a subject line showing
who sent the mail.
The body of the message includes the name of the machine
on which the mail can be read.
.BP xstr
Now handles multiple-line strings.
