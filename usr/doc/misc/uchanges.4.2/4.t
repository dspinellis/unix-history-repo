.SH
.LG
.ce
Section 3
.SM
.sp
.PP
The section 3 documentation has been reorganized to group manual
entries by library.  Introductory sections for each logical and
physical library contain lists of the entry points in the library.
.PP
A number of routines which had been system calls under 4.1BSD are
now user-level library routines in 4.2BSD.  These routines have
been grouped under section ``3C'' headings, ``C'' for compatibility.
Further, certain routines present in the standard C run-time
library which do not easily categorize as part of one of the standard
libraries, have been group under ``3X'' headings.
.BP curses
A number of bug fixes have been incorporated,
and the documentation has been revised.
.BP stdio
The standard i/o library has been modified to block i/o
operations to disk files according to the block size of
the underlying file system.  This is accomplished using the new
\fIst_blksize\fP value returned by \fIfstat\fP.  The resultant
performance improvement is significant as the old 1 kilobyte
buffer size often resulted in 7 memory-to-memory
copy operations by the system on 8 kilobyte block file systems.
.IP
End-of-file marks now ``stick''.  That is, all input requests
on a stdio channel after encountering end-of-file will return
end-of-file until a \fIclearerr\fP call is made.  This has
implications for programs which use stdio to read from a terminal
and do not process end-of-file as a terminating keystroke.
.IP
Two new functions may be used to control i/o buffering.
The \fIsetlinebuf\fP routine is used to change \fBstdout\fP
or \fBstderr\fP from block buffered or unbuffered to line
buffered. 
The \fIsetbuffer\fP routine is an alternate form of \fIsetbuf\fP
which can be used after a stream has been opened, but before
it is read or written.
.BP bstring
Three new routines, \fIbcmp\fP, \fIbcopy\fP, and \fIbzero\fP
have been added to the library.  These routines use the VAX
string instructions to manipulate binary byte strings of
a known size.
.BP ctime
Now uses the \fIgettimeofday\fP system call and supports
time conversion in six different time zones.  Daylight
savings calculations are also performed in each time zone
when appropriate.
.BP isprint
Now considers space a printing character; as the manual page
has always indicated.
.BP directory
Is a new directory interface package which provides a portable
interface to reading directories.  A version of this library
which operates under 4.1BSD is also available.
.BP getpass
Now properly handles being unable to open /dev/tty.
.BP getwd
Has been moved from the old jobs library to the standard
C run-time library.  It now returns an error string rather
than printing on the standard error when unable to decipher
the current working directory.
.BP perror
Now uses the writev system call to pass all its
arguments to the system in a single system call.  This
has profound effects on programs which transmit error
messages across a network.
.BP psignal
And sys_siglist are routines for printing signal names
in an equivalent manner to perror.
.BP qsort
Has been greatly sped up by choosing a random element
with which to apply its divide and conquer algorithm.
.BP random
Is a successor to rand which generates much better random
numbers.  The old rand routine is still available and most
programs have not been switched over to random as doing so
would make certain facilities such encrypted mail unable
to operate on existing data files.
.BP setjmp
And longjmp now save and restore the signal mask so that
non-local exit from a signal handler is transparent.  The
old semantics are available with _setjmp and _longjmp.
.BP net
Is a new set of routines for accessing database files 
for the DARPA Internet.  Four databases exist: one for
host names, one
for network names, one for protocol numbers, and one for
network services.  The latter returns an Internet
port and protocol to be used in accessing a given
network service.
.IP
An additional collection of routines, all prefaced
with ``inet_'' may be used to manipulate Internet
addresses, and interpret and convert between
Internet addresses and ASCII
representations in the Internet standard ``dot'' notation.
.IP
Finally, routines are available for converting
16 and 32 bit quantities between host and network
order (on high-ender machines these routines are
defined to be noops).
.BP fstab
The routines for manipulating /etc/fstab have been
rewritten to return arbitrary length null-terminated
strings.
