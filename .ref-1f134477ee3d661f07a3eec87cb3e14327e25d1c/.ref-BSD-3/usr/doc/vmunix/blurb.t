.LG
.B
.ce
Berkeley Software for UNIX\(dg on the VAX\(dd
.br
.ce
\s-4(The Third Berkeley Software Distribution)\s0
.R
.NL
.sp .75i
.PP
.FS
\(dg\s-2UNIX\s0 and \s-2UNIX/32V\s0 are trademarks of Bell Laboratories.
.FE
.FS
\(dd\s-2VAX\s0 and \s-2PDP\s0 are trademarks of Digital Equipment Corporation.
.FE
A new package of software for \s-2UNIX\s0 will be available from the Computer
Science Division of the University of California at Berkeley in early 
December, 1979.  This is a
package of software for \s-2UNIX/32V\s0\(dg licensees, and includes a paged
version of the kernel for the \s-2VAX\s0, as well as a large number of
other programs.  The tape includes:
.SH
New Languages for the VAX
.PP
Interpreters for \s-2APL\s0, \s-2LISP\s0 and Pascal.
The \s-2APL\s0 interpreter is the \s-2PDP-11\s0 version,
moved to the \s-2VAX\s0.
The \s-2LISP\s0 system, known as ``Franz Lisp'', is written in C and
\s-2LISP\s0, includes both an interpreter and a compiler,
and is compatible with a large subset of \s-2MACLISP\s0.
The Pascal system is the instructional system which has been distributed
previously for \s-2PDP\-11's\s0\(dd.  The language implemented is very close
to standard Pascal, and features
excellent diagnostics, and a source level execution profiling facility.
.SH
New System Facilities
.PP
The system is now fully and transparently demand paged.
As distributed it will support individual
process sizes up to 8M of data and 4M of program.
These numbers can be increased to 16M bytes of data and stack
and 16M bytes of program easily given the availability of a
reasonable amount of disk space to which to page.
Description is given of steps necessary to further increase
these limits.
.PP
A new load-on-demand
format allows large processes to start quickly.  A \fIvfork\fR system call
allows a large process to execute other processes without copying its
data space.
Virtual versions of the \fIread\fR and \fIwrite\fR system calls
known as \fIvread\fR and \fIvwrite\fR permit fast random access to large
files, fetching data pages as needed, and rewriting only changed pages.
The system supports UNIBUS disk drives, and can access and update files
on the console's floppy disk drive.
.SH
A display editor
.PP
The tape includes the display editor,
.I vi,
(vee-eye) which runs on a large number of intelligent and unintelligent
display terminals.
This editor uses a terminal description data base and a library
of routines for writing terminal independent programs which is also supplied.
The editor has a mnemonic command set which is easy to learn and remember,
and deals with the hierarchical structure of documents in a natural way.
Editor users are protected against loss of work if the system crashes,
and against casual mistakes by a general
.I undo
facility as well as visual feedback.
The editor is quite usable even on low speed lines and dumb terminals.
.SH
Command and mail processing programs
.PP
The tape also includes a new command processor
.I csh
which caters to interactive users by providing a history mechanism so
that recently given
commands can be easily repeated.  The shell also
has a powerful macro-like aliasing facility which can be used to
tailor a friendly, personalized, command environment.
A new interactive mail processing command
supports items such as subject and carbon copy fields, and
distribution lists, and makes it convenient to deal with
large volumes of mail.
.SH
Better debugger support
.PP
A version of the symbolic debugger
.I sdb
is provided which now can be used to debug
FORTRAN 77 programs.  The assembler has been rewritten and the C compiler
modified to reduce greatly the overhead of using the symbolic debugger.
.SH
Other software
.PP
Also included are a number of other useful packages including
the circuit analysis program \s-2SPICE\s0,
programs to simulate the phototypesetter on 200 bpi dot-matrix plotters
(these programs were moved from the \s-2PDP\-11\s0 to the \s-2VAX\s0 and
a large number of fonts available on the ARPANET have been converted
to the required format),
a bulletin board program, routines for data compression,
a modified version of the standard I/O library permitting
simultaneous reads and writes, a slow-speed network for
connecting heterogeneous
UNIX systems at low cost (1 tty port per connection per machine and
no system changes), and a new, flexible macro package for 
.I nroff
and
.I troff
called
.I \-me.
.PP
.PP
Source code, binaries and machine readable versions of all
documentation are included with the tape.
We supply the magnetic tape on which the software is written.
.PP
To receive the tape make two additional copies of the
the attached agreement, sign and return 2 of the 3 copies with
a \fBcheck\fR for $200 U.S. payable to ``Regents, University of
California,'' and a copy of your \s-2UNIX/32V\s0
license agreement to:
.DS
Berkeley Software Distribution for UNIX
c/o Keith Sklower
Computer Science Division, Department of EECS
Evans Hall
University of California, Berkeley
Berkeley, California  94720
.DE
We will return a fully executed copy of the agreement to you with the
distribution.
.PP
Included with the tape will be two volumes of documentation.  The first is
a programmers manual (similar to \s-2UNIX/32V\s0 Volume 1) modified
and updated to correspond accurately to the distributed system.
The second is a volume of documents (Volume 2C) similar to the two standard
volumes (2A and 2B) describing the major packages on the tape.
.PP
If you have questions about this tape they can be directed to Keith Sklower at
the address above or at (415) 642-4972 or leave messages for Keith at 642-1024.
