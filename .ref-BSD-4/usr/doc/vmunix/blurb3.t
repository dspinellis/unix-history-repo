.LG
.B
.ce
Berkeley Software for UNIX\(dg on the VAX\(dd
.br
.ce
(\s-2The Fourth Berkeley Distribution Tape, November 1980\s0)
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
.PP
A new package of software for \s-2UNIX\s0 will be available from the Computer
Science Division of the University of California at Berkeley in early November
1980.  This is an updated package of software for \s-2UNIX/32V\s0\(dg
licensees, and includes a refined version of the paging kernel for the
\s-2VAX\s0 as well as a large number of other programs.  This document
describes the major differences between standard \s-2UNIX/32V\s0 as distributed
by Western Electric and the November 1980 distribution known as 4BSD.
4BSD includes:
.SH
Languages for the VAX
.PP
Interpreters for \s-2APL\s0, \s-2LISP\s0 and both an interpreter
and compiler for Pascal.
The \s-2APL\s0 interpreter is the \s-2PDP-11\s0 version,
moved to the \s-2VAX\s0.
The \s-2LISP\s0 system, known as ``Franz Lisp'', is written in C and
\s-2LISP\s0, includes both an interpreter and a compiler,
and is compatible with a large subset of \s-2MACLISP\s0.
The Pascal system is the instructional system that has been distributed
previously for \s-2PDP\-11's\s0\(dd.  The language implemented is
standard Pascal.  The implementation
features excellent diagnostics, and allows
separate compilation with full type checking.
.SH
New System Facilities
.PP
The system is now fully and transparently demand paged.
As distributed it will support individual
process sizes up to 6M each of data and stack area and 6M of program.
These numbers can be increased on systems willing to dedicate increased
disk space for paging the process image.
.PP
A new load-on-demand
format allows large processes to start quickly.  A \fIvfork\fR system call
allows a large process to execute other processes without copying its
data space.
The system supports access to the console floppy disk,
and large UNIBUS disk drives.  It reboots automatically after hardware
and software failures, running an automatic procedure that recovers
from normal minor disk inconsistencies.  If hardware or software failures
cause unexpected problems on the disks, then a interactive semi-automatic
repair program can be used to fix up the disks.
.SH
System performance enhancements
.PP
The system performance has been enhanced in many ways.
Basic system overheads have been reduced by tightening up the system
code and improving system data structures.  Disk throughput has
been increase by increasing the logical block size on the disks to 1024
bytes.  Systems that run \s-2UNIX/32V\s0 convert to the new format
by saving files using the tape archiver and reading them into the new
system.  System algorithms such as the swapping and cacheing algorithms
have also been improved to increase system performance.
.PP
Because of the format changes in the file system and because some of the
changes described here required recompilation of all programs, a bootstrap
tape and all programs are distributed.
.SH
A display editor
.PP
The tape includes the display editor,
.I vi,
(vee-eye) that runs on over 100 different intelligent and unintelligent
display terminals.
This editor uses a terminal description data base; a library
of routines for writing terminal independent programs using
this data base is also supplied.
The editor has a mnemonic command set that is easy to learn and remember,
and deals with the hierarchical structure of documents in a natural way.
Editor users are protected against loss of work if the system crashes,
and against casual mistakes by a general
.I undo
facility as well as visual feedback.
The editor is usable even on low speed lines and dumb terminals.
.SH
Command and mail processing programs
.PP
The tape also includes a new command processor
.I csh
that caters to interactive users by providing a history mechanism so
that recently given
commands can be easily repeated.  The shell also
has a powerful macro-like aliasing facility that can be used to
tailor a friendly, personalized, command environment.
A new interactive mail processing command
supports items such as subject and carbon copy fields, and
distribution lists, and makes it convenient to deal with
large volumes of mail.
.SH
Job control facilities
.PP
The system now supports the multiplexing of terminals between jobs.
It is no longer necessary to decide in advance that a job is to be
run in the foreground or background; running jobs may be moved from
the foreground to the background and vice-versa, and mechanisms exist
in the C shell 
.I csh
for arbitrating the terminal between the active jobs.
.SH
Debugger support
.PP
A version of the symbolic debugger
.I sdb
is included in the distribution that can be used to debug
Pascal, C, and FORTRAN 77 programs.
The assembler has been rewritten and the C compiler
modified to reduce greatly the overhead of using the symbolic debugger.
.SH
Other software
.PP
Also included are a several other useful packages including
programs to simulate the phototypesetter on 200 bpi dot-matrix plotters
(these programs were moved from the \s-2PDP\-11\s0 to the \s-2VAX\s0 and
many fonts available on the ARPANET have been converted
to the required format),
a bulletin board program, routines for data compression,
a slow-speed network for connecting heterogeneous
UNIX systems at low cost (1 tty port per connection per machine and
no system changes), and a new, flexible macro package for 
.I nroff
and
.I troff
called
.I \-me.
