.LG
.B
.ce
Getting started with...
.sp
.ce
Berkeley Software for UNIX\(dg on the VAX\(dd
.br
.ce
\s-4(The third Berkeley Software Distribution)\s0
.R
.NL
.sp .75i
.PP
.FS
\(dg\s-2UNIX\s0 is a trademark of Bell Laboratories.
.FE
.FS
\(dd\s-2VAX\s0 is a trademark of Digital Equipment Corporation.
.FE
A package of software for \s-2UNIX\s0 developed at the Computer
Science Division of the University of California at Berkeley is
installed on our system.  This package includes a new version of the
operating system kernel which supports a virtual memory, demand-paged
environment.  While the kernel change should be transparent to most
programs, there are some things you should know if you plan to run
large programs to optimize their performance in a virtual memory
environment.  There are also a number of other new programs which are
now installed on our machine; the more important of these
are described below.
.SH
Documentation
.PP
The new software is described in two new volumes of documentation.  The first
is a new version of volume 1 of the \s-2UNIX\s0 programmers manual
which has integrated manual pages for the distributed software and
incorporates changes to the system made while introducing the virtual
memory facility.  The second volume of documentation is numbered volume
2c, of which this paper is a section.
This volume contains papers about programs which are in the distribution
package.
.SH
Where are the programs?
.PP
Most new programs from Berkeley reside in the directory
.B /usr/ucb.
A major exception is the C shell,
.I csh,
which lives in
.B /bin.
We will later describe how you can arrange for the programs
in the distribution to be part of your normal working environment.
.SH
Making use of the Virtual Memory
.PP
With a virtual memory system, it is no longer necessary for running programs to
be fully resident in memory.  Programs need have only a subset
of their address space resident in memory, and pages which are not
resident in memory will be
.I faulted
into memory if they are needed.
This allows programs larger than memory to run, but also places a penalty
on programs which do not exhibit
.I locality.
It is important to structure large programs so that at any one time
they are referring to as small a number of pages as possible.
.PP
If you are going to create very large programs, then you should know about
a new demand load format.  This format causes large programs to begin execution
more rapidly, without loading in all the pages of the program before execution
begins.  It is most suitable for programs which are large (say > 50K bytes
of program and initialized data),
and especially when a program has a large number of facilities,
not all of which are used in any one run.
To create an executable file with this format, you use the
.B \-z
loader directive; thus you can say ``cc \-z ...'' or ``f77 \-z ...''.
The
.I file
command will show such files to be ``demand paged pure executable''
files.
See the manual page for
.I ld
in section 1 and
.I a.out
in section 5 of volume 1 of the manual for more information.
.PP
If you have or are writing
a large program which creates new processes as children,
then you should know about
.B vfork
system call.
The
.B fork
system call creates a new process by copying the data space of
the parent process to create a child process.  In a virtual environment
this is very expensive.  
.B Vfork
allows creation of a new process without copying the parent's address
space by letting the parent execute in the child's system context.
The parent can set up the input/output for the child and then return
to its own context after a call to
.B exec
or
.B _exit.
If you use the standard I/O routine
.B system()
to execute commands from within your programs, then
.B vfork
will be used automatically.
If you have been calling
.B fork
yourself,
you should read the manual page for
.B vfork
and use it when you can.
.PP
In order that efficient random access be permitted in a portable way to
large data files, a pair of new system calls has been added:
.B vread
and
.B vwrite.
These calls resemble the normal \s-2UNIX\s0
.B read
and
.B write
calls, but are potentially much more efficient for sparse and random
access to large data files.
.B Vread
does not cause all the data which is virtually read to be immediately
transferred to your address space.  Rather, the data can be fetched
as required by references, at the systems discretion.  At the point of the
.B vread,
the system merely computes the disk block numbers of the corresponding
pages and stores these in the page tables.  Faulting in a page from
the file system is thus no more expensive than faulting in a page from
the paging device.  In both cases all the mapping information is immediately
available or can be easily computed from incore information.
.B Vwrite
works with
.B vread
to allow efficient updating of large data which is only partially
accessed, by rewriting to the file only those pages which have been modified.
.PP
Downward compatibility to non-virtual systems is achieved by the
fact that
.B read
and
.B write
calls have the same semantics as
.B vread
and
.B vwrite
calls; only the efficiency is different.
If you have programs which access large files, and do so sparsely,
read the manual pages for
.B vread
and
.B vwrite
in section 2 of volume 1 of the manual.
.SH
File System blocksize changes
.PP
The size of blocks in the file system has been changed to improve the
throughput of the disks.  The constant ``512'' is not the ``best'' size
to use when reading/writing the disk; rather you should use ``BUFSIZ''
blocks as defined in the include file <stdio.h>.
Because this constant has been changed
.B
all old \&.o files must be removed and then recreated.
.R
They will not load successfully, since the distributed
library routines assume that BUFSIZ is 1024 (its current value at
our installation).
Old executable images may be preserved by running the command
.DS
1kfix file
.DE
on each such file.
Note that this only works for ``a.out'' type files, not ``.o's.''
It is recommended that all old programs be recompiled to take
advantage of the larger disk block size.
.SH
New Languages for the VAX
.PP
There are now available interpreters for \s-2APL\s0 and Pascal
for the \s-2VAX\s0, and a \s-2LISP\s0 system supporting a dialect of
\s-2LISP\s0 compatible with a large subset of \s-2MACLISP\s0.
The \s-2APL\s0 interpreter is the \s-211\s0 version, moved to the \s-2VAX\s0,
and now has a large workspace capability (but
has not been extensively used.)
The Pascal system has been used extensively for instruction and research
and is the same system which was available on the PDP-11.  The only
limitations of the Pascal system are a maximum of 32K bytes per stack
frame (due to the implementation of the interpreter), and 64K bytes per
variable allocated with
.I new.
Essentially arbitrary sized programs can be run with the system,
which supports a very standard Pascal with no language extensions.
The Pascal system features very good error diagnostics, and
includes a source level execution profiling facility.\(dg
.FS
\(dg A compiler for Pascal based on this system is currently being developed,
but is not part of this distribution.
.FE
.PP
The \s-2LISP\s0 system, ``Franz Lisp'', was developed at Berkeley as part
of a project to move the \s-2MIT\s0 \s-2MACSYMA\s0 system from
the \s-2PDP-10\s0 to the \s-2VAX\s0.  A compiler
.I liszt
for Franz Lisp, written at Bell Laboratories, is also included with the system.
.PP
For more information about
\s-2APL\s0 refer to its manual page in volume 1 of the manual.
The Pascal system consists of the programs
.B pi,
.B px,
.B pix,
.B pxp,
.B pxref,
and
.B pic,
all of which are documented in section 1 of volume 1 of the manual.
There is also a paper introducing the system in volume 2c.
The \s-2LISP\s0 system is described in
.I "The Franz Lisp Manual"
in volume 2c of the manual.
.SH
A display editor \- \fIvi\fR
.PP
The system includes the latest version of the display editor
.I vi
which runs on a large number of intelligent and unintelligent display terminals.
This editor runs using a terminal description data base and a library
of routines for writing terminal independent programs which is also supplied.
The editor has a mnemonic command set which is easy to learn and remember,
and deals with the hierarchical structure of documents in a natural way.
Editor users are protected against loss of work if the system crashes,
and against casual mistakes by a general
.I undo
facility as well as visual feedback.
The editor is quite usable even on low speed lines and dumb terminals.
.PP
For users who prefer line oriented editing, the
.I ex
command enters the same editor, but in a line oriented editing mode.
For beginners who have never used a line editor before,
there is a version of the editor known as
.I edit
which has a well-written tutorial introducing it.
.PP
For more information about
.I edit
see
.I "Edit: a Tutorial"
in volume 2c of the manual.
The line editor features are described in the
.I "Ex Reference Manual"
which is in volume 2c of the manual.
Also in volume 2c are
.I "An Introduction to Display Editing with Vi"
and a
.I vi
reference card.
.SH
Command and mail processing programs
.PP
There is also a new command processor
.I csh
which caters to interactive users by providing a history list of
recent commands, which can be easily repeated.  The shell also
has a powerful macro-like aliasing facility which can be used to
tailor a friendly command environment.
.I Csh
is implemented so that both it and the standard shell
.B /bin/sh
can be run on the same system.
.PP
The
.I "Introduction to the C shell"
introduces the shell.  If you have used the standard shell, then
you should especially read about the
.I history
and
.I alias
mechanisms of the shell.
.PP
In order that the manual distributed with the tape correspond to the
commands which are available to you, the default execution search
path is
.DS
PATH=:/usr/ucb:/bin:/usr/bin
.DE
in the language of
.B /bin/sh
or
.DS
setenv PATH :/usr/ucb:/bin:/usr/bin:
set path=(. /usr/ucb /bin /usr/bin)
.DE
in the language of
.B /bin/csh.
.PP
For sending and receiving mail,
a new interactive mail processing command provides a hospitable environment,
supporting items such as subject and carbon copy fields, and allowing creation
of distribution lists.  This command also has a mail reading mode which
makes it convenient to deal with
large volumes of mail.
See the manual page for
.I mail
in section 1, volume 1 of the manual, and the
.I "Mail reference Manual"
in volume 2c of the manual for more details.
.SH
Better debugger support
.PP
A version of the symbolic debugger
.I sdb
is provided which now can debug
FORTRAN 77 programs.  The assembler has been rewritten and the C compiler
modified to reduce greatly the overhead of using the symbolic debugger,
making it much more feasible for heavy use.
If you are interested, then you should read the new document for
.I sdb,
provided in volume 2c.
.SH
Other software
.PP
Other new programs include 
programs to simulate the phototypesetter on 200 bpi plotters,
a common system messages facility, routines for data compression,
a modified version of the standard I/O library permitting
simultaneous reads and writes, a network for connecting heterogeneous
UNIX systems at low cost (1 tty port per connection per machine and
no system changes), and a new, flexible macro package for n/troff
.I \-me.
New command
.B whatis
and
.B apropos
can be used to identify programs and to locate commands based on keywords.
Try
.DS
cd /usr/ucb
whatis *
.DE
and to find out about Pascal:
.DS
apropos pascal
.DE
.SH
Monitoring the new system
.PP
If you want to see what is happening in the new system, you can use the
new
.B vmstat
command, described in section 1 of the manual, which shows the current
virtual load on the system.  The system recomputes the information printed
by
.B vmstat
every five seconds, so a ``vmstat 5'' is a good command to try.
.PP
To see what processes are active virtual processes, you can do
.DS
ps av
.DE
The command
.DS
ps v
.DE
will print only the active processes which you are running.
