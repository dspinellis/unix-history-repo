.LG
.B
.ce
Berkeley Software for UNIX\(dg on the VAX\(dd
.br
.sp
.ce
4.1bsd version of May, 1981
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
A new version of the \s-2UNIX\s0 system for the \s-2VAX\s0 family of computers
is available from the Computer Systems Research Group
of the University of California at Berkeley.
This is an updated package of software for \s-2UNIX/32V\s0\(dg
licensees, and includes a refined version of the paging kernel for the
\s-2VAX\s0 as well as a large number of other programs.  This document
describes the major differences between standard \s-2UNIX/32V\s0 as distributed
by Western Electric and the May, 1981 distribution known as 4.1BSD.
.PP
The new release may be used in two ways: as a bootstrap system for new hardware
(or to bootstrap systems which were previously
running 3bsd or \s-2UNIX/32V\s0),
or to update a system running the 4bsd release of November, 1980
(now called 4.0bsd).
Hardware configurations supported for booting are described below and in the
document ``Installing and operating up 4.1bsd''.
The things most notable for sites which are updating 4.0bsd to this new release
are:
.IP 1)
Additional hardware support, including a number of new mass storage
peripherals, and especially support for the VAX 11/750.
.IP 2)
Performance enhancement to the paging portion of the system, which
should provide additional throughput in virtual-memory intensive
environments.
.IP 3)
A number of bug fixes.  Most of the bug fixes are minor, but several
irritating bugs were present in 4.0bsd; this distribution, when used
as an update, attempts to correct the known bugs while introducing few
new ones.
.IP 4)
The INGRES data base system, developed at Berkeley by the INGRES project,
is now supplied with the standard system.*
.FS
* Support for INGRES is not, however, available either through our
group or through the INGRES project.  The software is public domain,
and organizations independent of the University of California
exist which will sell INGRES support.
.FE
.br
.ne 10
.sp 4
.LG
.LG
.ce
.B "System facilities"
.NL
.SH
Hardware support
.PP
The system runs on VAX 11/750 and VAX 11/780 processors and supports
the standard DEC mass storage peripherals: RM03, RM05, RP06, RP07, RA80,
RM80, and
RK07 disks; TS11, TE16, TU45, TU77, and TU78 tapes.  DEC standard bad block
handling on disk drives is supported on all the DEC disks except the RP06.
The EMULEX SC21-V UNIBUS
storage module disk controller is supported with AMPEX and CDC 300 Megabyte
disk drives and FUJITSU 160 Megabyte Winchester drives.  The EMULEX
TC-11 tape controller (which emulates a TM-11 DEC UNIBUS controller)
.	\" and System Industries Model 9700 tape drive (which emulates 
.	\" a DEC TU45 controller on the UNIBUS)
.	\" are
is supported with a variety of tape drives such as the KENNEDY 9300.
Any supported disk plus a tape drive is
sufficient to bootstrap the system on either processor.
Two drives are required for operation of the system
using RK07's.  Most RK07 systems have no tape media,
and so we can supply a distribution on two RK07 packs.
.PP
For terminal interfaces, in addition
to the standard (non-DMA) DEC DZ-11 terminal interface, (DMA) DH-11 emulators
are supported for terminal support, such as the ABLE DH/DM (which replaces
the ABLE DMAX) and EMULEX CS-11.
The system also provides support for standard line printer
interfaces emulating the DEC LP-11 and the use of 1200 baud terminals
such as a DECWRITER-III acting as a slow speed printer.  Printer-plotters
such as made by BENSON/VARIAN or VERSATEC are also supported with standard
drivers.
.PP
Unlike previous releases of the system, this release supports any number
of any of these devices.  The devices may be placed arbitrarily on any
available MASSBUS and UNIBUS interfaces.  The system configures at boot
time, locating available devices, using a system configuration compiled
into the kernel.  The configuration description contains all the information
about the topology of the machine and the addresses at which the various
devices are located.  It is possible (and desirable) to write the description
using ``pattern matching'' to only partially specify some of the interconnects.
.PP
The system configuration program sizes system data structures based on
a specification of the maximum number of active users to be present on the
system.  To build a system for a larger or smaller workload you only need
change this single constant.  The system also now initializes the parameters
to the paging system and sizes its file system buffer cache based
on the amount of available memory; it is no longer necessary to adjust
these by hand.
.PP
The system supports access to the 11/780 console floppy disk and
11/750 TU58 console cassette tape.  However, reliable access to the
TU58 cassette
interface is possible only on a totally quiescent system.
.PP
For further information on device support and recommendations
for configuration of VAX systems to run UNIX see
``Hints on configuring a VAX to run UNIX''
by Bob Kridle and Bill Joy.
.SH
System reliability and performance
.PP
The system reboots automatically after hardware
and software failures, running an automatic procedure that recovers
from normal minor disk inconsistencies.  If hardware or software failures
cause unexpected problems on the disks, then an interactive semi-automatic
repair program can be used to fix up the disks.
.PP
The system is fully and transparently demand paged.
As distributed it will support individual
process sizes up to 6 Megabytes each of data and stack area and
6 Megabytes of program.
These numbers can be increased on systems willing to dedicate increased
disk space for paging the process image.
.PP
The default loader format is load-on-demand, and
allows large processes to start quickly.  A \fIvfork\fR system call
allows a large process to execute other processes without copying its
data space.
.PP
The system performance has been enhanced in a number of ways.
Relative to \s-2UNIX/32V\s0, the
basic system overheads have been reduced by tightening up the system
code and improving system data structures.  Disk throughput has
been increased by increasing the logical block size on the disks to 1024
bytes.*
File system performance may also be increased, beyond that normally
obtained with \s-2UNIX/32V\s0 or the 4.0bsd release, by properly interleaving
the file systems to account for device speed; instructions on this
and proper interleaving constants are given in the setup instructions
for the system and in the
\fImkfs\fR\|(8) manual page.\(dg
.FS
\(dg Systems that run \s-2UNIX/32V\s0 convert to the new format
by saving files using the tape archiver and reading them into the new
system.
Because of the format changes in the file system and because some of the
changes described here required recompilation of all programs, a bootstrap
tape and all programs are distributed.
.FE
System algorithms such as the swapping and file system caching algorithms
have also been improved to increase system performance.
.PP
Since the 4.0bsd (previously known as 4bsd) distribution of November, 1980,
performance of the system under heavy paging load has been substantially
improved by correcting a problem with placement of pre-paged pages.
The system now pre-pages more data, greatly benefiting processes which have
locality in their behavior.  System degradation due to pre-paging
and lock-out of other processes during heavy paging has been greatly reduced
(even though more pre-paging is done).\(dg
.FS
\(dg The actual change here is to place the pre-paged pages at the
bottom of the memory free list, rather than putting them into the
systems global ``clock'' replacement loop.  This is a very minor
change, but has significant performance implications.
.FE
.PP
Since the 4.0bsd release, facilities have also been added for processes
which serially reference large amounts of virtual memory to inform
the system of this kind of paging behavior.  This helps the system
to deal with these kinds of processes, which are not as well served by
using the current default paging algorithm.  Processes which are known to
need only a small amount of memory but which tend to accumulate large amounts
of memory due to strange page referencing patterns may declare a soft limit
on the amount of memory to be used.\(dd
.FS
\(dd If the system needs memory and there are processes which are over
their declared limits, the system tends to take pages from these processes
first.
.FE
.PP
For further information on system performance and recent measurements see
the (revised) paper
``Performance of UNIX on the VAX''
by Bill Joy.
.SH
What this distribution does not contain
.PP
A number of new system facilities are under development at Berkeley
and will be included in future releases of the system.  We mention
them here mostly to point out that they are not in this system:
.IP 1)
Arpanet TCP/IP support.  This is being developed at BBN, and will
be integrated into the system very shortly; if you need this right away, you
should contact Alan Nemeth at BBN.
.IP 2)
Local network support.
.IP 3)
Inter-process communication support, providing facilities for user
processes to take advantage of the network support and for writing
cooperating user processes.
.IP 4)
Shared-segment access to large files, mapped in virtual memory.
.IP 5)
Performance changes for the file system to provide a higher access rate
for applications which are currently i/o bound.
of very large scale image processing.
.PP
Development of the system in these areas is currently underway.
The facilities should become available in roughly the order listed above.
The design for these new facilities is spelled out in other documents
available from our group.
.br
.SH
.ne 10
.sp 4
.LG
.LG
.ce
User Software
.NL
.sp
.PP
The following sections detail additional user-level software
available with this distribution; we describe only software which is
not part of the \s-2UNIX/32V\s0 distribution.
Full documentation and source for this (and all supplied)
software is made available with the distribution.
.SH
Languages for the VAX
.PP
Interpreters for \s-2APL\s0, \s-2LISP\s0 and both an interpreter
and compiler for Pascal.
The \s-2APL\s0 interpreter is the \s-2PDP-11\s0 version,
moved to the \s-2VAX\s0 and has not been extensively used.*
.FS
* A number of groups are working on improved versions
of the \s-2APL\s0 interpreter, and we hope to obtain a better
version of \s-2APL\s0 soon.
.FE
The \s-2LISP\s0 system, known as ``Franz Lisp'', is written in C and
\s-2LISP\s0, includes both an interpreter and a compiler,
and is compatible with a large subset of \s-2MACLISP\s0.
The Pascal system is the instructional system that has been distributed
previously for \s-2PDP\-11's\s0.  The language implemented is
standard Pascal.  The implementation
features excellent diagnostics, and allows
separate compilation and use of C and FORTRAN procedures
with full type checking.
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
Also included are several other useful packages including
programs to simulate the phototypesetter on 200 bpi dot-matrix plotters
(these programs were moved from the \s-2PDP\-11\s0 to the \s-2VAX\s0 and
many fonts available on the Arpanet have been converted
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
