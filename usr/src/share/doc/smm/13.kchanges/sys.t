.\" Copyright (c) 1986 Regents of the University of California.
.\" All rights reserved.  The Berkeley software License Agreement
.\" specifies the terms and conditions for redistribution.
.\"
.\"	@(#)sys.t	1.7 (Berkeley) 4/11/86
.\"
.NH
Changes in the kernel proper
.PP
The next several sections describe changes in the parts of the kernel
that reside in \fB/sys/sys\fP.
This section summarizes several of the changes that impact
several different areas.
.NH 2
Process table management
.PP
Although the process table has grown considerably since its original
design, its use was largely the same as in its first incarnation.
Several parts of the system used a linear search of the entire
table to locate a process, a group of processes, or group of processes
in a certain state.
4.2BSD maintained linkages between the children of each parent process,
but made no use of these pointers.
In order to reduce the time spent examining the process table,
several changes have been made.
The first is to place all process table entries onto one of three 
doubly-linked lists,
one each for entries in use by existing processes (\fIallproc\fP),
entries for zombie processes (\fIzombproc\fP), and free entries 
(\fIfreeproc\fP).
This allows the scheduler and other facilities that must examine all
existing processes to limit their search to those entries actually in use.
Other searches are avoided by using the linkage among the children
of each process and by noting a range of usable process ID's when searching
for a new unique ID.
.NH 2
Signals
.PP
One of the major incompatibilities introduced in 4.2BSD
was that system calls interrupted by a caught signal were restarted.
This facility, while necessary for many programs that use signals to drive
background activities without disrupting the foreground processing,
caused problems for other, more naive, programs.
In order to resolve this difficulty, the 4.2BSD signal model has been
extended to allow signal handlers to specify whether or not the signal
is to abort or to resume interrupted system calls.
This option is specified with the \fIsigvec\fP call used to specify the handler.
The \fIsv_onstack\fP field has been usurped for a flag field,
with flags available to indicate whether the handler should be invoked
on the signal stack and whether it should interrupt pending system calls
on its return.
As a result of this change, those system calls that may be restarted
and that therefore take control over system call interruptions
must be modified to support this new behavior.
The calls affected in 4.3BSD are \fIopen\fP, \fIread\fP/\fIwrite\fP,
\fIioctl\fP, \fIflock\fP and \fIwait\fP.
.PP
Another change in signal usage in 4.3BSD affects fewer programs
and less kernel code.
In 4.2BSD, invocation of a signal handler on the signal stack caused
some of the saved status to be pushed onto the normal stack before
switching to the signal stack to build the call frame.
The status information on the normal stack included the saved PC and PSL;
this allowed a user-mode \fIrei\fP instruction to be used in implementing
the return to the interrupted context.
In order to avoid changes to the normal runtime stack when switching to
the signal stack, the return procedure has been changed.
As the return mechanism requires a special system call for restoring
the signal state, that system call was replaced with a new call,
\fIsigreturn\fP, that implements the complete return to the previous context.
The old call, number 139, remains in 4.3BSD for binary compatibility with
the 4.2BSD version of \fIlongjmp\fP.
.NH 2
Open file handling
.PP
Previous versions of
.UX
have traditionally limited each process
to at most 20 files open simultaneously.
In 4.2BSD, that limit could not be increased past 30,
as a 5-bit field in the page table entry was used to specify
either a file number or the reserved values PGTEXT or PGZERO
(fill from text file or zero fill).
However, the file mapping facility that previously used this field
no longer existed, and its replacement is unlikely to require this low
limit.
Accordingly, the internal virtual memory system support for mapped files
has been removed and the number of open files increased.
The standard limit is 64, but this may easily be increased if sufficient
memory for the user structure is provided.
In order to avoid searching through this longer list of open files
when the actual number in use is small,
the index of the last used open file slot is maintained in the field
\fIu.u_lastfile\fP.
The routines that implement open and close or implicit close (\fIexit\fP
and \fIexec\fP) maintain this field, and it is used whenever
the open file array \fIu.u_ofile\fP is scanned.
.NH 2
Niceness
.PP
The values for \fInice\fP used in 4.2BSD and previous systems
ranged from 0 though 39.
Each use of this scheduling parameter offset the actual value by the default,
NZERO (20).
This has been changed in 4.3BSD to use a range of -20 to 20,
with NZERO redefined as zero.
.NH 2
Software interrupts and terminal multiplexors
.PP
The DH11 and DZ11 terminal multiplexor handlers had been modified
to use the hardware's received-character silo
when those devices were used by the Berknet network.
In order to avoid stagnation of input characters and slow response to input
during periods of reduced input,
the low-level software clock interrupt handler had been made to call
the terminal drivers to drain input.
When the clock rate was increased in 4.2BSD, the overhead of checking
the input silos with each clock tick was increased,
and the use of specialized network hardware reduced the need for this
optimization.
Therefore, the terminal multiplexors in 4.3BSD use per-character
interrupts during periods of low input rate, and enable the silos
only during periods of high-speed input.
While the silo is enabled, the routine to drain it runs less 
frequently than every clock tick; it is scheduled
using the standard timeout mechanism.
As a result, the software clock service routine
need not to be invoked on every clock tick, but only when timeouts or
profiling require service.
