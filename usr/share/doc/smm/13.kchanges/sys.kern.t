.\" Copyright (c) 1986 Regents of the University of California.
.\" All rights reserved.  The Berkeley software License Agreement
.\" specifies the terms and conditions for redistribution.
.\"
.\"	@(#)sys.kern.t	1.7 (Berkeley) 4/11/86
.\"
.EQ
delim $$
.EN
.ne 1i
.NH 2
Changes in initialization and kernel-level support
.PP
This section describes changes in the kernel files in \fB/sys/sys\fP
with prefixes
\fIinit_\fP or \fIkern_\fP.
.XP init_main.c
Several subsystems have new or renamed initialization routines that are called
by \fImain\fP.
These include \fIpqinit\fP for process queues,
\fIxinit\fP for the text table handling routines,
and \fInchinit\fP for the name translation cache.
The virtual memory startup \fIsetupclock\fP has been replaced by \fIvminit\fP,
that also sets the initial virtual memory limits for process 0 and its
descendants.
Process 1, \fIinit\fP, is now created before process 2, \fIpagedaemon\fP.
.XP init_sysent.c
In addition to entries for the two system calls new in 4.3BSD,
the system call table specifies a range of system call numbers
that are reserved for redistributors of 4.3BSD.  Other unused slots
in earlier parts of the table should be reserved for future Berkeley use.
Syscall 63 is no longer special.
.XP kern_acct.c
The process time accounting file in 4.2BSD stored times in seconds
rather than clock ticks.
This made accounting independent of the clock rate, but was too large
a granularity to be useful.
Therefore, 4.3BSD uses a smaller but unvarying unit for accounting times,
1/64 second, specified in \fIacct.h\fP as its reciprocal AHZ.
The \fIcompress\fP function converts seconds and microseconds
to these new units, expressed as before in 16-bit pseudo-floating point numbers.
.XP kern_clock.c
The hardware clock handler implements the new time-correction primitive
\fIadjtime\fP by skewing the rate at which time increases until a specified
correction has been achieved.
The \fIbumptime\fP routine used to increment the time has been changed into
a macro.
The overhead of software interrupts used to schedule the \fIsoftclock\fP
handler has been reduced
by noting whether any profiling or timeout activity requires it to run,
and by calling \fIsoftclock\fP directly from \fIhardclock\fP
(with reduced processor priority) if the previous
priority was sufficiently low.
.XP kern_descrip.c
Most uses of the \fIgetf()\fP function have been replaced by the
GETF macro form.
The \fIdup\fP calls (including that from \fIfcntl\fP\^) no longer copy
the close-on-exec flag from the original file descriptor.
Most of the changes to support the open file descriptor high-water mark,
\fIu.u_lastfile\fP, are in this file.
The \fIflock\fP system call has had several bugs fixed.
Unix-domain file descriptor garbage collection is no longer triggered
from \fIclosef\fP, but when a socket is torn down.
.XP kern_exec.c
The \fIa.out\fP header used in the course of \fIexec\fP is no longer in the user
structure, but is local to \fIexec\fP.
Argument and environment strings are copied to and from the user address space
a string at a time using the new \fIcopyinstr\fP and \fIcopyoutstr\fP
primitives.
When invoking an executable script, the first argument is now the name
of the interpreter rather than the file name; the file name appears only
after the interpreter name and optional argument.
An \fIiput\fP was moved to avoid a deadlock when the executable image
had been opened and marked close-on-exec.
The \fIsetregs\fP routine has been split;
machine-independent parts such as signal action modification are done
in \fIexecve\fP directly, and the remaining machine-dependent routine
was moved to \fImachdep.c\fP.
Image size verification using \fIchksize\fP checks data and bss sizes
separately to avoid overflow on their addition.
.XP kern_exit.c
Instead of looping at location 0x13 in user mode if \fI/etc/init\fP cannot
be executed, the system now prints a message and pauses.
This is done by \fIexit\fP if process 1 could not run.
The search for child processes in \fIexit\fP uses the child and sibling
linkage in the \fIproc\fP entry
instead of a linear search of the \fIproc\fP table.
Failures when copying out resource usage information from \fIwait\fP
are now reflected to the caller.
.XP kern_fork.c
One of the two linear searches of the proc table during process creation
has been eliminated, the other looks only at active processes.
As the first scan is needed only to count the number of processes
for this user, it is bypassed for root.
A comment dating to version 7 (``Partially simulate the environment
so that when it is actually created (by copying) it will look right.'')
has finally been removed; it relates only to PDP-11 code.
.XP kern_mman.c
\fIChksize\fP takes an extra argument so that data and bss expansion
can be checked separately to avoid problems with overflow.
.XP kern_proc.c
The \fIspgrp\fP routine has been corrected.
An attempt to optimize its $ O ( n sup 2 ) $ algorithm
(multiple scans of the process table)
did so incorrectly; it now uses the child and sibling pointers in the proc
table to find all descendents in linear time.
\fIPqinit\fP is called at initialization time to set up the process
queues and free all process slots.
.XP kern_prot.c
A number of changes were needed to reflect the type changes
of the user and group ID's.
The \fIgetgroups\fP and \fIsetgroups\fP routines pass groups as arrays
of integers and thus must convert.
All scans of the groups array look for an explicit NOGROUP terminator
rather than any negative group.
For consistency, the \fIsetreuid\fP call sets the process \fIp_uid\fP
to the new effective user ID instead of the real ID as before.
This prevents the anomaly of a process not being
allowed to send signals to itself.
.XP kern_resource.c
Attempts to change resource limits for process sizes are checked
against the maximum segment size that the swap map supports, \fImaxdmap\fP.
The error returned when attempting to change another user's priority
was changed from EACCESS to EPERM.
.XP kern_sig.c
The \fIsigmask\fP macro is now used throughout the kernel.
The treatment of the \fIsigvec\fP flag has 
been expanded to include the SV_INTERRUPT
option.
\fIKill\fP and \fIkillpg\fP have been rewritten, and the errors returned
are now closer to those of System V.
In particular, unprivileged users may broadcast signals with no error
if they managed to kill something,
and an attempt to signal process group 0 (one's own group) when no group
is set receives an ESRCH instead of an EINVAL.
SIGWINCH joins the class of signals whose default action is to ignore.
When a process stops under \fIptrace\fP, its parent now receives a SIGCHLD.
.XP kern_synch.c
The CPU overhead of \fIschedcpu\fP has been reduced as much as possible
by removing loop invariants
and by ignoring processes that have not run since the last calculation.
When long-sleeping processes are awakened, their priority is recomputed
to consider their sleep time.
\fISchedcpu\fP need not remove processes with new priorities
from their run queues and reinsert them unless they are moving to a new queue.
The sleep queues are now treated as circular (FIFO) lists,
as the old LIFO behavior caused problems for some programs queued
for locks.
\fISleep\fP no longer allows context switches after a panic,
but simply drops the processor priority momentarily then returns;
this converts sleeps during the filesystem update into busy-waits.
.XP kern_time.c
\fIGettimeofday\fP returns the microsecond time on hardware supporting
it, including the VAX.
It is now possible to set the timezone as well as the time
with \fIsettimeofday\fP.
A system call, \fIadjtime\fP,
has been added to correct the time by a small amount
using gradual skew rather than discontinuous jumps forward or backward.
.XP kern_xxx.c
The 4.1-compatible \fIsignal\fP entry sets the signal SV_INTERRUPT option
as well as the per-process SOUSIG, which now controls only the resetting
of signal action to default upon invocation of a caught signal.
.XP subr_log.c
This new file contains routines that implement a kernel error
log device.
Kernel messages are placed in the message buffer as before,
and can be read from there through the log device \fI/dev/klog\fP.
.XP subr_mcount.c
The kernel profiling buffers are allocated with \fIcalloc\fP instead 
of \fIwmemall\fP
to avoid the dramatic decrease in user virtual memory that could be supported
after allocation of a large section of \fIusrpt\fP.
.XP subr_prf.c
Support was added for the kernel error log.
The \fIlog\fP routine is similar to \fIprintf\fP
but does not print on the console, thereby suspending system operation.
\fILog\fP takes a priority as well as a format, both of which are read from
the log device by the system error logger \fIsyslogd\fP.
\fIUprintf\fP was modified to check its terminal output queue
and to block rather than to use all of the system clists;
it is now even less appropriate for use from interrupt level.
\fITprintf\fP is similar to \fIuprintf\fP but prints to the tty specified
as an argument rather than to that of the current user.
\fITprintf\fP does not block if the output queue is overfull, but logs only
to the error log;
it may thus be used from interrupt level.
Because of these changes, \fIputchar\fP and \fIprintn\fP require
an additional argument
specifying the destination(s) of the character.
The \fItablefull\fP error routine was changed to use \fIlog\fP rather than
\fIprintf\fP.
.XP subr_rmap.c
An off-by-one error in \fIrmget\fP was corrected.
.XP sys_generic.c
The \fIselect\fP call may now be used with more than 32 file descriptors,
requiring that the masks be treated as arrays.
The result masks are returned to the user if and only if no error (including
EINTR) occurs.
A select bug that caused processes to disappear was fixed;
\fIselwakeup\fP needed to handle stopped processes differently
than sleeping processes.
.XP sys_inode.c
Problems occurring after an interrupted close were corrected
by forcing \fIino_close\fP to return to \fIclosef\fP even after an interrupt;
otherwise, \fIf_count\fP could be cleared too early or twice.
The code to unhash text pages being overwritten needed to be protected
from memory allocations at interrupt level to avoid a bogus ``panic: munhash.'' 
The internal routine implementing \fIflock\fP was reworked to avoid several
bad assumptions and to allow restarts after an interruption.
.XP sys_process.c
\fIProcxmt\fP uses the new \fIptrace.h\fP header file;
hopefully, the next release will have neither \fIptrace\fP nor \fIprocxmt\fP.
The text XTRC flag is set when modifying a pure text image,
protecting it from sharing and overwriting.
.XP sys_socket.c
The socket involved in an interface \fIioctl\fP is passed to \fIifioctl\fP
so that it can call the protocol if necessary, as when setting the interface
address for the protocol.
It is now possible to be notified of pending out-of-band data by selecting
for exceptional conditions.
.XP syscalls.c
The system call names here have been made to agree with reality.
