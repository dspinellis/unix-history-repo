.\" Copyright (c) 1983, 1993, 1994
.\"	The Regents of the University of California.  All rights reserved.
.\"
.\" Redistribution and use in source and binary forms, with or without
.\" modification, are permitted provided that the following conditions
.\" are met:
.\" 1. Redistributions of source code must retain the above copyright
.\"    notice, this list of conditions and the following disclaimer.
.\" 2. Redistributions in binary form must reproduce the above copyright
.\"    notice, this list of conditions and the following disclaimer in the
.\"    documentation and/or other materials provided with the distribution.
.\" 3. All advertising materials mentioning features or use of this software
.\"    must display the following acknowledgement:
.\"	This product includes software developed by the University of
.\"	California, Berkeley and its contributors.
.\" 4. Neither the name of the University nor the names of its contributors
.\"    may be used to endorse or promote products derived from this software
.\"    without specific prior written permission.
.\"
.\" THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
.\" ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
.\" IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
.\" ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
.\" FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
.\" DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
.\" OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
.\" HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
.\" LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
.\" OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
.\" SUCH DAMAGE.
.\"
.\"	@(#)1.6.t	8.5 (Berkeley) 6/1/94
.\"
.Sh 2 "Resource controls
.Sh 3 "Process priorities
.PP
The system gives CPU scheduling priority to processes that have not used
CPU time recently.  This tends to favor interactive processes and
processes that execute only for short periods.
The instantaneous scheduling priority is a function of CPU usage
and a settable priority value used in adjusting the instantaneous
priority with CPU usage or inactivity.
It is possible to determine the settable priority factor currently
assigned to a process (PRIO_PROCESS),
process group (PRIO_PGRP),
or the processes of a specified user (PRIO_USER),
or to alter this priority using the calls:
.DS
.Fd getpriority 2 "get program scheduling priority
prio = getpriority(which, who);
result int prio; int which, who;
.DE
.DS
.Fd setpriority 3 "set program scheduling priority
setpriority(which, who, prio);
int which, who, prio;
.DE
The value \fIprio\fP is in the range \-20 to 20.
The default priority is 0; lower priorities cause more
favorable execution.
The
.Fn getpriority
call returns the highest priority (lowest numerical value)
enjoyed by any of the specified processes.
The
.Fn setpriority
call sets the priorities of all the
specified processes to the specified value.
Only the super-user may lower priorities.
.Sh 3 "Resource utilization
.PP
The
.Fn getrusage
call returns information describing the resources utilized by the
current process (RUSAGE_SELF),
or all its terminated descendent processes (RUSAGE_CHILDREN):
.DS
.Fd getrusage 2 "get information about resource utilization
getrusage(who, rusage);
int who; result struct rusage *rusage;
.DE
The information is returned in a structure defined in \fI<sys/resource.h>\fP:
.DS
.TS
l s s s
l l l l.
struct rusage {
	struct	timeval ru_utime;	/* user time used */
	struct	timeval ru_stime;	/* system time used */
	int	ru_maxrss;	/* maximum core resident set size: kbytes */
	int	ru_ixrss;	/* integral shared memory size (kbytes*sec) */
	int	ru_idrss;	/* unshared data memory size */
	int	ru_isrss;	/* unshared stack memory size */
	int	ru_minflt;	/* page-reclaims */
	int	ru_majflt;	/* page faults */
	int	ru_nswap;	/* swaps */
	int	ru_inblock;	/* block input operations */
	int	ru_oublock;	/* block output operations */
	int	ru_msgsnd;	/* messages sent */
	int	ru_msgrcv;	/* messages received */
	int	ru_nsignals;	/* signals received */
	int	ru_nvcsw;	/* voluntary context switches */
	int	ru_nivcsw;	/* involuntary context switches */
};
.TE
.DE
.Sh 3 "Resource limits
.PP
The resources of a process for which limits are controlled by the
kernel are defined in \fI<sys/resource.h>\fP, and controlled by the
.Fn getrlimit
and
.Fn setrlimit
calls:
.DS
.Fd getrlimit 2 "get maximum system resource consumption
getrlimit(resource, rlp);
int resource; result struct rlimit *rlp;
.DE
.DS
.Fd setrlimit 2 "set maximum system resource consumption
setrlimit(resource, rlp);
int resource; struct rlimit *rlp;
.DE
The resources that may currently be controlled include:
.DS
.TS
l l.
RLIMIT_CPU	/* cpu time in milliseconds */
RLIMIT_FSIZE	/* maximum file size */
RLIMIT_DATA	/* data size */
RLIMIT_STACK	/* stack size */
RLIMIT_CORE	/* core file size */
RLIMIT_RSS	/* resident set size */
RLIMIT_MEMLOCK	/* locked-in-memory address space */
RLIMIT_NPROC	/* number of processes */
RLIMIT_NOFILE	/* number of open files */
.TE
.DE
.ne 1i
Each limit has a current value and a maximum defined
by the \fIrlimit\fP structure:
.DS
.TS
l s s s
l l l l.
struct rlimit {
	quad_t	rlim_cur;	/* current (soft) limit */
	quad_t	rlim_max;	/* hard limit */
};
.TE
.DE
.PP
Only the super-user can raise the maximum limits.
Other users may only
alter \fIrlim_cur\fP within the range from 0 to \fIrlim_max\fP
or (irreversibly) lower \fIrlim_max\fP.
To remove a limit on a resource,
the value is set to RLIM_INFINITY.
