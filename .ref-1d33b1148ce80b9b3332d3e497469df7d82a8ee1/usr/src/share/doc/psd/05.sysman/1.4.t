.\" Copyright (c) 1983, 1993, 1994
.\"	The Regents of the University of California.  All rights reserved.
.\"
.\" %sccs.include.redist.roff%
.\"
.\"	@(#)1.4.t	8.5 (Berkeley) %G%
.\"
.Sh 2 "Timers
.Sh 3 "Real time
.PP
The system's notion of the current time is in Coordinated Universal Time
(UTC, previously GMT) and the current time
zone is set and returned by the calls:
.DS
.Fd settimeofday 2 "set date and time
settimeofday(tp, tzp);
struct timeval *tp;
struct timezone *tzp;
.DE
.DS
.Fd gettimeofday 2 "get date and time
gettimeofday(tp, tzp);
result struct timeval *tp;
result struct timezone *tzp;
.DE
where the structures are defined in \fI<sys/time.h>\fP as:
.DS
.TS
l s s s
l l l l.
struct timeval {
	long	tv_sec;	/* seconds since Jan 1, 1970 */
	long	tv_usec;	/* and microseconds */
};
.T&
l s s s
l l l l.
struct timezone {
	int	tz_minuteswest;	/* of Greenwich */
	int	tz_dsttime;	/* type of dst correction to apply */
};
.TE
.DE
The timezone information is present only for historical reasons
and is unused by the current system.
.LP
The precision of the system clock is hardware dependent.
Earlier versions of UNIX contained only a 1-second resolution version
of this call, which remains as a library routine:
.DS
time(tvsec);
result time_t *tvsec;
.DE
returning only the tv_sec field from the
.Fn gettimeofday
call.
.LP
The
.Fn adjtime
system calls allows for small changes in time without abrupt changes
by skewing the rate at which time advances:
.DS
.Fd adjtime 2 "synchronization of the system clock
adjtime(delta, olddelta);
struct timeval *delta; result struct timeval *olddelta;
.DE
.Sh 3 "Interval time
.LP
The system provides each process with three interval timers,
defined in \fI<sys/time.h>\fP:
.DS
.TS
l l.
ITIMER_REAL	/* real time intervals */
ITIMER_VIRTUAL	/* virtual time intervals */
ITIMER_PROF	/* user and system virtual time */
.TE
.DE
The ITIMER_REAL timer decrements
in real time.  It could be used by a library routine to
maintain a wakeup service queue.  A SIGALRM signal is delivered
when this timer expires.
.PP
The ITIMER_VIRTUAL timer decrements in process virtual time.
It runs only when the process is executing.  A SIGVTALRM signal
is delivered when it expires.
.PP
The ITIMER_PROF timer decrements both in process virtual time and when
the system is running on behalf of the process.
It is designed to be used by processes to statistically profile
their execution.
A SIGPROF signal is delivered when it expires.
.LP
A timer value is defined by the \fIitimerval\fP structure:
.DS
.TS
l s s s
l l l l.
struct itimerval {
	struct	timeval it_interval;	/* timer interval */
	struct	timeval it_value;	/* current value */
};
.TE
.DE
and a timer is set or read by the call:
.DS
.Fd setitimer 3 "set value of interval timer
setitimer(which, value, ovalue);
int which; struct itimerval *value; result struct itimerval *ovalue;
.DE
.DS
.Fd getitimer 2 "get value of interval timer
getitimer(which, value);
int which; result struct itimerval *value;
.DE
The \fIit_value\fP specifies the time until the next signal;
the \fIit_interval\fP specifies a new interval that should
be loaded into the timer on each expiration.
The third argument to
.Fn setitimer
specifies an optional structure
to receive the previous contents of the interval timer.
A timer can be disabled by setting \fIit_value\fP and \fIit_interval\fP to 0.
.PP
The system rounds argument timer intervals to be not less than the
resolution of its clock.  This clock resolution can be determined
by loading a very small value into a timer and reading the timer back to
see what value resulted.
.PP
The
.Fn alarm
system call of earlier versions of UNIX is provided
as a library routine using the ITIMER_REAL timer.
.PP
The process profiling facilities of earlier versions of UNIX
remain because it is not always possible to guarantee
the automatic restart of system calls after receipt of a signal.
The
.Fn profil
call arranges for the kernel to begin gathering
execution statistics for a process:
.DS
.Fd profil 4 "control process profiling
profil(samples, size, offset, scale);
result char *samples; int size, offset, scale;
.DE
This call begins sampling the program counter,
with statistics maintained in the user-provided buffer.
