.\" Copyright (c) 1983 The Regents of the University of California.
.\" All rights reserved.
.\"
.\" %sccs.include.redist.roff%
.\"
.\"	@(#)6.t	6.6 (Berkeley) %G%
.\"
.NH 1
Line printer Administration
.PP
The
.I lpc
program provides local control over line printer activity.
The major commands and their intended use will be described.
The command format and remaining commands are described in
.IR lpc (8).
.LP
\fBabort\fP and \fBstart\fP
.IP
.I Abort
terminates an active spooling daemon on the local host immediately and
then disables printing (preventing new daemons from being started by
.IR lpr ).
This is normally used to forcibly restart a hung line printer daemon
(i.e., \fIlpq\fP reports that there is a daemon present but nothing is
happening).  It does not remove any jobs from the queue
(use the \fIlprm\fP command instead).
.I Start
enables printing and requests \fIlpd\fP to start printing jobs.
.LP
\fBenable\fP and \fBdisable\fP
.IP
\fIEnable\fP and \fIdisable\fP allow spooling in the local queue to be
turned on/off.
This will allow/prevent
.I lpr
from putting new jobs in the spool queue.  It is frequently convenient
to turn spooling off while testing new line printer filters since the
.I root
user can still use
.I lpr
to put jobs in the queue but no one else can.
The other main use is to prevent users from putting jobs in the queue
when the printer is expected to be unavailable for a long time.
.LP
\fBrestart\fP
.IP
.I Restart
allows ordinary users to restart printer daemons when
.I lpq
reports that there is no daemon present.
.LP
\fBstop\fP
.IP
.I Stop
halts a spooling daemon after the current job completes;
this also disables printing.  This is a clean way to shutdown a
printer to do maintenance, etc.  Note that users can still enter jobs in a
spool queue while a printer is
.IR stopped .
.LP
\fBtopq\fP
.IP
.I Topq
places jobs at the top of a printer queue.  This can be used
to reorder high priority jobs since
.I lpr
only provides first-come-first-serve ordering of jobs.
