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
.\"	@(#)2.5.t	8.6 (Berkeley) 5/29/94
.\"
.Sh 2 "Process debugging
.Sh 3 "Traditional debugging
.LP
Debuggers traditionally use the
.Fn ptrace
interface:
.DS
.Fd ptrace 4 "process trace
ptrace(request, pid, addr, data);
int request, pid, *addr, data;
.DE
This interface provides a means by which a parent process may
control the execution of a child process,
and examine and change its core image.
Its primary use is for the implementation of breakpoint debugging.
There are four arguments whose interpretation
depends on a request argument.
A process being traced behaves normally until it
encounters a signal (whether internally generated like
``illegal instruction'' or externally generated
like ``interrupt'').
Then the traced process enters a stopped state
and its parent is notified via
.Fn wait .
When the child is in the stopped state,
its core image can be examined and modified using
.Fn ptrace .
Another ptrace request can then cause the child either to terminate
or to continue, possibly ignoring the signal.
.PP
A more general interface is also provided in 4.4BSD;
the \fImount_procfs\fP filesystem attaches an instance of
the process name space to the global filesystem name space.
The conventional mount point is \fI/proc\fP.
The root of the process filesystem contains an entry for each active
process.
These processes are visible as directories named by the process' ID.
In addition, the special entry \fIcurproc\fP references the current
process.
Each directory contains several files, including a \fIctl\fP file.
The debugger finds (or creates) the process that it wants to
debug and then issues an attach command via the \fIctl\fP file.
Further interaction can then be done with the process through
the other files provided by the \fI/proc\fP filesystem.
.Sh 3 "Kernel tracing
.LP
Another facility for debugging programs is provided by the
.Fn ktrace
interface:
.DS
.Fd ktrace 4 "process tracing
ktrace(tracefile, ops, trpoints, pid);
char *tracefile; int ops, trpoints, pid;
.DE
.Fn Ktrace
does kernel trace logging for the specified processes.
The kernel operations that are traced include system calls,
pathname translations, signal processing, and I/O.
This facility can be particularly useful to debug
programs for which you do not have the source.
.\"
.\" We currently do not document the vtrace call
.\" .Fd vtrace 2
