.\" Copyright (c) 1985 The Regents of the University of California.
.\" All rights reserved.
.\"
.\" %sccs.include.redist.man%
.\"
.\"	@(#)6.t	5.1 (Berkeley) %G%
.\"
.ds RH Security Tightening
.NH
Security Tightening
.PP
Since we do not wish to encourage rampant system cracking,
we describe only briefly the changes made to enhance security.
.NH 2
Generic Kernel
.PP
Several loopholes in the process tracing facility have been corrected.
Programs being traced may not be executed;
executing programs may not be traced.
Programs may not provide input to terminals to which they do not
have read permission.
The handling of process groups has been tightened to eliminate
some problems.
When a program attempts to change its process group,
the system checks to see if the process with the pid of the process 
group was started by the same user.
If it exists and was started by a different user the process group
number change is denied.
.NH 2
Security Problems in Utilities
.PP
Setuid utilities no longer use the \fIpopen\fP or \fIsystem\fP library routines.
Access to the kernel's data structures through the kmem device
is now restricted to programs that are set group id ``kmem''.
Thus many programs that used to run with root privileges
no longer need to do so.
Access to disk devices is now controlled by an ``operator'' group id;
this permission allows operators to function without being the super-user.
Only users in group wheel can do ``su root''; this restriction
allows administrators to define a super-user access list.
Numerous holes have been closed in the shell to prevent
users from gaining privileges from set user id shell scripts,
although use of such scripts is still highly discouraged on systems
that are concerned about security.
