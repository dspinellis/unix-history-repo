.\" Copyright (c) 1983, 1993, 1994
.\"	The Regents of the University of California.  All rights reserved.
.\"
.\" %sccs.include.redist.roff%
.\"
.\"	@(#)1.1.t	8.5 (Berkeley) %G%
.\"
.Sh 2 "Processes and protection
.Sh 3 "Host identifiers
.PP
Each host has associated with it an integer host ID, and a host
name of up to MAXHOSTNAMELEN (256) characters (as defined in
\fI<sys/param.h>\fP).
These identifiers are set (by a privileged user) and retrieved using the
.Fn sysctl
interface described in section
.Xr 1.7.1 .
The host ID is seldom used (or set), and is deprecated.
For convenience and backward compatibility,
the following library routines are provided:
.DS
.Fd sethostid 1 "set host identifier
sethostid(hostid);
long hostid;
.DE
.DS
.Fd gethostid 0 "get host identifier
hostid = gethostid();
result long hostid;
.DE
.DS
.Fd sethostname 2 "set host name
sethostname(name, len);
char *name; int len;
.DE
.DS
.Fd gethostname 2 "get host name
len = gethostname(buf, buflen);
result int len; result char *buf; int buflen;
.DE
.Sh 3 "Process identifiers
Each host runs a set of \fIprocesses\fP.
Each process is largely independent of other processes,
having its own protection domain, address space, timers, and
an independent set of references to system or user implemented objects.
.PP
Each process in a host is named by an integer
called the \fIprocess ID\fP.  This number is
in the range 1-30000
and is returned by the
.Fn getpid
routine:
.DS
.Fd getpid 0 "get process identifier
pid = getpid();
result pid_t pid;
.DE
On each host this identifier is guaranteed to be unique;
in a multi-host environment, the (hostid, process ID) pairs are
guaranteed unique.
The parent process identifier can be obtained using the
.Fn getppid
routine:
.DS
.Fd getppid 0 "get parent process identifier
pid = getppid();
result pid_t pid;
.DE
.Sh 3 "Process creation and termination
.LP
A new process is created by making a logical duplicate of an
existing process:
.DS
.Fd fork 0 "create a new process
pid = fork();
result pid_t pid;
.DE
The
.Fn fork
call returns twice, once in the parent process, where
\fIpid\fP is the process identifier of the child,
and once in the child process where \fIpid\fP is 0.
The parent-child relationship imposes a hierarchical structure on
the set of processes in the system.
.PP
For processes that are forking solely for the purpose of
.Fn execve 'ing
another program, the
.Fn vfork
system call provides a faster interface:
.DS
.Fd vfork 0 "create a new process
pid = vfork();
result pid_t pid;
.DE
Like
.Fn fork ,
the
.Fn vfork
call returns twice, once in the parent process, where
\fIpid\fP is the process identifier of the child,
and once in the child process where \fIpid\fP is 0.
The parent process is suspended until the child process calls
either \fIexecve\fP or \fIexit\fP.
.LP
A process may terminate by executing an
.Fn exit
call:
.DS
.Fd exit 1 "terminate a process
exit(status);
int status;
.DE
The lower 8 bits of exit status are available to its parent.
.PP
When a child process exits or
terminates abnormally, the parent process receives
information about the
event which caused termination of the child process.
The interface allows the parent to wait for a particular process,
process group, or any direct descendent and
to retrieve information about resources consumed
by the process during its lifetime.
The request may be done either synchronously
(waiting for one of the requested processes to exit),
or asynchronously
(polling to see if any of the requested processes have exited):
.DS
.Fd wait4 4 "collect exit status of child
pid = wait4(wpid, astatus, options, arusage);
result pid_t pid; pid_t wpid; result int *astatus;
int options; result struct rusage *arusage;
.DE
.PP
A process can overlay itself with the memory image of another process,
passing the newly created process a set of parameters, using the call:
.DS
.Fd execve 3 "execute a new program
execve(name, argv, envp);
char *name, *argv[], *envp[];
.DE
The specified \fIname\fP must be a file which is in a format recognized
by the system, either a binary executable file or a file which causes
the execution of a specified interpreter program to process its contents.
If the set-user-ID mode bit is set,
the effective user ID is set to the owner of the file;
if the set-group-ID mode bit is set,
the effective group ID is set to the group of the file.
Whether changed or not, the effective user ID is then copied to the
saved user ID, and the effective group ID is copied to the
saved group ID.
.Sh 3 "User and group IDs
.PP
Each process in the system has associated with it three user IDs:
a \fIreal user ID\fP, an \fIeffective user ID\fP, and a \fIsaved user ID\fP,
all unsigned integral types (\fBuid_t\fP).
Each process has a \fIreal group ID\fP
and a set of \fIaccess group IDs\fP,
the first of which is the \fIeffective group ID\fP.
The group IDs are unsigned integral types (\fBgid_t\fP).
Each process may be in multiple access groups.
The maximum concurrent number of access groups is a system compilation
parameter,
represented by the constant NGROUPS in the file \fI<sys/param.h>\fP.
It is guaranteed to be at least 16.
.LP
The real group ID is used in process accounting and in testing whether
the effective group ID may be changed; it is not otherwise used for
access control.
The members of the access group ID set are used for access control.
Because the first member of the set is the effective group ID, which
is changed when executing a set-group-ID program, that element is normally
duplicated in the set so that access privileges for the original group
are not lost when using a set-group-ID program.
.LP
The real and effective user IDs associated with a process are returned by:
.DS
.Fd getuid 0 "get real user identifier
ruid = getuid();
result uid_t ruid;
.DE
.DS
.Fd geteuid 0 "get effective user identifier
euid = geteuid();
result uid_t euid;
.DE
the real and effective group IDs by:
.DS
.Fd getgid 0 "get real group identifier
rgid = getgid();
result gid_t rgid;
.DE
.DS
.Fd getegid 0 "get effective group identifier
egid = getegid();
result gid_t egid;
.DE
The access group ID set is returned by a
.Fn getgroups
call:
.DS
.Fd getgroups 2 "get access group set
ngroups = getgroups(gidsetsize, gidset);
result int ngroups; int gidsetsize; result gid_t gidset[gidsetsize];
.DE
.LP
The user and group IDs
are assigned at login time using the
.Fn setuid ,
.Fn setgid ,
and
.Fn setgroups
calls:
.DS
.Fd setuid 1 "set real, effective, and saved user identifiers
setuid(uid);
uid_t uid;
.DE
.DS
.Fd setgid 1 "set real, effective, and saved group identifiers
setgid(gid);
gid_t gid;
.DE
.DS
.Fd setgroups 2 "set access group set
setgroups(gidsetsize, gidset);
int gidsetsize; gid_t gidset[gidsetsize];
.DE
The
.Fn setuid
call sets the real, effective, and saved user IDs,
and is permitted only if the specified \fIuid\fP is the current real user ID
or if the caller is the super-user.
The
.Fn setgid
call sets the real, effective, and saved group IDs;
it is permitted only if the specified \fIgid\fP is the current real group ID
or if the caller is the super-user.
The
.Fn setgroups
call sets the access group ID set, and is restricted to the super-user.
.LP
The
.Fn seteuid
routine allows any process to set its effective user ID to either its
real or saved user ID:
.DS
.Fd seteuid 1 "set effective user identifier
seteuid(uid);
uid_t uid;
.DE
The
.Fn setegid
routine allows any process to set its effective group ID to either its
real or saved group ID:
.DS
.Fd setegid 1 "set effective group identifier
setegid(gid);
gid_t gid;
.DE
.Sh 3 "Sessions
.PP
When a user first logs onto the system,
they are put into a session with a controlling process
(usually a shell).
The session is created with the call:
.DS
.Fd setsid 0 "create a new session
pid = setsid();
result pid_t pid;
.DE
All subsequent processes created by the user
(that do not call
.Fn setsid )
will be part of the session.
The session also has a login name associated with it
which is set using the privileged call:
.DS
.Fd setlogin 1 "set login name
setlogin(name);
char *name;
.DE
The login name can be retrieved using the call:
.DS
.Fd getlogin 0 "get login name
name = getlogin();
result char *name;
.DE
Unlike historic systems, the value returned by
.Fn getlogin
is stored in the kernel and can be trusted.
.Sh 3 "Process groups
.PP
Each process in the system is also associated with a \fIprocess
group\fP.  The group of processes in a process group is sometimes
referred to as a \fIjob\fP and manipulated by high-level system
software (such as the shell).
All members of a process group are members of the same session.
The current process group of a process is returned by the
.Fn getpgrp
call:
.DS
.Fd getpgrp 0 "get process group
pgrp = getpgrp();
result pid_t pgrp;
.DE
When a process is in a specific process group it may receive
software interrupts affecting the group, causing the group to
suspend or resume execution or to be interrupted or terminated.
In particular, a system terminal has a process group and only processes
which are in the process group of the terminal may read from the terminal,
allowing arbitration of a terminal among several different jobs.
.LP
The process group associated with a process may be changed by the
.Fn setpgid
call:
.DS
.Fd setpgid 2 "set process group
setpgid(pid, pgrp);
pid_t pid, pgrp;
.DE
Newly created processes are assigned process IDs distinct from all
processes and process groups, and the same process group as their
parent.
Any process may set its process group equal to its process ID or
to the value of any process group within its session.
