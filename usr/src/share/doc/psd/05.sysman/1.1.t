.\" Copyright (c) 1983, 1993
.\"	The Regents of the University of California.  All rights reserved.
.\"
.\" %sccs.include.redist.roff%
.\"
.\"	@(#)1.1.t	8.2 (Berkeley) %G%
.\"
.Sh 2 "Processes and protection
.Sh 3 "Host and process identifiers
.PP
Each host has associated with it a 32-bit host id, and a host
name of up to MAXHOSTNAMELEN (64) characters (as defined in
\fI<sys/param.h>\fP).
These identifiers are set (by a privileged user) and retrieved using the
.Fn sysctl
interface described in section
.Xr 1.7.1 .
For convenience and backward compatibility,
the following library routines are provided:
.DS
.Fd sethostid 1 "set host identifier
sethostid(hostid)
long hostid;
.DE
.DS
.Fd gethostid 0 "get host identifier
hostid = gethostid();
result long hostid;
.DE
.DS
.Fd sethostname 2 "set host name
sethostname(name, len)
char *name; int len;
.DE
.DS
.Fd gethostname 2 "get host name
len = gethostname(buf, buflen)
result int len; result char *buf; int buflen;
.DE
Each host runs a set of \fIprocesses\fP.
Each process is largely independent of other processes,
having its own protection domain, address space, timers, and
an independent set of references to system or user implemented objects.
.PP
Each process in a host is named by an integer
called the \fIprocess id\fP.  This number is
in the range 1-30000
and is returned by the
.Fn getpid
routine:
.DS
.Fd getpid 0 "get process identifier
pid = getpid();
result int pid;
.DE
On each host this identifier is guaranteed to be unique;
in a multi-host environment, the (hostid, process id) pairs are
guaranteed unique.
The parent process identifier can be obtained using the
.Fn getppid
routine:
.DS
.Fd getppid 0 "get parent process identifier
pid = getppid();
result int pid;
.DE
.Sh 3 "Process creation and termination
.LP
A new process is created by making a logical duplicate of an
existing process:
.DS
.Fd fork 0 "create a new process
pid = fork();
result int pid;
.DE
The
.Fn fork
call returns twice, once in the parent process, where
\fIpid\fP is the process identifier of the child,
and once in the child process where \fIpid\fP is 0.
The parent-child relationship induces a hierarchical structure on
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
result int pid;
.DE
Like
.Fn fork ,
the
.Fn vfork
call returns twice, once in the parent process, where
\fIpid\fP is the process identifier of the child,
and once in the child process where \fIpid\fP is 0.
.LP
A process may terminate by executing an
.Fn exit
call:
.DS
.Fd exit 1 "terminate a process
exit(status)
int status;
.DE
returning 8 bits of exit status to its parent.
.PP
When a child process exits or
terminates abnormally, the parent process receives
information about any
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
execve(name, argv, envp)
char *name, **argv, **envp;
.DE
The specified \fIname\fP must be a file which is in a format recognized
by the system, either a binary executable file or a file which causes
the execution of a specified interpreter program to process its contents.
If the set-user-id mode bit is set,
the effective user-id is set to the owner of the file;
if the set-group-id mode bit is set,
the effective group-id is set to the group of the file.
Whether changed or not, the effective user-id is copied to the
saved user-id and the effective group-id is copied to the
saved group-id.
.Sh 3 "User and group ids
.PP
Each process in the system has associated with it two user-id's:
a \fIreal user id\fP and a \fIeffective user id\fP, both 32-bit
unsigned integers (type \fBuid_t\fP).
Each process has an \fIreal accounting group id\fP
and a set of \fIaccess group id's\fP
the first of which is the \fIeffective accounting group id\fP.
The group id's are 32-bit unsigned integers (type \fBgid_t\fP).
Each process may be in several different access groups, with the maximum
concurrent number of access groups a system compilation parameter,
the constant NGROUPS in the file \fI<sys/param.h>\fP,
guaranteed to be at least eight.
.LP
The real and effective user ids associated with a process are returned by:
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
the real and effective accounting group ids by:
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
The access group id set is returned by a
.Fn getgroups
call:
.DS
.Fd getgroups 2 "get access group set
ngroups = getgroups(gidsetsize, gidset);
result int ngroups; int gidsetsize; result gid_t gidset[gidsetsize];
.DE
.LP
The user and group id's
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
setgroups(gidsetsize, gidset)
int gidsetsize; gid_t gidset[gidsetsize];
.DE
These calls are restricted to the super-user.
The
.Fn setuid
call sets the real, effective, and saved user-id's, while the
.Fn setgid
call sets the real, effective, and saved group id's.
The
.Fn seteuid
routine allows any process to set its effective user-id to either its
real or saved user-id:
.DS
.Fd seteuid 1 "set effective user identifier
seteuid(uid);
uid_t uid;
.DE
The
.Fn setegid
routine allows any process to set its effective group-id to either its
real or saved group-id:
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
All subsequent processes created by the user will be part of the session.
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
.Sh 3 "Process groups
.PP
Each process in the system is also normally associated with a \fIprocess
group\fP.  The group of processes in a process group is sometimes
referred to as a \fIjob\fP and manipulated by high-level system
software (such as the shell).
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
which are in the process group of the terminal may read from the
terminal, allowing arbitration of terminals among several different jobs.
.LP
The process group associated with a process may be changed by the
.Fn setpgid
call:
.DS
.Fd setpgid 2 "set process group
setpgid(pid, pgrp);
int pid, pgrp;
.DE
Newly created processes are assigned process id's distinct from all
processes and process groups, and the same process group as their
parent.  A normal (unprivileged) process may set its process group equal
to its process id or to the value of any process group within its session.
