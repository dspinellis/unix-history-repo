.NH 2
/sys/sys
.PP
This directory contains the ``mainstream'' kernel code.
Files in this directory are intended to be shared between
4.2BSD implementations on all machines.  As there is
little correspondence between the current files in this
directory and those which were present in 4.1BSD a
general overview of each files's contents will be presented
rather than a file-by-file comparison.
.PP
Files in the \fIsys\fP directory are named with prefixes
which indicate their placement in the internal system
layering.  The following table summarizes these naming
conventions.
.DS
.TS
lw(1.0i) l.
init_	system initialization
kern_	kernel (authentication, process management, etc.)
quota_	disk quotas
sys_	system calls and similar
tty_	terminal handling
ufs_	file system
uipc_	interprocess communication
vm_	virtual memory
.TE
.DE
.NH 3
Initialization code
.IP \fBinit_main.c\fP 17
contains system startup code
.IP \fBinit_sysent.c\fP 17
contains the definition of the \fIsysent\fP table \- the table
of system calls supported by 4.2BSD
.NH 3
Kernel-level support
.IP \fBkern_acct.c\fP 17
contains code used in per-process accounting
.IP \fBkern_clock.c\fP 17
contains code for clock processing; work was done
here to minimize time spent in the \fIhardclock\fP routine;
support for kernel profiling and statistics collection
from an alternate clock source have been added;
a bug which caused the system to lose time has been
fixed; the code which drained terminal multiplexor silos
has been made the default mode of operation and moved to locore.s
.IP \fBkern_descrip.c\fP 17
contains code for management of descriptors; descriptor
related system calls such as \fIdup\fP and \fIclose\fP (the
upper-most levels) are present here
.IP \fBkern_exec.c\fP 17
contains code for the \fIexec\fP system call
.IP \fBkern_exit.c\fP 17
contains code for the \fIexit\fP system call
.IP \fBkern_fork.c\fP 17
contains code for the \fIfork\fP (and \fIvfork\fP) system
call
.IP \fBkern_mman.c\fP 17
contains code for memory management related calls; the
contents of this file is expected to change when the
revamped memory management facilities are added to the
system
.IP \fBkern_proc.c\fP 17
contains code related to process management; in particular,
support routines for process groups
.IP \fBkern_prot.c\fP 17
contains code related to access control and protection;
the notions of user ID, group ID, and the group access list
are implemented here
.IP \fBkern_resource.c\fP 17
code related to resource accounting and limits; the
\fIgetrusage\fP and ``get'' and ``set'' resource
limit system calls are found here
.IP \fBkern_sig.c\fP 17
the signal facilities; in particular, kernel level routines
for posting and processing signals
.IP \fBkern_subr.c\fP 17
support routines for manipulating the \fIuio\fP structure:
\fIuiomove\fP, \fIureadc\fP, and \fIuwritec\fP
.IP \fBkern_synch.c\fP 17
code related to process synchonization and scheduling:
\fIsleep\fP and \fIwakeup\fP among others
.IP \fBkern_time.c\fP 17
code related to processing time; the handling of interval
timers and time of day
.IP \fBkern_xxx.c\fP 17
miscellaneous system facilities and code for supporting
4.1BSD compatibility mode (kernel level)
.NH 3
Disk quotas
.IP \fBquota_kern.c\fP 17
``kernel'' of disk quota suppport
.IP \fBquota_subr.c\fP 17
miscellaneous support routines for disk quotas
.IP \fBquota_sys.c\fP 17
disk quota system call routines
.IP \fBquota_ufs.c\fP 17
portions of the disk quota facilities which interface
to the file system routines
.NH 3
General subroutines
.IP \fBsubr_mcount.c\fP 17
code used when profiling the kernel
.IP \fBsubr_prf.c\fP 17
\fIprintf\fP and friends; also, code related to
handling of the diagnostic message buffer
.IP \fBsubr_rmap.c\fP 17
subroutines which manage resource maps
.IP \fBsubr_xxx.c\fP 17
miscellaneous routines and code for routines implemented
with special VAX instructions, e.g. bcopy
.NH 3
System level support
.IP \fBsys_generic.c\fP 17
code for the upper-most levels of the ``generic'' system
calls: \fIread\fP, \fIwrite\fP, \fIioctl\fP, and \fIselect\fP;
a ``must read'' file for the system guru trying to shake
out 4.1BSD bad habits
.IP \fBsys_inode.c\fP 17
code supporting the ``generic'' system calls of sys_generic.c
as they apply to inodes; the guts of the byte stream file i/o
interface
.IP \fBsys_process.c\fP 17
code related to process debugging: \fIptrace\fP and its
support routine \fIprocxmt\fP; this file is expected to
change as better process debugging facilities are developed
.IP \fBsys_socket.c\fP 17
code supporting the ``generic'' system calls of sys_generic.c
as they apply to sockets
.NH 3
Terminal handling
.IP \fBtty.c\fP 17
the terminal handler proper; both 4.1BSD and version 7 terminal
interfaces have been merged into a single set of routines which
are selected as line disciplines; a bug which caused new line
delays past column 127 to be calculated incorrectly has been
fixed; the high water marks for terminals running in tandem mode
at 19.2 or 38.4 kilobaud have been upped
.IP \fBtty_bk.c\fP 17
the old Berknet line discipline (defunct)
.IP \fBtty_conf.c\fP 17
initialized data structures related to terminal handling;
.IP \fBtty_pty.c\fP 17
support for pseudo-terminals; actually two device drivers in
one; additions over 4.1BSD pseudo-terminals include a simple
``packet protocol'' used to support flow-control and output 
flushing on interrupt, as well as a ``transparent'' mode
used in programs such as emacs
.IP \fBtty_subr.c\fP 17
c-list support routines
.IP \fBtty_tb.c\fP 17
two line disciplines for supporting RS232 interfaces to
Genisco and Hitachi tablets
.IP \fBtty_tty.c\fP 17
trivial support routines for ``/dev/tty''
.NH 3
File system support
.IP \fBufs_alloc.c\fP 17
code which handles allocation and deallocation of file
system related resources: disk blocks, on-disk inodes, etc.
.IP \fBufs_bio.c\fP 17
block i/o support; the buffer cache proper; see description
of buf.h and ``A Fast File System for UNIX'' for information
.IP \fBufs_bmap.c\fP 17
code which handles logical file system to logical disk block 
number mapping; understands structure of indirect blocks and files
with holes; handles automatic extension of files on write
.IP \fBufs_dsort.c\fP 17
sort routine implementing prioritized seek sort algorithm
for disk i/o operations
.IP \fBufs_fio.c\fP 17
code handling file system specific issues of access
control and protection
.IP \fBufs_inode.c\fP 17
inode management routines; in-core inodes are now hashed
and cached; inode synchronization has been revamped since
4.1BSD to eliminate race conditions present in 4.1
.IP \fBufs_mount.c\fP 17
code related to demountable file systems
.IP \fBufs_nami.c\fP 17
the \fInamei\fP routine (and related support routines) \- the
routine that maps pathnames to inode numbers
.IP \fBufs_subr.c\fP 17
miscellaneous subroutines: this code is shared with certain
user programs such as \fIfsck\fP\|(8); for a good time
look at the \fIbufstats\fP routine in this file
.IP \fBufs_syscalls.c\fP 17
file system related system calls, everything from \fIopen\fP
to \fIunlink\fP; many new system calls are found here:
\fIrename\fP, \fImkdir\fP, \fIrmdir\fP, \fItruncate\fP, etc.
.IP \fBufs_tables.c\fP 17
static tables used in block and fragment accounting;
this file is shared with user programs such as \fIfsck\fP\|(8)
.IP \fBufs_xxx.c\fP 17
miscellaneous routines and 4.1BSD compatibility code; all of
the code which still understands the old inode format is in here
.NH 3
Interprocess communication
.IP \fBuipc_domain.c\fP 17
code implementing the ``communication domain'' concept;
this file must be augmented to incorporate new domains
.IP \fBuipc_mbuf.c\fP 17
memory management routines for the ipc and network facilities;
refer to the document ``4.2BSD Networking Implementation Notes''
for a detailed description of the routines in this file
.IP \fBuipc_pipe.c\fP 17
leftover code for connecting two sockets into a pipe; actually
a special case of the code for the \fIsocketpair\fP system call
.IP \fBuipc_proto.c\fP 17
UNIX ipc communication domain configuration definitions; contains
UNIX domain data structure initialization
.IP \fBuipc_socket.c\fP 17
top level socket support routines; these routines handle the
interface to the protocol request routines, move data between
user address space and socket data queues, understand the
majority of the logic in process synchronization as it relates
to the ipc facilities
.IP \fBuipc_socket2.c\fP 17
lower level socket support routines; provide nitty gritty bit
twiddling of socket data structures; manage placement of data on
socket data queues
.IP \fBuipc_syscalls.c\fP 17
user interface code to ipc system calls: \fIsocket\fP, \fIbind\fP,
\fIconnect\fP, \fIaccept\fP, etc.; concerned exclusively
with system call argument passing and validation
.IP \fBuipc_usrreq.c\fP 17
UNIX ipc domain support; user request routine and supporting
utility routines
.NH 3
Virtual memory support
.PP
The code in the virtual memory subsystem has changed very little
from 4.1BSD; changes made in these files were either to gain
portability, handle the new swap space configuration scheme,
or fix bugs.
.IP \fBvm_drum.c\fP 17
code for the management of disk space used in paging and swapping
.IP \fBvm_mem.c\fP 17
management of physical memory; the ``core map''
is implemented here as well as the routines which lock down
pages for physical i/o (the latter will have to change when the 
memory management facilities are modified to support sharing of
pages); a sign extension bug on block numbers extracted from
the core map has been fixed (this caused the system to crash
with certain disk partition layouts on RA81 disks)
.IP \fBvm_mon.c\fP 17
support for virtual memory monitoring; code in this file
is included in the system only if the PGINPROF and/or TRACE
options are configured
.IP \fBvm_page.c\fP 17
the code which handles and processes page faults: \fIpagein\fP;
race conditions in accessing pages in transit and requests
to lock pages for raw i/o have been fixed in this code; a
major path through \fIpagein\fP whose sole purpose was to implement
the software simulated reference bit has been ``parallel coded''
in assembly language (this appears to decrease system time by
at least 5% when a system is paging heavily); \fIpagein\fP now
has a second parameter indicating if the page to be faulted in
should be left locked (this eliminated the need for the 
SDLYU flag in the \fIproc\fP structure)
.IP \fBvm_proc.c\fP 17
mainly code to manage virtual memory allocation during
process creation and destruction (the virtual memory
equivalent of ``passing the buck'' is done here).
.IP \fBvm_pt.c\fP 17
code for manipulating process page tables; knowledge
of the user area is found here as it relates to the
user address space page tables
.IP \fBvm_sched.c\fP 17
the code for process 0, the scheduler, lives here; other
routines which monitor and meter virtual memory activity (used
in implementing high level scheduling policies) also are present;
this code has been better parameterized to isolate machine-dependent
heuristics used in the scheduling policies
.IP \fBvm_subr.c\fP 17
miscellaneous routines: some for manipulating accessability
of virtual memory, others for mapping virtual addresses to
logical segments (text, data, stack)
.IP \fBvm_sw.c\fP 17
indirect driver for interleaved, multi-controller, paging
area; modified to support interleaved partitions of
different sizes
.IP \fBvm_swap.c\fP 17
code to handle process related issues of swapping
.IP \fBvm_swp.c\fP 17
code to handle swap i/o
.IP \fBvm_text.c\fP 17
code to handle shared text segments \- the ``text'' table
