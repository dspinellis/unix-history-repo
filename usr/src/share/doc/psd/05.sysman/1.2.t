.\" Copyright (c) 1983 Regents of the University of California.
.\" All rights reserved.  The Berkeley software License Agreement
.\" specifies the terms and conditions for redistribution.
.\"
.\"	@(#)1.2.t	6.1 (Berkeley) %G%
.\"
.sh "Memory management\(dg
.NH 3
Text, data and stack
.PP
.FS
\(dg This section represents the interface planned for later
releases of the system.  Of the calls described in this section,
only \fIsbrk\fP and \fIgetpagesize\fP are included in 4.3BSD.
.FE
Each process begins execution with three logical areas of memory
called text, data and stack.  
The text area is read-only and shared, while the data and stack
areas are private to the process.  Both the data and stack areas may
be extended and contracted on program request.  The call
.DS
addr = sbrk(incr);
result caddr_t addr; int incr;
.DE
changes the size of the data area by \fIincr\fP bytes and
returns the new end of the data area, while
.DS
addr = sstk(incr);
result caddr_t addr; int incr;
.DE
changes the size of the stack area.
The stack area is also automatically extended as needed.
On the VAX the text and data areas are adjacent in the P0 region,
while the stack section is in the P1 region, and grows downward.
.NH 3
Mapping pages
.PP
The system supports sharing of data between processes
by allowing pages to be mapped into memory.  These mapped
pages may be \fIshared\fP with other processes or \fIprivate\fP
to the process.
Protection and sharing options are defined in <mman.h> as:
.DS
._d
/* protections are chosen from these bits, or-ed together */
#define	PROT_READ	0x4	/* pages can be read */
#define	PROT_WRITE	0x2	/* pages can be written */
#define	PROT_EXEC	0x1	/* pages can be executed */

/* mapping type; choose one */
#define MAP_FILE	0x0001	/* mapped from a file */
#define MAP_SWAP	0x0002	/* mapped to swap space */
#define MAP_MEMORY	0x0004	/* mapped to device memory */

/* sharing types; choose either SHARED or PRIVATE */
#define	MAP_SHARED	0x0010	/* share changes */
#define	MAP_PRIVATE	0x0020	/* changes are private */

/* other options */
#define MAP_FIXED	0x0040	/* map segment must be allocated at addr */
#define MAP_EXTEND	0x0080	/* for MAP_FILE, the file may be extended */
.DE
The cpu-dependent size of a page is returned by the
\fIgetpagesize\fP system call:
.DS
pagesize = getpagesize();
result int pagesize;
.DE
.PP
The call:
.DS
caddr_t
maddr = mmap(addr, len, prot, share, fd, pos);
result caddr_t maddr; caddr_t addr; int *len, prot, share, fd; off_t pos;
.DE
causes the pages starting at \fIaddr\fP and continuing
for at most \fIlen\fP bytes to be mapped from the object represented by
descriptor \fIfd\fP, at absolute position \fIpos\fP.
The starting address of the region is returned.
The actual amount mapped is returned in len.
The parameter \fIshare\fP specifies whether modifications made to
this mapped copy of the page,
are to be kept \fIprivate\fP, or are to be \fIshared\fP with
other references.
The parameter \fIprot\fP specifies the accessibility
of the mapped pages.
The \fIaddr\fP, \fIlen\fP, and \fIpos\fP parameters
must all be multiples of the pagesize.
.PP
A process can move pages within its own memory by using the
.I mremap
call:
.DS
mremap(addr, len, prot, share, fromaddr);
caddr_t addr; int len, prot, share; caddr_t fromaddr;
.DE
This call maps the pages starting at \fIfromaddr\fP to the address specified
by \fIaddr\fP.
.PP
A mapping can be removed by the call
.DS
munmap(addr, len);
caddr_t addr; int len;
.DE
This call causes further references to these pages
to generate invalid memory references.
If the segment is mapped MAP_FILE with mode PROT_WRITE,
the file is truncated to the length specified by \fIlen\fP.
.NH 3
Page protection control
.PP
A process can control the protection of pages using the call
.DS
mprotect(addr, len, prot);
caddr_t addr; int len, prot;
.DE
This call changes the specified pages to have protection \fIprot\fP\|.
.NH 3
Giving and getting advice
.PP
A process that has knowledge of its memory behavior may
use the \fImadvise\fP call:
.DS
madvise(addr, len, behav);
caddr_t addr; int len, behav;
.DE
\fIBehav\fP describes expected behavior, as given
in <mman.h>:
.DS
._d
#define	MADV_NORMAL	0	/* no further special treatment */
#define	MADV_RANDOM	1	/* expect random page references */
#define	MADV_SEQUENTIAL	2	/* expect sequential references */
#define	MADV_WILLNEED	3	/* will need these pages */
#define	MADV_DONTNEED	4	/* don't need these pages */
.DE
Finally, a process may obtain information about whether pages are
core resident by using the call
.DS
mincore(addr, len, vec)
caddr_t addr; int len; result char *vec;
.DE
Here the current core residency of the pages is returned
in the character array \fIvec\fP, with a value of 1 meaning
that the page is in-core.
.NH 3
Synchronization primitives
.PP
Two routines provide services analogous to the kernel
sleep and wakeup functions interpreted in the domain of
shared memory.
A process may relinquish the processor by calling msleep:
.DS
msleep(addr)
caddr_t addr;
.DE
Addr must lie within a MAP_SHARED segment with at least modes
PROT_READ and PROT_WRITE.
The process will remain in a sleeping state
until some other process issues an \fImwakeup\fP for the same byte
within the region (possibly from a different virtual address)
using the call:
.DS
mwakeup(addr)
caddr_t addr;
.DE
.PP
To avoid system calls for the usual case of an uncontested lock,
library routines are provided to acquire and release locks.
To acquire a lock a process calls:
.DS
mset(addr)
caddr_t addr;
.DE
\fIMset\fP indivisibly tests and sets the memory location addr.
If the the previous value is zero, the process has acquired the lock
and \fImset\fP returns immediately.
If the previous value is non-zero, the ``want'' bit is set and
the test-and-set is retried;
if the lock is still unavailable \fImset\fP calls \fImsleep\fP and tries again.
.PP
To release a lock a process calls:
.DS
mclear(addr)
caddr_t addr;
.DE
\fIMclear\fP indivisibly tests and clears the memory location addr.
If the ``want'' bit is zero in the previous value,
\fImclear\fP returns immediately.
If the ``want'' bit is non-zero in the previous value,
\fImclear\fP calls \fImwakeup\fP before returning.
