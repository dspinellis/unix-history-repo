.\" Copyright (c) 1983, 1993, 1994
.\"	The Regents of the University of California.  All rights reserved.
.\"
.\" %sccs.include.redist.roff%
.\"
.\"	@(#)1.2.t	8.9 (Berkeley) %G%
.\"
.Sh 2 "Memory management
.Sh 3 "Text, data, and stack
.PP
Each process begins execution with three logical areas of memory
called text, data, and stack.  
The text area is read-only and shared,
while the data and stack areas are writable and private to the process.
Both the data and stack areas may be extended and contracted on program
request.
The call:
.DS
.Fd brk 1 "set data section size
brk(addr);
caddr_t addr;
.DE
sets the end of the data segment to the specified address.
More conveniently, the end can be extended by \fIincr\fP bytes,
and the base of the new area returned with the call:
.DS
.Fd sbrk 1 "change data section size
addr = sbrk(incr);
result caddr_t addr; int incr;
.DE
Application programs normally use the library routines
.Fn malloc
and
.Fn free ,
which provide a more convenient interface than
.Fn brk
and
.Fn sbrk .
.LP
There is no call for extending the stack,
as it is automatically extended as needed.
.Sh 3 "Mapping pages
.PP
The system supports sharing of data between processes
by allowing pages to be mapped into memory.  These mapped
pages may be \fIshared\fP with other processes or \fIprivate\fP
to the process.
Protection and sharing options are defined in \fI<sys/mman.h>\fP as:
.DS
.TS
l s
l l.
Protections are chosen from these bits, or-ed together:

PROT_READ	/* pages can be read */
PROT_WRITE	/* pages can be written */
PROT_EXEC	/* pages can be executed */
.TE
.DE
.DS
.TS
l s
l l.
Flags contain sharing type and options.  Sharing options, choose one:

MAP_SHARED	/* share changes */
MAP_PRIVATE	/* changes are private */
.TE
.DE
.DS
.TS
l s
l l.
Option flags\(dg:

MAP_ANON	/* allocated from virtual memory; \fIfd\fP ignored */
MAP_FIXED	/* map addr must be exactly as requested */
MAP_NORESERVE	/* don't reserve needed swap area */
MAP_INHERIT	/* region is retained after exec */
MAP_HASSEMAPHORE	/* region may contain semaphores */
.TE
.DE
.FS
\(dg In 4.4BSD, only MAP_ANON and MAP_FIXED are implemented.
.FE
The size of a page is cpu-dependent, and is returned by the
.Fn sysctl
interface described in section
.Xr 1.7.1 .
The
.Fn getpagesize
library routine is provided for convenience and backward compatibility:
.DS
.Fd getpagesize 0 "get system page size
pagesize = getpagesize();
result int pagesize;
.DE
.LP
The call:
.DS
.Fd mmap 6 "map files or devices into memory
maddr = mmap(addr, len, prot, flags, fd, pos);
result caddr_t maddr; caddr_t addr; size_t len; int prot, flags, fd; off_t pos;
.DE
causes the pages starting at \fIaddr\fP and continuing
for at most \fIlen\fP bytes to be mapped from the object represented by
descriptor \fIfd\fP, starting at byte offset \fIpos\fP.
If \fIaddr\fP is NULL, the system picks an unused address for the region.
The starting address of the region is returned;
for the convenience of the system,
it may differ from that supplied
unless the MAP_FIXED flag is given,
in which case the exact address will be used or the call will fail.
The \fIaddr\fP parameter
must be a multiple of the pagesize (if MAP_FIXED is given).
If \fIpos\fP and \fIlen\fP are not a multiple of pagesize,
they will be rounded (down and up respectively)
to a page boundary by the system;
the rounding will cause the mapped region to extend past the specified range.
A successful
.Fn mmap
will delete any previous mapping
in the allocated address range.
The parameter \fIprot\fP specifies the accessibility
of the mapped pages.
The parameter \fIflags\fP specifies
the type of object to be mapped,
mapping options, and
whether modifications made to
this mapped copy of the page
are to be kept \fIprivate\fP, or are to be \fIshared\fP with
other references.
Possible types include MAP_SHARED or MAP_PRIVATE that
map a regular file or character-special device memory,
and MAP_ANON, which maps memory not associated with any specific file.
The file descriptor used when creating MAP_ANON regions is not used
and should be \-1.
The MAP_INHERIT flag allows a region to be inherited after an
.Fn execve .
The MAP_HASSEMAPHORE flag allows special handling for
regions that may contain semaphores.
The MAP_NORESERVE flag allows processes to allocate regions whose
virtual address space, if fully allocated,
would exceed the available memory plus swap resources.
Such regions may get a SIGSEGV signal if they page fault and resources
are not available to service their request;
typically they would free up some resources via
.Fn munmap
so that when they return from the signal the page
fault could be completed successfully.
.LP
A facility is provided to synchronize a mapped region with the file
it maps; the call:
.DS
.Fd msync 2 "synchronize a mapped region
msync(addr, len);
caddr_t addr; size_t len;
.DE
causes any modified pages in the specified region to be synchronized
with their source and other mappings.
If necessary, it writes any modified pages back to the filesystem, and updates
the file modification time.
If \fIlen\fP is 0, all modified pages within the region containing \fIaddr\fP
will be flushed;
this usage is provisional, and may be withdrawn.
If \fIlen\fP is non-zero, only the pages containing \fIaddr\fP and \fIlen\fP
succeeding locations will be examined.
Any required synchronization of memory caches
will also take place at this time.
.LP
Filesystem operations on a file that is mapped for shared modifications
are currently unpredictable except after an
.Fn msync .
.LP
A mapping can be removed by the call
.DS
.Fd munmap 2 "remove a mapping
munmap(addr, len);
caddr_t addr; size_t len;
.DE
This call deletes the mappings for the specified address range,
and causes further references to addresses within the range
to generate invalid memory references.
.Sh 3 "Page protection control
.LP
A process can control the protection of pages using the call:
.DS
.Fd mprotect 3 "control the protection of pages
mprotect(addr, len, prot);
caddr_t addr; size_t len; int prot;
.DE
This call changes the specified pages to have protection \fIprot\fP\|.
Not all implementations will guarantee protection on a page basis;
the granularity of protection changes may be as large as an entire region.
.Sh 3 "Giving and getting advice
.LP
A process that has knowledge of its memory behavior may
use the
.Fn madvise \(dg
call:
.FS
\(dg The entry point for this system call is defined,
but is not implemented,
so currently always returns with the error ``Operation not supported.''
.FE
.DS
.Fd madvise 3 "give advise about use of memory
madvise(addr, len, behav);
caddr_t addr; size_t len; int behav;
.DE
\fIBehav\fP describes expected behavior, as given
in \fI<sys/mman.h>\fP:
.DS
.TS
l l.
MADV_NORMAL	/* no further special treatment */
MADV_RANDOM	/* expect random page references */
MADV_SEQUENTIAL	/* expect sequential references */
MADV_WILLNEED	/* will need these pages */
MADV_DONTNEED	/* don't need these pages */
.TE
.DE
The
.Fn mincore \(dg
function allows a process to obtain information
about whether pages are memory resident:
.DS
.Fd mincore 3 "get advise about use of memory
mincore(addr, len, vec);
caddr_t addr; size_t len; result char *vec;
.DE
Here the current memory residency of the pages is returned
in the character array \fIvec\fP, with a value of 1 meaning
that the page is in-memory.
.Fn Mincore
provides only transient information about page residency.
Real-time processes that need guaranteed residence over time
can use the call:
.DS
.Fd mlock  2 "lock physical pages in memory
mlock(addr, len);
caddr_t addr; size_t len;
.DE
This call locks the pages for the specified address range into memory
(paging them in if necessary)
ensuring that further references to addresses within the range
will never generate page faults.
The amount of memory that may be locked is controlled by a resource limit,
see section
.Xr 1.6.3 .
When the memory is no longer critical it can be unlocked using:
.DS
.Fd munlock  2 "unlock physical pages in memory
munlock(addr, len);
caddr_t addr; size_t len;
.DE
After the
.Fn munlock
call, the pages in the specified address range are still accessible
but may be paged out if memory is needed and they are not accessed.
.Sh 3 "Synchronization primitives
Primitives are provided for synchronization using semaphores
in shared memory.\(dd
.FS
\(dd All currently unimplemented, no entry points exists.
.FE
These primitives are expected to be superseded by the semaphore
interface being specified by the POSIX 1003 Pthread standard.
They are provided as an efficient interim solution.
Application programmers are encouraged to use the Pthread interface
when it becomes available.
.PP
Semaphores must lie within a MAP_SHARED region with at least modes
PROT_READ and PROT_WRITE.
The MAP_HASSEMAPHORE flag must have been specified when the region was created.
To acquire a lock a process calls:
.DS
.Fd mset 2 "acquire and set a semaphore
value = mset(sem, wait);
result int value; semaphore *sem; int wait;
.DE
.Fn Mset
indivisibly tests and sets the semaphore \fIsem\fP.
If the previous value is zero, the process has acquired the lock and
.Fn mset
returns true immediately.
Otherwise, if the \fIwait\fP flag is zero,
failure is returned.
If \fIwait\fP is true and the previous value is non-zero,
.Fn mset
relinquishes the processor until notified that it should retry.
.LP
To release a lock a process calls:
.DS
.Fd mclear 2 "release a semaphore and awaken waiting processes
mclear(sem);
semaphore *sem;
.DE
.Fn Mclear
indivisibly tests and clears the semaphore \fIsem\fP.
If the ``WANT'' flag is zero in the previous value,
.Fn mclear
returns immediately.
If the ``WANT'' flag is non-zero in the previous value,
.Fn mclear
arranges for waiting processes to retry before returning.
.PP
Two routines provide services analogous to the kernel
.Fn sleep
and
.Fn wakeup
functions interpreted in the domain of shared memory.
A process may relinquish the processor by calling
.Fn msleep
with a set semaphore:
.DS
.Fd msleep 1 "wait for a semaphore
msleep(sem);
semaphore *sem;
.DE
If the semaphore is still set when it is checked by the kernel,
the process will be put in a sleeping state
until some other process issues an
.Fn mwakeup
for the same semaphore within the region using the call:
.DS
.Fd mwakeup 1 "awaken process(es) sleeping on a semaphore
mwakeup(sem);
semaphore *sem;
.DE
An
.Fn mwakeup
may awaken all sleepers on the semaphore,
or may awaken only the next sleeper on a queue.
