.\" Copyright (c) 1986 Regents of the University of California.
.\" All rights reserved.  The Berkeley software License Agreement
.\" specifies the terms and conditions for redistribution.
.\"
.\"	@(#)sys.vm.t	1.8 (Berkeley) 4/11/86
.\"
.NH 2
Changes in the virtual memory system
.PP
The virtual memory system in 4.3BSD is largely unchanged from 4.2BSD.
The changes that have been made were in two areas: adapting the VM
substem to larger physical memories, and optimization by simplifying
many of the macros.
.PP
Many of the internal limits on the virtual memory system
were imposed by the \fIcmap\fP structure.
This structure was enlarged to increase those limits.
The limit on physical memory has been changed from 8 megabytes to 64 megabytes,
with expansion space provided for larger limits,
and the limit of 15 mounted file systems has been changed to 255.
The maximum file system size has been increased to 8 gigabytes,
number of processes to 65536,
and per-process size to 64 megabytes of data and 64 megabytes of stack.
Configuration parameters and other segment size limits were
converted from pages to bytes. 
Note that most of these are upper bounds;
the default limits for these quantities are tuned for systems
with 4-8 megabytes of physical memory.
The process region sizes may be adjusted
with kernel configuration file options;
for example,
.nf
.RS
.sp
options	MAXDSIZ=33554432
.sp
.RE
.fi
increases the data segment to 32 megabytes.
With no option, data segments receive a hard limit of roughly 17Mb
and a soft limit of 6Mb
(that may be increased with the csh limit command).
.PP
The global clock page replacement algorithm used to have a single
hand that was used both to mark and to reclaim memory.
The first time that it encountered a page it would clear its reference bit.
If the reference bit was still clear on its next pass across the page,
it would reclaim the page.
(On the VAX, the reference bit was simulated using the valid bit.)
The use of a single hand does not work well with large physical
memories as the time to complete a single revolution of the hand
can take up to a minute or more.
By the time the hand gets around to the marked pages,
the information is usually no longer pertinent.
During periods of sudden shortages,
the page daemon will not be able to find any reclaimable pages until
it has completed a full revolution.
To alleviate this problem,
the clock hand has been split into two separate hands.
The front hand clears the reference bits,
and the back hand follows a constant number of pages behind,
reclaiming pages that have have not been referenced since the front hand
passed.
While the code has been written in such a way as
to allow the distance between
the hands to be varied, we have not yet found any algorithms
suitable for determining how to dynamically adjust this distance.
The parameters determining the rate of page scan
have also been updated to reflect larger configurations.
The free memory threshold at which \fIpageout\fP begins was reduced
from one-fourth of memory to 512K
for machines with more than 2 megabytes of user memory.
The scan rate is now independent of memory size
instead of proportional to memory size.
.PP
The text table is now managed differently.
Unused entries are treated
as a cache, similar to the usage of the inode table.
Entries with reference counts of 0 are placed in an LRU cache
for potential reuse.
In effect, all texts are ``sticky,'' except that they are flushed
after a period of disuse or overflow of the table.
The sticky bit works
as before, preventing entries from being freed and locking
text files into the cache.
The code to prevent modification of running
texts was cleaned up by keeping a pointer to the text entry in the
inode, allowing texts to be freed when unlinking files without linear
searches.
.PP
The swap code was changed
to handle errors a bit better (\fIswapout\fP doesn't do \fIswkill\fPs, it just
reflects errors to the caller for action there).
During swapouts,
interrupts are now blocked for less time after freeing the pages
of the user structure and page tables
(as explained by the old comment from \fIswapout\fP,
``XXX hack memory interlock''),
and this is now done only when swapping out the current process.
The same situation existed in \fIexit\fP, but had not yet been protected
by raised priority.
.PP
Various routines that took page numbers as arguments
now take \fIcmap\fP pointers instead to reduce the number of conversions.
These include \fImlink\fP, \fImunlink\fP, \fImlock\fP, \fImunlock\fP, and
\fImwait\fP.
\fIMlock\fP and \fImunlock\fP are generally used in their macro forms.
.PP
The remainder of the section details the other changes according to source
file.
.XP vm_mem.c
Low-level support for mapped files was removed,
as the descriptor field in the page table entry was too small.
Callers of \fImunhash\fP must block interrupts with \fIsplimp\fP
between checking for the presence of a block in the hash list
and removing it with \fImunhash\fP in order to avoid reallocation
of the page and a subsequent panic.
.XP vm_page.c
When filling a page from the text file, \fIpagein\fP uses a new routine,
\fIfodkluster\fP, to bring in additional pages that are contiguous
in the filesystem.
If errors occur while reading in text pages,
no page-table change is propagated to other users of the shared image,
allowing them to retry and notice the error
if they attempt to use the same page.
Virtual memory initialization code has been collected into \fIvminit\fP,
which adjusts swap interleaving to allow the configured size limits,
set up the parameters for the clock algorithm, and set the initial
virtual memory-related resource limits.
The limit to resident-set size is set to the size of the available user memory.
This change causes a single large process occupying most of memory
to begin random page replacement as memory resources run short.
Several races in \fIpagein\fP have been detected and fixed.
Most of the \fIpageout\fP code was moved to \fIcheckpage\fP
in implementing the two-handed clock algorithm.
.XP vm_proc.c
The \fIsetjmp\fP in \fIprocdup\fP was changed to \fIsavectx\fP,
which saves all registers, not just those needed to locate
the others on the stack.
.XP vm_pt.c
The \fIsetjmp\fP call in \fIptexpand\fP was changed to \fIsavectx\fP
to save all registers before initiating a swapout.
\fIVrelu\fP does an \fIsplimp\fP before freeing user-structure pages
if running on behalf of the current process.
This had been done by \fIswapout\fP before, but not by \fIexit\fP.
.XP vm_sched.c
The swap scheduler looks through the \fIallproc\fP list for processes
to swap in or out.
A call to \fIremrq\fP when swapping sleeping processes was unnecessary
and was removed.
If swapouts fail upon exhaustion of swap space,
\fIsched\fP does not continue to attempt swapouts.
.XP vm_subr.c
The \fIptetov\fP function and the unused \fIvtopte\fP function
were recoded without using the usual macros
in order to fold the similar cases together.
.XP vm_sw.c
The error returned by \fIswapon\fP when the device is not one of those
configured was changed from ENODEV to EINVAL for accuracy.
The search for the specified device begins with the first entry
so that the error is correct (EBUSY) when attempting to enable the primary
swap area.
.XP vm_swap.c
The \fIswapout\fP routine now leaves any \fIswkill\fP to its caller.
This avoids killing processes in a few situations.
It uses \fIxdetach\fP instead of \fIxccdec\fP.
Several unneeded \fIspl\fP's were deleted.
.XP vm_swp.c
The \fIswap\fP routine now consistently returns error status.
\fIPhysio\fP was modified to do scatter-gather I/O correctly.
.XP vm_text.c
The text routines use a text free list as a cache of text images,
resulting in numerous changes throughout this file.
\fIXccdec\fP now works only on locked text entries,
and is replaced by \fIxdetach\fP for external callers.
\fIXumount\fP frees unused swap images from all
devices when called with NODEV as argument.
It is no longer necessary to search the text table
to find any text associated with an inode in \fIxrele\fP,
as the inode stores a pointer to any text entry mapping it.
Statistics are gathered on the hit rate of the cache and its cost.
