.RP
.if n .ds dg +
.if t .ds dg \(dg
.if n .ds dd =
.if t .ds dd \(dd
.if n .ds _ _
.if t .ds _ \d\(mi\u
.TL
Data Structures Added in the
.br
Berkeley Virtual Memory Extensions to the
.br
UNIX\(dg Operating System\(dd
.AU
\u\*:\dOzalp Babao\*~glu
.AU
William Joy
.AI
Computer Science Division
Department of Electrical Engineering and Computer Science
University of California, Berkeley
Berkeley, California 94720
.AB
.FS
\*(dg UNIX is a Trademark of Bell Laboratories
.FE
.FS
\*(dd Work supported by the National Science Foundation under grants
MCS 7807291, MCS 7824618, MCS 7407644-A03 and by an IBM Graduate Fellowship
to the second author.
.FE
.FS
* VAX is a trademark of Digital Equipment Corporation.
.FE
.PP
This document describes the major new data structures that have
been introduced to the Version 7 \s-2UNIX\s0 system to support 
demand paging on the
\s-2VAX\u*\d-11/780\s0.  The reader should be basically familiar 
with the \s-2VAX\s0 architecture, as described in the
.I "VAX-11/780 Hardware Handbook."
.PP
When relevant, along with the data structures, we present
related system constants and macro definitions, and
some indications of how the data is used by the system algorithms.
We also describe the extensions/deletions that have been made to some
of the existing data structures.
Full description of the paging system algorithms, however, is not
given here.
.AE
.SH
Introduction
.PP
The paging subsystem of the virtual memory extension to the system
maintains four new, basic data structures:
the
.I "(system and process) page tables,"
the 
.I "kernel map,"
the
.I "core map "
and the 
.I "disk map."
This document consists of a description of each of these data structures
in turn.
.br
.ne .75i
.sp .375i
.SH
PAGE TABLES
.PP
The format of the process page tables are defined in the system header
file
.B pte.h.\*(dg
.FS
\*(dg Copies of the system definitions (header) files related to the
paging subsystem appear at the end of this document.
.FE
The basic form of the Page Table Entry (PTE) is dictated 
by the \s-2VAX-11/780\s0 architecture.
Both the first level page table,
known as
.I Sysmap,
and the second level per-process
page tables consist of arrays of this structure. 
The paging system makes use of several bit fields which
have no meaning to the hardware.
The individual fields are:
.IP \fBpg\*_prot\fR 16n
The
.I Protection
Bits.  Define the access mode of the corresponding page.
Modes used by the paging system include PG\*_NOACC for invalid entries,
PG\*_KR for the text of the kernel, PG\*_KW for the kernel data space,
PG\*_URKR for text portions of user processes, PG\*_URKW for text portions
of user processes during modification (old form \fIexec\fR, and \fIptrace\fR\|)
of text images, and PG\*_UW for normal user data pages.
.IP \fBpg\*_m\fR 16n
The
.I Modify
Bit.  Set by hardware as a result of a write access to the page.  Examined 
and altered by the paging subsystem to find out if a page is
.I dirty
and has to be written back to disk before the page frame can be released.
.IP \fBpg\*_swapm\fR
Indicates whether the page has been initialized, but never written to
the swapping area.  This bit is necessary because \fBpg\*_m\fR is normally
or'ed with \fBpg\*_vreadm\fR to see if the page has ever been modified,
and thus \fBpg\*_m\fR is unavailable to force a swap in this case.
.IP \fBpg\*_vreadm\fR 16n
Indicates whether the page has been modified since it was last initialized.
(Initialization occurs during stack growth, growth due to
.I break
system calls, and as a result of
.I exec
and
.I vread
system calls.)
For use by the
.I vwrite
system call, which looks at the inclusive-or of this and the
\fBpg\*_m\fR bit.
A
.I vwrite
also clears this bit.
.IP \fBpg\*_fod\fR 16n
The
.I "Fill on Demand"
Bit.  Set only when the valid bit (described below) is reset,
indicating that the page has not yet been initialized.  When
referenced, the page will either be zero filled, or filled with a block
from the file system based on other fields described next.
.IP \fBpg\*_fileno\fR 16n
Meaningful only when the
.I pg\*_fod
bit is set.
If the
.I pg\*_fileno
field has value
PG\*_FZERO, then a reference to such a page results in the
allocation and clearing of a page frame rather than a disk transfer.
When stack or data segment growth
occurs, the page table entries are initialized to fill on
demand pages with PG\*_FZERO
in their
.I pg\*_fileno
fields. 
The page is otherwise filled in from the file system, either from the inode
associated with the currently executing process (if
.I pg\*_fileno
is PG\*_FTEXT), or from a file.
.IP \fBpg\*_blkno\fR 16n
Gives the block number, in a file system determined by the value of the
.I pg\*_fileno
field, from which the page is to be initialized.  Note that this is the
logical block number in the file system,
not in the mapped object.  Thus no mapping is required at page fault time
to locate the actual disk block; the system simply uses the
.I pg\*_fileno
field to locate an inode and uses the
.I i\*_dev
field of that inode in a call to a device strategy routine.
The size of this field (20 bits) limits the maximum size of a filesystem
to 2\(ua20 blocks (1M block).
.IP \fBpg\*_v\fR 16n
The
.I Valid
bit.
Set only when the
.I pg\*_fod
bit is reset.
Indicates that the mapping established in the PTE is valid and can
be used by the hardware for address translation.  Access to the
PTE when this bit is reset causes an
.I "Address Translation Not Valid"
fault and triggers the paging mechanism. 
If both the valid and fill on demand bits are reset, but the
.I pg\*_pfnum
field is non-zero, then the page is still in memory and can 
be reclaimed either from the loop or the free list
without an I/O operation by simply
revalidating the page, after possibly removing it from the free list.
If the
.I pg\*_fod
bit is not set, and the
.I pg\*_pfnum
field is zero, then the page has to be retrieved from disk.
Note that resetting the valid bit for pages which are still resident allows
for software detection and recording of references to pages, simulating a
.I reference
bit, which the \s-2VAX\s0 hardware does not provide.
.IP \fBpg\*_pfnum\fR 16n
The
.I "Page Frame Number."
Meaningful only when the
.I pg\*_fod
bit is reset.
If the page frame is valid, then this gives the physical page frame number
that the virtual page is currently mapped to.  If no physical page frame
is currently allocated this field is zero
(except in page table entries in
.I Sysmap,
where unused entries are not always cleared.)
.SH
System Page Tables
.PP
The first level page table
.I Sysmap
consists of a physically contiguous
array of PTEs defined by the processor registers SBR
(System Base Register), and SLR
(System Length Register).
SLR is loaded with the constant
.I Syssize
at system initialization and remains fixed thereafter.
.PP
The first four pages of the
.I Sysmap
map the kernel virtual memory from addresses 0x80000000 to the end of
kernel data space onto the first pages of physical memory.  Four pages
is enough to map a kernel supporting a full load of memory 
on a \s-2VAX-11/780\s0.
Immediately after the pages which map the kernel text and data are the
entries which map the user structure of the current running process
(the
.I u.\&
area.)
The
.I u.\&
area is currently six pages long, with the first two of these pages mapping the
.I user
structure itself, and the other four pages mapping the per-process kernel
stack.\*(dg
.FS
\*(dg Currently all six pages are allocated physical memory; it is planned
that in the future, the third of these six pages will be made a
read-only copy of
.I zeropage.
Since the stack is observed rarely to enter the third page
this will leave a full page for unanticipated worst-case stack growth,
and give a clean termination condition should the stack ever accidentally
grow beyond three pages.
.FE
The position of the
.I u.\&
in
.I Sysmap
determines that it will live, in this system,
at 0x80040000.
.PP
After the map entries reserved for the
.I u.\&
area are 16 words of system map reserved for utility usage.
The
.I copyseg
routine uses one of these (CMAP2) while making a copy of one data page
to map the destination page frame.
This is necessary because at the point of copying (during the
.I fork
system call) the parent process is running, while the
destination page is not in the parents address space, but rather
destined for the childs address space.  Since the parent may
fault on the source page during the copy, the contents of this
map are saved in the software extension to the
.I pcb
during context switch.
Other utilities are used by
.I clearseg
to map pages to be cleared in implementing zero fill on demand pages,
and by the
.B mem.c
driver to map pages in
.B /dev/mem
when accessing physical memory.
.PP
The
.I Sysmap
continues with sets of entries for the UBA control and map registers,
the physical device memory of a UNIBUS adaptor, and
the control and map registers of upto three MASSBUSS adaptors.
Each of these consists of 16 page table entries, mapping 8K bytes.
.PP
Next, there are a set of map entries for manipulating
.I u.\&
structures other than the one of the current running process.  For instance,
the page out demon, which runs as process 2, needs access to the diskmap
information of a process whose page is being written to the disk.
To get access to this, it uses six entries in the
.I Sysmap,
known as
.I Pushmap,
to map this 
.I u.\&
into kernel virtual memory at a virtual address given by
.I pushutl.
There are several other map/utl pairs:
.I Swapmap
and
.I swaputl,
.I Xswapmap
and
.I xswaputl,
.I Xswap2map
and
.I xswap2utl,
.I Forkmap
and
.I forkutl,
.I Vfmap
and
.I vfutl.
These are used in swapping and forking new processes.
.PP
The final portion of the
.I Sysmap
consists of a map/utl like pair
.I Usrptmap/usrpt
which is a resource allocated to hold the first level page tables
of all currently core-resident processes.  This is a very important
structure and will be described after we describe the basic structure
of the page tables of a process.
.SH
Per-process page tables
.PP
Each process possesses three logical page tables: one to map each
of the text, data and stack segments.  Large portions of the system
refer to page table entries in each of these segments by an index, with
the first page of each segment being numbered 0.
.PP
For the \s-2VAX-11/780\s0 version of the system, these page 
tables are implemented by two physically distinct page tables, the
.I "P0 Page Table,"
mapping the text and data segments, and the
.I "P1 Page Table,"
mapping the stack segment. 
Within the P0 region, the text segment is mapped starting at virtual
address 0 with the data segment following on the first page boundary
after it.*
.FS
*Later versions of the system for the VAX-11/780
may align the data starting at a 64K byte boundary, i.e. each of the
text, data and stack segments will use an integral number of first
level (\fISysmap\fR\|) entries.  There would then be a minimum of one page of
page tables for each segment, and sharing of text page table pages
will be made simple using the
ability of the first level entries to point to common page table pages.
.FE
The stack segment, on the other hand, starts at the bottom of the P1
region and grows toward smaller virtual addresses.  The constant USRSTACK
corresponds to the address of the byte one beyond the user stack.
The process page tables are contiguous in kernel virtual
memory (KVM) and are situated with
the P0 table followed by the P1 table such that the top of the first
and the bottom of the second are aligned at page boundaries.  Note that this
results in a
.I gap
between the two page tables whose size does not normally exceed one page.
.PP
The size of the process' page tables (P0 + gap + P1) in pages is
contained in the software extension to the pcb located at the top of
the process' 
.I u.\&
area (in
.I u\*_pcb.pcb\*_szpt).
This number is also duplicated in the 
.I proc
structure field
.I p\*_szpt.
.PP
Given
.I x
as the virtual size of a process,
.B ctopt(x)
results in the minimum number of pages of page tables required to map it.
A process accesses its page tables through the descriptors
P0BR, P0LR, P1BR, and P1LR.
The per-process copies of these processor registers are contained
in the pcb and are loaded and saved at process context switch time.
A copy of the P0 region base register is contained in the 
.I proc
structure field
.I p\*_p0br.
.PP
Given the above description of the process layout in virtual memory, a
pointer to a process and a page table entry,
the
.B isa?pte
macros result in 
.I true
if the PTE is within the respective segment of process
.I p:
.DS
.ta 1.75i
\fBisaspte(p, pte)\fR	stack segment?
\fBisatpte(p, pte)\fR	text segment?
\fBisadpte(p, pte)\fR	data segment?
.DE
Conversion between segment page numbers and pointers to page 
table entries can be achieved by the following macros, where
.I p
is the process of interest, and
.I i
is the virtual page number within the segment (a non-negative integer).
These are used in dealing with the
.I "core map"
where the page numbers are kept in this form for compactness.
.DS
.ta 1.75i
\fBtptopte(p, i)\fR	text page number to pte
\fBdptopte(p, i)\fR	data page number to pte
\fBsptopte(p, i)\fR	stack page number to pte
.DE
.PP
The \s-2VAX\s0 hardware also supports a virtual page frame number.
These begin at 0 for the first page of the P0 region and increase
through the text and data regions.  For the stack region they
begin at the frame before
.I "btop(USRSTACK)"
and decrease.  Note that the first stack page has a large (but positive)
virtual page frame number.
.PP
Page frame numbers in the system are very machine dependent, and are
referred to as ``v''s.  The function
.B "vtopte(p, v)"
will take a
.I v
for a given process
.I p
and give back a pointer to the corresponding page table entry.
The function
.B "ptetov(p, pte)"
performs the inverse operation.
To decide which segment a pte is in, and to thereafter convert
from pte's to segment indices and back, the following macros can be
used: 
.LP
.ID
.nf
.ta 1.75i
\fBisatsv(p, v)\fR	is v in the text segment of process p?
\fBisadsv(p, v)\fR	is v in the data segment of process p?
\fBisassv(p, v)\fR	is v in the stack segment of process p?
\fBvtotp(p, v)\fR	segment page number of page v, which is in text
\fBvtodp(p, v)\fR	segment page number of page v, which is in data
\fBvtosp(p, v)\fR	segment page number of page v, which is in stack
\fBtptov(p, i)\fR	v of i'th text page
\fBdptov(p, i)\fR	v of i'th data page
\fBsptov(p, i)\fR	v of i'th stack page
\fBptetotp(p, pte)\fR	pte to a text segment page number
\fBptetodp(p, pte)\fR	pte to a data segment page number
\fBptetosp(p, pte)\fR	pte to a stack segment page number
\fBtptopte(p, i)\fR	pte pointer for i'th text page
\fBdptopte(p, i)\fR	pte pointer for i'th data page
\fBsptopte(p, i)\fR	pte pointer for i'th stack page
.fi
.DE
The functions
.I vtopte
and
.I ptetov
have trivial definitions in terms of these macros.
.SH
Page table entries as integers
.PP
In a few places in the kernel, it is convenient to deal with page
table entry fields
.I "en masse."
In this case we cast pointers to
page table entries to be pointers to integers and deal with the
bits of the page table entry in parallel.
Thus
.DS
\fBstruct pte\fR *pte;

*(\fBint\fR *)pte = PG\*_UW;
.DE
clears a page table entry to have only an access field allowing user writes,
by referencing it as an integer.
When accessing the page table entry in this way, we use the manifest
constant declarations in the
.I pte.h
file which give us the appropriate bits.
.br
.ne .75i
.sp .375i
.SH
THE KERNEL MAP
.PP
Defined in
.I map.h.
The kernel map is used to manage the portion of kernel virtual memory
(KVM) allocated to mapping page tables of those processes that are
currently loaded.  On the \s-2VAX-11/780\s0 this involves managing page table
entries in the first level page table, in the
.I "Usrptmap/usrpt"
portion of the
.I Sysmap.
The size of the KVM devoted to mapping resident process page tables is
set by USRPTSIZE in number of Sysmap entries.  Note that this allows
the mapping of a maximum of 64K * USRPTSIZE bytes of resident user
virtual address space.  The maximum can be achieved only if there is no
fragmentation in the allocation.
.PP
KVM required to map the page tables of a process 
that is being swapped in is allocated according to a 
.I "first fit"
policy through a call to the standard system resource allocator
.I malloc.
Once a process is swapped in, its page tables remain stationary
in KVM unless the process grows such that it
requires additional pages of page tables.  At that time, the process'
page tables are moved to a new region of KVM that is large enough to
contain them.
Upon swap out, the process deallocates KVM required to map its page
tables through a call to the standard resource release routine
.I mfree.\*(dg
.FS
\*(dg Due to the way in which
.I malloc
works, the KVM mapped by the first entry in
.I Usrptmap
(index 0) is not used.
.FE
.PP
There are two macros which can be used for conversion 
between
.I Usrptmap
indices and kernel virtual addresses.
.DS
.ta 2.0i
\fBkmxtob(a)\fR	converts \fIUsrptmap\fR index to virtual address
\fBbtokmx(b)\fR	converts virtual address to \fIUsrptmap\fR index
.DE
.br
.ne .75i
.sp .375i
.SH
CORE MAP
.PP
The core map structure is defined in
.B cmap.h.
Each entry of core map contains eight bytes of information 
consisting of the following fields:
.IP \fBc\*_next\fR 16n
Index to the next entry in the free list.
The size of this field (14 bits) limits the number of
page frames that can exist in the main memory to 16K (8M bytes).
.IP \fBc\*_prev\fR 16n
Index to the previous entry in the free list.
.IP \fBc\*_page\fR 16n
Virtual page number within the respective segment (text, data or stack).
The size of this field (17 bits) limits the virtual size of
a process segment to 128K pages (i.e., 64M bytes).
.IP \fBc\*_ndx\fR 16n
Index of the proc structure that owns this page frame.  In the case of
shared text, the index is that  of the corresponding text structure.
The size of this field (10 bits) limits the number of
slots in the
.I proc
and
.I text
structures to 1024.
.IP \fBc\*_intrans\fR 16n
The intransit
bit.  Important only for shared text segment pages, but set for private
data pages for purposes of post-mortem analysis.
Indicates that a page-in operation for the corresponding page has already 
been initiated by another process.  Causes the faulting process to 
enter a wait state until awakened by the process that initiated the transfer.
(This is logically part of the \fBc\*_flag\fR field, and is separate because
of alignment considerations in the coremap.)
.IP \fBc\*_flag\fR 16n
8 bits of flags.
.PP
The meanings of the flags are:
.IP \fBMWANT\fR 16n
The page frame has a process sleeping on it.  The process to free it
should perform a wakeup on the page.
.IP \fBMLOCK\fR 16n
Lock bit.  The page frame is involved in raw I/O or page I/O and
consequently unavailable for replacement.
.IP \fBMFREE\fR 16n
Free list bit.  The page frame is currently in the free list.
.IP \fBMGONE\fR 16n
Indicates that the virtual page corresponding to this page frame has
vanished due to either having been deallocated (contraction of the data
segment) or swapped out.
The page will eventually be freed by the process which is holding it, usually
the page-out demon.
.IP \fBMSYS\fR 16n
System page bit.  The page frame has been allocated to a user process'
.I u.\&
area or page tables and therefore unavailable for replacement.
.IP \fBMSTACK\fR 16n
Page frame belongs to a stack segment.
.IP \fBMDATA\fR 16n
Page frame belongs to a data segment.
.IP \fBMTEXT\fR 16n
Page frame belongs to a shared text segment.
.PP
The core map is the central data base for the paging subsystem.
It consists of an array of these structures, one entry for each page 
frame in the main memory excluding those allocated to kernel text and data.
.PP
The memory free list, managed by
.B "memall"
and
.B "memfree"
is created by doubly linking entries in core map.  The reverse link is
provided to speed up page reclaims from the free list which have to
perform an unlink operation.
.PP
There are a pair of macros for converting between core map indices and page
frame numbers, since no core map entries exist for the system.
.DS
.ta 1.75i
\fBcmtopg(x)\fR	converts core map index x to a page frame number
\fBpgtocm(x)\fR	converts a page frame number to a core map index
.DE
The macros for manipulating segment page numbers, which we described
in the section on page tables above, are very useful when dealing with
the core map.
.SH
DISK MAP
.PP
Defined in
.I dmap.h.
The disk map is a per-process data structure that is kept in the process
.I u.\&
area.  The fields are:
.IP \fBdm\*_size\fR 16n
The amount of disk space allocated that is actually used by the segment.
.IP \fBdm\*_alloc\fR 16n
The amount of physical disk space that is allocated to the segment.
.IP \fBdm\*_dmap\fR 16n
An array of disk block numbers marking the beginning of disk areas that
constitute the segment disk image.
.PP
The four instances of the disk map
allow the mapping of process virtual addresses to disk addresses
for the parent data, parent stack, child data, and child stack segments.
The two child maps are used during the
.I fork
system call serving to make both the parent and the child disk images
accessible simultaneously.\*(dg
.FS
\*(dg These could actually be located on the kernel stack, rather than in the 
.I u.\&
area.
.FE
.PP
Each entry in the disk map array contains a disk block number (relative to
the beginning of the swap area) that marks the beginning of a disk area
mapping the corresponding segment of virtual space.
The initial creation of the segment results in DMMIN
blocks (512 bytes each) pointed to by the first disk map entry to be
allocated.  These disk blocks map precisely to the first DMMIN virtual
pages of the corresponding segment.
Subsequent growth of the segment beyond this size results in the
allocation of 2*DMMIN blocks mapping segment virtual page numbers
DMMIN through 3*DMMIN-1.  This doubling process continues until
the segment reaches a size such that the next disk area allocated has size
.I DMMAX
blocks.  Beyond this point, the segment receives DMMAX additional blocks
should it require them.  Limiting the exponential growth at this size
is in an effort to reduce severe disk fragmentation that would otherwise
result for very large segments.
.PP
Note that increasing entries in the array map increasing segment virtual
page numbers.  However, in the case of the stack segment, this actually
means mapping
.I decreasing
process virtual page numbers.  Also note that since a shared text segment
is static in size, its disk image is allocated in one contiguous
block that is described by the text structure fields
.I x\*_daddr
and
.I x\*_size.
.PP
The maximum size (in pages) that a segment can grow is determined by
MAXTSIZ, MAXDSIZ, or MAXSSIZ for text, data, or stack segment
respectively.
Since the procedures that deal with the disk map panic on segment
length overrun, setting the maximum size of a segment to a value
greater than that can be mapped by it's disk map can lead to a
system failure.
To avoid such a situation, the disk map parameters should be set so that
possible segment overgrowth will be detected at an earlier time in
life of a process by
.B "chksize."
Note that the maximum segment size that can be mapped by disk map can be
increased through raising any one or more of the constants NDMIN, 
DMMAX, and NDMAP.
.br
.ne .75i
.sp .375i
.SH
INSTRUMENTATION RELATED STRUCTURES
.PP
Currently, the system maintains counters for various paging related
events that are accumulated and averaged at discrete points in time.
The basic structure as defined in
.I vm.h
has the following fields:
.IP \fBv\*_swpin\fR 16n
Process swap ins.
.IP \fBv\*_swpout\fR 16n
Process swap outs.
.IP \fBv\*_pswpin\fR 16n
Pages swapped in.
.IP \fBv\*_pswpout\fR 16n
Pages swapped out.
.IP \fBv\*_pgin\fR 16n
Page faults requiring disk I/O.
.IP \fBv\*_pgout\fR 16n
Dirty page writes.
.IP \fBv\*_intrans\fR 16n
Page faults on shared text segment pages that were found to be intransit.
.IP \fBv\*_pgrec\fR 16n
Page faults that were serviced by reclaiming the page from memory.
.IP \fBv\*_exfod\fR 16n
Fill on demand from file system of executable pages (text or data from
demand initialized executables.)
.IP \fBv\*_zfod\fR 16n
Fill on demand type page faults which filled zeros.
.IP \fBv\*_vrfod\fR 16n
Fill on demand from file systems of pages mapped by
.I vread.
.IP \fBv\*_nexfod\fR 16n
Number of pages set up for fill on demand from executed files.
.IP \fBv\*_nzfod\fR 16n
Number of pages set up for zero fill on demand.
.IP \fBv\*_vrfod\fR 16n
Number of pages set up for fill on demand with
.I vread.
.IP \fBv\*_pgfrec\fR 16n
Pages reclaimed from the free list.
.IP \fBv\*_faults\fR 16n
Address translation faults, any one of the above categories.
.IP \fBv\*_scan\fR 16n
Page frames examined by the page demon.
.IP \fBv\*_rev\fR 16n
Revolutions around the loop by the page demon.
.IP \fBv\*_dfree\fR 16n
Pages freed by the page demon.
.IP \fBv\*_swtch\fR 16n
Cpu context switches.
.PP
The three instances of this structure under the names of
.I cnt, 
.I rate,
and
.I sum
serve the following purposes:
.IP \fBcnt\fR 16n
Incremental counters for the above events.
.IP \fBrate\fR 16n
The moving averages for the above events that are updated at various
integral clock tick periods.
The relevant macro for this operation is
.B "ave(smooth, cnt, time)"
which averages the incremental count
.I cnt
into
.I smooth
with aging factor
.I time.
.IP \fBsum\fR 16n
Accumulated totals for the above events since reboot.
.br
.ne .75i
.sp .375i
.SH
EXISTING DATA STRUCTURES
.PP
Here we describe fields within existing data structures that have
been newly introduced or have taken a new meaning.
.SH
The Process Structure
.IP \fBp\*_slptime\fR 16n
Clock ticks since last sleep.
.IP \fBp\*_szpt\fR 16n
Number of pages of page table.  This field is a copy of the
.I pcb\*_szpt
field in the pcb structure.
.IP \fBp\*_tsize\fR 16n
Text segment size in pages.  This is a copy of the 
.I x\*_size 
field in the text structure.
.IP \fBp\*_dsize\fR 16n
Data segment size in pages.
.IP \fBp\*_ssize\fR 16n
Stack segment size in pages.
.IP \fBp\*_rssize\fR 16n
The current private segment (data + stack)
.I "resident set size"
for the process.  The resident set is defined as the set of pages
owned by the process that are either valid or reclaimable but not in the
free list.
.IP \fBp\*_swrss\fR 16n
The size of the resident set at time of last swap out.
.IP \fBp\*_p0br\fR 16n
Pointer to the base of the P0 region page table.  This is a copy of the
.I pcb\*_p0br
field in the pcb structure.
.IP \fBp\*_xlink\fR 16n
Pointer to another proc structure that is currently loaded and linked
to the same text segment.  The head of this linked list of such processes
is contained in the text structure field
.I x\*_caddr.
Since the shared text portion of the process page tables are duplicated
for each resident process attached to the same text segment, modifications
to any one are reflected in all of them by sequentially updating the
page table of each process that is on this linked list.\*(dg
.FS
\*(dg Used slightly differently when otherwise unused during
.I vfork,
see
.B SNOVM
below.
.FE
.IP \fBp\*_poip\fR 16n
Count of number of page outs in progress on this process.  If non-zero,
prevents the process from being swapped in.
.IP \fBp\*_faults\fR 16n
Incremental number of page faults taken by the process that resulted in
disk I/O.
.IP \fBp\*_aveflt\fR 16n
Moving average of above field.
.IP \fBp\*_ndx\fR 16n
Index of the process slot on behalf of which memory is to be allocated.
During
.I vfork,
the memory of a process will be given to a child, but the reverse
entries in
.I cmap
must still point to the original process
so that the reverse links will point there when the
.I vfork
completes.  This field thus indicates the original owner of the current
process' virtual memory.
.PP
The new bits in the
.I p\*_flag
field are:
.IP \fBSSYS\fR 16n
The swapper or the page demon process.
.IP \fBSLOCK\fR 16n
Process being swapped out.
.IP \fBSSWAP\fR 16n
Context to be restored from
.I u\*_ssave
upon resume.
.IP \fBSPAGE\fR 16n
Process in page wait state.
.IP \fBSKEEP\fR 16n
Prevents process from being swapped out.  Set during the reading of the
text segment from the inode during exec and process duplication in fork.
.IP \fBSDLYU\fR 16n
Delayed unlock of pages.  Causes the pages of the process that are faulted
in to remain locked, thus ineligible for replacement, until explicitly
unlocked by the process.
.IP \fBSWEXIT\fR 16n
Process working on
.I exit.
.IP \fBSVFORK\fR 16n
Indicates that this process is the child in a
.I vfork
context; i.e. that the virtual memory being used by this process actually
belongs to another process.
.IP \fBSVFDONE\fR 16n
A handshaking flag for
.I vfork.
.IP \fBSNOVM\fR 16n
The parent of a
.I vfork.
The process has no virtual memory during this time.
While this bit is set, the
.I p\*_xlink
field points to the process to which the memory was given.
.SH
The Text Structure
.PP
The new fields in the text structure are:
.IP \fBx\*_caddr\fR 16n
Points to the head of the linked list of proc structures of processes
that are currently loaded and attached to this text segment.
.IP \fBx\*_rssize\fR 16n
The resident set size for this text segment.
.IP \fBx\*_swrss\fR 16n
The resident set size for this text segment at the time of last swap out.
.IP \fBx\*_poip\fR 16n
Count of number of page outs in progress on this text segment.  If non-zero,
prevents the process from being swapped in.
.SH
The User Area Structure
.PP
The per-process user area contains the
.I u.\&
structure as well as the kernel stack.  It is mapped to a fixed kernel virtual
address (starting at 0x80040000) at process context switch.  The user area
is swapped in and out of disk as a separate entity and is pointed to by
the proc structure field
.I p\*_swaddr
when not resident.
The number of pages allocated for the process' user area and kernel stack
is six pages (UPAGES),
thus the base of the kernel stack for a process is 0x80040c00.
.PP
The new fields that have been added to the
.I u.\&
structure are the following:
.IP \fBu\*_pcb.pcb\*_cmap2\fR 16n
.br
Contains the copy of Sysmap entry CMAP2
at context switch time.  This kernel virtual address space mapping is made
part of the process context due to the operation of the process duplication
code that implements fork.  Briefly, the process duplication is accomplished
by copying from parent process' virtual address space to the child's
virtual address space by mapping it to kernel virtual memory through
CMAP2.  Since this can result in faulting in the parent's address space,
thus causing a block and context switch, the mapping of the child memory
in the kernel must be saved and restored before the process can resume.
.IP \fBu\*_nswap\fR 16n
Number of times the process has been swapped.  Not yet maintained.
.IP \fBu\*_majorflt\fR 16n
Number of faults taken by the process that resulted in disk I/O.
.IP \fBu\*_cnswap\fR 16n
Number of times the children of this process have been swapped.  Not
yet maintained.
.IP \fBu\*_cmajorflt\fR 16n
Number of faults taken by the children of this process that resulted in
disk I/O.  Not yet maintained.
.IP \fBu\*_minorflt\fR 16n
Number of faults taken by the process that were reclaims.
.IP \fBu\*_dmap\fR 16n
The disk map for the data segment.
.IP \fBu\*_smap\fR 16n
The disk map for the stack segment.
.IP \fBu\*_cdmap\fR 16n
The disk map for the child's data segment to be used during fork.
.IP \fBu\*_csmap\fR 16n
The disk map for the child's stack segment to be used during fork.
.IP \fBu\*_stklim\fR 16n
Limit of maximum stack growth.  To be varied through system calls.
Currently not implemented.
.IP \fBu\*_wantcore\fR 16n
Flag to cause core dump even if the process is very large.  Set by a
system call.  Currently not implemented.
.IP \fBu\*_vrpages\fR 16n
An array with an element for each file descriptor.  Gives the number
of fill on demand page table entries which have this file as their
.B pg\*_fileno.
If the count is non-zero, then the file cannot be closed, either by
.I close
or implicitly by
.I dup2.
.SH
The Inode Structure.
.PP
One field was added to the inode structure to support the
.I vread
system call:
.IP \fBi\*_vfdcnt\fR 16n
This counts the number of file descriptors (fd's) that have pages mapping
this file with
.I vread.
If the count is non-zero, then the file cannot be truncated.
