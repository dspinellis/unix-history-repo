.\"	@(#)1.t	1.1	(Copyright 1990 M. K. McKusick)	90/04/13
.nr PS 11
.nr VS 13
.SH
Introduction
.PP
This paper describes the motivation for and implementation of
a memory-based filesystem.
Memory-based filesystems have existed for a long time;
they have generally been marketed as RAM-disks or sometimes
as software packages that use the general purpose memory on
a machine.
.[
white
.]
.PP
A RAM-disk is designed to appear like any other disk peripheral
connected to a machine.
It is normally interfaced to the processor through the I/O bus
and is accessed through a device driver similar or sometimes identical
to the device driver used for a normal magnetic disk.
The device driver sends requests for blocks of data to the device
and the requested data is then DMA'ed to or from the requested block.
Instead of storing its data on a rotating magnetic disk,
the RAM-disk stores its data in a large array of random access memory
or bubble memory.
Thus, the latency of accessing the RAM-disk is nearly zero
compared to the 15-50 milliseconds of latency incurred when
access rotating magnetic media.
RAM-disks also have the benefit of being able to transfer data at
the maximum DMA rate of the system,
while disks are typically limited by the rate that the data passes
under the disk head.
.PP
Software packages simulating RAM-disks operate by allocating
a fixed partition of the system memory.
The software then provides a device driver interface similar
to the one described for hardware RAM-disks,
except that it uses memory-to-memory copy instead of DMA to move
the data between the RAM-disk and the system buffers.
Because the memory used by the RAM-disk is not available for
other purposes, software RAM-disk solutions are used primarily
for machines with limited addressing capabilities such as PC's
that do not have an effective way of using the extra memory anyway.
.PP
Most RAM-disks lose their contents when the system is powered
down or is rebooted.
The contents can be saved by using battery backed-up memory,
by storing critical filesystem data structure in the filesystem,
and by running a consistency check program after each reboot.
These conditions increase the hardware cost,
and potentially slow down the speed of the disk.
Thus, RAM-disk filesystems are not typically
designed to survive power failures;
because of their volatility, their usefulness is limited to transient
or easily recreated information such as would be found in
.PN /tmp .
Their primary benefit is that they have higher throughput
than disk based filesystems.
.[
smith
.]
This improved throughput is particularly useful for utilities that
make heavy use of temporary files such as many compilers.
On fast processors, nearly half of the elapsed time for a compilation
is spent waiting for synchronous operations required for file
creation and deletion.
The use of the memory-based filesystem nearly eliminates this waiting time.
.PP
Using dedicated memory to exclusively support a RAM-disk
is a poor use of resources.
The overall throughput of the system can be improved
by using the memory where it is getting the highest access rate.
These needs may shift between supporting virtual address space
and cacheing frequently used disk blocks.
If the memory is to be dedicated to the filesystem,
it is better used in a buffer cache.
The buffer cache permits faster access to the data
because it requires only a single memory-to-memory copy
from the kernel to the user process.
When it is used in a RAM-disk configuration it requires two
memory-to-memory copies, one from the RAM-disk
to the buffer cache,
then another copy from the buffer cache to the user process.
.PP
The new work being presented in this paper is to build
a RAM-disk filesystem in pageable memory instead of dedicated memory.
The goal is to provide the speed benefits of a RAM-disk
without paying the performance penalty inherent in dedicating
part of the physical memory on the machine to the RAM-disk.
By building the filesystem in pageable memory,
it competes with other processes for the available memory.
When memory runs short, the paging system pushes its
least-recently-used pages to backing store.
Being pageable also allows the filesystem to be much larger than
would be practical if it were limited by the amount of physical
memory that could be dedicated to that purpose.
We typically operate our
.PN /tmp
with 30 to 60 megabytes of space
which is larger than the amount of memory on the machine.
This configuration allows small files to be accessed quickly,
while still allowing
.PN /tmp
to be used for big files albeit
at the speed more typical of normal disk based filesystems.
.PP
An alternative to building a memory-based filesystem would be to have
a filesystem that never did operations synchronously and never
flushed its dirty buffers to disk.
However, we believe that such a filesystem would either
use a disproportionately large percentage of the buffer
cache space to the detriment of other disk based filesystems,
or would require the equivalent of the
paging system to push out its dirty pages.
Waiting for other filesystems to push dirty pages
subjects them to delays while waiting for the pages to be written.
We await the results of others trying out this approach.
.[
Ohta
.]
.SH
Implementation
.PP
The implementation took less time to write than this paper.
It consists of 560 lines of kernel code (1.7K text + data)
and some minor modifications to the program that builds
disk based filesystems, ``newfs''.
The interesting parts of the kernel code for the
memory-based filesystem is reproduced in Appendix 1.
.PP
A filesystem is created by invoking ``newfs'' with
an option telling it to create a memory-based filesystem.
It allocates a piece of virtual address space of the requested
size and builds a filesystem in the memory
instead of on a disk partition.
When built, it does a ``mount'' system call specifying a filesystem type of
.SM MFS .
The auxillary data parameter specifies a pointer
to the base of the memory in which it has built the filesystem.
(The auxillary data parameter is used by the local filesystem
to specify the block device containing the filesystem.)
.PP
The mount system call allocates and initializes a mount table
entry then calls the filesystem specific mount routine.
The filesystem specific routine is responsible for doing
the mount and initializing the filesystem specific
portion of the mount table entry.
The memory-based filesystem specific mount routine,
.RN mfs_mount ,
is shown in Appendix 1.
It allocates a block device vnode to represent the memory disk device.
In the private area of this vnode it stores the base address of
the filesystem and the process identifier of the ``newfs'' process
for later reference when doing I/O.
It also initializes an I/O list that it
uses to record outstanding I/O requests.
It can then call the local filesystem mount routine
passing it the special block-device vnode that it has created
instead of the usual disk block-device vnode
that it would normally receive.
The mount proceeds just as an other local mount except that
requests to read from the block device are vectored through
.RN mfs_strategy
(described below) instead of the usual
.RN spec_strategy
block device.
When the mount is completed,
.RN mfs_mount
does not return as most other filesystem mount types do;
instead it sleeps in the kernel waiting for I/O requests.
Each time an I/O request is posted for the filesystem,
a wakeup is issued to the corresponding ``newfs'' process.
When awakened, the process checks for requests on its buffer list.
A read request is serviced by copying data from the piece of the
``newfs'' address space corresponding to the requested disk block
to the kernel buffer.
Similarly a write request is serviced by copying data to the piece of the
``newfs'' address space corresponding to the requested disk block
from the kernel buffer.
When all the requests have been serviced, the ``newfs''
process returns to sleep to await more requests.
.PP
Once mounted,
all operations on files in the memory-based filesystem are handled
by the local filesystem code until they get to the point where the
filesystem needs to do I/O on the block device.
Here, the filesystem encounters the second piece of the
memory-based filesystem.
Instead of calling the special-device strategy routine,
it calls the memory-based strategy routine,
.RN mfs_strategy .
Usually,
the request is serviced by linking the buffer onto the
I/O list for the memory-based filesystem
vnode and sending a wakeup to the ``newfs'' process.
This wakeup results in a context-switch to the ``newfs''
process that does a copyin or copyout as described above.
The strategy routine must be careful to check whether
the I/O request is coming from the ``newfs'' process itself.
Self requests happen during mount and unmount operations
when the kernel is reading and writing the superblock.
Here,
.RN mfs_strategy
must do the I/O itself to avoid deadlock.
.PP
The final piece of kernel code to support the
memory-based filesystem is in the close routine.
After a filesystem is successfully unmounted,
the device close routine is called.
For a memory-based filesystem, the device close routine is
.RN mfs_close .
This routine flushes any pending I/O requests,
then sets the I/O list head to a special value
that is recognized by the I/O servicing loop in
.RN mfs_mount
as an indication that the filesystem is unmounted.
The
.RN mfs_mount
routine exits which in turn causes the ``newfs'' process
to exit resulting in the filesystem vanishing in a cloud of dirty pages.
.PP
The paging of the filesystem does not require any additional
code beyond that already in the kernel to support virtual memory.
The ``newfs'' process competes with other processes on an equal basis
for the available memory on the machine.
If memory is plentiful, the entire contents of the filesystem
remain memory resident.
When memory runs short, the oldest pages of ``newfs'' are
pushed to backing store.
The pages that are pushed usually hold the contents of
files that have been created in the memory-based filesystem
but have not been recently accessed.
.[
leffler
.]
.SH
Performance
.PP
The performance of the memory-based filesystem is determined by
the memory-to-memory copy speed of the processor.
Empirically we find that the throughput is about 45% of this
memory-to-memory copy speed.
The basic set of steps for each block written is:
.IP 1)
memory-to-memory copy from the user process doing the write to a kernel buffer
.IP 2)
context-switch to the ``newfs'' process
.IP 3)
memory-to-memory copy from the kernel buffer to the ``newfs'' address space
.IP 4)
context switch back to the writing process
.LP
Thus each write requires at least two memory-to-memory copies
accounting for about 90% of the
.SM CPU
time.
The remaining 10% is consumed in the context switches and
the filesystem allocation and block location code.
The actual context switch count is really only about half
of the worst case outlined above because
read-ahead and write-behind allow multiple blocks
to be handled with each context switch.
.PP
On the six
.SM "MIPS CCI"
Power 6/32 machine,
the raw reading and writing speed is only about twice that of
a regular disk based filesystem.
However, for processes that create and delete many files,
the speedup is considerably greater.
The reason for the speedup is that the filesystem
must do two synchronous operations to create a file,
first writing the allocated inode to disk, then creating the
directory entry.
Deleting a file similarly requires at least two synchronous
operations.
Here, the low latency of the memory-based filesystem is
noticeable compared to the disk based filesystem
since a synchronous operation can be done with
just two context switches instead of incurring the disk latency.
.SH
Future Work
.PP
Ideally part of the kernel's address space could reside in pageable memory.
Once such a facility is available it would be more efficient to
build a memory-based filesystem within the kernel.
Such a filesystem would eliminate at least one of the memory-to-memory
copies in the present system, and would eliminate all the context switches.
One potential problem with such a scheme is that many kernels
are limited to a small address space (usually a few megabytes).
This restriction limits the size of memory-based
filesystem that such a machine can support.
On such a machine, the kernel can describe a memory-based filesystem
that is larger than its address space and use a ``window''
to map the larger filesystem address space into its limited address space.
The window would maintain a cache of recently accessed pages.
The problem with this scheme is that if the working set of
active pages is greater than the size of the window, then
much time is spent remapping pages and invalidating
translation buffers.
.PP
The current system uses the existing local filesystem code to manage
the virtual memory comprising the memory-based filesystem.
It would be interesting to explore other filesystem implementations
that would be less expensive to execute and that would make better
use of the space.
One problem is that the resulting filesystem would almost
certainly be bigger than the memory-based filesystem described
in this paper.
Unless the performance was significantly improved,
it is unlikely that the larger code size would be worthwhile.
.[
$LIST$
.]
