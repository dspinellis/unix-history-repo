.SH
Shared Memory
.PP
On a fast display and processor, X may be performing more than
one thousand operations (X requests) per second.
If every access to the device requires a system call, the overhead
rapidly predominates all other costs.
X uses a shared memory structure with the device driver for two purposes:
1) to get mouse and keyboard input
and
2) to access the device or write into a memory bitmap.
.PP
As pointed out before, X is a single threaded server.
Since client programs should be able to overlap with
the window system as much as possible (remember that you may be
running applications on other machines), it is particularly
important to send input events to the correct client as soon
as possible.
It is therefore desirable to test if there is input after each
graphic output operation.
This test can be performed in only a couple of instructions given shared
memory, and would otherwise require either one system call/output
operation (to check for new input) or a compromise in how quickly
input would be handled.
.PP
All input events are put into a shared memory circular buffer; since
the driver only inserts into the buffer, and X only removes from the
buffer, synchronization is easy to provide with separate head and tail
indices (presuming a write to shared memory is atomic).
.PP
Output on the QVSS is directly to a mapped bitmap.
In the case of the Vs100, a piece of the UNIBUS\(dg and a shared DMA buffer
are statically mapped where both the driver and the X server can access
them.
.FS \(dg
UNIBUS is a trademark of Digital Equipment Corporation.
.sp
.FE
Output requests to the Vs100 are directly formated into this buffer,
minimizing copying of data.\(dd
.FS \(dd
Our thanks go to Phil Karlton, of Digital's Western Research Lab, for
the first implementation of this mechanism.
.FE
This permits the device dependent routines to start I/O transfers without
system call overhead (by directly accessing device CSR registers),
and avoids UNIBUS map setup overhead that DMA from user space requires.
.PP
These changes dramatically increased performance and improved
interactive feel when implemented, while greatly reducting CPU overhead.
Since proper memory sharing primitives are lacking in 4.2BSD,
it was implemented by making pages readable and writable in system space,
where they are accessible to any process.
In theory, any program on the machine could cause a Vs100 implementation to
machine check (odd byte access in the UNIBUS space), though in practice it
has never happened.
None the less, it is the ugliest piece of the current X implementation.
We are more willing to allow a server process to access hardware
directly than kernel code,
as it is much easier to debug user processes than kernel code.
.PP
The current X implementation uses a TCP stream both locally and
remotely, though one could easily use 
.UX
domain sockets for the local
case at the cost of a file descriptor.
For current applications, the bandwidth limitations (of approximately
1 million bits/second on 780 class processor) is not major,
though faster devices (and image processing applications) would probably
benefit from implementation of a shared memory path between the X server
and client applications.
.PP
Current shared memory implementations in variants of 
.UX
are not sufficient.
Memory sharing primitives should allow appropriately
privileged programs to both share memory with other processes and map to
both kernel space and I/O space.
Shared libraries (available in some versions of 
.UX )
would also increase the options available to window system
designers (see below).
