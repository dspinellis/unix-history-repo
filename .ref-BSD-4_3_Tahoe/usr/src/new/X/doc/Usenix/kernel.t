.SH
Alternatives to User Process Window Systems
.PP
As currently implemented on most machines, the 
.UX
kernel
does not
permit preemption once a user process has started executing a system call
unless the system call explicitly blocks.
Any asynchrony occurs at device driver interrupt level.
.UX
presumes either that system calls are very fast
or quickly block waiting for I/O completion.
.PP
This has strong implications for kernel window system implementations.
While window system requests do not take very long
(if they did, the presumptions made in X would be unacceptable),
they may take very long relative to normal system calls.
If a system call is compute bound for a ``long period'', interactive
response of other processes suffers badly, as the offending process
will tend to monopolize the CPU.
One might argue that this is not offensive on a single user machine
but it is a disaster on a multiuser machine.
If graphics code and data is in the kernel for sharing,
it permanently uses that much of
kernel memory, incurs system call overhead for access,
and cannot be paged out when not in use.
.PP
Similarly, in X as well as most other window systems,
if a window system request takes too long,
other clients will not get their fair share of the display.
This is currently somewhat of a problem during complex fill or
polyline primitives on slow displays.
The concept of interrupting a graphics primitive is so difficult that
we have chosen to ignore the problem, which is seldom noticeable.
If such graphics primitives occur in system calls, they have a much
greater impact on process scheduling.
.PP
An alternative to a strictly kernel window system implementation
splits responsibility between the kernel and user processes.
Synchronization, window creation and manipulation primitives are put in
the kernel, and clients are relied on to be well behaved for clipping.
Output to the window is then performed in each user process.
This has several disadvantages (presuming no shared libraries, not
available on most current 
.UX
implementations).
Each client of the window system must must then have
a full copy of graphics code.
This can be quite large on
some hardware, replicated in each client of the window system.
For example, the current bit blit, graphics and clipping code for QVSS
is approximately
90kbytes, or 18000 lines of C source code.
Fill algorithms may also require a large amount of buffer space.
.PP
Even worse  (as the number of different display hardware proliferates
with time on a single machine architecture) is that this split
approach requires the inclusion in your image 
of code for
hardware you do not currently have.
Upward compatibility to new display hardware is also impossible without
shared libraries,
but dynamic linking is really required for
the general solution.
.PP
With much existing hardware it is hard
to synchronize requests from multiple processes
if the hardware has not been designed to efficiently support context switching.
There are sometimes work arounds for these problems by ``shadowing''
the write only state in the hardware.
We have seen displays which incur additional hardware cost 
to allow for
such multiprocess access.
One must also then face the locking of critical sections of window system
data structures if the window system is interruptible.
.PP
.UX
internal kernel structuring currently provides most services directly
to user processes.
It would be difficult to provide network access to the window system
if it were in the kernel due to this horizontal structure but a
better ability to layer one facility on another would improve this
situation.
Again, this is a failure of the kernel to be sufficiently modular to
anticipate the evolving environment.
.PP
X finesses all of these problems:
1) X and client applications are user processes; ergo no scheduling biases.
2) There is only one copy of display code required, in the server,
which can be paged since it is completely user code.
This also saves swap space, in short supply on most current workstations.
The resulting client code is thus small.
Minimal X applications are as small as 16k bytes.
No graphics code is in an application program.
3) Client code can potentially work with new hardware without relinking, as
no display specific code appears in a client program image.
4) Network access to the window system comes at no additional cost,
and no performance penalty (in practice, performance is often gained).
5) X avoids system call overhead by buffering requests into a single
buffer and delaying writing in a fashion similar to the standard I/O library.
The system call overhead for output is therefore reduced by well over an
order of magnitude per X operation.
6) User process code is easy to debug.
Some complications can arise due to the distributed nature of the system.
In practice, this has rarely been a problem.
7) Applications requiring a ``compute server'' can be run from the
user's workstation.
.PP
Kernel lightweight processes could be used to solve
the non-preemptable nature
of system calls and would create more
options for window system implementations.
Since raster operations can be quite long lived,
performing these in the current structure allows one process to
monopolize the system to the detriment of other processes.
Since all context in the system call layer of the
kernel is associated with a user process, there is
currently no way to divorce such operations from a process and
schedule them independently.
.PP
While lightweight processes would unnecessarily complicate the X server
design (requiring us to lock data structures and perform synchronization),
they could be used prevent the most common X programming mistake.
Programmers new to X invariably forget to flush the output buffer when
testing their first program.
A timer driven lightweight process in clients  would be useful to guarantee
automatic flushing of the buffers.
