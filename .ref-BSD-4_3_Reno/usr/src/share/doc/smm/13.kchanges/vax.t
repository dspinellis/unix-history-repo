.\" Copyright (c) 1986 Regents of the University of California.
.\" All rights reserved.  The Berkeley software License Agreement
.\" specifies the terms and conditions for redistribution.
.\"
.\"	@(#)vax.t	1.6 (Berkeley) 4/11/86
.\"
.NH
Machine specific support
.PP
The next several sections describe changes to the VAX-specific
portion of the kernel whose sources reside in \fB/sys/vax\fP.
.NH 2
Autoconfiguration
.PP
The data structures and top level of autoconfiguration
have been generalized to support the VAX 8600 and
machines whose main I/O busses are not similar to an SBI.
The \fIpercpu\fP structure has been broken into three
structures.
The \fIpercpu\fP structure itself contains only the CPU type,
an approximate value for the speed of the CPU,
and a pointer to an array of I/O bus descriptions.
Each of these, in turn, contain general information about one I/O bus that must
be configured and a pointer to the private data for its configuration
routine.
The third new structure that has been defined
describes the SBI and the other interconnects that emulate it.
At boot time,
\fIconfigure\fP calls \fIprobeio\fP to configure the I/O bus(ses).
\fIProbeio\fP looks through the array of bus descriptions,
indirecting to the correct routine to configure each bus.
For the VAXen currently supported,
the main bus is configured by either
\fIprobe_Abus\fP (on the 8600 and 8650)
or by \fIprobenexi\fP, that is used on anything resembling an SBI.
Multiple SBI adaptors on the 8600 are handled by multiple calls
to \fIprobenexi\fP.
(Although the code has been tested with a second SBI,
there were no adaptors installed on the second SBI.)
This structure is easily extensible to other architectures
using the BI bus, Q bus, or any combination of busses.
.PP
The CPU speed value is used to scale the DELAY macro
so that autoconfiguration of old devices on faster CPU's
will continue to work.
The units are roughly millions of instructions per second (MIPS),
with a value of 1 for the 780,
although fractional values are not used.
When multiple CPU's share the same CPU type,
the largest value for any of them is used.
.PP
UNIBUS autoconfiguration has been modified to accommodate UNIBUS
memory devices correctly.
A new routine, \fIubameminit\fP, is used to configure UNIBUS memory
before probing other devices,
and is also used after a UNIBUS reset to remap these memory areas.
The device probe or attach routines may then allocate and hold
UNIBUS map registers without interfering with these devices.
.NH 2
Memory controller support
.PP
The introduction of the MS780-E memory controller for the VAX 780
made it necessary to configure the memory controller(s) on a VAX
separately from the CPU.
During autoconfiguration, the types of the memory controllers
are recorded in an array.
Memory error routines that must know the type of controller
then use this information rather than the CPU type.
The MS780-E controller is listed as two controllers, as each half
reports errors independently.
Both 1Mb and 4Mb boards using 64K and 256K dRAM chips are supported.
.XP Locore.c
For \fIlint\fP's sake, \fILocore.c\fP has been updated to include the functions
provided by \fIinline\fP and the new functions in \fIlocore.s\fP.
.XP autoconf.c
Most of the changes to autoconfiguration are described above.
Other minor changes:
UNIBUS controller probe routines are now passed an additional argument,
a pointer to the \fIuba_ctlr\fP structure,
and similarly device probe routines are passed a pointer to the \fIuba_device\fP
structure.
\fIUbaaccess\fP and \fInxaccess\fP were combined into a single routine
to map I/O register areas.
A logic error was corrected so that swap device sizes that were initialized
from information in the machine configuration file
are used unmodified.
\fIDumplo\fP is set at configuration time according
to the sizes of the dump device and memory.
.XP conf.c
Several new devices have been added and old entries have been deleted.
A number of devices incorrectly set unused UNIBUS reset entries
to \fInodev\fP; these were changed to \fInulldev\fP.
An entry was added for the new error log device.
Additional device numbers have been reserved for local use.
.XP cons.h
New definitions have been added for the 8600 console.
.XP "crl.h,crl.c"
New files for the VAX 8600 console RL02 (our third RL02 driver!).
.XP flp.c
It was discovered that not all VAXen that are not 780's are 750's;
the console floppy driver for the 780 now checks for cpu == 780,
not cpu != 750.
An error causing the floppy to be locked in the busy state was corrected.
.XP genassym.c
Several new structure offsets were needed by the assembly language routines.
.XP in_cksum.c
It was discovered that the instruction used to clear the carry
in the checksum loops did not actually clear carry.
As the carry bit was always off when entering the checksum loop,
this was never noticed.
.XP inline
This directory contains the new \fIinline\fP program used to edit
the assembly language output by the compiler.
.XP locore.s
The assembly language support for the kernel has a number of changes,
some of which are VAX specific and some of which are needed
on all machines.
They are simply enumerated here without distinction.
.XP
The \fIdoadump\fP routine sometimes faulted because it changed the page
table entry for the \fIrpb\fP without flushing the translation buffer.
In order to reconfigure UNIBUS memory devices again after UNIBUS resets,
\fIbadaddr\fP was reimplemented without the need to modify the
system control block.
The machine check handler catches faults predicted by \fIbadaddr\fP,
cleans up and then returns to the error handler.
The interrupt vectors have each been modified to count the number
of interrupts from their respective devices, so that it is possible
to account for software interrupts and UBA interrupts,
and to determine which of several similar devices is generating
unexpected interrupt loads.
The \fIconfig\fP program generates the definitions for the indices
into this interrupt count table.
Software clock interrupts no longer call timer entries in the dz
and dh drivers.
The processing of network software interrupts has been reordered
so that new interrupts requested during the protocol interrupt routine
are likely to be handled before return from the software interrupt.
Additional map entries were added to the network buffer and user page
table page maps, as both use origin-1 indexing.
The memory size limit and  the offsets into the coremap are both obtained
from \fIcmap.h\fP instead of inline constants.
The signal trampoline code is all new and uses the \fIsigreturn\fP
system call to reset signal masks and perform the \fIrei\fP to user mode.
The initialization code for process 1, \fIicode\fP, was moved to this file
to avoid hand assembly; it has been changed to exit instead of looping
if \fI/etc/init\fP cannot be executed, and to allow arguments
to be passed to \fIinit\fP.
The routines that are called with \fIjsb\fP rather than \fIcalls\fP
use a new entry macro that allows them to be profiled if profiling
is enabled.
.XP
Several new routines were added to move data from address space to address
space a character string at a time; they are \fIcopyinstr\fP,
\fIcopyoutstr\fP, and \fIcopystr\fP.
\fICopyin\fP and \fIcopyout\fP now receive their arguments
in registers.
\fISetjmp\fP and \fIlongjmp\fP are now similar to the user-level
routines; \fIsetjmp\fP saves the stack and frame pointers and PC only
(all implemented in line),
and \fIlongjmp\fP unwinds the stack to recover the other registers.
This optimizes the common case, \fIsetjmp\fP, and allows the
same semantics for register variables as for stack variables.
For swaps and alternate returns using \fIu.u_save\fP, however,
all registers must be saved as in a context switch, and \fIsavectx\fP
is provided for that purpose.
.XP
Redundant context switches were caused by two bugs in \fIswtch\fP.
First, \fIswtch\fP cleared \fIrunrun\fP before entering the idle loop.
Once an interrupt caused a \fIwakeup\fP, \fIrunrun\fP would be set,
requesting another context switch at system call exit.
Also, the use of the VAX AST mechanism caused a similar problem,
posting AST's to one process that would then \fIswtch\fP (or might
already be in the idle loop), only to catch the AST after being rescheduled
and completing its system service.
The AST is no longer marked in the process control block and is cancelled
during the context switch.
The idle loop has been separated from \fIswtch\fP for profiling.
.XP machdep.c
The \fIstartup\fP code to calculate the core map size and the limit
to the buffer cache's virtual memory allocation was corrected
and reworked.
The number of buffer pages was reduced for larger memories
(10% of the first 2 Mb of physical memory is used for buffers,
as before, and 5% thereafter).
The default number of buffers or buffer pages may be overridden
with configuration-file options.
If the number of buffers must be reduced to fit the system page table,
a warning message is printed.
Buffers are allocated after all of the fully dense data structures,
allowing the other tables allocated at boot time to be mapped by the identity
map once again.
The new signal stack call and return mechanisms are implemented here
by \fIsendsig\fP and \fIsigreturn\fP; \fIsigcleanup\fP remains for compatibility
with 4.2BSD's \fIlongjmp\fP.
There are a number of modifications for the VAX 8600, particularly
in the machine check and memory error handlers and in the use of the console
flags.
On the VAX-11/750 more translation-buffer
parity faults are considered recoverable.
The \fIreboot\fP routine flushes the text cache before initiating
the filesystem update, and may wait longer for the update to complete.
The time-of-day register is set, as any earlier time adjustments are not
reflected there yet.
The \fImicrotime\fP function was completed and is now used;
it is careful not to allow time to appear to reverse during time corrections.
An \fIinitcpu\fP routine was added to enable caches, floating point
accelerators, etc.
.XP machparam.h
The file \fIvax/param.h\fP was renamed to avoid ambiguity
when including \fI``param.h''\fP.
.XP ns_cksum.c
This new file contains the checksum code for the Xerox NS network protocols.
.XP pcb.h
The \fIaston\fP() and \fIastoff\fP\^() macros no longer set an AST
in the process control block (see \fIlocore.s\fP).
.XP pte.h
The \fIpg_blkno\fP field was increased to 24 bits to correspond
with the \fIcmap\fP structure; the \fIpg_fileno\fP field was reduced
to a single bit, as it no longer contains a file descriptor.
.XP swapgeneric.c
\fIDumpdev\fP and \fIargdev\fP are initialized to NODEV,
preventing accidents should they be used before configuration completes.
DEL is now recognized as an erase character by the kernel \fIgets\fP.
.XP tmscp.h
A new file which contains definitions for the
Tape Mass Storage Control Protocol.
.XP trap.c
Syscall 63 is no longer reserved by \fIsyscall\fP for out-of-range calls.
In order to make \fIwait3\fP restartable, \fIsyscall\fP must not clear
the carry bit in the program status longword before beginning a system
call, but only after successful completion.
.XP tu.c
There were several important fixes in the console TU58 driver.
.XP vm_machdep.c
The \fIchksize\fP routine requires an additional argument,
allowing it to check data size and bss growth separately without overflow.
.XP vmparam.h
The limits to user process virtual memory allow nondefault values
to be defined by configuration file options.
The definition of DMMAX here now defines only the maximum value;
it will be reduced according to the definition of MAXDSIZ.
The space allocated to user page tables was increased substantially.
The free-memory threshold at which \fIpageout\fP begins
was changed to be at most 512K.
