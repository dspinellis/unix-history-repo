.\" Copyright (c) 1986 Regents of the University of California.
.\" All rights reserved.  The Berkeley software License Agreement
.\" specifies the terms and conditions for redistribution.
.\"
.\"	@(#)gen.t	1.5 (Berkeley) 4/10/86
.\"
.NH
General changes in the kernel
.PP
This section details some of the changes that affect multiple sections
of the kernel.
.NH 2
Header files
.PP
The kernel is now compiled with an include path that specifies the
standard location of the common header files, generally \fB/sys/h\fP
or \fB../h\fP, and all kernel sources have had pathname prefixes
removed from the \fB#include\fP directives for files in \fB../h\fP or the source
directory.
This makes it possible to substitute replacements for individual header
files by placing them in the system compilation directory or in another
directory in the include path.
.NH 2
Types
.PP
There have been relatively few changes in the types defined and used
by the system.
One significant exception is that new typedefs have been added
for user ID's and group ID's in the kernel and common data structures.
These typedefs, \fIuid_t\fP and \fIgid_t\fP, are both of type \fIu_short\fP.
This change from the previous usage (explicit \fIshort\fP ints)
allows user and group ID's greater than 32767 to work reasonably.
.NH 2
Inline
.PP
The inline expansion of calls to various trivial
or hardware-dependent operations
has been a useful technique in the kernel.
In prior releases this substitution was done 
by editing the assembly language output of the compiler with the
sed script \fIasm.sed\fP.
This technique has been refined in 4.3BSD by using a new program,
\fIinline\fP, to perform the in-line code expansion and also optimize
the code used to push the subroutine's operands; where possible,
\fIinline\fP will merge stack pushes and pops into direct register loads.
Also, this program performs the in-line code expansion significantly
faster than the general-purpose stream editor it replaces.
.NH 2
Processor priorities
.PP
Functions to set the processor interrupt priority to block classes
of interrupts have been used in
.UX
on all processors, but the names of these routines have always been
derived from the priority levels of the PDP11 and the UNIBUS.
In order to clarify both the intent of elevated processor priority
and the assumptions about their dependencies, all of the functions \fIsplN\fP,
where \fIN\fP is a small nonzero integer, have been renamed.
In each case, the new name indicates the group of devices that are to be
blocked from interrupts.  The following table indicates the old and new names
of these functions.
.TS
center box;
l | l | l | l
l | l | l | c.
new name	devices blocked	old name	VAX IPL
_
spl0	none	spl0	0
splsoftclock	software clock interrupts	none	0x08
splnet	software network interrupts	splnet	0x0c
spltty	terminal multiplexors	spl5	0x15
splbio	disk and tape controllers	spl5	0x15
splimp	all network interfaces	splimp	0x16
splclock	interval timer	spl6	0x18
splhigh	all devices and state transitions	spl7	0x31
.TE
For use in device drivers only, UNIBUS priorities BR4 through BR7 may be set
using the functions spl4, spl5, spl6 and spl7.
Note that the latter two now correspond
to VAX priorities 0x16 and 0x17 respectively,
rather than the previous 0x18 and 0x1f priorities.
