.\" Copyright (c) 1986 Regents of the University of California.
.\" All rights reserved.  The Berkeley software License Agreement
.\" specifies the terms and conditions for redistribution.
.\"
.\"	@(#)vaxmba.t	1.6 (Berkeley) 4/11/86
.\"
.NH
VAX MASSBUS device drivers
.PP
This section documents the modifications in the drivers for devices
on the VAX MASSBUS, with sources in \fI/sys/vaxmba\fP,
as well as general changes made to all disk and tape drivers.
.NH 2
General changes in disk drivers
.PP
Most of the disk drivers' strategy routines were changed to report
an end-of-file when attempting to read the first block after the end
of a partition.
Distinct errors are returned for nonexistent drives,
blocks out of range, and hard I/O errors.
The \fIdkblock\fP and \fIdkunit\fP macros once used to support
disk interleaving were removed, as interleaving makes no sense
with the current file system organization.
Messages for recoverable errors, such as soft ECC's,
are now handled by \fIlog\fP instead of \fIprintf\fP.
.NH 2
General changes in tape drivers
.PP
The open functions in the tape drivers now return sensible errors
if a drive is in use.
They save a pointer to the user's terminal when opened,
so that error messages from interrupt level may be printed
on the user's terminal using \fItprintf\fP.
.NH 2
Modifications to individual MASSBUS device drivers
.XP hp.c
Error recovery in the MASSBUS disk driver is considerably better now
than it was.
The driver deals with multiple errors in the same transfer
much more gracefully.
Earlier versions could go into an endless loop correcting one error,
then retrying the transfer from the beginning when a second error
was encountered.
The driver now restarts with the first sector not yet successfully
transferred.
ECC correction is now possible on bad-sector replacements.
The correct sector number is now printed in most error messages.
The code to decide whether to initiate a data transfer or
whether to do a search was corrected, and the \fIsdist/rdist\fP parameters
were split into three parameters for each drive: the minimum and maximum
rotational distances from the desired sector between which to start
a transfer, and the number of sectors to allow after a search before 
the desired sector.
The values chosen for these parameters are probably still not optimal.
.XP
There were races when doing a retry on one drive that continued with
a repositioning command (recal or seek) and when then beginning a data transfer
on another drive.
These were corrected by using a distinguished
return value, MBD_REPOSITION, from \fIhpdtint\fP to change the controller
state when reverting to positioning operations during a recovery.
The remaining steps in the recovery are then managed by \fIhpustart\fP.
Offset commands were previously done under interrupt control,
but only on the same retries as recals (every eighth retry starting
with the fourth).
They are now done on each read retry after the 16th and are done
by busy-waiting to avoid the race described above.
The tests in the error decoding section of the interrupt
handler were rearranged for clarity and to simplify the tests
for special conditions such as format operations.
The \fIhpdtint\fP times out if the drive does not become ready
after an interrupt rather than hanging at high priority.
When forwarding bad sectors, \fIhpecc\fP correctly handles partial-sector
transfers; prior versions would transfer a full sector, then continue
with a negative byte count, encountering an invalid map register immediately
thereafter.
Partial-sector transfers are requested by the virtual memory system
when swapping page tables.
.XP mba.c
The top level MASSBUS driver supports the new return code from data-transfer
interrupts that indicate a return to positioning commands before restarting
a data transfer.
It is capable of restarting a transfer after partial completion and
adjusting the starting address and byte count according to the amount
remaining.
It has also been modified to support data transfers in reverse,
required for proper error recovery on the TU78.
\fIMbustart\fP does not check drives to see that they are present,
as dual-ported disks may appear to have a type of zero
if the other port is using the disk;
in this case, the disk unit start will return MBU_BUSY.
.XP mt.c
The TU78 driver has been extensively modified and tested
to do better error recovery and to support additional operations.
