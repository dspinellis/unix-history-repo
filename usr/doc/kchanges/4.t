.NH
Standalone support
.PP
This section describes changes made to the standalone
i/o facilities and the new methods used in system bootstrapping.
.NH 2
Disk formatting
.PP
A new disk formatting program has
been developed for use with non-DEC UNIBUS and MASSBUS disk controllers.
The \fIformat\fP\|(8V) program has been tested mainly with
disk drives attached to Emulex MASSBUS and UNIBUS disk
controllers, but should operate with any controller which
handles bad sector forwarding in an identical fashion to
DEC RM03/RM05 or RM80 (but not RP06) disk controllers.
The program runs standalone formatting
disk headers and creating a bad sector table in the DEC
standard 144 format.
.NH 2
Standalone i/o library
.LP
Changes to support more
complex standalone i/o applications as well as changes
for the new file system
organization, have resulted in significant revisions to the
standalone i/o library.  Device drivers now support a new
entry point for \fIioctl\fP requests and library routines
now return error codes a la the UNIX system calls. 
In addition, standalone i/o library routines now make
many more internal consistency
checks to verify data structures have not been corrupted by
faulty device drivers and that i/o errors have not
occurred when reading critical file system information.
In conjunction with the new disk formatter,
the \fIup\fP and \fIhp\fP standalone
drivers have been rewritten to support ECC correction and
bad sector handling.  These drivers are used in bootstrapping
from the console media on 11/780's and 11/730's thereby
eliminating the requirement for error free root
partitions on disks attached to \fIhp\fP and \fIup\fP controllers.
Many bugs in the standalone tape drivers have been fixed.
.NH 2
System bootstrapping
.PP
On 11/780's and 11/730's, the
console device is still used to load the ``boot'' program.
This in turn loads the system image from the root file
system.
.PP
The method by which the system bootstraps on 11/750's
is different in 4.2BSD.  The system is still bootstrapped
from disk using a boot block in sector 0 of the root file
system partition, but now this boot block simply reads
in the next 7.5 kilobytes.  The 7.5 kilobyte program is
a version of the ``/boot'' program loaded only with the
device driver required to read the ``/boot'' program from
the root file system.  The ``/boot'' program then reads
in the system image, as done on 11/780's and 11/730's.
.PP
The additional level of bootstrap code was done to simplify
the sector 0 boot programs and minimize the total amount
of assembly language code which had to be maintained.
It was also expected that 7.5 kilobytes would be sufficient
to allow the new \fIhp\fP and \fIup\fP standalone drivers
which support ECC correction and bad sector handling to be
used.  Unfortunately, the standalone system has not yet
been trimmed down to allow the second level boot programs,
loaded with the new drivers, to fit in the space provided.
Sites which have Winchester disk drives with bad
sectors in the root file system partition and which require
this support should be able to trim the size of the second
level boot program to make it fit.
