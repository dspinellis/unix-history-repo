.NH
Carrying over local software
.PP
With the massive changes made to the system, both in organization
and in content, it may take some time to understand how to
carry over local software.  The majority of this document is
devoted to describing the contents of each important source file
in the system.  If you have local software other than device
drivers to incorporate in the system you should first read this
document completely, then study the source code to more fully
understand the changes as they affect you.
.PP
Locally written device drivers will need to be converted to
work in the new system.  The changes required of device drivers are:
.IP 1)
The calling convention
for the driver \fIioctl\fP routine has changed.  Any data
copied in or out of the system is now
done at the highest level inside \fIioctl\fP\|().  The third
parameter to the driver \fIioctl\fP routine
is a data buffer passed by reference.  Values to be
returned by a driver must be copied into the associated buffer
from which the system then copies them into the user address space.
.IP 2)
The \fIread\fP, \fIwrite\fP, and \fIioctl\fP entry points in
device drivers must return 0 or an error code from <\fIerrno.h\fP>.
.IP 3)
The \fIread\fP and \fIwrite\fP entry points should no longer 
reference global variables out of the user area.  A new \fIuio\fP
parameter is passed to these routines which should, in turn,
be passed to the \fIphysio\fP\|() routine if the driver supports
raw i/o.
.IP 4)
Disk drivers which are to support swapping/paging must have
a new routine which returns the size, in sectors, of a disk
partition.  This value is used in calculating the size of
swapping/paging areas at boot time.
.IP 5)
Code which previously used the \fIiomove\fP, \fIpassc\fP, or
\fIcpass\fP routines will have to be modified to use the
new \fIuiomove\fP, \fIureadc\fP, and \fIuwritec\fP routines.
The new routines all use a \fIuio\fP structure to communicate
the i/o base, offset, count, and segflag values previously
passed globally in the user area.
.IP 6)
Include files have been rearranged and new ones have
been created.  Common machine-dependent
files such as \fImtpr.h\fP, \fIpte.h\fP, \fIreg.h\fP,
and \fIpsl.h\fP are no longer in
the ``h'' directory; see below under organizational changes.
.IP 7)
The handling of UNIBUS resets has changed.  The reset routine
should no longer deallocate UNIBUS resources allocated to
pending i/o requests (this is done in the \fIubareset\fP routine).
For most drivers this means the reset routine simply needs to
invalidate any \fIub_info\fP values stored in local data
structures to insure new UNIBUS resources will be allocated
the next time the ``device start'' routine is entered.
