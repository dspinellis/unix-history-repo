.NH 3
/sys/vaxuba
.PP
This directory contains UNIBUS device drivers and their
related include files.  The latter have moved from /sys/h
in an effort to isolate machine-dependent portions of the
system.  The following device drivers were not present in
the 4.1BSD release.
.IP \fBad.c\fP 10
a driver for the Data Translation A/D converter
.IP \fBik.c\fP 10
an Ikonas frame buffer graphics interphase; user access to
the device is implemented by mapping the device registers
directly into the virtual address space of a user (the
routines to map memory are included in uba.c only if an
Ikonas is configured in the system)
.IP \fBkgclock.c\fP 10
a driver for a DL11-W or KL11-W used as an auxiliary
real-time clock source for kernel profiling and/or statistics
gathering; if this device is present, the system will automatically
collect its i/o statistics (and if profiling, pc samples)
off the secondary clock; very useful in kernel profiling as
the second clock source eliminates most of the statistical
anomalies and shows the true time spent in the clock routine
.IP \fBps.c\fP 10
driver for an Evans and Sutherland Picture System 2
.IP \fBrl.c\fP 10
driver for RL11 controller with RL02 cartridge disks; does
not support RL01 disks though it should only require additions
to disk geometry and partition tables
.IP \fBrx.c\fP 10
driver for RX211 floppy disk controller; provides both block
and character device interfaces; \fIioctl\fP calls support
floppy disk formatting and ``deleted data mark'' sensing and
writing; makes a great paging device
.IP \fBut.c\fP 10
driver for tape controllers which emulate a TU45 on the
UNIBUS; in particular, the System Industries Model 9700
triple density tape drive
.IP \fBuu.c\fP 10
driver for dual UNIBUS TU58 cartridge tape cassettes
accessed through a DL11 serial line; uses assembly language
code in locore.s which provides pseudo-DMA on input
(necessary to avoid data overruns); using this driver
while the system runs multi-user degrades response severely
(developed at Berkeley exclusively to produce distribution TU58
cassettes)
.PP
In addition to the above device drivers, many drivers present
in 4.1BSD now sport corresponding include files which contain
device register definitions.  For example, the DH11 driver
is now broken into three files: dh.c, dhreg.h, and dmreg.h.
.PP
The following drivers have been significantly modified, or
had bugs fixed in them, since the 4.1BSD release:
.IP \fBdh.c\fP 10
changes to reflect the revised tty data organization
.IP \fBdmf.c\fP 10
a bug where device register accesses caused unwitting
modification of certain status bits has been fixed;
modem control has been fixed; a remnant of the DH11
include file which caused incorrect definitions for even/odd
parity has been fixed;
changes to reflect the revised tty data organization
.IP \fBdz.c\fP 10
now supports the DZ32;
changes to reflect the revised tty data organization
.IP \fBlp.c\fP 10
now takes a non-zero flags value specified in the configuration
file as the printer width (default is 132 columns); thus, to configure
an 80 column printer, include ``flags 80'' in the device
specification
.IP \fBrk.c\fP 10
a race condition has been fixed where a seek finishing
on one drive appeared as an i/o transfer completeing on another
(this bug actually was present in all UNIBUS disk drivers); changes
for \fIuio\fP and swap space configuration
.IP \fBtm.c\fP 10
a typo which made the system crash with multiple slaves on
a single controller has been fixed;  an incorrect priority
level change in the watchdog timer routine which caused
the system to crash when a device operation timed out has
been fixed;
changes for \fIuio\fP processing of raw i/o
.IP \fBts.c\fP 10
changes for \fIuio\fP processing of raw i/o
.IP \fBuba.c\fP 10
a new support routine for allocating UNIBUS memory for memory-mapped
devices such as the 3Com Ethernet interface; the handling of UNIBUS
resets has been changed, all UNIBUS resources are now reclaimed in
the \fIubareset\fP routine prior to calling individual device driver
reset routines \- this implies driver reset routines should no longer
free up allocated UNIBUS resources; new routines for mapping UNIBUS
memory into the virtual address space of a process have been added
to support the Ikonas device driver;  changes to fix the race condition
described above in the RK07 device driver; processes awaiting
UNIBUS map registers now sleep on a different event than those
waiting for buffered data paths
.IP \fBuda.c\fP 10
the problem with multiplexing buffered data paths on an 11/750
has been fixed; a bug in the setup of the \fIui_dk\fP field
has been fixed; now properly defines the field indicating the
disk transfer rate; changes for \fIuio\fP processing and
swap space configuration
.IP \fBup.c\fP 10
now supports ECC correction and bad sector forwarding; significant
changes have been made to make configuration of various disk drives
simple (by probing the holding register and using the resultant 
value indicating the number of tracks on the disk); the race condition
described under rk.c has been fixed; references to UNIBUS
map registers are now done with longword instructions so the
device driver does not cause the system to crash when an ECC or
bad sector error occurs on a disk attached to a 730 UNIBUS;  the
upSDIST/upRDIST parameters which control the use
of search and seek operations on controllers with multiple drives
have been made drive dependent; a bug whereby the probe routine
would belive certain non-existant drives were present has been fixed;
changes for \fIuio\fP processing and swap space configuration
.IP \fBva.c\fP 10
has been rewritten to honor the software support for
exclusive access to the UNIBUS so that the device may 
coexist on the same UNIBUS with RK07 disk drives;
the driver now works with controllers which have a GO bit
