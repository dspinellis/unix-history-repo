.\" Copyright (c) 1988, 1993 The Regents of the University of California.
.\" All rights reserved.
.\"
.\" %sccs.include.redist.roff%
.\"
.\"	@(#)1.t	6.1 (Berkeley) %G%
.\"
.ds lq ``
.ds rq ''
.ds LH "Installing/Operating \*(4B
.ds RH Introduction
.ds CF \*(Dy
.LP
.bp
.NH 1
Introduction
.PP
This document explains how to install the Berkeley
version of \*(Ux on your system.
The filesystem format is compatible with \*(Ps
and it will only be necessary for you to do a full bootstrap
procedure if you are installing the release on a new machine.
The object file formats are completely different in the System V releases.
Thus, the most straightforward procedure for upgrading a System V
system is to perform a full bootstrap.
.PP
The full bootstrap procedure
is outlined in chapter 2; the process starts with copying a filesystem
image onto a new disk.
This filesystem is then booted and used to extract the remainder of the
system binaries and sources from the archives on the tape(s).
.PP
The technique for upgrading a \*(Ps system is described
in chapter 3 of this document.
The upgrade procedure involves extracting a new set of system binaries
onto new root and /usr filesystems.
The sources are then extracted, and local configuration files are merged
into the new system.  User filesystems may be upgraded in place.
Most \*(Ps binaries may be used with \*(4B in the course
of the conversion.
It is desirable to recompile local sources after the conversion,
as the compilers have had many fixes installed
and many new system call interfaces have been added.
Consult section 3 for a description of the differences
between \*(4B and \*(Ps.
.NH 2
Distribution format
.PP
The distribution comes in two formats:
.DS
(3)\0\0 6250bpi 2400' 9-track magnetic tapes, or
(1)\0\0 8mm Exabyte tape
.DE
.PP
If you have the facilities, we \fBstrongly\fP recommend copying the
magnetic tape(s) in the distribution kit to guard against disaster.
The tapes contain some 1024-byte records followed by many
10240-byte records.  There are interspersed tape marks;
end-of-tape is signaled by a double end-of-file.  The first file
on the tape is a disk image of the root file system.
Following the disk-image root file is a full dump of the root file system
(see \fIdump\fP\|(8)*).
.FS
\ * References of the form \fIX\fP(Y) mean the entry named
\fIX\fP in section Y of the
.UX
programmer's manual.
.FE
Additional files on the tape(s)
contain tape archive images of the system binaries and sources (see
\fItar\fP\|(1)).  See the tape label for a description of the contents
and format of the tape(s).
.NH 2
\*(Ux device naming
.PP
The standalone system, used to boot the full \*(Ux system,
uses device names of the form:
.DS
xx(c,d,p)
.DE
where \fIxx\fP is the device type, normally \fI\*(Dk\fP or \fI\*(Mt\fP.
The value \fIc\fP specifies the controller to use, and \fId\fP specifies
the device.  The \fIp\fP value is interpreted differently for tapes
and disks: for disks it is a disk \fIpartition\fP (in the range 0-7),
and for tapes it is a file number offset on the tape.  Thus, partition
1 of a ``\*(Dk'' type disk drive on controller 0 at unit 2 would be
``\*(Dk(0,2,1)''.  Normally the controller will be controller 0; it
may therefore be omitted from the device specification, and most of
the examples in this document reflect this convention.  When not running
standalone, this partition would normally be available as ``/dev/\*(Dk2b''.
Here the prefix ``/dev'' is the name of the directory where all
``special files'' normally live, the ``\*(Dk'' serves the obvious purpose,
the ``2'' identifies this as a partition of \*(Dk drive number ``2'' and
the ``b'' identifies this as the second partition.
.PP
In all simple cases, where only a single controller is present, a drive
with unit number 0 (determined by its unit plug on the front of the drive)
will be called unit 0 in its \*(Ux file name.  This is not, however, strictly
necessary, since the system has a level of indirection in this naming.
If there are multiple controllers, the disk unit numbers will normally
be counted sequentially across controllers.  This can be taken
advantage of to make the system less dependent on the interconnect
topology, and to make reconfiguration after hardware failure extremely
easy.
.PP
Each \*(Ux physical disk is divided into at most 8 logical disk partitions,
each of which may occupy any consecutive cylinder range on the physical
device.  The cylinders occupied by the 8 partitions for each drive type
are specified initially in the disk description file /etc/disktab
(c.f. \fIdisktab\fP(5)).  The partition information and description of the
drive geometry are written in the first sector of each disk with the
\fIdisklabel\|\fP(8) program.  Each partition may be used for either a
raw data area such as a paging area or to store a \*(Ux file system.
It is conventional for the first partition on a disk to be used
to store a root file system, from which \*(Ux may be bootstrapped.
The second partition is traditionally used as a paging area, and the
rest of the disk is divided into spaces for additional ``mounted
file systems'' by use of one or more additional partitions.
.NH 2
\*(Ux devices: block and raw
.PP
\*(Ux makes a distinction between ``block'' and ``raw'' (character)
devices.  Each disk has a block device interface where
the system makes the device byte addressable and you can write
a single byte in the middle of the disk.  The system will read
out the data from the disk sector, insert the byte you gave it
and put the modified data back.  The disks with the names
``/dev/xx0[a-h]'', etc., are block devices.
There are also raw devices available.
These have names like ``/dev/rxx0[a-h]'', the
``r'' here standing for ``raw''.
Raw devices bypass the buffer cache and use DMA directly to/from
the program's I/O buffers;
they are normally restricted to full-sector transfers.
In the bootstrap procedures we
will often suggest using the raw devices, because these tend
to work faster.
Raw devices are used when making new filesystems,
when checking unmounted filesystems,
or for copying quiescent filesystems.
The block devices are used to mount file systems,
or when operating on a mounted filesystem such as the root.
.PP
You should be aware that it is sometimes important whether to use
the character device (for efficiency) or not (because it wouldn't
work, e.g. to write a single byte in the middle of a sector).
Don't change the instructions by using the wrong type of device
indiscriminately.
