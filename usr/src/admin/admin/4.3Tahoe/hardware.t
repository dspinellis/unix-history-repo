.\" Copyright (c) 1988 The Regents of the University of California.
.\" All rights reserved.  The Berkeley software License Agreement
.\" specifies the terms and conditions for redistribution.
.\"
.\"	@(#)hardware.t	1.1 (Berkeley) %G%
.\"
.TL
Berkeley Software for UNIX:\s-4\u\(dg\d\s0
.br
Hardware Support
.sp
4.3BSD tahoe release of June 1988
.AU
.AI
Computer Systems Research Group
Department of Electrical Engineering and Computer Science
University of California, Berkeley
Berkeley, California 94720
.SH
Tahoe Hardware support in the 4.3BSD tahoe release
.PP
The system runs on the Computer Consoles, Inc. Power 6/32 processor,
known as ``tahoe.''
The distribution supports the following machines:
.IP \(bu
CCI Power 6/32, 6/32S, 6/32SX; the 6/32 MP multiprocessor is not yet supported
.IP \(bu
Harris HCX-7 (Harris HCX-9 support is planned for the near future)
.IP \(bu
Unisys (Sperry) 7000/40 and similar uniprocessors
.IP \(bu
ICL Clan 7
.FS
\(dg\s-1UNIX\s0 is a registered trademark of AT&T in the U.S.A.
and other countries.
.FE
.FS
\(dd\s-1 DEC, VAX, UNIBUS, MASSBUS\s0, and \s-1DEC\s0writer are
trademarks of Digital Equipment Corporation.
.FE
.PP
Any of the standard CCI-based disk controllers (VDDC or SMD, SMD-E and enhanced
SMD-E with scatter-gather I/O support) may be used.
The following disks are known to work; others may be used by creating
new disk labels, but might not allow initial bootstrapping:
CDC FSD (160 MB), CDC 9766 (300 MB),
CDC XFSD (340 MB), CDC 515MB, Fujitsu 360 MB, Fujitsu Eagle (440 MB),
and Maxtor 340Mb.
.PP
The VIOC-X and MPCC controllers are supported for asynchronous serial
communications.
.PP
The ACC (\fIace\fP) and CMC (\fIenp\fP) Ethernet controllers are supported.
.PP
The only tape controllers currently supported are the Ciprico Tapemaster
F880 and M990 controllers, normally used with Cipher 1/2" magnetic tape drives.
.SH
VAX Hardware support in the 4.3BSD tahoe release
.PP
The system runs on VAX-11/725, VAX-11/730, VAX-11/750, VAX-11/780,
VAX-11/785, VAX-8200, VAX-8250, VAX-8600, and VAX-8650 processors and supports
the standard DEC mass storage peripherals: RM03, RM05
RP06, RP07, RA60, RA70, RA80, RA81, RA82, RM80, RL02, and
RK07 disks; TS11, TU80, TE16, TU45, TU77, TU78, and TU81 tapes.
MicroVAX II processors and workstations are supported, including QVSS and QDSS
displays (VSII and VAXStation/GPX), RQDX2/3 disk controllers with RD52, RD53
and RD54 disk, and TK50 or other standard tape.
DEC standard bad block
handling is supported on all the DEC disk drives
except the RL02.
.PP
The EMULEX SC21-V and SC31 UNIBUS
storage module disk controllers are supported with AMPEX 9300 and CDC 9766
300 Megabyte disk drives, AMPEX Capricorn 330 Megabyte Winchester disk drives,
FUJITSU 160 Megabyte Winchester drives.
In addition, the SC31 supports FUJITSU Eagle 404 Megabyte Winchester drives. 
Other drives may be supported by addition of table entries to the driver,
although a listed drive is required for initial bootstrapping.
The EMULEX SC750 (emulating the DEC RH750 MASSBUS interface) and the
SC780 and SC7000 series disk controllers
(emulating the DEC RH780 MASSBUS interface)
are supported with the CDC 9775, 9766, and 9730 disk drives,
AMPEX 9300 and Capricorn disk drives,
and the FUJITSU Eagle 404 Megabyte Winchester disk drive.
Additional drives may be supported by creation of a new entry
in the
.IR disktab (4)
file followed by installation of a disk label on the drive
using
.IR disklabel (8).
Bad blocking support is provided on all disks attached to an EMULEX
UNIBUS or MASSBUS controller.
.TS
center, box;
c s s
l l l.
VAX disk controller and drive support
name*	controllers	drives
_
hp	RH750, RH780	RM03, RM05, RP06, RP07, RM80
hp	SC750, SC780, SC7000\(dg	various SMD\(dd
uda/ra	UDA50, KDB50, KDA50	RA60, RA70, RA80, RA81, RA82
kdb/kra	KDB50	RA60, RA70, RA80, RA81, RA82
uda/ra	RQDX2, RQDX3	RD52, RD53, RD54
uda/ra	any UNIBUS/Q-bus MSCP	various\(dd
up	SC21-V, SC31\(dg	various SMD**
rl	RL211	RL02
hk	RK611	RK06, RK07
idc/rb	IDC (725, 730)	R80, RL02
.sp
.T&
l s s.
* first name is controller/driver name, second is drive device name
\(dg also other compatible controllers
\(dd any compatible SMD drive may be used by creating a disk label
** any compatible SMD drive may be used by addition of a table entry
.TE
.PP
The EMULEX TC-11 tape controller
(which emulates a TM11 DEC UNIBUS controller),
the EMULEX TC-7000 tape controller
(which emulates a TU77 DEC MASSBUS controller),
and System Industries Model 9700 tape drive
(which emulates a DEC TU45 controller on the UNIBUS) are supported. 
Other controllers which emulate supported DEC tape controllers should also work.
.TS
center, box;
c s s s
l l l l.
VAX tape controller and drive support
name*	controllers	formatter	drives
_
ht	DEC RH750, RH780	TM03	TE16, TU45, TU77
mt	DEC RH750, RH780	TU78	TU78
mt	Emulex SC7000	various	various
tm	Emulex TC-11	various	various
ut	SI 9700	various	various
ts	DEC TS11, TU80		TS11, TU80
.T&
l l s l.
tmscp/tms	any UNIBUS/Q-bus TMSCP		TU81, TK50, TK70
.sp
.T&
l s s s.
* first name is controller/driver name, second is device name if different
.TE
.PP
To bootstrap the system, a supported disk and
tape drive are needed.  To maintain all the system
sources and binary programs, at least 120 Megabytes
of disk storage is required.
.PP
For terminal interfaces,
the standard DEC DZ11, DMZ32, DHU11, DHV11 and DMF32 terminal
interfaces are supported.
In addition, DH11 emulators such as
the ABLE DH/DM (which replaces the ABLE DMAX)
and EMULEX CS-11 and CS-21 are supported.
The system also provides support for standard line printer
interfaces emulting the DEC LP11 or the parallel port of the DMF32.
Support for serial printers such as a DECwriter-III and printer-plotters
such as those made by BENSON/VARIAN or VERSATEC is 
available with standard drivers.
.PP
This release supports any number
of any of the devices described above.
The devices may be placed arbitrarily on any
available MASSBUS and UNIBUS interfaces.
For the 8600 and the 8650, adapters may be on either of the SBIA's.
The system configures at boot time,
locating available devices,
using a system configuration compiled into the kernel.
The configuration description contains all the information
about the topology of the machine and the addresses at which the various
devices are located.
It is possible (and desirable) to write the description using
``pattern matching'' to only partially specify some of the interconnects.
The bootstrap passes the identity of the load device to the kernel,
which will use it as the root file system.
.KS
.PP
The following controllers may be used for 10Mb/s Ethernet
on the appropriate I/O buss:
.TS
center, box;
c s
l l.
VAX Ethernet controller support
name	controller
_
de	DEC DEUNA, DELUA (UNIBUS)
ec	3Com 3C300 (UNIBUS)
ex	Excelan EXOS-204 (UNIBUS), EXOS-203 (Q-bus)
il	Interlan NI1010, NI1010A (UNIBUS)
np	Interlan NP100 (UNIBUS)
qe	DEC DEQNA, DELQA (Q-bus)
.TE
.KE
.PP
The system configuration program sizes system data structures based on
a specification of the maximum number of active users to be present on the
system.  To build a system for a larger or smaller workload you only need
change this single constant.  The system also initializes the parameters
to the paging system and sizes its file system buffer cache based
on the amount of available memory.
.PP
The system supports access to the 11/780 and 11/785 console floppy disk,
the 8600 and 8650 RL02 console disk drive, and
the 11/725, 11/730, and 11/750 TU58 console cassette tape.
However, reliable access to the 11/750 TU58 cassette
interface is possible only on a totally quiescent system.
