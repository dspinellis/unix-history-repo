.\" Copyright (c) 1986 Regents of the University of California.
.\" All rights reserved.  The Berkeley software License Agreement
.\" specifies the terms and conditions for redistribution.
.\"
.\"	@(#)vaxuba.t	1.5 (Berkeley) 4/11/86
.\"
.NH
VAX UNIBUS device drivers
.PP
This section includes changes in device drivers
for UNIBUS peripherals other than network interfaces.
Modifications common to all of the disk and tape drivers
are listed in the previous section on MASSBUS drivers.
Many of the UNIBUS drivers were missing null terminations
on their lists of standard addresses; this has been corrected.
.NH 2
Changes in terminal multiplexor handling
.PP
There are numerous changes that were made uniformly
in each of the drivers for UNIBUS terminal multiplexors
(DH11, DHU11, DMF32, DMZ32, DZ11 and DZ32).
The DMA terminal boards on the same UNIBUS share map registers
to map the \fIclists\fP to UNIBUS address space.
The initialization of \fItty\fPs at open and changes from \fIioctl\fPs
have been made uniform; the default speed is 9600 baud.
Hardware parameters are changed when local modes change;
these include LLITOUT and the new LPASS8 options for 8-bit
output and input respectively.
The code conditional on PORTSELECTOR to accept characters while or before
carrier is recognized is the same in all drivers.
The processing done for carrier transitions
was line discipline-specific, and has been moved into
the standard \fItty\fP code;
it is called through the previously-unused \fIl_modem\fP entry
to the line discipline.
This routine's return is used to decide whether to drop DTR.
DTR is asserted on lines regardless of the state of the software
carrier flag.
The drivers for hardware without silo timeouts (DH11, DZ11)
dynamically switch between use of the silo during periods of high input
and per-character interrupts when input is slow.
The timer routines schedule themselves via timeouts
and are no longer called directly from the \fIsoftclock\fP interrupt.
The timeout runs once per second unless silos are enabled.
Hardware faults such as nonexistent memory errors and silo overflows
use \fIlog\fP instead of \fIprintf\fP to avoid blocking the system
at interrupt level.
.NH 2
Changes in individual drivers
.XP dmf.c
The use of the parallel printer port on the DMF32 is now supported.
Autoconfiguration of the DMF includes a test for the sections
of the DMF that are present; if only the asynchronous serial ports
or parallel printer ports are present, the number of interrupt vectors
used is reduced to the minimum number.
The common code for the DMF and DMZ drivers was moved to \fIdmfdmz.c\fP.
Output is done by DMA.
The Emulex DMF emulator should work with this driver,
despite the incorrect update of the bus address register
with odd byte counts.
Flow control should work properly with DMA or silo output.
.XP dmfdmz.c
This file contains common code for the DMF and DMZ drivers.
.XP dmz.c
This is a new device driver for the DMZ32 terminal multiplexor.
.XP idc.c
The ECC code for the Integral Disk Controller on the VAX 11/730
was corrected.
.XP kgclock.c
The profiling clock using a DL11 serial interface can be disabled
by patching a global variable in the load image before booting 
or in memory while running.
It may thus be used for a profiling run and then turned off.
The \fIprobe\fP routine returns the correct value now.
.XP lp.c
A fix was made so that slow printers complete printing after device close.
The \fIspl\fP's were cleaned up.
.XP ps.c
The handler for the E & S Picture System 2 has substantial changes
to fix refresh problems and clean up the code.
.XP rk.c
Missing entries in the RK07 size table were added.
.XP rl.c
A missing partition was added to the RL02 driver.
Drives that aren't spun up during autoconfiguration
are now discovered.
.XP rx.c
It is no longer possible to leave a floppy drive locked
if no floppy is present at open.
Incorrect open counts were corrected.
.XP tm.c
Hacks were added for density selection on Aviv triple-density controllers.
.XP tmscp.c
This is a new driver for tape controllers using the Tape Mass Storage
Control Protocol such as the TU81.
.XP ts.c
Adjustment for odd byte addresses when using a buffered
data path was incorrect and has been fixed.
.XP uba.c
The UBA_NEED16 flag is tested, and unusable map registers are not
allocated for 16-bit addressing devices.
Optimizations were made to improve code generation in \fIubasetup\fP.
Zero-vector interrupts on the DW780 now cause resets only when
they occur at an unacceptably high rate;
this is appreciated by the users who happen to be on the dialups
at the time of the 250000th passive release since boot time.
UNIBUS memory is now configured separately from devices during
autoconfiguration by \fIubameminit\fP, and this process is repeated
after a UNIBUS reset.
Devices that consist of UNIBUS memory only may be configured more easily.
On a DW780, any map registers made useless by UNIBUS memory
above or near them are discarded.
.XP ubareg.h
Definitions were added to include the VAX8600.
.XP ubavar.h
Modifications to the \fIuba_hd\fP structure allow zero vectors
and UNIBUS memory allocation to be handled more sensibly.
The \fIuba_driver\fP has a new entry for configuration of UNIBUS
memory.
This routine may probe for UNIBUS memory,
and if no further configuration is required may signify the completion
of device configuration.
A macro was added to extract the UNIBUS address from the value
returned by \fIubasetup\fP and \fIuballoc\fP.
.XP uda.c
This driver is considerably more robust than the one released with 4.2BSD.
It configures the drive types so that each type may use its own
partition tables.
The partitions in the tables as distributed are much more useful,
but are mostly incompatible with the previously released driver;
a configuration option, RACOMPAT, provides a combination of new
and old filesystems for use during conversion.
The buffered-data-path handling has been fixed.
A dump routine was added.
.XP up.c
Entries were added for the Fujitsu Eagle (2351) in 48-sector mode
on an Emulex SC31 controller.
.XP vs.c
This is a driver for the VS100 display on the UNIBUS.
