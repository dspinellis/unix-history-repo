.\" Copyright (c) 1986 Regents of the University of California.
.\" All rights reserved.  The Berkeley software License Agreement
.\" specifies the terms and conditions for redistribution.
.\"
.\"	@(#)vaxif.t	1.7 (Berkeley) 4/11/86
.\"
.NH
VAX Network Interface drivers
.PP
Most of the changes in the network interfaces follow common patterns
that are summarized in categories.
In addition, there are a number of bug fixes.
The change that was made universally to the interface handlers
was to remove the \fIioctl\fP routines that set the interface address
and flags, replacing them by simpler routines
that merely initialize the hardware if this has not already been done.
Several of the drivers notice when the IFF_UP flag is cleared
and perform a hardware reset, then reinitialize the interface
when IFF_UP is set again.
This allows interfaces to be turned off, and also provides
a mechanism to reset devices that have lost interrupts
or otherwise stopped functioning.
The handling of the other interface flags has been made more consistent.
IFF_RUNNING is used uniformly to indicate that UNIBUS resources
have been allocated and that the board has been initialized.
The reset routines clear this flag before reinitializing
so that both operations will be repeated.
.NH 2
Interface UNIBUS support
.PP
The UNIBUS common support routines for network interfaces
have been modified to support multiple transmit and receive buffers
per device.
A set of macros provide a nearly-compatible interface
for devices using a single buffer of each type.
When placing received packets into mbufs, \fIif_ubaget\fP
prepends a pointer to the receiving interface to the data;
this requires that the interface pointer be passed to \fIif_ubaget\fP
or \fIif_rubaget\fP as an additional argument.
When removing the trailer header from the front of a packet,
interface receive routines must move the interface pointer
which precedes the header; see one of the existing drivers
for an example.
When received data is larger than half of an mbuf cluster,
the data will be placed in an mbuf cluster rather than a chain of small
mbufs.
Similarly, in \fIif_ubaput\fP, clusters may be remapped instead of copied
if they are at least one-half full and are the last mbuf of the chain.
For devices like the DEC DEUNA that wish to perform receive operations
on a transmit buffer, the transmit buffers are marked.
Receive operations from transmit buffers force page mapping to be consistent
before attempting to read data or swap pages from them.
.NH 2
10 Mb/s Ethernet
.PP
The 10Mb/s Ethernet handlers have been modified to use the new ARP
interfaces.
They no longer use \fIarpattach\fP, and the call to \fIarpresolve\fP
contains an additional argument for a second return, a boolean for
the use of trailer encapsulations.
Input and output functions were augmented to handle NS IDP packets.
For hosts using Xerox NS with multiple interfaces,
the drivers are able to reprogram the physical address on each
board so that all interfaces use the address of the first
configured interface.
The hardware Ethernet addresses are printed during autoconfiguration.
.NH 2
Changes specific to individual drivers
.XP if_acc.c
An additional word was added to the input buffer
to allow space for the end-of-message bit on a maximum-sized
message without segmentation.
This avoids a hardware problem that sometimes causes the next packet
to be concatenated with the end-of-message segment.
.XP if_ddn.c
A new driver from ACC for the ACC DDN Standard mode X.25 IMP interface.
.XP if_de.c
A new driver for the DEC DEUNA 10 Mb/s Ethernet controller.
The hardware is reset when \fIifconfig\fPed down
and reinitialized when marked up again.
.XP if_dmc.c
The DMC-11/DMR-11 driver has been made much more robust.
It now uses multiple transmit and receive buffers.
A link-layer encapsulation is used to indicate the type of the packet;
this driver is thus incompatible with the 4.2BSD DMC driver.
(The driver is, however, compatible with current ULTRIX drivers.)
.XP if_ec.c
The handler for the 3Com 10 Mb/s Ethernet controller
is now able to support multiple units.
The address of the UNIBUS memory is taken from the flags in the configuration
file; note that address 0 is still the default.
The UNIBUS memory is configured in a separate memory-probe routine
that is called during autoconfiguration and after a UNIBUS reset.
This allows the 3Com interface reset to work correctly.
The collision backoff algorithm was corrected so that the maximum backoff
is within the specification, rather than waiting seconds after numerous
collisions.
The private \fIecget\fP and \fIecput\fP routines were modified
to correspond with the \fIif_uba\fP routines.
The hardware is reset when \fIifconfig\fPed down
and reinitialized when marked up again.
.XP if_en.c
The 3 Mb/s Experimental Ethernet driver now supports NS IDP packets,
using a simple algorithmic conversion of host to Ethernet addresses.
The \fIenswab\fP function was corrected.
.XP if_ex.c
A new driver for the Excelan 204 10 Mb/s Ethernet controller,
used as a link-layer interface.
.XP if_hdh.c
A new driver for the ACC HDH IMP interface.
.XP if_hy.c
A new version of the Hyperchannel driver from Tektronix was installed.
It is untested with 4.3BSD.
.XP if_il.c
The Interlan 1010 and 1010A driver now resets the interface
and checks the result of hardware diagnostics when initializing the board.
The hardware is reset when \fIifconfig\fPed down
and reinitialized when marked up again.
.XP if_ix.c
A new driver for using the Interlan NP100 10 Mb/s Ethernet controller
as a link-level interface.
.XP if_uba.c
In addition to the major changes in UNIBUS support functions,
there were several bug fixes made.
Interfaces with no link-level header are set up properly.
A variable was reused incorrectly in \fIif_wubaput\fP, and this
has been corrected.
.XP if_vv.c
The driver for the Proteon proNET has been reworked in several areas.
The elaborate error handling code had several problems and was simplified
considerably.
The driver includes support for both the 10 Mb/s and 80 Mb/s rings.
The byte ordering of the trailer fields was corrected;
this makes the trailer format incompatible with the 4.2BSD driver.
