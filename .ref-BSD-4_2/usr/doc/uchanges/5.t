.SH
.LG
.ce
Section 4
.SM
.sp
.PP
The system now supports the 11/730, the new 64Kbit RAM memory
controllers for the 11/750 and 11/780, and the second UNIBUS
adapter for the 11/750.
Several new character and/or block device drivers have been added, 
as well as support for many hardware devices which are
accessible only through the network facilities.  Each new
piece of hardware supported is listed below.
.PP
New manual entries in section 4 have been created to describe
communications protocols, and network architectures supported.
At present the only network architecture fully supported is
the DARPA Internet with the TCP, IP, UDP, and ICMP protocols.
.PP
.BP acc
A network driver for the ACC LH/DH IMP interface.
.BP ad
A driver for the Data Translation A/D converter.
.BP arp
The Address Resolution Protocol for dynamically
mapping betwee 32-bit DARPA Internet addresses and
48-bit Xerox 10Mb/s Ethernet addresses.
.BP css
A network driver for the DEC IMP-11A LH/DH IMP interface.
.BP dmc
A network interface driver for the DEC DMC-11/DMR-11
point-to-point communications device.
.BP ec
A network interface driver for the 3Com 10Mb/s Ethernet controller.
.BP en
A network interface driver for the Xerox 3Mb/s experimental 
Ethernet controller.
.BP hy
A network interface driver for the Network Systems Hyperchannel
Adapter.
.BP ik
A driver for an Ikonas frame buffer graphics device interface.
.BP il
A network interface driver for the Interlan 10Mb/s Ethernet
interface.
.BP imp
A network interface driver for the standard 1822 interface
to an IMP; used in conjunction with either acc or css hardware.
.BP kg
A driver for a KL-11/DL-11W used as an alternate real
time clock source for gathering kernel statistics and profiling
information.
.BP lo
A software loopback network interface for protocol
testing and performance analysis.
.BP pcl
A network interface driver for the DEC PCL-11B communications
controller.
.BP ps
A driver for an Evans and Sutherland Picture System 2
graphics device connected with a DMA interface.
.BP pty
Now includes a simple packet protocol to support 
flow controlled operation with mechanisms for flushing
data to be read and/or written.
.BP rx
A driver for the DEC dual RX02 floppy disk unit.
.BP ts
Now supports TU80 tape drives.
.BP tu
The VAX-11/750 console cassette interface has been
made somewhat usable when operating in single-user
mode.  The device driver employs assembly language
pseudo-dma code for the reception of incoming
packets from the cassette.
.BP uda
Now supports RA81, RA80, and RA60 disk drives.
.BP un
A network interface driver for an Ungermann-Bass network
interface unit connected to the host via a DR-11W.
.BP up
Now supports ECC correction and bad sector handling.
Also has improved logic for recognizing many different
kinds of disk drives automatically at boot time.
.BP uu
A driver for DEC dual TU58 tape cartridges connected
via a DL-11W interface.
.BP va
The Varian driver has been rewritten so that it may
coexist on the same UNIBUS with devices which require
exclusive use of the bus; i.e. RK07's.
.BP vv
A network interface driver for the Proteon proNET 10Mb/s
ring network controller.
