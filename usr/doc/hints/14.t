.sh "VAX 11/750 PACKAGES"
We want to put together a small 11/750 system capable of supporting about
8 time-sharing UNIX users, and a larger 11/750 system for roughly 16-24
users.  We need a minimum of 100 megabytes of space for the small
system and a reasonable tape drive, preferably a 125ips unit so
that tape operations can be done in a reasonable amount of time;
if the system is to include only non-removable disks, we consider
the faster tape system to be important.
For the larger system, we wish to add disk space to give the system
a minimum of 250 megabytes of space, and have more than one disk arm.
.SH
Small system
.LP
.DS B
.TS
box;
cb s s
l l l
l l l.
Small 750 System
_
	DEC System	Mixed Vendor System
_
CPU	11/750	11/750 from Broker or Integrator	
\^	\^	with .50 Mbyte DEC Memory but 8
\^	\^	Mbyte capacity.

Memory	1 Mbyte DEC	1 Mbyte National/Trendata/Mostek

Disk System	UDA50 Unibus Controller	Emulex SC750 RH750 Emulator
\^	RA80 121 Mbyte Drive	Fujitsu 134 Mbyte Drive

Tape System	TGE16 45 ips Tape Sys.	Emulex or Wesperco Controller
\^	\^	Cipher or Kennedy 125 ips tape
.TE
.DE
.LP
The small DEC system is based on the SV-BXGMB-CA package,
and includes an RL02 in addition to the RA80.
We basically ignore the RL02 which is of little
use to us and use the package because it is the cheapest way to get
started.
We add a TGE16 tape system as the best choice among a myriad of evils.
It is really too slow, but it is reliable and not too expensive.
DEC has been promising some better low cost tape units soon.
.LP
The mixed vendor system
is as inexpensive as possible while retaining
upward expandability.
If the builder were sure that this system was not going to be expanded
much then a substantial amount more could be shaved from the cost
by making several substitutions.
A National Semiconductor or Spectra Logics UNIBUS combination
disk and tape controller
could be substituted for the separate CMI disk controller and UNIBUS tape
controller shown.
A slower, perhaps 45 ips, tape unit with built in formatter could be
substituted for the 125 ips tape drive.
An older CPU with 2 Megabyte maximum memory capacity could be
used. These are available for substantially less than the CPUs equipped
with the newer memory controller and backplane.
Even with these modifications, another disk and another Megabyte of memory
could easily be added to produce substantial performance improvement.
One advantage of the mixed vendor system as shown
is that the Emulex SC750 controller
keeps the disk drives off the UNIBUS.  If an Ethernet controllers is
added to the system, they will not be contending for the bus.
.SH
Medium system.
.LP
To expand this basic system to support more users,
we would add additional lines, disk storage
and memory.
To the small all-DEC system we would add another
RA80, another Megabyte of memory and a DZ-11E.
To the mixed vendor system we would add another Fujitsu 134 Mbyte disk,
an Able or Emulex DH-11 emulator and another Mbyte of memory:
.DS
.TS
box;
cb s s
l l l
l l l.
Augmenting the Small 750 to a ``Medium'' System
_
	DEC System	Mixed Vendor System
_
Additional Disk	RA80 121 Mbyte Drive	Fujitsu 134 Mbyte Drive

More Memory	1 Mbyte DEC	1 Mbyte National/Trendata/Mostek

More Serial Lines	DZ-11E	Able/Emulex ``DH''
.TE
.DE
.LP
There are, of course, further expansion possibilities for the 11/750.
These vary depending on the application but could include a floating
point accelerator, more memory up to 8 Mbytes, and an additional UNIBUS
adaptor on the DEC system if other high speed devices like network
interfaces are to be on the UNIBUS along with the UDA50.
.bp
