.sh "VAX 11/780 PACKAGES"
For a system with more growth possibilities than an 11/750, faster
processing, and higher i/o bandwidth,
we recommend starting with a small 11/780.
Our goal here is to start with a system capable of supporting
8-16 timesharing users and expanding the system to be capable of supporting
roughly 24 users.
We also consider a large expansion of this system, to a system
that might support 32 to 40 terminal users to the exhaustion of available
CPU cycles.*
.FS
* Using systems similar to the largest shown here, in an environment
consisting of small student programming some sites have reported
running up to 70 interactive users; CPU cycles are the critical
resource with this many users.
.FE
.SH
Small system
.LP
For our small system we use 400 Megabytes of disk storage and a
125ips 6250bpi tape
drive that will be capable of handling file backups if the system
is eventually expanded.  In our first expansion of this small
system, we wish to add to the available space to a minimum of 800 Megabytes
of disk storage, acquire at least two disk arms,
and add additional terminal lines.
In a large expansion of this system we include more terminals,
an additional disk controller to get at least two separate disk channels,
and an additional 800 Megabytes of storage for a total of 1600 Megabytes.
.LP
To build a small system from all DEC equipment, we would start with the
RUA81/TU78 based system, the SV-AXECA-CA.  This system includes
8 terminal lines, 4 Megabytes of memory, a 456 Megabyte disk drive
and a 125ips 6250bpi tape.
The system is equipped with two UNIBUS adaptors so that the UDA50
does not contend with other UNIBUS devices.
To this we would add a floating point accelerator.
.LP
On the mixed vendor system we would substitute a Fujitsu
Eagle 404 Mbyte disk drive on an Emulex SC780 SBI interfaced
controller and an Aviv/Telex 6250 tape subsystem.
.DS
.TS
box;
cb s s
l l l
l l l.
Small 780 System
_
	DEC System	Mixed Vendor System
_
CPU	11/780	11/780 from Broker or Integrator	
\^	\^	with .25 Mbyte DEC Memory and
\^	\^	UNIBUS Adaptor Included

Memory	4 Mbyte DEC	4 Mbyte National/Trendata/Mostek

Disk System	UDA50 Unibus Controller	Emulex SC780 RH780 Emulator
\^	RA81 456 Mbyte Drive	Fujitsu 404 Mbyte Drive
\^	on own UBA

Tape System	TEU78 125ips 6250 ips	Aviv Controller
\^	Tape Subsystem	Telex Drive/Formatter

Serial Lines	DZ-11A	Able/Emulex ``DH''

Other	DEC Floating Pt. Acc.	DEC Floating Pt. Acc.
.TE
.DE
.SH
Medium system
.LP
To expand this basic system to support more users and get additional
disk space, we would add additional lines and disk storage.
.DS
.TS
box;
cb s s
l l l
l l l.
Augmenting the Small 780 to a ``Medium'' System
_
	DEC System	Mixed Vendor System
_
Additional Disk	RA81 456 Mbyte Drive	Fujitsu 404 Mbyte Drive

More Serial Lines	DZ-11E	Able/Emulex ``DH''
.TE
.DE
.SH
Large system
.LP
To form a system with the emphasis on handling of data-intensive
applications, and to emphasize total growth of the system, we would add
a second disk channel and interleave memory to increase i/o throughput
and reduce average CPU memory access as much as possible.
In both the DEC and mixed vendor systems a CPU extension cabinet would be
required in addition to another DEC memory controller.
We would fill out the second memory system to 4 Megabytes.
.LP
For more disk throughput, we would add
an REP07-AA 504MB disk drive on a MASSBUS controller to the basic DEC system.
This disk provides a very high burst data throughput and could share the
MASSBUS Adaptor of the Tape Unit with only minor performance loss while
the tape unit was being used.
.LP
To accomplish the same ends with the mixed vendor system, we would
simply add a second Emulex SC780 disk controller channel and at 
least one more Fujitsu Eagle 404 Mbyte disk drive.
.DS
.TS
box;
cb s s
l l l
l l l.
Augmenting ``Medium'' 780 to ``Really Big'' System
_
	DEC System	Mixed Vendor System
_
Additional Disk	RP07 (516 Mbyte) on	Fujitsu Eagle (404 Mbyte)
and Channel	MASSBUS with tape sys.	on another SC780 controller

Second Memory	DEC	DEC
Controller	\^	\^
and Cabinet	\^	\^

Additional Memory	DEC	Trendata/National Mostek

More Serial Lines	DZ-11E	Able/Emulex ``DH''
.TE
.DE
