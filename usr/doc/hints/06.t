.sh DISKS\(dg
.FS
\(dg Disk sizes shown throughout this document are in bytes
of formatted space available.
.FE
The area of disks and disk controllers is one which has seen
a great deal of change since the last revision of this paper in mid
1981.
At that time we had no experience with Winchester technology disk
drives.
Now, after some painful experimentation, we have settled on a few
Winchester products which fill our needs reliably.
We no longer
buy, or recommend, any removable media disk products.
.LP
The choice of available controllers is also wider and much
improved.
High quality controllers are available which interface to the native
busses of 750s and 780s as well as the UNIBUS.
In addition, DEC has introduced an entire new storage system architecture
which places a great deal more function in the controller, incorporates
a new controller-drive interconnect, and uses improved error correction
algorithms.
.LP
First, we will discuss some of the major areas of change in disk/controller
technology.
We will then explore how these improve, or otherwise affect, our
methods of doing business.
Finally, we will consider some specific DEC and non-DEC products.
.LP
The availability of large capacity, low cost, high reliability
Winchester technology disk drives has had an enormous impact on us.
The rack mountable,
300 Megabyte or bigger disk which was
always ``just around the corner'' is really here.
It is hard to see how we got along without it.
We can now put about 2 Gigabytes of storage in the same
footprint that previously held 256 Megabytes.
In addition, we consume and dissipate about 25% of the
energy we did with older, removable media, drives.
The prospective buyer should be warned, however, that not all
``winnies'' live up to expectations with respect to reliability.
We are happy with the reliability of the equipment we
describe here.
If you want to try something else, be sure and have some long
heart to heart talks with other users of the product.
.LP
Cost per Megabyte of disk storage is down significantly.
Cost ranges from $30 to $110 per Megabyte for disks, not
counting the price of the controller(s).
This value depends on the size of the units purchased
and the choice of vendor.
Cost per unit storage in terms of both purchase price and cost
to operate are a stronger inverse function of the total drive
capacity than ever before.
For example, the cost per Megabyte of the 456 Mbyte DEC RA81
is about 35% of that of the 121 Mbyte RA80.
The reason for this becomes clear when the drives are examined;
many of the components are identical.
.LP
The higher recording densities of new disk drives has also been
a strong motivator in controller evolution.
One technique for increasing the recording density of the drives
has been to rely more heavily on sophisticated error correction
and block remapping schemes.
No large Winchester drive can be depended on to be ``error free.''
In fact, most the drives we use have uncorrectable media defects.
These locations must be remapped using some combination of
controller firmware and handler software.
In addition, the higher bit rates of new disk drives demand
faster serial logic in the controller interface.
Many older disk controllers are limited to the burst
transfer rate of 3330 style disks of about 1.25 Mbyte/sec.
.LP
Two types of controller have evolved for the newer, high
bit density disks.
The first is simply a version of the traditional SMD or
Storage Module Drive interface reengineered for higher
data rates.
This type of interface characterizes all of the non-DEC
controllers which have been produced for VAXes of the
last few years.
These controllers interface to the native busses of the
VAX (SBI or CMI) where possible to allow the higher data rates available
to be passed all the way through to memory.
Where the controller must operate on a bus incapable of a
continuous transfer rate as high as the disk, some amount
of internal buffering is provided to maximize the amount of date
transferred before the disk ``blows a rev''.*
.FS *
* By ``blowing a rev'', we mean a data transfer can not
be completed without extraneous disk revolutions.  This
is mainly a function of the time required by a processor to
service an interrupt, the bandwidth of the bus, and the buffering
in the controller.  With the 4.2BSD file system, disk controllers
are now being extended to their limitations, and beyond.
This has significantly influenced our concern for the their 
limitations as bandwidth suffers greatly when such an event
takes place.
.FE
.LP
Non-DEC controllers most often emulate the DEC RH11, RH750, or RH780
interface.
Some support for error correction is provided by the controller
although
a substantial assist is usually required from the system driver.
Remapping of uncorrectable media defects is entirely handled by
the driver.
All 4.2BSD device drivers support
bad block remapping.  In addition, error correction and
remapping support is, optionally, available
in the standalone utilities\(dg. 
.FS
\(dg Due to limitations in the size of a binary image which
may be placed on a boot cassette or floppy, the error
correction and bad sector forwarding code is not
included in the standalone utilities by default.
.FE
The only part of the system
which does not gracefully handle errors or
media defects is the first
level bootstrap code used on 750s.
.LP
DEC has produced a very different type of controller, partially
to deal with the challenges of higher density disk drives.
This controller, the UDA50, is an example of
DEC's long range plan for mass storage (this \*(lqplan\*(rq
is called the Digital Storage Architecture, or DSA).
One of the fundamental goals of DSA is to provide a
standard set of disk \*(lqoperations\*(rq across a
variety of storage products.
With DSA it should be possible to construct
standard drivers which know very
little about the characteristics or geometry of the
actual storage devices they are dealing with. In order
to meet this goal,
error correction, bad block forwarding, and even the mapping
of logical blocks onto the physical disk are handled in the
controller.
Requests to the controller typically consist of logical
block addresses and counts, along with a memory transfer address.
Responses then contain either data or a failure message.
The controller independently takes all possible measures to
recover data before returning failure.
.LP
In addition to increasing the functionality of the controller,
DSA specifies a new controller to drive interface. 
The Standard Disk Interface, or SDI, is capable of handling 
the transfer rates of any drive which DEC may produce in the
foreseeable future.
This interface is implemented using four electrically isolated radial
mini-coax cables to each disk drive embedded in a tough rubber-like
umbilical.
.LP
On 750 and 780 systems we are,
or will be, buying either large (404 Mbyte) Fujitsu
disk drives and Emulex SBI or CMI interfaced controllers,
or DEC UDA50 controllers with (456 Megabyte) RA81 disk drives.
The choice here is not clear as the two packages are both attractive
and each has a different set of advantages.
Although we do not currently have any UDA50/RA81s at Berkeley,
several users of 4BSD do have them, and are very satisfied.
In addition, we have visited Colorado Springs,
where the drives are manufactured, and run benchmarks on them
using an early version of 4.2BSD.
The preliminary measurements support our
optimism about the UDA50/RA81 combination, though we
are not yet ready to publish these results
(they will be available at a later time).
.LP
It is important not to place too much emphasis on raw performance issues
when comparing products as similar in capabilities
as the large disk choices presented here.
Reliability, freedom from bugs, and ease of maintenance are equally if not
more important to us.
The value of the product in future configurations is also important.
For example, the UDA50/RA81 disk system represents an early implementation
of a new architecture.
It incorporates many new features heretofore unavailable to us.
In addition, it is expandable in the sense that the disk/controller interface
is designed to handle future density increases which are not likely
to be useable with the traditional SMD interface.
On the otherhand, any implementation as new as the UDA50/RA81 is 
not as likely to be as bug free or as well understood as the traditional
RH style interface architecture.
.LP
Table 1 indicates some of the tradeoffs as
we now understand them.
.KF
.DS B
.TS
box;
l l l
ltiw(1.0i) ltw(2.0i) ltw(2.0i).
Criterion	UDA50/RA81	Emulex SC7?0/Fujitsu Eagle
_
T{
.fi
.na
Initial Purchase Cost \- 750
T}	T{
.fi
.na
UDA50 and 1st RA81 \- $57.00/Mbyte w/o additional UNIBUS adaptor;
$70.00/Mbyte with UNIBUS adaptor
T}	T{
.fi
.na
SC750 and first Eagle \- $55.00/Mbyte
T}
.sp .5
T{
.fi
.na
Initial Purchase Cost \- 780
T}	T{
.fi
.na
UDA50 and 1st RA81 \- 
$83.00/Mbyte with UNIBUS adaptor
T}	T{
.fi
.na
SC780 and 1st Eagle \- $65.00/Mbyte
T}
.sp .5
T{
.fi
.na
Cost for Incremental Addition
T}	T{
.fi
.na
Additional RA81s \- $41.00/Mbyte
T}	T{
.fi
.na
Additional Eagles \- $32.00/Mbyte
T}
.sp .5
T{
.fi
Performance
T}	T{
.fi
May be somewhat better in mixed request, multi drive environment due
to ordering optimizations possible in controller; software handler at 
present is suboptimal
T}	T{
.fi
Initial tests indicate 5-10% better single file throughput due to better
sustained burst rate
T}
.sp .5
T{
.fi
.na
Maintenance Costs
T}	T{
.fi
Very low \- $111/Mo. for 1st drive and controller (compare to $326 for RM05)
T}	T{
.fi
.na
Unknown but believed very low
T}
.sp .5
T{
.fi
.na
Mean Time Between Failure
T}	T{
.fi
Too little experience available yet;
RM80 is precursor of RA81 mechanically and has been quite good
T}	T{
.fi
Not a lot of experience on these yet either; initial experience
looks excellent (smaller Fujis are phenomenal; 30,000 MTBF!)
T}
.sp .5
T{
.fi
.na
Mean Time to Repair
T}	T{
.fi
Designed for quick field removal of HDA; easy to repair
T}	T{
.fi
Not as easy; more complex disassembly
T}
.sp .5
T{
.fi
.na
Sources of Maintenance
T}	T{
.fi
DEC; maint. contract cheap, real, and available
T}	T{
.fi
Not so clear; ask for exchange contract from vendor
T}
.sp .5
T{
.fi
.na
Robustness of Drive Interconnect
T}	T{
.fi
Incredible \- electrical isolation and you could run over cables
with a fork lift! Radial connection allows easy removal of a single drive
T}	T{
.fi
Same old SMD flat cables; daisy chain
T}
.sp .5
T{
.fi
.na
Future Value
T}	T{
.fi
Early implementation of new architecture; if it pans out, likely
to be compatible with future, high performance, products; DEC resale high
anyway
T}	T{
.fi
High performance (stretched to limits) implementation of old interface
standard; not likely to work again for next increase
T}
.sp .5
T{
.fi
.na
Cost to Integrate
T}	T{
.fi
Handler is new; some initial bugs likely;
probably a bug or two left in controller firmware too
T}	T{
.fi
Well known interface; much more likely to be bug free
T}
.TE
.sp 0.5
.ce
Table 1.  Large Disk System Comparison
.DE
.KE
.LP
When searching for less storage 
for smaller smaller systems, or where two arms
are needed for performance and 800+ Megabytes of storage is overkill,
another choice is required.
Even at $50/Mbyte, a 404 Megabyte drive is not cheap.
One of the authors has had good experience on a small 750 system
with a 160 Mbyte Winchester disk drive from Tecstore and a National
Semiconductor HEX-3000 combination tape and disk controller.
We also know of successful use of the Spectra Logic combination
controller on a 730 system.
Using slightly less expensive disk drives and a combination
controller one can obtain cost effective (< $75.00/Mbyte) storage in
smaller amounts
and provide a tape interface to boot
(so to speak.)
