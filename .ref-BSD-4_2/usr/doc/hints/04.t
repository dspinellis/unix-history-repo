.sh "BANDWIDTH CONSIDERATIONS"
Evaluation of the data transfer capacities between the various parts
of VAX systems is a complex task that plays a critical part in
system configuration.
Unfortunately, there is a tremendous amount of misinformation available
on this subject and little useful hard data.
We have made many measurements and are always in the process
of making more.
What we currently know follows.
.LP
The 11/780 UNIBUS adapter is the device most frequently shrouded in confusion.
DEC documents variously give the bandwidth at between 1.2 MB/sec and
1.5 MB/sec when transferring through a buffered data path.
We are not aware of any specifications for the unbuffered data path
but have not been able to use it with some devices as slow as 50 KB/sec.
One experiment we conducted involved examining the UNIBUS protocol
lines with a scope while constantly transferring from a disk drive.
We observed that while the drive was transferring at an average rate
of about 1.2 MB/sec the UNIBUS was close to one hundred
precent busy.
This test was conducted on an otherwise idle system.
No other devices were active on the UNIBUS and large disk
transfers (cylinders) reduced any register set up time to
a minimum.
We conclude from this that 1.2 MB/sec is the
.I
absolute maximum
.R
transfer rate possible through a 11/780 UNIBUS adapter.
Our observations showed that the largest delays while transferring
data occurred while the buffered data path was being loaded or
unloaded from the SBI.
Since the UBA is controlled by a micro sequencer that is also involved
in other UBA activities such as processing interrupts, we suspect
that on an active UBA this bandwidth may be somewhat reduced.
.LP
Measurements of the available throughput to and from
the 4.2BSD file system indicate a significant
difference between disks running on the native processor bus
(CMI or SBI)
and those running on the UNIBUS.
Average data rates are consistently lower
on disks residing on the UNIBUS, even when the controller provides
a few sectors of buffering.
This leads us to believe that when average reads are 4-8
Kilobytes (the average block size of a 4.2BSD file system),
most UNIBUS controllers will fall behind and 
eventually lose a revolution.
This does not, however, seem to occur with the UDA50
UNIBUS controller as it has a much larger amount
(16 Kbytes) of buffering\(dg.
.FS
\(dg A few of the initial UDA50 controllers were delivered with
only 4 Kbytes of buffering. Avoid these.
.FE
.LP
There are troublesome devices that cannot buffer enough data to
guarantee that the maximum size record can always be transferred
(6250bpi tape drives), or do not buffer an adequate amount of data
(RK07 disk controller).
To handle these devices
UNIX provides a software interlock mechanism that
prevents excess UBA contention.
.LP
The MASSBUS adapters are specified to have a higher potential bandwidth
of 2.5 MB/sec.
Since they are selector channels that allow only one device
to transfer data at a time, the realized bandwidth is limited to
the rate of the fastest device.
The fastest devices currently available from DEC for 11/750 systems
or 11/780 systems with a single memory controller
transfer at 1.3 MB/sec.
Large 11/780 systems with two memory controllers and interleaved memory
may run RP07 disk drives that then transfer data at 2.2 MB/sec.*
.FS
* On machines with only one memory controller the RP07 hardware is arranged
to transfer at 1.3 MB/sec.
.FE
An interesting bandwidth limit may be established by the memory
controller particularly on 11/780s.
We suspect that
the CPU may be slowed considerably by memory contention
when two disk channels are being used simultaneously.
This should be alleviated by using interleaved memory controllers.
.LP
The appendix to the
.I "VAX Hardware Handbook"
titled ``System Throughput Considerations''
seems to bear out these impressions and
should be read carefully by anyone hoping to understand the consequences
for VAX applications involving high bandwidth input or output.
If we had data intensive applications
we would seriously consider the use of RP07 disks
(and interleaved memory controllers)
because of the resultant higher burst transfer rate;
this will be discussed further below.
.ds LH Components
