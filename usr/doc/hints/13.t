.sh "SYSTEM PACKAGES"
We now present some sample system packages.  Each represents
a balanced system for timesharing use under UNIX.  People often
ask us how many users can be supported UNIX in these configurations.
In the absence of specific information about applications to be run,
this is an unanswerable question.
The amount of load presented to
the system by different applications varies widely.
We mention with each system the count of interactive users typically
supported in our university research environment.
.LP
We first present systems based on 11/750s and then systems based on 11/780s.
With each example we suggest functionally similar systems configured
in at least two different ways: first with as much equipment as possible from
DEC and second with the best equipment known to us.  We will not
consider the VAX 11/730 as we believe it is not a viable option for
most timesharing environments.  Our experience with the 730 indicates it
has approximately the raw processing power of a PDP-11/34 size CPU.  Thus,
even though it is a reasonable choice for people looking for an
entry level VAX, we consider it mostly a single user machine.
.LP
Various measurements of the speed of the 11/750 and 11/780 indicate that
the 11/750 executes at roughly 60 percent of the speed of an
11/780.  By comparison, an 11/70 runs at roughly 75 percent of the speed
of an 11/780 using the same benchmarks, which involve no floating point,
no 32 bit arithmetic on the 11/70, and no system calls.
For UNIX time sharing usage we believe that the 11/750
has better performance than an 11/70. 
This is due mainly to additional tuning and performance
enhancements to the VAX kernel, and to the larger address
space of the VAX architecture.
.LP
The first system we consider is a small 11/750.
This is followed by an expansion of the 11/750 into
a larger system.
We are fond of the VAX 11/750 as it provides the most computational
power per unit cost of the three VAX implementations.
.LP
The second base system is a small 11/780.
We show how it can be built from a DEC RUA81/TU78 package system, and 
how to build it from mixed vendor equipment.
We then expand it in two increments.
.LP
The small systems we suggest start with a single disk and tape controller
and some memory.  For time-sharing applications we configure our
VAX systems allowing 256K bytes of memory for the kernel and
roughly an additional 100k bytes of memory per active user.*
.FS
* These numbers work reasonably well in an environment typical
of University work (course work,
paper preparation, debugging programs, developing
applications for microcomputers, etc.)
More demanding applications could require substantially more memory per user.
.FE
Memory is cheap, especially for the 11/780, so we don't skimp on it.
.LP
With more than a few users, it is critical that more than one disk
arm be present in the system.  Thus all but the smallest systems include
more than one disk.  As the active user count rises, having more than
one disk controller is also a good idea.  The large system packages
include two disk controllers.  For really
large and i/o intensive systems
we recommend
high bit density disk drives like the Fujitsu Eagle or
the RP07 drive from DEC as they
provide a higher transfer rate than the 1.25 Mbytes typical
of the remaining drives.
Using this transfer rate effectively
requires running with interleaved memory.
.LP
It is desirable on all UNIX systems to have at least 100MB of disk space
so that all the source for the system and all the standard programs
may be kept on line with some room for locally developed programs.
The amount of space required by user programs varies per installation;
we manage to run many of our instructional/research machines using about
300-600 megabytes of space actively, although slightly more than this would
be desirable.
.LP
Our large research machine runs with 1 Gigabyte of disk storage, with
2 disks on a UNIBUS and 2 disks on MASSBUS adapters.  The weakest point
in this system is that it has only a 45ips TE16 tape drive for backups.
For even the smallest systems, 45ips will soon seem slow.
We therefore recommend starting with a 125ips 1600bpi tape
drive.  As full 2400 foot tape reels hold only 30MB at 1600bpi,
large systems should consider including at least one tape drive
capable of writing 6250bpi tapes.
