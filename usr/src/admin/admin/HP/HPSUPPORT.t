.TL
HP9000/300 series support in the 4.3 Reno distribution
.AU
Mike Hibler (mike@cs.utah.edu)
.AI
University of Utah
.LP
.B
Due to various considerations, the 4.3 Reno release does not include a
bootable distribution for the HP9000 series 300 CPUs.
However, we believe that it does include all the necessary sources to
construct a fully-functional BSD system given a suitable cross-compilation
environment to bootstrap in.
.LP
Hardware supported:
.TS
lw(1i)	lw(4i).
SPUs	T{
318, 319, 320, 330, 340, 345, 350, 360, 370, 375
(all 68020 and 68030 based SPUs except 332).
T}

DISKs	T{
All HP SCSI drives (including 650/A magneto-optical) on 98265A and 98658A
interfaces.
A variety of old and new CS80 drives
(7912, 7914, 7933, 7936, 7937, 7945, 7957A/B, 7958A/B, 7959A/B)
on 98262A, 98624A and 98625A/B interfaces.
T}

TAPEs	T{
9144 and 9145 CS80 cartridge drives.
T}

SERIAL	T{
98644 built-in single port and 98642 4-port.
T}

LAN	T{
98643 internal and external.
T}

DISPLAY	T{
98544-98547 "topcat",
98548-98550 "catseye",
98700/98710 "gatorbox",
98720/98721 "Renaissance",
98730/98731 "DaVinci".
T}
.TE
.LP
The kernel is completely compatible at the system call level with the VAX
or Tahoe equivalent.
However, not all kernel facilities have been tested on an HP,
for example, the ISO protocol family support.
.LP
One significant feature of the HP implementation is emulation for most
HP-UX system calls (as of release 6.5).
This means that many HP-UX binaries and third party applications will
run unchanged.
This is particularly useful for running Starbase graphics applications.
.LP
GCC 1.36 is included as the standard compiler for the kernel,
utilities and all user applications.
The GNU assembler (GAS) and debugger (GDB) are also standard tools.
There is currently no FORTRAN, Pascal or Lisp support,
though the native HP-UX compilers will run under BSD.
.LP
The HP300 series support is derived from the 4.3bsd port distributed by
the University of Utah though it is not completely backward compatible
with it or with the related More/BSD release from Mt. Xinu.
.LP
We would like to thank all those who contributed to the evolution of BSD
on the HP300.
These include:
University of California at Berkeley,
University of Colorado,
University of Oregon,
Oregon State University,
University of Wisconsin,
University College London,
Victoria University of Wellington,
Lawrence Berkeley Labs,
Free Software Foundation,
Mt Xinu, HP Labs
and last but not least the HP Research Grants Program for
their generous support.
