.NH 3
/sys/vax
.PP
The following files are new in 4.2BSD:
.IP \fBcrt0.ex\fP 15
edit script for creating a profiled kernel
.IP \fBframe.h\fP 15
copied from /usr/include
.IP \fBin_cksum.c\fP 15
checksum routine for the DARPA Internet protocols
.IP \fBparam.h\fP 15
machine-dependent portion of /sys/h/param.h
.IP \fBpup_cksum.c\fP 15
checksum routine for PUP-I protocols
.IP \fBrsp.h\fP 15
protocol definitions for communicating with a TU58
.IP \fBsys_machdep.c\fP 15
machine-dependent portion of the ``sys_*'' files of /sys/sys
.IP \fBufs_machdep.c\fP 15
machine-dependent portion of the ``ufs_*'' files of /sys/sys
.IP \fBvm_machdep.c\fP 15
machine-dependent portion of the ``vm_*'' files of /sys/sys
.IP \fBvmparam.h\fP 15
machine-dependent portion of /sys/h/vmparam.h
.PP
The following files have been modified for 4.2BSD:
.IP \fBLocore.c\fP 15
includes new definitions for linting the network and ipc code
.IP \fBasm.sed\fP 15
now massages \fIinsque\fP, \fIremque\fP, and various routines
which do byte swapping into assembly language
.IP \fBautoconf.c\fP 15
handles MASSBUS drives which come on-line after the initial
autoconfiguration process; sizes and configures swap space
at boot time in addition to calculating the swap area allocation
parameters \fIdmtext\fP, \fIdmmax\fP, and \fIdmmin\fP (which
were manifest constants in 4.1BSD); calculates the disk partition
offset for system dumps at boot time to take into account variable
sized swap areas; now uses the per-driver array
of standard control status register addresses when probing
for devices on the UNIBUS; now allows MASSBUS tapes and disks
to be wildcarded across controllers
.IP \fBconf.c\fP 15
uses many ``local'' spaces for new and uncommon device drivers
.IP \fBgenassym.c\fP 15
generates several new definitions for use in locore.s
.IP \fBlocore.s\fP 15
includes code to vector software interrupts to protocol
processing modules; assembly language assist routines for
the console and UNIBUS TU58 cassette drives; a new routine,
\fIFastreclaim\fP is a fast coding of a major path through
the \fIpagein\fP routine; copyin and copyout
now handle greater than 64Kbyte data copies and return EFAULT
on failure; understands the new signal trampoline code; now contains
code for draining terminal multiplexor silos at clock time;
a bug where a the translation buffer was sometimes being improperly
flushed during a \fIresume\fP operation has been fixed
.IP \fBmachdep.c\fP 15
a bug which caused memory errors to not be reported on 11/750's
has been fixed; has new code for handling the new signals;
recovers from translation buffer parity fault
machine checks apparently caused by substandard memory chips
used in many 11/750's; includes optional code to pinpoint
bad memory chips on Trendata memory boards; the machine check routine
now calls the \fImemerr\fP routine to print out
the memory controller status registers in case the fault 
occurred because of a memory error
.IP \fBmem.c\fP 15
now has correct definitions to enable correctable 
memory error reporting on 11/750's: DEC documentation
incorrectly specifies use of the ICRD bit
.IP \fBpcb.h\fP 15
has changes related to the new signal trampoline code
.IP \fBswapgeneric.c\fP 15
supports more devices which can be used as a generic
root device; interacts with the new swap configuration
code to size the swap area properly when running a 
generic system; understands the special ``swap on root''
device syntax used when installing the system
.IP \fBtrap.c\fP 15
can be compiled with a SYSCALLTRACE define to allow system
calls to be traced when the variable \fIsyscalltrace\fP is
non-zero; 
.IP \fBtu.c\fP 15
includes (limited) support for the TU58 console cassette on the 11/750,
sufficient for use in single-user mode; supports the use of the MRSP ROM
on the 11/750.
