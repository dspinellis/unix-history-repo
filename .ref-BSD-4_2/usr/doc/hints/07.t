.sh TAPES
We use Emulex TC-11/P UNIBUS tape controllers
and Kennedy model 9300-3 800/1600 BPI 125 IPS transports.
Cipher tape drives and Wesperco controllers are also widely used.
When purchasing second vendor equipment, one will also need cables and
a rack in which to mount the tape drive.
The Kennedy transport comes with a 15 month factory warranty. Our distributor
exchanges/repairs the cards in the controllers based on a local diagnostic
mode in the transport.  After the warranty period, card
swaps cost about $75.  For transport mechanical failures the transport
is returned to the factory in Monrovia, California, or we fix it
ourselves.
.LP
George Goble at Purdue is using a 6250 tape system with UNIX.
It includes a Telex 6253 drive (800/1600/6250 BPI) 125 IPS
with a TELEX Formatter and an Aviv 1 board UNIBUS
interface. The UNIBUS interface has 4KB of buffering,
to help with bus latency problems, and it really
appears to be necessary. The whole system cost him
about three times what our 1600 bpi systems cost.
The Aviv controller emulates
a TU10 which is similar to the Emulex NRZ/PE controller.
When heavy data transfer is done to the drive at 6250 bpi
it uses the entire bandwidth of the UBA.
This forces UNIBUS access through the UBA
to be arbitrated by the operating system in order that
the tape drive and a disk 
controller may coexist on the same UBA.
\fBN.B.\fP: The driver for this controller/transport combination
is not currently included in the standard 4BSD system but is
trivially cloned from the TM11 handler which is a standard
part of the distribution.
Aviv also has a TM-11 compatible controller, the TFC 822,
which supports both Kennedy and Cipher transports.
This controller has more internal buffering than the
Emulex TM-11 emulator and may be preferable for this reason.
.DS
.TS
l l l.
Name	Speed	Densities
_
Kennedy	125ips	800/1600
Telex	125ips	800/1600/6250
.TE
.DE
.LP
Our original VAX system came in a package with a DEC TE16 on its own MBA.
The TE16 is reliable but slow.
The DEC TU45 is faster, but fraught with problems as the high maintenance
cost reflects.  The DEC TU77 is a good transport, but the auto-loading
features do not seem to work well, and it is expensive.
Finally, there is a relatively
new product from DEC, a 1600/6250bpi 125ips
tape drive, the TU78.  This is the same transport as the TU77.
We have two TU78s in use on campus with mixed results.
.LP
The UNIBUS tape drive, the
TS11, is included in packages for the 11/750 except for the RK07
package system.  It does not have a vacuum column, and is thus
hard on tapes.
It is a problem to load and has been found to be unreliable.
.DS
.TS
l r l l.
Name	Speed	Densities
_
TS11	45ips	1600	(Not recommended)
TE16	45ips	800/1600
TU45	75ips	800/1600	(Not recommended)
TU77	125ips	800/1600
TU78	125ips	1600/6250
.TE
.DE
