.NH
Organizational changes
.PP
The directory organization and file names are very different
from 4.1BSD.
The new directory layout
breaks machine-specific and network-specific portions
of the system out into separate directories.  A new file,
\fImachine\fP is a symbolic link to a directory for the 
target machine, e.g. \fIvax\fP.  This allows a single set
of sources to be shared between multiple machine types
(by including header files as ``../machine/file'').
The directory naming conventions, as they relate to the
network support, are intended to allow expansion in supporting
multiple ``protocol families''.   The following directories
comprise the system sources for the VAX:
.DS
.TS
lw(1.0i) l.
/sys/h	machine independent include files
/sys/sys	machine independent system source files
/sys/conf	site configuration files and basic templates
/sys/net	network independent, but network related code
/sys/netinet	DARPA Internet code
/sys/netimp	IMP support code
/sys/netpup	PUP-1 support code
/sys/vax	VAX specific mainline code
/sys/vaxif	VAX network interface code
/sys/vaxmba	VAX MASSBUS device drivers and related code
/sys/vaxuba	VAX UNIBUS device drivers and related code
.TE
.DE
.PP
Files indicated as \fImachine independent\fP are shared among
4.2BSD systems running on the VAX and Motorola 68010.  Files
indicated as \fImachine dependent\fP are located in directories
indicative of the machine on which they are used; the 4.2BSD
release from Berkeley contains support only for the VAX.
Files marked \fInetwork independent\fP form the ``core'' of the
networking subsystem, and are shared among all network software;
the 4.2BSD release from Berkeley contains complete support only
for the DARPA Internet protocols IP, TCP, UDP, and ICMP.
