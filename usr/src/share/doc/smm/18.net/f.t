.\" Copyright (c) 1983 Regents of the University of California.
.\" All rights reserved.  The Berkeley software License Agreement
.\" specifies the terms and conditions for redistribution.
.\"
.\"	@(#)f.t	5.1 (Berkeley) %G%
.\"
.nr H2 1
.ds RH Acknowledgements
.SH
\s+2Acknowledgements\s0
.PP
The internal structure of the system is patterned
after the Xerox PUP architecture [Boggs79], while in certain
places the Internet
protocol family has had a great deal of influence in the design.
The use of software interrupts for process invocation
is based on similar facilities found in
the VMS operating system.
Many of the
ideas related to protocol modularity, memory management, and network
interfaces are based on Rob Gurwitz's TCP/IP implementation for the 
4.1BSD version of UNIX on the VAX [Gurwitz81].
Greg Chesson explained his use of trailer encapsulations in Datakit,
instigating their use in our system.
.ds RH References
.nr H2 1
.sp 2
.SH
\s+2References\s0
.LP
.IP [Boggs79] 20
Boggs, D. R., J. F. Shoch, E. A. Taft, and R. M. Metcalfe;
\fIPUP: An Internetwork Architecture\fP.  Report CSL-79-10.
XEROX Palo Alto Research Center, July 1979.
.IP [BBN78] 20
Bolt Beranek and Newman;
\fISpecification for the Interconnection of Host and IMP\fP.
BBN Technical Report 1822.  May 1978.
.IP [Cerf78] 20
Cerf, V. G.;  The Catenet Model for Internetworking.
Internet Working Group, IEN 48.  July 1978.
.IP [Clark82] 20
Clark, D. D.;  Window and Acknowledgement Strategy in TCP. 
Internet Working Group, IEN Draft Clark-2.  March 1982.
.IP [DEC80] 20
Digital Equipment Corporation;  \fIDECnet DIGITAL Network
Architecture \- General Description\fP.  Order No.
AA-K179A-TK.  October 1980.
.IP [Gurwitz81] 20
Gurwitz, R. F.;  VAX-UNIX Networking Support Project \- Implementation
Description.  Internetwork Working Group, IEN 168.
January 1981.
.IP [ISO81] 20
International Organization for Standardization.
\fIISO Open Systems Interconnection \- Basic Reference Model\fP.
ISO/TC 97/SC 16 N 719.  August 1981.
.IP [Joy82a] 20
Joy, W.; Cooper, E.; Fabry, R.; Leffler, S.; and McKusick, M.;
\fI4.2BSD System Manual\fP.  Computer Systems Research Group,
Technical Report 5.  University of California, Berkeley.  Draft
of September 1, 1982.
.IP [Postel79] 20
Postel, J., ed.  \fIDOD Standard User Datagram Protocol\fP.
Internet Working Group, IEN 88.  May 1979.
.IP [Postel80a] 20
Postel, J., ed.  \fIDOD Standard Internet Protocol\fP.
Internet Working Group, IEN 128.  January 1980.
.IP [Postel80b] 20
Postel, J., ed.  \fIDOD Standard Transmission Control Protocol\fP.
Internet Working Group, IEN 129.  January 1980.
.IP [Xerox81] 20
Xerox Corporation.  \fIInternet Transport Protocols\fP. 
Xerox System Integration Standard 028112.  December 1981.
.IP [Zimmermann80] 20
Zimmermann, H.  OSI Reference Model \- The ISO Model of
Architecture for Open Systems Interconnection.
IEEE Transactions on Communications.  Com-28(4); 425-432.
April 1980.
