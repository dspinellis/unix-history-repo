.\" Copyright (c) 1983,1986 Regents of the University of California.
.\" All rights reserved.  The Berkeley software License Agreement
.\" specifies the terms and conditions for redistribution.
.\"
.\"	@(#)4.t	6.3 (Berkeley) 6/5/86
.\"
.nr H2 1
.\".ds RH "Address representation
.br
.ne 2i
.NH
\s+2Internal address representation\s0
.PP
Common to all portions of the system are two data structures.
These structures are used to represent
addresses and various data objects.
Addresses, internally are described by the \fIsockaddr\fP structure,
.DS
._f
struct sockaddr {
	short	sa_family;	/* data format identifier */
	char	sa_data[14];	/* address */
};
.DE
All addresses belong to one or more \fIaddress families\fP
which define their format and interpretation.
The \fIsa_family\fP field indicates the address family to which the address
belongs, and the \fIsa_data\fP field contains the actual data value.
The size of the data field, 14 bytes, was selected based on a study
of current address formats.*
Specific address formats use private structure definitions
that define the format of the data field.
The system interface supports larger address structures,
although address-family-independent support facilities, for example routing
and raw socket interfaces, provide only 14 bytes for address storage.
Protocols that do not use those facilities (e.g, the current Unix domain)
may use larger data areas.
.FS
* Later versions of the system may support variable length addresses.
.FE
