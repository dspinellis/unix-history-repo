.\" Copyright (c) 1983 Regents of the University of California.
.\" All rights reserved.  The Berkeley software License Agreement
.\" specifies the terms and conditions for redistribution.
.\"
.\"	@(#)4.t	6.1 (Berkeley) %G%
.\"
.nr H2 1
.ds RH "Address representation
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
The \fIsa_family\fP field indicates which address family the address
belongs to, the \fIsa_data\fP field contains the actual data value.
The size of the data field, 14 bytes, was selected based on a study
of current address formats*.
.FS
* Later versions of the system support variable length addresses.
.FE
.ds RH "Memory management
.bp
