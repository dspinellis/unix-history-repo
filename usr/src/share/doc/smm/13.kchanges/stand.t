.\" Copyright (c) 1986 Regents of the University of California.
.\" All rights reserved.  The Berkeley software License Agreement
.\" specifies the terms and conditions for redistribution.
.\"
.\"	@(#)stand.t	1.4 (Berkeley) 4/11/86
.\"
.NH
Bootstrap and standalone utilities
.PP
The standalone routines in \fB/sys/stand\fP and \fB/sys/mdec\fP
have received some work.
The bootstrap code is now capable of booting from drives other than drive 0.
The device type passed from level to level during the bootstrap operation
now encodes the device type, partition number, unit number, and MASSBUS
or UNIBUS adaptor number (one byte for each field,
from least significant to most significant).
The bootstrap is much faster, as the standalone \fIread\fP operation
uses raw I/O when possible.
.PP
The formatter has been much improved.
It deals with skip-sector devices correctly; the previous version
tested for skip-sector capability incorrectly, and thus never dealt with it.
The formatter is capable of formatting sections of the disk, track by track,
and can run a variable number of passes.
The error retry logic in the standalone disk drivers was corrected
and parameterized so that the formatter may disable most corrections.
