.\" Copyright (c) 1985 Regents of the University of California.
.\" All rights reserved.  The Berkeley software License Agreement
.\" specifies the terms and conditions for redistribution.
.\"
.\"	@(#)1.t	1.1 (Berkeley) %G%
.\"
.ds RH Introduction
.af PN 1
.bp 1
.NH
Introduction
.PP
The 4.2 Berkeley Software Distribution of 
.UX
for the VAX has added many new capabilities that were
previously unavailable under
.UX .
The development effort for 4.2BSD concentrated on providing new
facilities, and in getting them to work correctly.
Many new data structures were added to the system to support
these new capabilities.
In addition,
many of the existing data structures and algorithms
were put to new uses or their old functions placed under increased demand.
The limited development period left little time for tuning the completed system.
The effect of these changes was that
mechanisms that were well tuned under 4.1BSD
no longer provided adequate performance for 4.2BSD.
The increased user feedback that came with the release of
4.2BSD and a growing body of experience with the system
highlighted the performance shortcomings of 4.2BSD.
.PP
This paper details the work that we have done since
the release of 4.2BSD to measure the performance of the system,
detect the bottlenecks,
and find solutions to remedy them.
Most of our tuning has been in the context of the real
timesharing systems in our environment.
Rather than using simulated workloads,
we have sought to analyze our tuning efforts under
realistic conditions.
Much of the work has been done in the machine independent parts
of the system, hence these improvements could be applied to
other variants of UNIX with equal success.
.PP
The first part of this paper describes the tools and techniques
available for measuring system performance.
The second part describes the results of using these tools.
The third part of this paper describes the performance improvements
that have been made to the system based on our measurements.
The fourth part describes functional enhancements that have
been made.
The final part discusses some of the security problems that
have been addressed.
