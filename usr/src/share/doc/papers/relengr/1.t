.\"	@(#)1.t	1.1	(Copyright 1989 M. K. McKusick)	89/02/18
.NH
Introduction
.PP
.SM CSRG
has always been a small group of software developers.
This resource limitation requires careful software-engineering management.
Careful coordination is needed not only of
.SM CSRG
personnel, but also of members of the general community who
contribute to the development of the system.
.PP
Releases from Berkeley tend to alternate between
those that introduce major new facilities and those that introduce
bug fixes and efficiency improvements.
This alternation allows timely releases,
while providing for refinement and correction of the new facilities
and the elimination of performance problems produced by the new facilities.
The timely followup of releases that include new facilities
reflects the importance
.SM CSRG
places on providing a reliable and robust system on which its
user community can depend.
.PP
Developments from
.SM CSRG
are released in three steps: alpha, beta, and final.
Alpha and beta releases are not true distributions\(emthey
are test systems.
Alpha releases are normally available to only a few sites, most of those
within the University.
More sites get beta releases, but they do not get them directly;
a tree structure is imposed to allow bug reports, fixes, and new software
to be collected, evaluated, and checked for redundancies
by first-level sites before forwarding to
.SM CSRG .
For example,
\*(b3 beta ran at more than 100 sites, but there were only about
15 primary beta sites.
The beta-test tree allowed the developers at
.SM CSRG
to concentrate on actual development rather than sifting through details
from every beta-test site.
.PP
Many of the primary beta-test personnel not only had copies of the release
running on their own machines, but also had login accounts on the development
machine at Berkeley.
Such users were commonly found logged in at Berkeley over the
.SM ARPA
Internet, or sometimes via telephone dialup, from places far away,
such as Massachusetts, Utah, Maryland, Texas,
and Illinois, and from closer places, such as
Stanford.
For the \*(b3 release,
certain accounts and users had permission to modify the master copy of the
system source directly.
Several facilities, such as the
Fortran and C compilers,
as well as important system programs, such as
.NM telnet
and
.NM ftp ,
include significant contributions from people who did not work for
.SM CSRG .
One important exception to this approach was that changes to the kernel
were made by only
.SM CSRG
personnel, although the changes often were suggested by the larger community.
.PP
People given access to the master sources
were carefully screened beforehand, but were not
closely supervised.
Their work was checked at the end of the beta-test period by
.SM CSRG
personnel, who did a complete comparison of the source of the previous
release with the current master sources\(emfor example, of \*(b3
with 4.2\s-1BSD\s+1.
Facilities deemed inappropriate, such as new options to
the directory-listing command or a changed return value for the
.RN fseek
library routine, were removed from the source before final distribution.
.PP
.SM BSD
releases have usually included
a pair of documents detailing
changes to every user-level command
.[
Bug Fixes and Changes
.]
and to every kernel source file.
.[
Changes to the Kernel
.]
These documents are delivered with the final distribution.
A user can look up any command by name and see immediately
what has changed,
and a developer can similarly look up any kernel
file by name and get a summary of that file's changes.
.PP
This process illustrates an \fIadvantage\fP of having a few
principal developers:
The developers all know the whole system thoroughly enough
to be able to coordinate their own work with
that of other people to produce a coherent final system.
Companies with large development organizations find
this result difficult to duplicate.
This paper describes in more detail the process by which
the release engineering is managed.
