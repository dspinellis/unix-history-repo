.\"	@(#)2.t	1.1	(Copyright 1989 M. K. McKusick)	89/02/18
.NH
System Development
.PP
The first phase of each Berkeley system is its development.
We maintain a continuously evolving list of projects that we would
like to see integrated into the system.
Some additions are prompted by emerging ideas from the
research world such as the availability of new technology.
A typical project would be to incorporate support for the
new technology into \*(b3.
Other additions are suggested from the commercial world such
as the introduction of new standards like
.SM POSIX .
The resulting project would be to make the
.SM BSD
system compliant with the standard.
.PP
The projects are listed in order of importance.
Those at the top of the list are selected for inclusion
in the next release.
For each project, a determination is made of the best way to
get it done.
Usually there is at least a prototype available from
some group outside
.SM CSRG .
If possible, the prototype is obtained to use as a starting base
for integration into the
.SM BSD
system.
Only if the code is unavailable and if the project is
not too large is it developed in-house.
.PP
Unlike larger development groups, the staff of
.SM CSRG
specializes by projects rather than by particular levels
of the system.
Thus, a staff person will be responsible for all aspects of a project.
This responsibility starts at the associated kernel device drivers;
it proceeds up through the top of the kernel,
through the C library and system utility programs,
ending at the programs that the user interacts with directly.
Each staff person is also responsible for the documentation and manual pages.
Projects proceed in parallel,
interacting with other projects as their paths cross.
.PP
All source code, documentation, and auxiliary files are kept
under a source code control system.
During development,
the source code control system is critical to notifying people
when they are colliding with other changes.
Even more important however,
is the audit trail maintained by the source code control system that
is critical to the release engineering phase of the project
described in the next section.
.PP
As projects reach completion, new projects are started.
The development phase continues until
.SM CSRG
decides that it is appropriate to make a release.
The decision to halt development and switch to release mode
is driven by several factors.
The most important is that enough projects have been completed
to make the system decidedly superior to the previously released
version of the system.
For example,
\*(b3 was released primarily because of the need for
the improved networking capabilities and the markedly
improved system performance.
Secondly is the issue of timing.
If the releases are too infrequent, then
.SM CSRG
gets inundated with requests for interim releases.
Conversely,
if systems are released too frequently,
the integration cost for many vendors will be too high,
and they will tend to ignore the releases.
Finally,
the process of release engineering is long and tedious.
Frequent releases slows the rate of development and
causes undue tedium to the staff.
