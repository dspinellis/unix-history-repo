.\"	@(#)3.t	1.1	(Copyright 1989 M. K. McKusick)	89/02/18
.NH
System Release
.PP
Once the decision has been made to finish development
and begin release engineering,
the currently unfinished projects are evaluated.
The evaluation involves figuring how much time the project
will require to complete,
and how important the outcome of the project is to
the upcoming release.
Projects that are not selected for completion are mothballed
so that they can be restarted in the future.
The remaining unfinished projects are
brought to an orderly completion.
.NH 2
The Alpha Distribution
.PP
The first step is to evaluate the existing state of the system
and to initially decide which software should be included in the release.
The decision process includes not only deciding what needs to be
added, but also what older software should be retired from the
distribution.
The new software includes the successful projects that have been
completed at
.SM CSRG
as well as an evaluation of the vast quantity of contributed
software that has been offered during the development period.
.PP
Once an initial list has been defined,
a prototype filesystem corresponding to the distribution
is constructed, typically called
.PN /nbsd .
This prototype will eventually turn into the master for the
final distribution.
During the period that the alpha distribution is being created,
.PN /nbsd
is mounted read/write, and is highly fluid.
Programs are created and deleted,
new versions are dropped in,
and the correspondence between the sources and binaries
is only loosely tracked.
People outside
.SM CSRG
that are helping with the distribution are free to
change their related parts of the distribution.
.PP
During this period the newly forming distribution is
checked for interoperability.
For example,
in \*(b3 the output of context differences from
.PN diff
was changed to merge overlapping sections.
Unfortunately, this change broke the
.PN patch
program that could no longer interpret the new diff output.
Since both changes had originated outside Berkeley,
.SM CSRG
had to coordinate the authors of these two programs
to get them to work together harmoniously.
.PP
Once we are satisfied with the state of the sources,
an attempt is made to build the entire source tree.
Often this turns up errors because of changed header files,
or use of obsoleted C library interfaces.
If the incompatibilities affect too many programs,
or require excessive amounts of change in the programs
that are affected,
the incompatibility is backed out, or a way to provide
backward compatibility is found.
The incompatibilities that are found and left in are noted
in a list that is later incorporated into the release notes.
Thus, users moving to the new system can anticipate problems
in their own software that will require change.
.PP
Once the source code completely compiles,
it is installed and becomes the running system that
.SM CSRG
uses on the main development machine.
Once in day-to-day use,
other interoperability problems become apparent,
and are resolved.
When a complete system can be built
and the known problems are resolved,
an alpha distribution tape is made
from the contents of
.PN /nbsd .
.PP
After the alpha tape is made,
the distribution filesystem is changed over to read-only.
Further changes are requested in a change log rather than
being made directly on the distribution itself.
The change requests are inspected and run by a
.SM CSRG
staff person, followed by a compilation of the affected
programs to ensure that they still build correctly.
Notably, once the alpha tape has been cut,
changes to the distribution are no longer made by people outside
.SM CSRG .
.PP
The alpha distribution is sent out to a select set of test sites.
These test sites are selected to have a
sophisticated user population that can not only find bugs,
but can also determine the cause and find a fix to the problem.
These sites are mostly composed of the groups
that are involved in contributing software to the distribution.
.NH 2
The Beta Distribution
.PP
As the alpha sites install and begin running the alpha tape,
they monitor the problems that they encounter.
For minor bugs, they typically report back the bug along with
a suggested fix.
Since many of the alpha sites are selected from among the people
working closely with
.SM CSRG ,
they often have accounts on, and access to, the primary
.SM CSRG
development machine.
Thus, they are able to directly install the fix themselves,
and simply notify us when they have fixed the problem.
After verifying the fix, the affected files are added to
the list to be updated on
.PN /nbsd .
.PP
The more important task of the beta sites is to test out the
new facilities that have been added to the system.
The alpha sites often manage to find major design flaws
or operational shortcomings of the facilities.
When such problems are found,
the person in charge of that facility is responsible
for resolving the problem.
Often this requires rearchitecting and reimplementing
parts of the affected facility.
For example,
in 4.2\s-1BSD\s+1,
the alpha release of the networking did not have connection queueing.
This shortcoming prevented the networking from handling many
connections to a single server.
The result was that the networking interface had to be
rearchitected to allow connection queueing.
.PP
The alpha sites are also responsible for ferreting out interoperability
problems with the different utilities.
The sites are selected with different types of user populations so
that the utilities will be exercised in ways that differ
from the ways that they are used at
.SM CSRG .
These differences in usage patterns turn up problems that
do not show up in our initial test environment.
.PP
The alpha sites frequently redistribute the alpha tape to several
of their own alpha sites.
These additional sites are responsible for reporting
problems back to the site from which they received the distribution,
not to
.SM CSRG .
Often these redistribution sites are less sophisticated than the
direct alpha sites, so their reports need to be filtered
to avoid spurious and voluminous reports.
The direct alpha sites sift through the reports to find those that
are relevant, and usually verify the suggested fix if one is given,
or develop a fix if none is provided.
The tree structured testing process ensures that the small group
of people at
.SM CSRG
are not swamped by an overwhelming influx of reports.
Instead, they can concentrate on tracking the changes being made
to the system to ensure as high a level of consistency as possible.
.PP
Once the major problems have been attended to,
the focus turns to getting the documentation synchronized
with the code that is being shipped.
The manual pages need to be checked to be sure that
they accurately reflect the changes to the programs that
they describe.
Usually the manual pages are kept up to date as
the program they describe evolves.
However, the supporting documents frequently do not get changed,
and must be editted to bring them up to date.
During this review, the need for other documents becomes evident.
For example,
during this phase of \*(b3 we decided that it would be desirable
to have a tutorial document on how to use the socket
interprocess communication primitives.
.PP
A final task during this period is to contact the people that
have contributed complete software packages
(such as
.PN RCS
or
.PN MH )
in the past to see if they wish to
make any revisions to their software.
For those that do,
the new software has to be obtained,
and checked out to verify that it compiles and runs
correctly on the system to be released.
Again, this integration and testing can often be done by the
contributors themselves by logging into the master machine
across the network.
.PP
After the stream of bug reports has slowed down
to a reasonable level,
we begin a carefully review of all the changes to the
system since the previous release.
The review is done by running a recursive
.PN diff
of the entire source tree.
All the changes are checked to ensure that they are reasonable,
and have been properly documented.
The process often turns up questionable changes.
When such a questionable change is found,
the source code control system log is examined to find
out who made the change and what their explanation was
for the change.
If the log does not resolve the problem,
the person responsible for the change is asked to explain
what they were trying to accomplish.
If the reason is not compelling,
the change is backed out.
Although this process is long and tedious,
it forces us to get a picture of the entire set of
changes to the system into out head.
This exercise often turns up inconsistencies that would
otherwise never be found.
.PP
Having completed the review of the entire system,
we begin the preparation of the beta distribution.
Unlike the alpha distribution, where pieces of the system
may be unfinished and the documentation incomplete,
the beta distribution is put together as if it were
going to be the final distribution.
All known problems are fixed, and any remaining development
is completed.
Once the beta tape has been prepared,
no further changes are permitted to
.PN /nbsd
without careful review as spurious changes made after the system has been
.PN diff 'ed
are unlikely to be caught.
.NH 2
The Final Distribution
.PP
The beta distribution goes to a larger set of sites than the
alpha distribution for two main reasons.
First, since it closer to the final release more sites are willing
to run it in a production environment without fear of catastrophic failures.
Second, more commercial sites with
.SM BSD
derived systems are interested in getting a preview of the
upcoming changes in preparation for merging them into their
own systems.
Because the beta tape has fewer problems,
it is beneficial to cast a wider net in the hopes of
finding as many of the remaining problems as possible.
Also, by handing the system out to less sophisticated sites,
issues that would be ignored by the users of the alpha sites
will be brought to our attention.
.PP
The anticipation is that the beta tape will not require
extensive changes to either the programs or the documentation.
Most of the work involves sifting through the reported bugs
to find those that are relevant and devising the minimal
reasonable set of changes to fix them.
After throughly testing the fix, it is listed in the update log for
.PN /nbsd .
One person's responsibility is to do the update of
.PN /nbsd
and to ensure that everything affected by the change is rebuilt and tested.
Thus, a change to a C library routine requires that the entire
system be rebuilt.
.PP
During this period the documentation is all printed out and proofread.
As minor changes are made to the manual pages and documentation,
the affected pages must be reprinted.
.PP
The final step in the release process is to preen the distribution tree
to ensure that it is in a consistent state.
This preening includes checking that every file and directory
on the distribution has the proper owner, group, and modes.
All source files must be checked to be sure that they have
appropriate source code control system headers.
Any unwanted error or temporary files must be removed.
Finally we have to ensure that the installed binaries correspond
exactly to the sources and libraries that are on the distribution.
.PP
This is a formidable task given that there are over 20,000 files on
a typical distribution.
Much of the checking can be done by a set of programs set to scan
over the distribution tree.
Unfortunately the exception list is long, and requires
hours of tedious hand checking.
.PP
Once the final set of checks has been run,
the master tape can be made, and the official distribution started.
As for the staff of
.SM CSRG ,
we usually take a brief vacation before plunging back into
a new development phase, doing all the things that we have
been unable to change during the release engineering period.
