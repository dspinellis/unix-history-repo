.\" Copyright (c) 1992 The Regents of the University of California.
.\" All rights reserved.
.\"
.\" Redistribution and use in source and binary forms, with or without
.\" modification, are permitted provided that the following conditions
.\" are met:
.\" 1. Redistributions of source code must retain the above copyright
.\"    notice, this list of conditions and the following disclaimer.
.\" 2. Redistributions in binary form must reproduce the above copyright
.\"    notice, this list of conditions and the following disclaimer in the
.\"    documentation and/or other materials provided with the distribution.
.\" 3. All advertising materials mentioning features or use of this software
.\"    must display the following acknowledgement:
.\"	This product includes software developed by the University of
.\"	California, Berkeley and its contributors.
.\" 4. Neither the name of the University nor the names of its contributors
.\"    may be used to endorse or promote products derived from this software
.\"    without specific prior written permission.
.\"
.\" THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
.\" ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
.\" IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
.\" ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
.\" FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
.\" DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
.\" OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
.\" HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
.\" LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
.\" OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
.\" SUCH DAMAGE.
.\"
.\"	@(#)paper.ms	5.3 (Berkeley) 5/26/92
.\"
.\" use roff -ms
.ds CM
.TL
Berkeley UNIX
Yesterday, Today and Tomorrow
.AU
Keith Bostic
.AU
Marshall Kirk McKusick
.AU
Michael J. Karels
.AI
Computer Systems Research Group
Computer Science Division
Department of Electrical Engineering and Computer Science
University of California, Berkeley
Berkeley, California  94720
.AB
This paper presents a brief overview of the historic Berkeley releases
and the role that Berkeley has played in the UNIX\(dg
.FS
\(dgUNIX is a registered trademark of UNIX System Laboratories.
.FE
world and discusses the role that University and research releases
should play in the future.
.AE
.NH
A Technical History of the Berkeley Project
.PP
This is a \fBbrief\fP technical history of Berkeley UNIX.
For more details, [McKusick85] is strongly recommended along with the
other papers listed in the Reference section.
Specifically, we apologize that space does not permit mentioning most
of the major contributors and influences on the Berkeley system!
.PP
The University of California, Berkeley, first ran Bell Laboratories
Version 4 UNIX on a PDP-11/45, in January of 1974.
Later that spring, a PDP-11/40 was configured with the newly available
Version 5.
Version 6 was running on a PDP-11/70 by the fall of 1975,
with the arrival of two new graduate students, Bill Joy and Chuck Haley.
They initially concentrating on making improvements to a Pascal system
written by Ken Thompson while on sabbatical at Berkeley.
With this completed, they turned their attention to the \fBed\fP editor,
eventually producing a new version they called \fBex\fP.
By the end of the summer of 1976, Joy and Haley began to take an
interest in exploring the internals of the UNIX kernel.
.PP
In 1977, a steady stream of requests for the enhanced Pascal system
had begun.
Early that year, Joy put together the first ``Berkeley Software
Distribution'', including the Pascal system and the source to \fBex\fP.
Over the next year, about thirty copies of this distribution
were sent out.
At around the same time, Joy began work on what was to become \fBvi\fP.
By mid-1978, the software distribution clearly needed to be updated.
The Pascal system had been made markedly more robust through feedback
from its expanding user community,
and had been split into two passes so that it could be run on PDP-11/34s.
The result of the update was the ``Second Berkeley Software Distribution''
a name that was quickly abbreviated to 2BSD.
Along with the Pascal system,
\fBvi\fP and \fBtermcap\fP for several terminals were included.
Once again Bill Joy single-handedly put together distributions,
answered the phone, and incorporated user feedback into the system.
Over the next year nearly seventy-five tapes were shipped.
Although Joy soon moved on to other projects, the 2BSD distribution
continued to expand.
Today the latest version of this distribution, 2.10BSD is still alive
and being used by people all around the world on PDP-11's.
.PP
Early in 1978, Berkeley obtained a newly announced Digital
Equipment Corp. (DEC) VAX\(dd
.FS
\(ddVAX is a registered trademark of Digital Equipment Corporation.
.FE
11-780's.
Shortly after the arrival of the VAX, Bell Laboratories provided Berkeley
with a copy of their 32/V port of UNIX to the VAX.
Although 32/V supported a Version 7 UNIX environment on the VAX,
it did not take advantage of the virtual memory capability of the VAX
hardware.
Like its predecessors on the PDP-11, it was a swapping system.
Ozalp Babaoglu, a Berkeley graduate student, set about finding a way of
implementing a working set paging system on the VAX.
As Babaoglu neared the completion of his first attempt at an implementation,
he approached Bill Joy for some help in understanding the intricacies
of the UNIX kernel.
Intrigued by Babaoglu's approach, Joy joined in helping to integrate
the code into 32/V and then with the ensuing debugging.
.PP
Joy realized that the 32-bit VAX would soon make the 16-bit PDP-11
obsolete, and he began to port the 2BSD software to the VAX.
By the end of 1979, a complete distribution had been created.
This distribution included the virtual memory kernel,
the standard 32/V utilities, and the 2BSD additions, which by now
included the Pascal system, \fBex\fP/\fBvi\fP, the \fBCshell\fP and
several other utilities.
In December, 1979, Joy shipped the first of nearly a hundred copies of 3BSD,
the first VAX distribution from Berkeley.
.PP
In the fall of 1979, the Defense Advanced Research Projects Agency, DARPA,
accepted a Berkeley proposal to develop an enhanced version of 3BSD for
the DARPA community.
With the already good reputation of 3BSD supporting the proposal, the
Berkeley project was funded.
Joy took charge of the project, which was to become the Computer Systems
Research Group (CSRG).
Joy soon incorporated job control, added auto reboot, a 1K block file
system, and support for the latest VAX machine, the VAX-11/750.
By October, 1980, a polished distribution that also included the Franz
Lisp system and an enhanced mail handling system was released as 4BSD.
During its nine-month lifetime, nearly 150 copies were shipped.
.PP
With the increasingly wide distribution and visibility of Berkeley UNIX,
several critics began to emerge.
The major objection cited was the performance of various benchmarks as
compared to the DEC VMS system.
Over the course of several months, Joy systematically tuned the kernel,
soon matching VMS's performance.
Rather than continue shipping 4BSD, the tuned system, with the addition
of Robert Elz's auto configuration code, was released as 4.1BSD in June,
1981.
Over its two year lifetime about 400 distributions were shipped.
.PP
With the release of 4.1BSD, much of the debate over performance died
down.
DARPA again funded Berkeley, this time with the intention of adding
new features to the BSD system.
These new features eventually included Berkeley reliable signals,
the fast filesystem, disk quotas,
and the socket interface with TCP/IP networking support.
Around this time Joy left the CSRG for Sun Microsystems, and Sam Leffler
took over responsibility for completing the project.
In August, 1983, this system was released as 4.2BSD [Joy83].
The popularity of 4.2BSD was impressive; within eighteen months more
copies of 4.2BSD had been shipped than of all the previous Berkeley
software distributions combined.
.PP
As with 4BSD, the major criticism of 4.2BSD was performance.
The problem, not surprisingly, was that the new facilities had not been
tuned and that many of the kernel data structures were not well suited
to their new uses.
In addition, many of the interfaces, particularly in the networking
area had been left unfinished.
As Sam Leffler had left the CSRG for Lucasfilm, the tuning and enhancement
of 4.2BSD was largely done under the direction of Michael Karels and Kirk
McKusick [Leffler84] [McKusick85].
This system was released in April of 1986 as 4.3BSD, and had greatly
enhanced performance and reliability over 4.2BSD, along with several
new features.
.PP
As it had done in 4.2BSD, the CSRG then embarked on a new development
phase to update other major components of the system, and
design and integrate new functionality.
The 4.4BSD release, scheduled for fall of 1992, will contain the results
of several major new projects.
Among these projects are an OSI network protocol suite integrated
with existing ISO applications, an IEEE POSIX 1003.1 standard interface,
a highly tuned TCP/IP networking interface, support for Sun Microsystem's
Network File System, the integration of a log-structured file system,
an integration of the MACH virtual memory system, volume labels and
user-level database support.
.PP
There will have been four interim releases made by the CSRG between 4.3BSD
and the upcoming 4.4BSD release.
The first two of these releases, 4.3BSD-Tahoe and 4.3BSD-Reno were
intended to distribute a subset of the new functionality found in 4.4BSD
available to vendors.
The 4.3BSD-Tahoe release, made in the summer of 1988, was the first Berkeley
release to support two architectures.
This goal was made possible by the reimplementation of much of the machine
specific kernel source and a fundamental restructuring of the source code
pool so that binaries for more than one architecture could be constructed
from a single source pool.
The two supported architectures were the VAX and the Computer Consoles Inc.
Power 6/32 (the Tahoe).
Since this release, architecture support for the Intel 386/486, the
Sun Microsystems SPARCstation\(dg
.FS
\(dgAll SPARC trademarks are trademarks or registered trademarks
of SPARC International, Inc.
SPARCstation is licensed exclusively to Sun Microsystems, Inc.
.FE
I and II, the DECstation 3100 and 5000 and the Hewlett-Packard 300 have
been added as well.
The 4.3BSD-Reno release, made in the summer of 1990, was intended to make
the Network File System code available to vendors using Berkeley-derived
systems, such as The Open Software Foundation (OSF).
This code had been written by Rick Macklem at the University of Guelph
and integrated by the CSRG, under a new version of the kernel file
system switch.
.PP
Two other interim releases, the first and second release of the
``Berkeley Network Software Distribution'', usually abbreviated as NET/1
and NET/2, were intended to make the source code of the 4BSD system
available to and redistributable by anyone.
Over the years of development by the CSRG and others, an increasingly
larger percentage of the system was not derived from the original AT&T
32/V distribution.
In the spring of 1988 Berkeley made its first distribution not requiring
an AT&T source license, NET/1.
This distribution primarily contained the networking portions of the system,
from the utilities all the way through to the kernel device drivers, although
other items such as \fBlogin\fP and other files were included for various
reasons.
This release was extremely popular with many vendors with their own
versions of UNIX but who wished to run the Berkeley TCP/IP code and
with vendors wishing to create smart networking cards, not to mention
the users that wanted access to the source code for class work or other
research purposes.
.PP
Around this time, the CSRG also began to search out freely redistributable
versions of the UNIX utilities and to rewrite, or encourage BSD users to
rewrite, those that were not available elsewhere.
This was an immensely time-consuming task, involving contributions by
hundreds of programmers from all around the world.
In the summer of 1991, Berkeley released NET/2, which, like NET/1, did
not require an AT&T source license.
The NET/2 release included about 80% of the source code found in
the 4.3BSD-Reno release.
This release has proved to be immensely popular, with hundreds of thousands
of copies taken from the public network archives and an unknown number
redistributed by other organizations.
.NH
The Role of the Berkeley Project in the UNIX World
.PP
The role that Berkeley has played in the UNIX world has been a
constantly changing one.
In the 1970's, Berkeley was among the first participants in the UNIX
research community, acting as host to several researchers on sabbatical
from Bell Laboratories.
This cooperation typified the harmony that was characteristic of the
early UNIX community, as led by Bell Laboratories.
Work that was contributed to the Laboratories by different members of
the community, Berkeley among them, helped produce a rapidly expanding
set of tools and facilities.
With the commercialization of UNIX, the Bell Laboratories researchers were
no longer able to act as a clearinghouse for the ongoing UNIX research.
As the research community continued to modify the UNIX system, it found
that it needed an organization that could produce leading edge research
releases.
Because of its early involvement in UNIX and its history of releasing
UNIX-based tools, the CSRG quickly filled this role.
.PP
For the first half of the 1980's, Berkeley served as the focus of the
leading edge of UNIX research.
The Berkeley system was widely used, ported and considered the arbiter
of what should comprise a UNIX system.
By the mid-1980's, largely because the networking component of the Berkeley
system was unique and unavailable from vendors for a period of time,
Berkeley was forced into the role of a vendor [McKusick89].
This role expanded to the point that there were two major variants of
UNIX, System V and BSD, and resulted in a breach in the UNIX world that
is only gradually being healed.
Acting as a vendor required an immense amount of time, money and effort
by the CSRG.
Thousands of hours were devoted to release engineering, thousands more
to participation in the emerging UNIX standards and thousands more in
distribution and user support.
Over the years it became increasingly clear to the people associated with
the Berkeley UNIX project that its limited funding and manpower were
insufficient to complete its historical task of designing, implementing
and supporting a complete, reliable, leading edge system.
As each portion of the system became more complex and additional features
were added, more and more effort had to be expended to keep the
system at a high level of quality, and less and less effort was available
to move the system technically forward.
Fortunately, during the last half of the 1980's, as the UNIX interface
became the consensus choice for an industry standard, and the number of
vendors marketing, selling and supporting UNIX systems grew, Berkeley
has been able to start to return to its historical orientation of doing
leading-edge research instead of customer support.
.NH 
Berkeley UNIX Tomorrow
.PP
For UNIX to become the system of choice for a large segment of the industry,
potential customers must have confidence that the product is supported,
that future versions will continue to be developed and enhanced, and that
future versions will be upwardly compatible with all past applications.
In addition, vendors desiring to maximize their return on investment
require that the source code for their systems be proprietary and are
unwilling to make it available to users under any but the most onerous
restrictions.
Many of these changes, while acceptable for most users,
are diametrically opposed to what has made UNIX the research platform
of choice: low cost, wide availability of source code, and leading edge
technology.
.PP
System development can be likened to the process of evolution.
While gene mutation is critical to the advancement of the species, only
one in 100 mutations produces a useful feature; the rest result in
needless or detrimental changes.
The mere existence of an environment for mutation is not enough --
some organization must bear responsibility for
brutally pruning the weak, outdated and useless ideas.
UNIX was fortunate in this sense.
Unlike other projects beset by competing groups jealously guarding their
work from one another, UNIX thrived in an open and cooperative community
willing to channel its ideas through a central clearinghouse (first Bell
Laboratories and later the CSRG), in spite of the clearinghouse's
reputation for selective technical scrutiny.
.PP
Here one must distinguish between the selection process provided
by research and commercial organizations.
Research organizations can base pruning decisions strictly on the
coherence of the system and the technical merit of the idea.
They need not concern themselves with how changes might affect 
past variants of the system.
Commercial organizations, though, must ensure that
changes will not affect programs built to an obsolete interface.
For example, paging might be a great idea, but it will cause problems for
software that depends on the execution predictability of a swap-based
system, making it impossible for paging to replace swapping.
As a result, both schemes must be maintained, dramatically increasing the
complexity of the system.
As the system becomes more complex, its evolutionary paths will become
increasingly restricted.
.PP
Here the role of a dynamic research version of UNIX becomes clear.
While it is only directly used by a small group of people,
it provides an important role as the feedstock for the commercial 
versions of UNIX.
Over the long term,
it is reasonable to expect that the most useful functionality
of the research systems will be grafted into the commercial versions.
Examples of ideas that began with BSD and moved into commercial systems
include the fast filesystem, TCP/IP networking, and nearly half of
the commands and utilities.
.PP
The CSRG spends a significant amount of time collecting prototypes of
projects throughout the research world and molding them together into
a coherent and usable system.
Many of the ideas do not work out and are dropped in later releases.
The ability to experiment without concern for past applications is
critical.
The resulting system is a third the size and a fraction of the complexity
of its roughly equivalently functioned commercial brethren.
This lean and mean approach allows the system to evolve rapidly (the
nightmare of every commercial user, but the dream of every researcher).
A recent example of this type of experimentation is the prototyping of
various proposed POSIX utilities and interfaces by the CSRG.
When drafts of the standard were implemented, basic flaws in the
specification became apparent.
These flaws and suggested solutions were presented to the standards
committees, resulting in changes to the standard ensuring that the
ratified standard could be efficiently and correctly implemented.
The research system users also benefit from having a reference
implementation of the standard almost from the day that it is finalized.
.PP
Another major influence on the UNIX systems of the future will be the
NET/2 release.
At least three separate groups (two in the U.S. and one in Europe) have
added the necessary source code to the NET/2 release to make it a fully
functional UNIX system.
As the NET/2 release was not proprietary to any person or organization
other than the University of California and may be freely redistributed,
the cost of a UNIX system with source code will be less in the future
than in the current UNIX market by two orders of magnitude.
The UNIX single-server release by the Carnegie Mellon University
MACH group will also use the NET/2 release as a starting point, making
their release freely redistributable without a UNIX source license.
The advent of cheaply available sources will make it far easier than
ever before for research groups and users to develop and exchange software.
.PP
The role of designing and implementing a leading-edge research version
of UNIX is one that Berkeley is uniquely equipped to fill.
Future Berkeley releases will be oriented, as they were in the early days
of Berkeley UNIX, toward the development and integration of a few
well-chosen pieces of new research into a leading-edge system.
.NH
References:
.sp
.IP Joy83
.br
Joy, W., E. Cooper, R. Fabry, S. Leffler, M. McKusick, D. Mosher,
``4.2BSD System Manual,''
\fI4.2BSD UNIX Programmer's Manual\fP, Vol 2c, Document #68
August 1983.
.sp
.IP Leffler84
Leffler, S., M.K. McKusick, M. Karels,
``Measuring and Improving the Performance of 4.2BSD,''
\fIUsenix Conference Proceedings\fP, pp. 237-252, June, 1984.
.sp
.IP McKusick85
McKusick, M.K., M. Karels, S. Leffler,
``Performance Improvements and Functional Enhancements in 4.3BSD,''
\fIUsenix Conference Proceedings\fP, pp. 519-531, June, 1985.
.sp
.IP McKusick87
M. McKusick, M. Karels,
``Directions of UNIX at Berkeley'',
\fIDigest of Papers of the Thirty-second IEEE Computer Society
International Conference\fP,
Compcon, San Francisco, pp. 196-199, February 23-27, 1987.
.sp
.IP McKusick89
M. McKusick, M. Karels, K. Bostic,
``The Release Engineering of 4.3BSD'',
\fIProceedings of the New Orleans Usenix Workshop on Software Management\fP,
pp. 95-100, April 1989.
