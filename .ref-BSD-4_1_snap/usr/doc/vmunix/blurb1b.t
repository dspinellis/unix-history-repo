.nr FM .5i
.ev 0
.nf
.LP
.nf
.ev 1
.ll +10
.ND
.ps 12
.ft B
UNIVERSITY OF CALIFORNIA, BERKELEY
.ft R 
.sp
.vs 7p
\s20\l'25_'\s6
.br
BERKELEY \  \(bu \  DAVIS \  \(bu \  IRVINE \  \(bu \ 
LOS ANGELES \  \(bu \  RIVERSIDE \  \(bu \  SAN DIEGO
\  \(bu \  SAN FRANCISCO \  \(bu \  SANTA BARBARA \  \(bu \  SANTA CRUZ
.br
\s20\l'25_'\s7
.nf
.vs 9p
COLLEGE OF ENGINEERING  \l'25 '                                         BERKELEY, CALIFORNIA 94720
DEPARTMENT OF ELECTRICAL ENGINEERING
  AND COMPUTER SCIENCES \l'30 ' \s10\*(DY\s0
COMPUTER SCIENCE DIVISION
.fi
.sp
.ev 0
.LP
.rs
.sp .75i
.tl '''April 30, 1982'
.LP
Dear Colleague:
.PP
We are happy to send you information about our June 1981 revision of the Fourth
Berkeley Software Distribution.  The Fourth Berkeley Software
Distribution is for VAX\(dg users with UNIX/32V\(dd or System III
.FS
\(dgVAX is a trademark of Digital Equipment Corporation
.FE
.FS
\(ddUNIX and UNIX/32V are trademarks of Bell Laboratories.
.FE
licenses. This distribution is called 4.1bsd and supersedes the
4.0bsd distribution made in November 1980.
.PP
The enclosed information is designed to serve two purposes.  The first
purpose is to acquaint you with the details of our distribution so you
can decide whether or not you would like to receive it.  The second
purpose is to tell you how to obtain our distribution.
.SH
What is 4.1bsd?
.PP
The distribution consists of two magnetic tapes plus printed
documentation.  The magnetic tapes contain copies of all binaries,
source code, and documentation.  The printed documentation contains a
complete manual set including both the Berkeley documentation and
material which is unchanged from the original Western Electric
distribution.  In addition, a high-quality one-sided typeset
duplication master is provided for Volume One of the UNIX Programmer's
Manual and for the sections of Volume Two which were added at
Berkeley.
.PP
Because we have a very limited capability for providing individual
information to the many sites which wish to run 4.1bsd, we are including
several documents which answer in great detail many of the
questions which we have been asked.  Please do not contact us with
questions until you have satisfied yourself that the answer you need is
not in one of these enclosed documents:
.RS
.IP \(bu
.B
Berkeley Software for UNIX on the VAX
.R
describes the basic differences between Western Electric's UNIX/32V and
Berkeley's 4.1bsd.  This document will help you decide whether or not
to try 4.1bsd.
.IP \(bu
.B
Bug Fixes and Changes in 4.1bsd
.R
describes the external differences between 4.1bsd and 4.0bsd.  This
document will help you decide whether or not to upgrade from 4.0bsd.
.IP \(bu
.B
Changes in the Kernel in 4.1bsd
.R
describes the internal differences between 4.1bsd and 4.0bsd.  This
document will help you plan for carrying any local changes you may
have made from 4.0bsd to 4.1bsd.
.IP \(bu
.B
Hints on Configuring VAX Systems for UNIX
.R
provides some basic information about hardware known to work with
4.1bsd.  This document will help you plan further hardware
acquisition.
.IP \(bu
.B
Installing and Operating 4.1bsd
.R
describes what is involved in running 4.1bsd.  This document will
help you plan your operations staffing.
.RE
.SH
How can I obtain 4.1bsd?
.PP
Before we can send the distribution to your company or institution we
must have all of the following:
.RS
.IP \(bu
A copy of the Software Agreement between your company or institution
and Western Electric (or AT&T) which authorizes you to have UNIX/32V
or System III.  Note that
this must be a complete copy of the agreement, not, for example, merely
the signature page or the cover page.
You do not need to document that any particular CPU is licensed
(since our agreement
allows you to use our software on all CPU's which you have properly
licensed with Western Electric or AT&T.)
.IP \(bu
Two copies of the License Agreement between your company or institution
and the Regents of the University of California executed on behalf of
your organization.  Two blank copies of this agreement are included
with this letter.  This agreement must be executed by an
individual who is authorized to sign such documents for your
organization.  Usually, this will be the same person who executed your
UNIX/32V or System III Software Agreement.
One of the copies will be returned to you after it
has been executed on behalf of the Regents.
.IP \(bu
A check for $600 U.S. payable to ``the Regents of the University of
California.''  We do not have staff for invoicing and collection.  We
cannot make a distribution without advance payment.  To ask us to do so
will slow down the processing of your request.  (We understand that
certain organizations are constitutionally unable to pay in advance and
we will try to make an exception if such a situation is well documented
in writing.)
.IP \(bu
A filled out copy of the enclosed Site Information Form.  This form
allows us to make sure you receive any special attention you may
require.
.RE
.PP
A checklist is included to aid you in assembling this material.  Send
the materials to:
.in +1i
.DS
Jeri Kotani, Distribution Coordinator
.br
Computer Systems Research Group
.br
Computer Science Division, EECS
.br
University of California
.br
Berkeley, CA  94720
.DE
.in -1i
.PP
If you are in a hurry, you should be aware that the biggest cause of
delay is incomplete material.  Many organizations have checks and legal
documents sent to us directly by the department which prepares them.
Such checks and documents have a tendency to be sent with partial
addresses and to end up on the desk of someone in a far corner of
the University who has no idea what to do with them.  If at all
possible, all of the material which we need to process your request
should be sent in a single package by the person who cares the most
about getting the distribution.
.SH
Special Cases
.PP
.B
Current Licensees.
.R
If your institution or company has already received one of the 4bsd
distributions (either 4.0bsd or 4.1bsd), then your organization is
already licensed for all revisions of 4bsd.  To obtain another copy of
our distribution, do not send a copy of your Software
Agreement with Western Electric or execute our License Agreement
again.  Instead, your request should be made in the form of a letter
including:
.in +.5i
``Please send the latest revision of the Fourth Berkeley Software Distribution
to us under the terms of our existing License Agreement with the
Regents of the University of California.  I warrant that I have the authority
to make this request.''
.in -.5i
This letter should be signed by the person who signed your original
License Agreement with us
(or his successor) and can request that the distribution be mailed
to some other person.
Send a check and a Site Information Form with the letter.
.bp
.PP
.B
RK07 Disk Pack Distributions.
.R
If you will be running our system on a VAX without a supported tape
drive, you can provide RK07 disk packs onto which we will copy the
system.  We can put enough of the system on two packs so that you will
have all of the basic system components.  If you want everything which
we send on the tapes, you will have to provide three packs.  If you
must get the system on RK07 disk packs, ship the packs to the above
address and note on your Site Information Form that you are doing so.
.PP
.B
Distribution within an Organization.
.R
If someone in your company or institution has already received our
distribution, our license allows you to get a copy of the system
directly from them to run on other CPU's which are properly licensed by
Western Electric.  If you do this, you must obtain any advice or
information which you need directly from them.  We are not willing to
be involved with second installations from a single distribution.
We recommend that installations under separate management
obtain their own copies of our distribution.
.PP
.B
Licensing for Companies with Divisions.
.R
Sometimes there is a question about whether two divisions of a company
should be covered by one License Agreement or two.  Judge this by
your Software Agreement with Western Electric:  If you have two
agreements with Western Electric, you must have two with us; if you
have one with them, you need only one with us.
.PP
.B
DARPA Sites.
.R
Part of the research which lead to 4.1bsd has been supported by the
Defense Advanced Research Projects Agency (DARPA.) Our contract with
DARPA includes a sum which prepays the distribution costs for a few
designated DARPA sites.  If you are one of these, send a note to that
effect instead of a check.
.PP
.B
University of California Sites.
.R
If you are a part of the University of California, do not execute the
License Agreement and do not send a copy of our Software Agreement with
Western Electric.  Instead, you must provide (1) a letter indicating
your site's intention to abide by the terms of our License Agreement,
(2) a Site Information Form, and (3) a check or IOC for $600.  Forward
these items to us through Katherine R. Delucchi, Assistant Materiel Manager,
Purchasing Department, 2405 Bowditch Street,
Berkeley, CA 94720.  Ms. Delucchi will verify the status
of your UNIX license for us.
.PP
If you have questions about the distribution or the licensing process,
contact Jeri Kotani at the above address, phone
her at (415) 642-7780, or send mail over the ARPANET to
csrg@berkeley.
.sp 2
.ti +2.5i
Sincerely yours,
.sp 3
.ti +2.5i
Professor Robert S. Fabry
.br
.ti +2.5i
Computer Systems Research Group
