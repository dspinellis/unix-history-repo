.\" Copyright (c) 1983 Regents of the University of California.
.\" All rights reserved.  The Berkeley software License Agreement
.\" specifies the terms and conditions for redistribution.
.\"
.\"	@(#)8.t	5.1 (Berkeley) %G%
.\"
.nr H2 1
.ds RH "Protocol/protocol interface
.NH
\s+2Protocol/protocol interface\s0
.PP
The interface between protocol modules is through the \fIpr_usrreq\fP,
\fIpr_input\fP, \fIpr_output\fP, \fIpr_ctlinput\fP, and
\fIpr_ctloutput\fP routines.  The calling conventions for all
but the \fIpr_usrreq\fP routine are expected to be specific to
the protocol
modules and are not guaranteed to be consistent across protocol
families.  We
will examine the conventions used for some of the Internet
protocols in this section as an example.
.NH 2
pr_output
.PP
The Internet protocol UDP uses the convention,
.DS
error = udp_output(inp, m);
int error; struct inpcb *inp; struct mbuf *m;
.DE
where the \fIinp\fP, ``\fIin\fPternet
\fIp\fProtocol \fIc\fPontrol \fIb\fPlock'',
passed between modules conveys per connection state information, and
the mbuf chain contains the data to be sent.  UDP
performs consistency checks, appends its header, calculates a
checksum, etc. before passing the packet on to the IP module:
.DS
error = ip_output(m, opt, ro, allowbroadcast);
int error; struct mbuf *m, *opt; struct route *ro; int allowbroadcast;
.DE
.PP
The call to IP's output routine is more complicated than that for
UDP, as befits the additional work the IP module must do.
The \fIm\fP parameter is the data to be sent, and the \fIopt\fP
parameter is an optional list of IP options which should
be placed in the IP packet header.  The \fIro\fP parameter is
is used in making routing decisions (and passing them back to the
caller).  The
final parameter, \fIallowbroadcast\fP is a flag indicating if the
user is allowed to transmit a broadcast packet.  This may
be inconsequential if the underlying hardware does not support the
notion of broadcasting.
.PP
All output routines return 0 on success and a UNIX error number
if a failure occured which could be immediately detected
(no buffer space available, no route to destination, etc.).
.NH 2
pr_input
.PP
Both UDP and TCP use the following calling convention,
.DS
(void) (*protosw[].pr_input)(m);
struct mbuf *m;
.DE
Each mbuf list passed is a single packet to be processed by
the protocol module.
.PP
The IP input routine is a VAX software interrupt level routine,
and so is not called with any parameters.  It instead communicates
with network interfaces through a queue, \fIipintrq\fP, which is
identical in structure to the queues used by the network interfaces
for storing packets awaiting transmission.
.NH 2
pr_ctlinput
.PP
This routine is used to convey ``control'' information to a
protocol module (i.e. information which might be passed to the
user, but is not data).  This routine, and the \fIpr_ctloutput\fP
routine, have not been extensively developed, and thus suffer
from a ``clumsiness'' that can only be improved as more demands
are placed on it.
.PP
The common calling convention for this routine is,
.DS
(void) (*protosw[].pr_ctlinput)(req, info);
int req; caddr_t info;
.DE
The \fIreq\fP parameter is one of the following,
.DS
.if t .ta .6i 2.6i 3.1i
.if n .ta .84i 3.1i 3.80i
#define	PRC_IFDOWN	0	/* interface transition */
#define	PRC_ROUTEDEAD	1	/* select new route if possible */
#define	PRC_QUENCH	4	/* some said to slow down */
#define	PRC_HOSTDEAD	6	/* normally from IMP */
#define	PRC_HOSTUNREACH	7	/* ditto */
#define	PRC_UNREACH_NET	8	/* no route to network */
#define	PRC_UNREACH_HOST	9	/* no route to host */
#define	PRC_UNREACH_PROTOCOL	10	/* dst says bad protocol */
#define	PRC_UNREACH_PORT	11	/* bad port # */
#define	PRC_MSGSIZE	12	/* message size forced drop */
#define	PRC_REDIRECT_NET	13	/* net routing redirect */
#define	PRC_REDIRECT_HOST	14	/* host routing redirect */
#define	PRC_TIMXCEED_INTRANS	17	/* packet lifetime expired in transit */
#define	PRC_TIMXCEED_REASS	18	/* lifetime expired on reass q */
#define	PRC_PARAMPROB	19	/* header incorrect */
.DE
while the \fIinfo\fP parameter is a ``catchall'' value which
is request dependent.  Many of the requests have obviously been
derived from ICMP (the Internet Control Message Protocol),
and from error messages defined in the 1822 host/IMP convention
[BBN78].  Mapping tables exist to convert
control requests to UNIX error codes which are delivered
to a user.
.NH 2
pr_ctloutput
.PP
This routine is not currently used by any protocol modules.
.ds RH "Protocol/network-interface
.bp
