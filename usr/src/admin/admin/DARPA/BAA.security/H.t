.\"     @(#)H.t	1.1     89/02/23
.LP
\fB\s+4H. Technical Rationale\fP\s-4
.PP
The following sections provide
an expanded description of each of these projects
described in the previous section.
.sp
.LP
\fB\s+2H.1 System Security\fP\s-2
.PP
The recent invasion of the ARPANET by a quickly reproducing worm
highlighted the need for a thorough review of the access
safeguards built into the system.
Until now we have taken a passive approach to dealing with
weaknesses in the system access mechanisms, rather than actively
searching for possible weaknesses.
When we are notified of a problem or loophole in a system utility
by one of our users,
we have a well defined procedure for fixing the problem and 
expeditiously disseminating the fix to the BSD mailing list.
This procedure has proven itself to be effective in
solving known problems as they arise
(witness its success in handling the recent worm).
However, we feel that it would be useful to take a more active
role in identifying problems before they are reported (or exploited).
We will make a complete audit of the system
utilities and network servers to find unintended system access mechanisms.
Once identified, we expect that they can be corrected
through the existing mechanism.
.PP
As a part of the work to make the system more resistant to attack
from local users or via the network, it will be necessary to produce
additional documentation on the configuration and operation of the system.
This documentation will cover such topics as file and directory ownership
and access, network and server configuration,
and control of privileged operations such as file system backups.
.PP
In addition, we have been discussing with DARPA possible plans for response
to emergencies such as the recent worm attack.
We believe that a plan such as the proposed Computer Emergency Response
Team (CERT) can respond to such attacks effectively, and intend to cooperate
with such a plan.
We can provide at least two members for a response team
and can serve as the primary coordinators for problems involving BSD
UNIX and networking software.
.PP
A group within the Internet Engineering Task Force has been drafting
a Host Requirements standard for Internet hosts.
We reviewed this draft recently and joined the working group.
For the most part, the existing Berkeley TCP/IP and networking application
software conforms with the draft.
We will review conformance with the Host Requirements standard
at the same time that we review the robustness of the network software.
We expect that some additions will have to be made
to the kernel network protocols to make them compliant.
In particular, support for IP type-of-service options and routing needs
to be added, and recent proposals for gateway monitoring should be implemented
and tested.
.PP
A major shortcoming of the present system is that authentication
over the network is based solely on the privileged port mechanism
between trusting hosts and users.
Although privileged ports can only be created by processes running as root,
such processes are easy for a workstation user to obtain;
they simply reboot their workstation in single user mode.
Thus, a real authentication mechanism is needed.
At present, we believe that the MIT Kerberos authentication
server provides the best solution to this problem.
We propose to investigate Kerberos further as well as other
authentication mechanisms and then to integrate
the best one into Berkeley UNIX.
Part of this integration would be the addition of the
authentication mechanism into utilities such as
telnet, login, remote shell, etc.
MIT currently supports the use of Kerberos for authentication
of rlogin, rsh, certain other servers used at MIT, and the NFS mount
protocol.
If we choose Kerberos,
we will add support for telnet (eventually replacing rlogin),
the X window system, and the mail system within an authentication
domain (a Kerberos \fIrealm\fP).
We would also need to add support for transparent third-party operations.
(The existing Kerberos software allows a user to connect to a server
on another system without supplying a password,
but does not allow processes started on the server to access the network
without sending another password over the network.)
We hope to replace the existing password authentication on each host
with the network authentication system.
.PP
Another subject of active work, at MIT and elsewhere, is the interaction
between servers for different authentication realms.
Currently, the servers for two Kerberos realms
may be mutually authenticated only by realm administrators
with a manual exchange of keys.
We would like to add a mechanism for a user in two realms to request that
a equivalence be established
between his identities in the two realms.
Such a facility could greatly reduce the need to transmit passwords
in the clear over wide-area networks.
.bp
