.\"     @(#)G.t	1.2     89/02/23
.LP
\fB\s+4G. Proposed Research\fP\s-4
.PP
The recent invasion of the DARPA Internet by a quickly reproducing worm
highlighted the need for a thorough review of the access
safeguards built into the system.
We will conduct a complete audit of the system
utilities, especially network servers,
to check for and eliminate unintended system access mechanisms.
.PP
The current security model of 4.3BSD is based
on the model of privileged ports.
The model assumes that a reserved range of port identifiers
can only be allocated by trusted processes.
As workstations have replaced central timesharing computers,
it has become easy for users to attain an authorization
that allows them to improperly obtain a privileged port.
Once they obtain a privileged port,
they can impersonate other users on the network.
To eliminate the impersonation of other users on the network,
the proposal includes the addition and integration of a real
authentication mechanism for utilities such as
telnet, login, remote shell, and other network based services.
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
.bp
