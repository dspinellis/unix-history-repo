.\"     @(#)D.t	1.2     89/02/24
.LP
\fB\s+4D. Milestones\fP\s-4
.PP
The security work described above has already been started informally,
and will be conducted more thoroughly at the start of the period covered
by this proposal.
.IP "   \(bu
When the Internet Host Requirements standard is finalized,
the necessary changes will be implemented and incorporated into the system.
The security audit will proceed in parallel.
The Telnet, FTP, SMTP and name servers will be completed within three months
of finalization of the standard.
.IP "   \(bu
The security audit of the login, remote login, remote process and printer 
servers will commence at the beginning of the project.
We will distribute the changes to both sets of servers electronically
as they are completed, with immediate distribution
of fixes for serious problems.
.IP "   \(bu
Network- and transport-level changes must be made in the kernel protocol
implementations.
In particular, support for IP type-of-service options and routing needs
to be added, and recent proposals for gateway monitoring should be implemented
and tested.
This work will be done in the third quarter.
.IP "   \(bu
We will also incorporate all secuirty and Host Requirement
changes into the following releases as described in section C.
.IP "   \(bu
Once an authentication mechanism has been identified,
it will be integrated into the system and incorporated
into the final release.
.bp
