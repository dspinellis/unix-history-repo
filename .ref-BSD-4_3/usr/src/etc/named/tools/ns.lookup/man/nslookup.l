.\" Copyright (c) 1986 Regents of the University of California.
.\" All rights reserved.  The Berkeley software License Agreement
.\" specifies the terms and conditions for redistribution.
.\"
.\"	@(#)nslookup.l	1.1 (Berkeley) 5/12/86
.\"
.TH NSLOOKUP 1 "May 12, 1986"
.UC 5
.SH NAME
nslookup \- query name servers interactively
.SH SYNOPSIS
.B nslookup
[
.I host-to-find
[
.I server address | server name
]] 
.SH DESCRIPTION
.IR Nslookup
with no arguments is an interactive program to query DARPA Internet
domain name servers. The user can contact servers to 
request information about a specific host or print
a list of hosts in the domain. 
.sp 1
.SH COMMANDS
Commands may be interrupted at any time by typing a control-C.
To exit, type a control-D (EOF).
The command line length must be less than 80 characters.
\fBN.B.\fP an unrecognized command will be interpreted as a host name.
.sp 1
.IP "host [server]"
Look up information for \fIhost\fP using the current default server
or using \fIserver\fP if it is specified.
.sp 1
.IP "\fBserver\fP \fIdomain\fP"
.ns
.IP "\fBlserver\fP \fIdomain\fP"
Change the default server to \fIdomain\fP. 
\fBLserver\fP uses the initial server to look up 
information about \fIdomain\fP while \fBserver\fP
uses the current default server. 
If an authoritative answer can't be found, the names of servers
that might have the answer are returned.
.sp 1
.IP \fBroot\fP
Changes the default server to the server for the root of the domain name space.
Currently, the host sri-nic.arpa is used.
(This command is a synonym for the \fBlserver sri-nic.arpa\fP.)
The name of the root server can be changed with the \fBset root\fP command.
.sp 1
.IP "\fBfinger\fP [\fIname\fP] [\fB>\fP \fIfilename\fP]"
.ns
.IP "\fBfinger\fP [\fIname\fP] [\fB>>\fP \fIfilename\fP]"
Connects with the finger server on the current host. 
The current host is defined when a previous lookup for a host
was successful and returned address information (see the 
\fBset querytype=A\fP command).
\fIName\fP is optional. 
\fB>\fP and \fB>>\fP can be used to redirect output in the
usual manner.
.sp 1
.IP "\fBls\fP \fIdomain\fP [\fB>\fP \fIfilename\fP]"
.ns
.IP "\fBls\fP \fIdomain\fP [\fB>>\fP \fIfilename\fP]"
.ns
.IP "\fBls -a\fP \fIdomain\fP [\fB>\fP \fIfilename\fP]"
.ns
.IP "\fBls -a\fP \fIdomain\fP [\fB>>\fP \fIfilename\fP]"
.ns
.IP "\fBls -h\fP \fIdomain\fP [\fB>\fP \fIfilename\fP]"
.ns
.IP "\fBls -h\fP \fIdomain\fP [\fB>>\fP \fIfilename\fP]"
List the information available for \fIdomain\fP. 
The default output contains host names and their Internet addresses. 
The \fB-a\fP option lists aliases of hosts in the domain.
The \fB-h\fP option lists CPU and operating system information for the domain.
When output is directed to a file, hash marks are printed for every
50 records received from the server.
.sp 1
.IP "\fBview\fP \fIfilename\fP"
Sorts and lists the output of the \fBls\fP command with \fImore\fP(1).
.sp 1
.IP "\fBhelp\fP"
.ns
.IP "\fB?\fP"
Prints a brief summary of commands.
.sp 1
.IP "\fBset\fP \fIkeyword\fP[=\fIvalue\fP]"
This command is used to change state information that affects the lookups.
Valid keywords are:
.RS
.IP "\fBall\fP"
Prints the current values of the various options to \fBset\fP.
Information about the  current default server and host is also printed.
.IP "\fB[no]debug\fP"
Turn debugging mode on. A lot more information is printed about the
packet sent to the server and the resulting answer.
.br
(Default = nodebug, abbreviation = [no]deb)
.IP "\fB[no]defname\fP"
Append the default domain name to every lookup.
.br
(Default = nodefname, abbreviation = [no]def)
.IP "\fBdomain=\fIname\fR"
Change the default domain name to \fIname\fP. 
The default domain name is appended to all lookup requests if 
the \fBdefname\fP option has been set.
.br
(Default = value in /etc/resolv.conf, abbreviation = do)
.IP "\fBquerytype=\fIvalue\fR"
Change the type of information returned from a query to one of:
.RS
.IP A 10
the host's Internet address (the default).
.IP CNAME  10
the canonical name for an alias. 
.IP HINFO 10
the host CPU and operating system type.
.IP MD 10
the mail destination.
.IP MX 10
the mail exchanger.
.IP MG     10
the mail group member.
.IP MINFO 10
the mailbox or mail list information.
.IP MR     10
the mail rename domain name.
.RE
Other types specified in the RFC883 document are valid but aren't
very useful.
.br
(Abbreviation = q)
.IP "\fB[no]recurse\fP"
Tell the name server to query other servers if it does not have the
information.
.br
(Default = recurse, abbreviation = [no]rec)
.IP \fBretry=\fInumber\fR
Set the number of retries to \fInumber\fP. 
When a reply to a request is not received within a certain 
amount of time (changed with \fBset timeout\fP), 
the request is resent. 
The retry value controls how many times a request is resent before giving up.
.br
(Default = 2, abbreviation = ret)
.IP \fBroot=\fIhost\fR
Change the name of the root server to \fIhost\fP. This
affects the \fBroot\fP command. 
.br
(Default = sri-nic.arpa, abbreviation = ro)
.IP \fBtimeout=\fInumber\fR
Change the time-out interval for waiting for a reply to \fInumber\fP seconds.
.br
(Default = 10 seconds, abbreviation = t)
.IP "\fB[no]vc\fP"
Always use a virtual circuit when sending requests to the server.
.br
(Default = novc, abbreviation = [no]v)
.RE
.SH TUTORIAL
The domain name space is tree-structured and currently has five top-level
domains: 
.IP \(bu 2
COM (for commercial establishments), 
.IP \(bu 2
EDU (for educational institutions), 
.IP \(bu 2
GOV (for government agencies) and 
.IP \(bu 2
ORG (for not for profit orginizations)
.IP \(bu 2
MIL (for MILNET hosts).
.PP
If you are looking for a specific host, 
you need to know something about the host's organization  
in order to determine the top-level domain it belongs to.
For instance, if you want to find the Internet address of a machine
at UCLA, do the following:
.IP a) 3
Connect with the root server using the \fBroot\fP command.
The root server of the name space has knowledge of the top-level domains.
.IP b) 3
Since UCLA is a university, its domain name is ucla.edu.
Connect with a server for the ucla.edu domain with the 
command \fBserver ucla.edu\fP.
The response will print
the names of hosts that act as servers for the domain ucla.edu.
Note that the root server does not have information about ucla.edu
but knows the names and addresses of hosts that do.
All future queries will be sent to the UCLA name server.
.IP c) 3
To request information about a particular host in the domain (e.g. locus),
just type the host name. 
To request a listing of hosts in the UCLA domain, use the \fBls\fP command.
The \fBls\fP command requires a domain name (in this case, ucla.edu) 
as an argument.
.PP
Note that if you are connected with a name server that handles
more than one domain, all lookups for host names must be fully specified 
with its domain.
For instance, the domain harvard.edu is served by seismo.css.gov, which
also services the css.gov and cornell.edu domains. A lookup request
for the host aiken in the harvard.edu domain must be specified as
aiken.harvard.edu. 
However, the \fBset domain=\fIname\fR and \fBset defname\fP
commands can be used to automatically append a domain name to each request.
.PP
After a successful lookup of a host, use the \fBfinger\fP command to
see who is on the system or to finger a specific person.
To get other information about the host, use the \fBset querytype=\fIvalue\fR
command to change the type of information desired and request another
lookup. 
(\fBFinger\fP requires the type to be A.)
.SH DIAGNOSTICS
If the lookup request was not successful, an error message is printed.
Possible errors are:
.IP "Time-out"
The server did not respond to a request after a certain amount of
time (changed with \fBset timeout=\fIvalue\fR) 
and a certain number of retries (changed with \fBset retry=\fIvalue\fR).
.IP "No information"
Depending on the query type set with the \fBset querytype\fP command,
no information about the host was available, though the host name is
valid.
.IP "Non-existent domain"
The host or domain name does not exist.
.IP "Connection refused"
.ns
.IP "Network is unreachable"
The connection to the name or finger server could not be made 
at the current time.
This error commonly occurs with \fBfinger\fP requests. 
.IP "Server failure"
The name server found an internal inconsistency in its database
and could not return a valid answer.
.IP "Refused"
The name server refused to service the request.
.sp 1
.PP
The following error should not occur and it indicates a bug in the program.
.IP "Format error"
The name server found that the request packet was not in the proper format.
.sp 1
.SH FILES
/etc/resolv.conf	initial domain name and name server addresses.
.SH SEE ALSO
resolver(3), resolver(5), named(8), RFC882, RFC883
.SH AUTHOR
Andrew Cherenson
