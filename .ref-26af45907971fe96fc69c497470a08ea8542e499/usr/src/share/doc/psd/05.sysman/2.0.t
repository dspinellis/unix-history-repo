.\" Copyright (c) 1983 The Regents of the University of California.
.\" All rights reserved.
.\"
.\" %sccs.include.redist.roff%
.\"
.\"	@(#)2.0.t	6.3 (Berkeley) %G%
.\"
.ds ss 1
.sh "System facilities
This section discusses the system facilities that
are not considered part of the kernel.
.PP
The system abstractions described are:
.IP "Directory contexts
.br
A directory context is a position in the UNIX file system name
space.  Operations on files and other named objects in a file system are
always specified relative to such a context.
.IP "Files
.br
Files are used to store uninterpreted sequence of bytes on which
random access \fIreads\fP and \fIwrites\fP may occur.
Pages from files may also be mapped into process address space.\(dg
A directory may be read as a file.
.FS
\(dg Support for mapping files is not included in the 4.3 release.
.FE
.IP "Communications domains
.br
A communications domain represents
an interprocess communications environment, such as the communications
facilities of the UNIX system,
communications in the INTERNET, or the resource sharing protocols
and access rights of a resource sharing system on a local network.
.IP "Sockets
.br
A socket is an endpoint of communication and the focal
point for IPC in a communications domain.  Sockets may be created in pairs,
or given names and used to rendezvous with other sockets
in a communications domain, accepting connections from these
sockets or exchanging messages with them.  These operations model
a labeled or unlabeled communications graph, and can be used in a
wide variety of communications domains.  Sockets can have different
\fItypes\fP\| to provide different semantics of communication,
increasing the flexibility of the model.
.IP "Terminals and other devices
.br
Devices include
terminals, providing input editing and interrupt generation
and output flow control and editing, magnetic tapes,
disks and other peripherals.  They often support the generic
\fIread\fP and \fIwrite\fP operations as well as a number of \fIioctl\fP\|s.
.IP "Processes
.br
Process descriptors provide facilities for control and debugging of
other processes.
.ds ss 2
