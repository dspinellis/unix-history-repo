.\"     @(#)G.t	1.1     89/02/23
.LP
\fB\s+4G. Results from the Proposed Research\fP\s-4
.PP
Since the 4.3BSD release,
we have been working on four major new areas of research,
which we propose to continue:
.IP 1)
Integration of an OSI network protocol suite and
existing ISO applications into Berkeley UNIX.
Several protocol layers were originally implemented
at the University of Wisconsin.
The network architecture and system interface of 4.2BSD accommodates
multiple network protocol families and address formats,
although some extensions have been required to accommodate
certain protocol features.
Ongoing work includes development of those interfaces
and a more flexible kernel routing interface.
.IP 2)
Support for an interface compliant
with the P1003.1 POSIX standard recently approved by the IEEE.
Converting the kernel to be POSIX compliant requires
the completion of several new kernel facilities including
a new terminal driver and job control facilities.
.IP 3)
Refinement of the network architecture
and implementation to improve
its performance and functionality.
Since the most recent networking release,
additional performance experiments have been done by
Van Jacobson of the Lawrence Berkeley Laboratory;
the resulting performance improvements are ready
to be merged into the Berkeley TCP.
.IP 4)
Provision of a standard interface to file systems
so that multiple local and remote file systems can be supported,
much as multiple networking protocols are supported by 4.3BSD.
The proposal currently under development
adopts the 4.3BSD calling convention for file name lookup
but otherwise is closely related to Sun's VFS.
We ultimately hope to support a public domain implementation of Sun's NFS.
.bp
