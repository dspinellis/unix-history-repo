.NH 2
/sys/conf
.PP
This directory contains files used in configuring systems.
The format of configuration files has changed slightly;
it is described completely in a new document
``Building 4.2BSD UNIX Systems with Config''.
Several new files exist for use by the \fIconfig\fP\|(8)
program, and several old files have had their meaning changed
slightly.
.IP \fBLINT\fP 15
a new configuration file for use in linting kernels
.IP \fBdevices.vax\fP 15
maps block device names to major device numbers (on the VAX)
.IP \fBfiles\fP 15
now has only files containing machine-independent code
.IP \fBfiles\fP.\fIxxx\fP 15
(where \fIxxx\fP is a system name)
optional, \fIxxx\fP-specific \fIfiles\fP files
.IP \fBfiles.vax\fP 15
new file describing files which contain machine-dependent code
.IP \fBmakefile.vax\fP 15
makefile template specific to the VAX
.IP \fBparam.c\fP 15
updated calculations of \fIntext\fP and \fInfile\fP
to reflect network requirements;  new quantities
added for disk quotas
