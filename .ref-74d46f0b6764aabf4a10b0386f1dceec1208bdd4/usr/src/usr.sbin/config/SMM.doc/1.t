.\" Copyright (c) 1983 The Regents of the University of California.
.\" All rights reserved.
.\"
.\" %sccs.include.redist.roff%
.\"
.\"	@(#)1.t	6.4 (Berkeley) %G%
.\"
.\".ds RH Introduction
.ne 2i
.sp 3
.NH
INTRODUCTION
.PP
.I Config
is a tool used in building 4.3BSD system images (the UNIX kernel).
It takes a file describing a system's tunable parameters and
hardware support, and generates a collection
of files which are then used to build a copy of UNIX appropriate
to that configuration.
.I Config
simplifies system maintenance by isolating system dependencies
in a single, easy to understand, file.
.PP
This document describes the content and 
format of system configuration
files and the rules which must be followed when creating 
these files.  Example configuration files are constructed
and discussed.
.PP
Later sections suggest guidelines to be used in modifying
system source and explain some of the inner workings of the
autoconfiguration process.  Appendix D summarizes the rules
used in calculating the most important system data structures
and indicates some inherent system data structure size
limitations (and how to go about modifying them).
