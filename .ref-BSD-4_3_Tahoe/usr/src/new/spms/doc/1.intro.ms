.PP
.nr PS 12
.NH
Introduction
.nr PS 10
.XS
\*(SN Introduction
.XE
.PP
Software packages on the
.UX
operating system are frequently organized in a haphazard manner. The
conventions for arranging parts of the package within the file system vary
from package to package, and absolute pathnames are often used to describe
the location of files. The net result is an amorphous non-portable
software package requiring a substantial maintenance effort.
.PP
It would seem extremely desirable to develop tools and file system
structures to support more coherent and portable software packages, and
reduce the maintenance effort associated with them. The Software
Project Management System (SPMS) is a system for the management of
medium- to large-scale software systems. SPMS provides, within the
.UX
environment\|[5], a number of commands which can greatly simplify many tasks
associated with program development and maintenance. SPMS does not
attempt to duplicate existing
.UX
program development tools such as
.I make
or
.I SCCS,
but instead provides a way of coordinating these tools.
.PP
If only the simpler commands are used, the SPMS system can be helpful
for inexperienced
.UX
users. If the more advanced SPMS features are used, the experienced
user can perform complex tasks with less effort and greater
reliability than by applying the standard
.UX
tools directly.
.PP
Each software package managed by SPMS is organized as a project\|[7,8]. A
project is a collection of directories, each of which supports a
specific activity such as program development, testing, or
documentation (see fig. 1). There is no restriction on the number
of directories belonging to a project. The directory layout is arbitrary,
and can be altered to reflect the changing needs of the package.
.KF
.sp 33
.SM
.ce
\fIFigure 1.  \fRProject organization

.NL
.KE
.PP
SPMS can be fitted to existing software systems. It retains the full
capabilities of the
.UX
environment with unrestricted access to
.UX
tools. As a result, software packages developed using SPMS do not depend
on the system for their survival and can be ported to versions of
.UX
that do not support SPMS.


