.ds lq ``
.ds rq ''
.\"  Local Section Header
.de sh
.ne 8
.sp 2
.LP
.B
.LG
.ce 1
\\$1
.R
.NL
.LP
.sp
..
.TL
Hints on Configuring VAX* Systems for UNIX\(dg
.sp 1
Revised for 4.2BSD: March 15, 1983
.AU
Bob Kridle
.AI
Computer Systems Support Group
U. C. Berkeley
kridle@berkeley, ucbvax!kridle
.AU
Sam Leffler
.AI
Computer Systems Research Group
U. C. Berkeley
sam@berkeley, ucbvax!sam
.AB
.LP
.ds LH Introduction
.ds RH "\(co rjk/sjl March, 1983
This document reflects our experiences and opinions in configuring over
thirty VAXes to run UNIX\(dg over the last five years.
.FS
\(dg UNIX is a trademark of Bell Laboratories.
.FE
.FS
* VAX, VMS, MASSBUS, and UNIBUS are trademarks of Digital Equipment Corporation.
.FE
.LP
Our prime considerations in choosing equipment are:
.IP \(bu
Cost
.ns
.IP \(bu
Performance
.ns
.IP \(bu
Reliability
.ns
.IP \(bu
Maintainability and maintenance cost
.ns
.IP \(bu
Delivery time
.ns
.IP \(bu
Redundancy of the system
.ns
.IP \(bu
Conservation of space, power, and cooling resources
.LP
We consider components individually and then describe several
system packages built from these components, emphasizing
independently single-source systems, minimization of cost, and
maximal expansion capability.
.br
.sp 1
.LP
Copyright \(co 1983, Bob Kridle and Sam Leffler. 
Copying in whole for personal
use by sites configuring UNIX systems is permitted.  Reproduction
in whole or in part for other purposes is permitted only with the express
written consent of the authors.  This paper is based on an earlier
paper of the same name authored by Bob Kridle and Bill Joy.
.AE
.LP
.bp
